(** QCheck property-based tests for CodeView type and symbol records. *)

module Buffer = Stdlib.Buffer

open Test_support

let u32 n = Unsigned.UInt32.of_int n

(** Default per-property iteration count. Override with the
    [QCHECK_COUNT] environment variable to make the suite faster or
    more thorough without editing each test. *)
let default_count =
  match Sys.getenv_opt "QCHECK_COUNT" with
  | Some s -> ( try int_of_string s with _ -> 200)
  | None -> 200

let q_test ?(count = default_count) name gen f =
  QCheck.Test.make ~name ~count gen f

(** {2 Generators} *)

(** Generate a non-negative u32 value *)
let gen_u32 =
  QCheck.Gen.(map (fun n -> u32 (abs n mod 0xFFFFFF)) int)

(** Generate a Type_index.t value *)
let gen_type_index =
  QCheck.Gen.(map Pdb.Type_index.of_u32 gen_u32)

(** Generate a simple printable name (no nulls) *)
let gen_name =
  QCheck.Gen.(
    let+ len = int_range 1 20 and+ base = int_range 97 122 in
    String.init len (fun i -> Char.chr (((base + i) mod 26) + 97)))

(** Generate an int64 in the numeric leaf range *)
let gen_numeric_leaf =
  QCheck.Gen.(
    oneof_weighted
      [
        (3, map Int64.of_int (int_range 0 0x7FFF)); (* literal *)
        (1, map Int64.of_int (int_range (-128) (-1))); (* LF_CHAR *)
        (1, map Int64.of_int (int_range (-32768) (-129))); (* LF_SHORT *)
        (1, map Int64.of_int (int_range 0x8000 0xFFFF)); (* LF_USHORT *)
        (1, map Int64.of_int (int_range 0x10000 0x7FFFFFFF)); (* LF_ULONG *)
        (1, map Int64.of_int (int_range (-100000) (-32769))); (* LF_LONG *)
      ])

(** {2 Numeric Leaf Round-trip} *)

let test_numeric_leaf_roundtrip =
  q_test "numeric leaf roundtrip" ~count:500
    (QCheck.make gen_numeric_leaf)
    (fun v ->
      let buf = Buffer.create 16 in
      Pdb.Codeview_types.write_numeric_leaf buf v;
      let bytes = Buffer.contents buf in
      let obj_buf = buffer_of_string bytes in
      let cur = Object.Buffer.cursor obj_buf in
      let result = Pdb.Codeview_types.parse_numeric_leaf cur in
      result = v)

(** {2 Type Properties Round-trip} *)

let gen_type_properties =
  QCheck.Gen.(map (fun n -> n land 0x0FFF) (int_range 0 0x0FFF))

let test_type_properties_roundtrip =
  q_test "type_properties roundtrip" ~count:500
    (QCheck.make gen_type_properties)
    (fun bits ->
      let props = Pdb.Codeview_types.parse_type_properties bits in
      let bits' = Pdb.Codeview_types.int_of_type_properties props in
      bits = bits')

(** {2 Type Record Round-trips}

    For each record kind, generate random data, write it, read it back,
    and check the key fields match. *)

let type_record_roundtrip record =
  let buf = Buffer.create 64 in
  Pdb.Codeview_types.write_type_record buf record;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let rec_len = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int in
  Pdb.Codeview_types.parse_type_record cur rec_len

let test_modifier_roundtrip =
  q_test "Modifier roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ modified_type = gen_type_index and+ modifiers = int_range 0 7 in
         (modified_type, modifiers)))
    (fun (modified_type, modifiers) ->
      let r =
        type_record_roundtrip
          (Pdb.Codeview_types.Modifier { modified_type; modifiers })
      in
      match r with
      | Pdb.Codeview_types.Modifier { modified_type = mt; modifiers = m } ->
          mt = modified_type && m = modifiers
      | _ -> false)

let test_pointer_roundtrip =
  q_test "Pointer roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ pointee_type = gen_type_index and+ attrs = gen_u32 in
         (pointee_type, attrs)))
    (fun (pointee_type, attrs) ->
      let r =
        type_record_roundtrip
          (Pdb.Codeview_types.Pointer { pointee_type; attrs })
      in
      match r with
      | Pdb.Codeview_types.Pointer { pointee_type = pt; attrs = a } ->
          pt = pointee_type && Unsigned.UInt32.equal a attrs
      | _ -> false)

let test_arglist_roundtrip =
  q_test "ArgList roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ count = int_range 0 10 in
         Array.init count (fun i -> Pdb.Type_index.of_u32 (u32 (0x1000 + i)))))
    (fun args ->
      let r =
        type_record_roundtrip (Pdb.Codeview_types.ArgList { args })
      in
      match r with
      | Pdb.Codeview_types.ArgList { args = args' } ->
          Array.length args = Array.length args'
          && Array.for_all2 (=) args args'
      | _ -> false)

let test_procedure_roundtrip =
  q_test "Procedure roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ return_type = gen_type_index
         and+ param_count = int_range 0 20
         and+ arg_list = gen_type_index in
         (return_type, param_count, arg_list)))
    (fun (return_type, param_count, arg_list) ->
      let r =
        type_record_roundtrip
          (Pdb.Codeview_types.Procedure
             {
               return_type;
               calling_conv = Pdb.Codeview_constants.NearC;
      options = 0;
               param_count;
               arg_list;
             })
      in
      match r with
      | Pdb.Codeview_types.Procedure { return_type = rt; param_count = pc; arg_list = al; _ }
        ->
          rt = return_type
          && pc = param_count
          && al = arg_list
      | _ -> false)

let test_bitfield_roundtrip =
  q_test "Bitfield roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ underlying_type = gen_type_index
         and+ length = int_range 0 31
         and+ position = int_range 0 31 in
         (underlying_type, length, position)))
    (fun (underlying_type, length, position) ->
      let r =
        type_record_roundtrip
          (Pdb.Codeview_types.Bitfield { underlying_type; length; position })
      in
      match r with
      | Pdb.Codeview_types.Bitfield
          { underlying_type = ut; length = l; position = p } ->
          ut = underlying_type && l = length && p = position
      | _ -> false)

let test_func_id_roundtrip =
  q_test "FuncId roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ scope_id = gen_type_index and+ func_type = gen_type_index and+ name = gen_name in
         (scope_id, func_type, name)))
    (fun (scope_id, func_type, name) ->
      let r =
        type_record_roundtrip
          (Pdb.Codeview_types.FuncId { scope_id; func_type; name })
      in
      match r with
      | Pdb.Codeview_types.FuncId { scope_id = si; func_type = ft; name = n }
        ->
          si = scope_id
          && ft = func_type
          && n = name
      | _ -> false)

let test_string_id_roundtrip =
  q_test "StringId roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ id = gen_type_index and+ str = gen_name in
         (id, str)))
    (fun (id, str) ->
      let r =
        type_record_roundtrip (Pdb.Codeview_types.StringId { id; str })
      in
      match r with
      | Pdb.Codeview_types.StringId { id = i; str = s } ->
          i = id && s = str
      | _ -> false)

let test_enum_roundtrip =
  q_test "Enum roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ field_count = int_range 0 50
         and+ underlying_type = gen_type_index
         and+ field_list = gen_type_index
         and+ name = gen_name in
         (field_count, underlying_type, field_list, name)))
    (fun (field_count, underlying_type, field_list, name) ->
      let r =
        type_record_roundtrip
          (Pdb.Codeview_types.Enum
             {
               field_count;
               properties = Pdb.Codeview_types.parse_type_properties 0;
               underlying_type;
               field_list;
               name;
               unique_name = Option.None;
             })
      in
      match r with
      | Pdb.Codeview_types.Enum { field_count = fc; name = n; _ } ->
          fc = field_count && n = name
      | _ -> false)

(** Generate a calling convention *)
let gen_calling_conv =
  QCheck.Gen.(
    oneof_list
      Pdb.Codeview_constants.
        [ NearC; ThisCall; NearStdCall; NearFast; Generic ])

let test_mfunction_roundtrip =
  q_test "MFunction roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ return_type = gen_type_index
         and+ class_type = gen_type_index
         and+ this_type = gen_type_index
         and+ calling_conv = gen_calling_conv
         and+ param_count = int_range 0 20
         and+ arg_list = gen_type_index
         and+ this_adjust = map Int32.of_int (int_range (-100) 100) in
         (return_type, class_type, this_type, calling_conv, param_count,
          arg_list, this_adjust)))
    (fun (return_type, class_type, this_type, calling_conv, param_count,
          arg_list, this_adjust) ->
      let r =
        type_record_roundtrip
          (Pdb.Codeview_types.MFunction
             { return_type; class_type; this_type; calling_conv;
               options = 0;
               param_count; arg_list; this_adjust })
      in
      match r with
      | Pdb.Codeview_types.MFunction
          { return_type = rt; class_type = ct; this_type = tt;
            param_count = pc; arg_list = al; this_adjust = ta; _ } ->
          rt = return_type
          && ct = class_type
          && tt = this_type
          && pc = param_count
          && al = arg_list
          && ta = this_adjust
      | _ -> false)

let test_array_roundtrip =
  q_test "Array roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ element_type = gen_type_index
         and+ index_type = gen_type_index
         and+ size = gen_numeric_leaf
         and+ name = gen_name in
         (element_type, index_type, size, name)))
    (fun (element_type, index_type, size, name) ->
      let size = Int64.abs size in (* array sizes are non-negative *)
      let r =
        type_record_roundtrip
          (Pdb.Codeview_types.Array { element_type; index_type; size; name })
      in
      match r with
      | Pdb.Codeview_types.Array
          { element_type = et; index_type = it; size = s; name = n } ->
          et = element_type
          && it = index_type
          && s = size && n = name
      | _ -> false)

let test_class_roundtrip =
  q_test "Class roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ field_count = int_range 0 50
         and+ field_list = gen_type_index
         and+ derived_from = gen_type_index
         and+ vtable_shape = gen_type_index
         and+ size = map (fun n -> Int64.of_int (abs n mod 10000)) int
         and+ name = gen_name in
         (field_count, field_list, derived_from, vtable_shape, size, name)))
    (fun (field_count, field_list, derived_from, vtable_shape, size, name) ->
      let r =
        type_record_roundtrip
          (Pdb.Codeview_types.Class
             { field_count;
               properties = Pdb.Codeview_types.parse_type_properties 0;
               field_list; derived_from; vtable_shape; size; name;
               unique_name = Option.None })
      in
      match r with
      | Pdb.Codeview_types.Class { field_count = fc; size = s; name = n; _ } ->
          fc = field_count && s = size && n = name
      | _ -> false)

let test_structure_roundtrip =
  q_test "Structure roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ field_count = int_range 0 50
         and+ field_list = gen_type_index
         and+ size = map (fun n -> Int64.of_int (abs n mod 10000)) int
         and+ name = gen_name in
         (field_count, field_list, size, name)))
    (fun (field_count, field_list, size, name) ->
      let r =
        type_record_roundtrip
          (Pdb.Codeview_types.Structure
             { field_count;
               properties = Pdb.Codeview_types.parse_type_properties 0;
               field_list; derived_from = Pdb.Type_index.of_u32 (u32 0); vtable_shape = Pdb.Type_index.of_u32 (u32 0);
               size; name; unique_name = Option.None })
      in
      match r with
      | Pdb.Codeview_types.Structure { field_count = fc; size = s; name = n; _ } ->
          fc = field_count && s = size && n = name
      | _ -> false)

let test_union_roundtrip =
  q_test "Union roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ field_count = int_range 0 50
         and+ field_list = gen_type_index
         and+ size = map (fun n -> Int64.of_int (abs n mod 10000)) int
         and+ name = gen_name in
         (field_count, field_list, size, name)))
    (fun (field_count, field_list, size, name) ->
      let r =
        type_record_roundtrip
          (Pdb.Codeview_types.Union
             { field_count;
               properties = Pdb.Codeview_types.parse_type_properties 0;
               field_list; size; name; unique_name = Option.None })
      in
      match r with
      | Pdb.Codeview_types.Union { field_count = fc; size = s; name = n; _ } ->
          fc = field_count && s = size && n = name
      | _ -> false)

let test_vtshape_roundtrip =
  q_test "VTShape roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ count = int_range 0 16 in
         Array.init count (fun i -> i mod 5)))
    (fun descriptors ->
      let r =
        type_record_roundtrip
          (Pdb.Codeview_types.VTShape { descriptors })
      in
      match r with
      | Pdb.Codeview_types.VTShape { descriptors = d } ->
          Array.length d = Array.length descriptors
          && Array.for_all2 ( = ) d descriptors
      | _ -> false)

let test_methodlist_roundtrip =
  q_test "MethodList roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ count = int_range 1 5 in
         (* Generate only vanilla methods (no vftable offset) to keep it simple.
            Method kind is in bits 2-4 of attrs. Vanilla = 0, so attrs with
            kind bits = 0 means no vftable offset. *)
         List.init count (fun i -> (3, Pdb.Type_index.of_u32 (u32 (0x1000 + i)), (Option.None : int option)))))
    (fun entries ->
      let r =
        type_record_roundtrip
          (Pdb.Codeview_types.MethodList { entries })
      in
      match r with
      | Pdb.Codeview_types.MethodList { entries = entries' } ->
          List.length entries = List.length entries'
          && List.for_all2
               (fun (a1, t1, v1) (a2, t2, v2) ->
                 a1 = a2 && t1 = t2 && v1 = v2)
               entries entries'
      | _ -> false)

let test_mfunc_id_roundtrip =
  q_test "MFuncId roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ parent_type = gen_type_index
         and+ func_type = gen_type_index
         and+ name = gen_name in
         (parent_type, func_type, name)))
    (fun (parent_type, func_type, name) ->
      let r =
        type_record_roundtrip
          (Pdb.Codeview_types.MFuncId { parent_type; func_type; name })
      in
      match r with
      | Pdb.Codeview_types.MFuncId
          { parent_type = pt; func_type = ft; name = n } ->
          pt = parent_type
          && ft = func_type
          && n = name
      | _ -> false)

let test_buildinfo_type_roundtrip =
  q_test "BuildInfo (type) roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ count = int_range 0 6 in
         Array.init count (fun i -> Pdb.Type_index.of_u32 (u32 (0x1000 + i)))))
    (fun args ->
      let r =
        type_record_roundtrip
          (Pdb.Codeview_types.BuildInfo { args })
      in
      match r with
      | Pdb.Codeview_types.BuildInfo { args = args' } ->
          Array.length args = Array.length args'
          && Array.for_all2 (=) args args'
      | _ -> false)

let test_udt_mod_src_line_roundtrip =
  q_test "UdtModSrcLine roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ udt = gen_type_index
         and+ source = gen_type_index
         and+ line = gen_u32
         and+ module_ = int_range 0 100 in
         (udt, source, line, module_)))
    (fun (udt, source, line, module_) ->
      let r =
        type_record_roundtrip
          (Pdb.Codeview_types.UdtModSrcLine { udt; source; line; module_ })
      in
      match r with
      | Pdb.Codeview_types.UdtModSrcLine
          { udt = u; source = s; line = l; module_ = m } ->
          u = udt
          && s = source
          && Unsigned.UInt32.equal l line
          && m = module_
      | _ -> false)

let test_substr_list_roundtrip =
  q_test "SubstrList roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ count = int_range 0 8 in
         Array.init count (fun i -> Pdb.Type_index.of_u32 (u32 (0x1000 + i)))))
    (fun strings ->
      let r =
        type_record_roundtrip
          (Pdb.Codeview_types.SubstrList { strings })
      in
      match r with
      | Pdb.Codeview_types.SubstrList { strings = s } ->
          Array.length s = Array.length strings
          && Array.for_all2 (=) s strings
      | _ -> false)

(** {2 Symbol Record Round-trips} *)

let symbol_record_roundtrip record =
  let buf = Buffer.create 64 in
  Pdb.Codeview_symbols.write_symbol_record buf record;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let rec_len = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int in
  Pdb.Codeview_symbols.parse_symbol_record cur rec_len

let test_objname_roundtrip =
  q_test "ObjName roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ signature = gen_u32 and+ name = gen_name in
         (signature, name)))
    (fun (signature, name) ->
      let r =
        symbol_record_roundtrip
          (Pdb.Codeview_symbols.ObjName { signature; name })
      in
      match r with
      | Pdb.Codeview_symbols.ObjName { signature = s; name = n } ->
          Unsigned.UInt32.equal s signature && n = name
      | _ -> false)

let test_pub32_roundtrip =
  q_test "Pub32 roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ flags = gen_u32
         and+ offset = gen_u32
         and+ segment = int_range 1 10
         and+ name = gen_name in
         (flags, offset, segment, name)))
    (fun (flags, offset, segment, name) ->
      let r =
        symbol_record_roundtrip
          (Pdb.Codeview_symbols.Pub32 { flags; offset; segment; name })
      in
      match r with
      | Pdb.Codeview_symbols.Pub32
          { flags = f; offset = o; segment = s; name = n } ->
          Unsigned.UInt32.equal f flags
          && Unsigned.UInt32.equal o offset
          && s = segment && n = name
      | _ -> false)

let test_udt_roundtrip =
  q_test "Udt roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ type_index = gen_type_index and+ name = gen_name in
         (type_index, name)))
    (fun (type_index, name) ->
      let r =
        symbol_record_roundtrip
          (Pdb.Codeview_symbols.Udt { type_index; name })
      in
      match r with
      | Pdb.Codeview_symbols.Udt { type_index = ti; name = n } ->
          ti = type_index && n = name
      | _ -> false)

let test_constant_roundtrip =
  q_test "Constant roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ type_index = gen_type_index
         and+ value = gen_numeric_leaf
         and+ name = gen_name in
         (type_index, value, name)))
    (fun (type_index, value, name) ->
      let r =
        symbol_record_roundtrip
          (Pdb.Codeview_symbols.Constant { type_index; value; name })
      in
      match r with
      | Pdb.Codeview_symbols.Constant { type_index = ti; value = v; name = n }
        ->
          ti = type_index && v = value && n = name
      | _ -> false)

let test_bprel32_roundtrip =
  q_test "BPRel32 roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ offset = map Int32.of_int (int_range (-1000) 1000)
         and+ type_index = gen_type_index
         and+ name = gen_name in
         (offset, type_index, name)))
    (fun (offset, type_index, name) ->
      let r =
        symbol_record_roundtrip
          (Pdb.Codeview_symbols.BPRel32 { offset; type_index; name })
      in
      match r with
      | Pdb.Codeview_symbols.BPRel32
          { offset = o; type_index = ti; name = n } ->
          o = offset && ti = type_index && n = name
      | _ -> false)

let test_local_roundtrip =
  q_test "Local roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ type_index = gen_type_index
         and+ flags = int_range 0 0xFFFF
         and+ name = gen_name in
         (type_index, flags, name)))
    (fun (type_index, flags, name) ->
      let r =
        symbol_record_roundtrip
          (Pdb.Codeview_symbols.Local { type_index; flags; name })
      in
      match r with
      | Pdb.Codeview_symbols.Local { type_index = ti; flags = f; name = n }
        ->
          ti = type_index && f = flags && n = name
      | _ -> false)

let gen_proc_record =
  QCheck.Gen.(
    let+ code_size = int_range 1 1000
    and+ type_index = gen_type_index
    and+ offset = gen_u32
    and+ segment = int_range 1 5
    and+ name = gen_name in
    {
      Pdb.Codeview_symbols.parent = u32 0;
      end_ = u32 0;
      next = u32 0;
      code_size = u32 code_size;
      debug_start = u32 0;
      debug_end = u32 (code_size - 1);
      type_index;
      offset;
      segment;
      flags = 0;
      name;
    })

let check_proc_roundtrip name constructor extractor =
  QCheck.Test.make ~name ~count:200
    (QCheck.make gen_proc_record)
    (fun proc ->
      let r = symbol_record_roundtrip (constructor proc) in
      match extractor r with
      | Some (p : Pdb.Codeview_symbols.proc_record) ->
          p.name = proc.name
          && Unsigned.UInt32.equal p.code_size proc.code_size
          && p.segment = proc.segment
      | Option.None -> false)

let test_qc_lproc32 =
  check_proc_roundtrip "LProc32 roundtrip"
    (fun p -> Pdb.Codeview_symbols.LProc32 p)
    (function Pdb.Codeview_symbols.LProc32 p -> Some p | _ -> Option.None)

let test_qc_gproc32id =
  check_proc_roundtrip "GProc32Id roundtrip"
    (fun p -> Pdb.Codeview_symbols.GProc32Id p)
    (function Pdb.Codeview_symbols.GProc32Id p -> Some p | _ -> Option.None)

let test_qc_lproc32id =
  check_proc_roundtrip "LProc32Id roundtrip"
    (fun p -> Pdb.Codeview_symbols.LProc32Id p)
    (function Pdb.Codeview_symbols.LProc32Id p -> Some p | _ -> Option.None)

let test_qc_gthread32 =
  q_test "GThread32 roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ type_index = gen_type_index
         and+ offset = gen_u32
         and+ segment = int_range 1 5
         and+ name = gen_name in
         (type_index, offset, segment, name)))
    (fun (type_index, offset, segment, name) ->
      let r =
        symbol_record_roundtrip
          (Pdb.Codeview_symbols.GThread32 { type_index; offset; segment; name })
      in
      match r with
      | Pdb.Codeview_symbols.GThread32 d ->
          d.type_index = type_index && d.name = name
      | _ -> false)

let test_qc_lthread32 =
  q_test "LThread32 roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ type_index = gen_type_index
         and+ offset = gen_u32
         and+ segment = int_range 1 5
         and+ name = gen_name in
         (type_index, offset, segment, name)))
    (fun (type_index, offset, segment, name) ->
      let r =
        symbol_record_roundtrip
          (Pdb.Codeview_symbols.LThread32 { type_index; offset; segment; name })
      in
      match r with
      | Pdb.Codeview_symbols.LThread32 d ->
          d.type_index = type_index && d.name = name
      | _ -> false)

let test_qc_regrel32 =
  q_test "RegRel32 roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ offset = map Int32.of_int (int_range (-1000) 1000)
         and+ type_index = gen_type_index
         and+ register = int_range 0 400
         and+ name = gen_name in
         (offset, type_index, register, name)))
    (fun (offset, type_index, register, name) ->
      let r =
        symbol_record_roundtrip
          (Pdb.Codeview_symbols.RegRel32
             { offset; type_index; register; name })
      in
      match r with
      | Pdb.Codeview_symbols.RegRel32
          { offset = o; type_index = ti; register = reg; name = n } ->
          o = offset && ti = type_index
          && reg = register && n = name
      | _ -> false)

let test_qc_register =
  q_test "Register roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ type_index = gen_type_index
         and+ register = int_range 0 400
         and+ name = gen_name in
         (type_index, register, name)))
    (fun (type_index, register, name) ->
      let r =
        symbol_record_roundtrip
          (Pdb.Codeview_symbols.Register { type_index; register; name })
      in
      match r with
      | Pdb.Codeview_symbols.Register
          { type_index = ti; register = reg; name = n } ->
          ti = type_index && reg = register && n = name
      | _ -> false)

let test_qc_label32 =
  q_test "Label32 roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ offset = gen_u32
         and+ segment = int_range 1 5
         and+ name = gen_name in
         (offset, segment, name)))
    (fun (offset, segment, name) ->
      let r =
        symbol_record_roundtrip
          (Pdb.Codeview_symbols.Label32
             { offset; segment; flags = 0; name })
      in
      match r with
      | Pdb.Codeview_symbols.Label32
          { offset = o; segment = s; name = n; _ } ->
          Unsigned.UInt32.equal o offset && s = segment && n = name
      | _ -> false)

let test_qc_ldata32 =
  q_test "LData32 roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ type_index = gen_type_index
         and+ offset = gen_u32
         and+ segment = int_range 1 5
         and+ name = gen_name in
         (type_index, offset, segment, name)))
    (fun (type_index, offset, segment, name) ->
      let r =
        symbol_record_roundtrip
          (Pdb.Codeview_symbols.LData32 { type_index; offset; segment; name })
      in
      match r with
      | Pdb.Codeview_symbols.LData32 d ->
          d.type_index = type_index && d.name = name
      | _ -> false)

let test_qc_gdata32 =
  q_test "GData32 roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ type_index = gen_type_index
         and+ offset = gen_u32
         and+ segment = int_range 1 5
         and+ name = gen_name in
         (type_index, offset, segment, name)))
    (fun (type_index, offset, segment, name) ->
      let r =
        symbol_record_roundtrip
          (Pdb.Codeview_symbols.GData32 { type_index; offset; segment; name })
      in
      match r with
      | Pdb.Codeview_symbols.GData32 d ->
          d.type_index = type_index && d.name = name
      | _ -> false)

let test_qc_block32 =
  q_test "Block32 roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ length = int_range 1 1000
         and+ offset = gen_u32
         and+ segment = int_range 1 5
         and+ name = gen_name in
         (length, offset, segment, name)))
    (fun (length, offset, segment, name) ->
      let r =
        symbol_record_roundtrip
          (Pdb.Codeview_symbols.Block32
             { parent = u32 0; end_ = u32 0; length = u32 length;
               offset; segment; name })
      in
      match r with
      | Pdb.Codeview_symbols.Block32
          { length = l; offset = o; segment = s; name = n; _ } ->
          Unsigned.UInt32.to_int l = length
          && Unsigned.UInt32.equal o offset
          && s = segment && n = name
      | _ -> false)

let test_qc_frameproc =
  q_test "FrameProc roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ total = int_range 0 1000
         and+ callee_saved = int_range 0 64
         and+ flags = int_range 0 0xFFFFFF in
         (total, callee_saved, flags)))
    (fun (total, callee_saved, flags) ->
      let r =
        symbol_record_roundtrip
          (Pdb.Codeview_symbols.FrameProc
             {
               total_frame_bytes = u32 total;
               padding_frame_bytes = u32 0;
               offset_to_padding = u32 0;
               callee_saved_reg_bytes = u32 callee_saved;
               exception_handler_offset = u32 0;
               exception_handler_section = 0;
               frame_proc_flags = u32 flags;
             })
      in
      match r with
      | Pdb.Codeview_symbols.FrameProc
          { total_frame_bytes; callee_saved_reg_bytes; frame_proc_flags; _ } ->
          Unsigned.UInt32.to_int total_frame_bytes = total
          && Unsigned.UInt32.to_int callee_saved_reg_bytes = callee_saved
          && Unsigned.UInt32.to_int frame_proc_flags = flags
      | _ -> false)

(** {2 Type-record holes} *)

let test_fieldlist_roundtrip =
  q_test "FieldList roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ count = int_range 1 4 in
         List.init count (fun i ->
             Pdb.Codeview_types.Member
               {
                 attrs = i land 0xFFFF;
                 field_type = Pdb.Type_index.of_u32 (u32 (0x1000 + i));
                 offset = Int64.of_int (i * 16);
                 name = Printf.sprintf "f%d" i;
               })))
    (fun members ->
      let r =
        type_record_roundtrip (Pdb.Codeview_types.FieldList { members })
      in
      match r with
      | Pdb.Codeview_types.FieldList { members = members' } -> members = members'
      | _ -> false)

let test_interface_roundtrip =
  q_test "Interface roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ field_count = int_range 0 50
         and+ field_list = gen_type_index
         and+ size = map (fun n -> Int64.of_int (abs n mod 10000)) int
         and+ name = gen_name in
         Pdb.Codeview_types.Interface
           {
             field_count;
             properties = Pdb.Codeview_types.parse_type_properties 0;
             field_list;
             derived_from = Pdb.Type_index.of_u32 (u32 0);
             vtable_shape = Pdb.Type_index.of_u32 (u32 0);
             size;
             name;
             unique_name = Option.None;
           }))
    (fun input -> type_record_roundtrip input = input)

let test_udt_src_line_roundtrip =
  q_test "UdtSrcLine roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ udt = gen_type_index
         and+ source = gen_type_index
         and+ line = gen_u32 in
         Pdb.Codeview_types.UdtSrcLine { udt; source; line }))
    (fun input -> type_record_roundtrip input = input)

let gen_guid =
  QCheck.Gen.(
    let+ data1 = gen_u32
    and+ data2 = int_range 0 0xFFFF
    and+ data3 = int_range 0 0xFFFF
    and+ data4 = string_size ~gen:(int_range 0 255 >|= Char.chr) (return 8) in
    {
      Pdb.Pdb_types.data1;
      data2 = Unsigned.UInt16.of_int data2;
      data3 = Unsigned.UInt16.of_int data3;
      data4;
    })

let test_typeserver2_roundtrip =
  q_test "TypeServer2 roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ guid = gen_guid and+ age = gen_u32 and+ name = gen_name in
         Pdb.Codeview_types.TypeServer2 { guid; age; name }))
    (fun input -> type_record_roundtrip input = input)

(** Type-record [Unknown] is lossy if the payload ends in a 0xf0..0xff
    byte (the type-record parser strips trailing padding bytes). Generate
    payloads that end in a low ASCII byte so the round-trip is exact. *)
let test_unknown_type_record_roundtrip =
  q_test "Unknown type record roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ kind = int_range 0x9000 0x9FFF
         and+ len = int_range 0 32 in
         let data =
           String.init len (fun i -> Char.chr (((i * 7) mod 64) + 32))
         in
         (kind, data)))
    (fun (kind, data) ->
      let r =
        type_record_roundtrip (Pdb.Codeview_types.Unknown { kind; data })
      in
      match r with
      | Pdb.Codeview_types.Unknown { kind = k; data = d } ->
          k = kind && d = data
      | _ -> false)

(** {2 Symbol-record holes} *)

let gen_version_quad =
  QCheck.Gen.(
    let+ a = int_range 0 0xFFFF
    and+ b = int_range 0 0xFFFF
    and+ c = int_range 0 0xFFFF
    and+ d = int_range 0 0xFFFF in
    (a, b, c, d))

let test_compile3_roundtrip =
  q_test "Compile3 roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ flags = gen_u32
         and+ machine = int_range 0 0xFFFF
         and+ frontend_version = gen_version_quad
         and+ backend_version = gen_version_quad
         and+ version_string = gen_name in
         Pdb.Codeview_symbols.Compile3
           {
             flags;
             machine;
             frontend_version;
             backend_version;
             version_string;
           }))
    (fun input -> symbol_record_roundtrip input = input)

let test_buildinfo_sym_roundtrip =
  q_test "BuildInfo (sym) roundtrip"
    (QCheck.make
       QCheck.Gen.(map (fun id -> Pdb.Codeview_symbols.BuildInfo { id })
                     gen_type_index))
    (fun input -> symbol_record_roundtrip input = input)

(** [InlineSite.annotations] is parsed by stripping trailing null bytes.
    Generate payloads ending in a non-null byte so the round-trip is
    exact. *)
let test_inlinesite_roundtrip =
  q_test "InlineSite roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ parent = gen_u32
         and+ end_ = gen_u32
         and+ inlinee = gen_type_index
         and+ len = int_range 1 16 in
         let annotations =
           String.init len (fun i -> Char.chr (((i * 5) mod 64) + 32))
         in
         Pdb.Codeview_symbols.InlineSite { parent; end_; inlinee; annotations }))
    (fun input -> symbol_record_roundtrip input = input)

let test_unamespace_roundtrip =
  q_test "UNamespace roundtrip"
    (QCheck.make
       QCheck.Gen.(map (fun name -> Pdb.Codeview_symbols.UNamespace { name })
                     gen_name))
    (fun input -> symbol_record_roundtrip input = input)

(** [EnvBlock] parser drops empty strings, so generate only non-empty
    fields here. *)
let test_envblock_roundtrip =
  q_test "EnvBlock roundtrip"
    (QCheck.make
       QCheck.Gen.(
         list_size (int_range 0 6)
           (let+ i = int_range 1 999 in
            Printf.sprintf "key%d=value%d" i (i * 2))))
    (fun fields ->
      symbol_record_roundtrip (Pdb.Codeview_symbols.EnvBlock { fields })
      = Pdb.Codeview_symbols.EnvBlock { fields })

(** Symbol [Unknown]: the writer pads the record to 4-byte alignment with
    null bytes, and the parser treats the entire post-kind region as
    [data]. To avoid the alignment padding appearing in the parsed [data],
    generate payload lengths that are multiples of 4 (so the writer adds
    zero padding bytes). *)
let test_unknown_symbol_record_roundtrip =
  q_test "Unknown symbol record roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ kind = int_range 0x4000 0x4FFF
         and+ k = int_range 0 8 in
         let len = k * 4 in
         let data =
           String.init len (fun i -> Char.chr (((i * 3) mod 64) + 32))
         in
         (kind, data)))
    (fun (kind, data) ->
      let r =
        symbol_record_roundtrip (Pdb.Codeview_symbols.Unknown { kind; data })
      in
      match r with
      | Pdb.Codeview_symbols.Unknown { kind = k; data = d } ->
          k = kind && d = data
      | _ -> false)

(** {3 DefRange* family} *)

let gen_range_triple =
  QCheck.Gen.(
    let+ range_offset = gen_u32
    and+ range_section = int_range 0 0xFFFF
    and+ range_length = int_range 0 0xFFFF in
    (range_offset, range_section, range_length))

let gen_i32_offset = QCheck.Gen.(map Int32.of_int (int_range (-1000) 1000))

let test_defrange_fp_rel_roundtrip =
  q_test "DefRangeFramePointerRel roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ offset = gen_i32_offset
         and+ (range_offset, range_section, range_length) = gen_range_triple in
         Pdb.Codeview_symbols.DefRangeFramePointerRel
           { offset; range_offset; range_section; range_length }))
    (fun input -> symbol_record_roundtrip input = input)

let test_defrange_register_rel_roundtrip =
  q_test "DefRangeRegisterRel roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ base_register = int_range 0 0xFFFF
         and+ offset = gen_i32_offset
         and+ (range_offset, range_section, range_length) = gen_range_triple in
         Pdb.Codeview_symbols.DefRangeRegisterRel
           {
             base_register;
             offset;
             range_offset;
             range_section;
             range_length;
           }))
    (fun input -> symbol_record_roundtrip input = input)

let test_defrange_register_roundtrip =
  q_test "DefRangeRegister roundtrip"
    (QCheck.make
       QCheck.Gen.(
         let+ register = int_range 0 0xFFFF
         and+ may_have_no_name = int_range 0 0xFFFF
         and+ (range_offset, range_section, range_length) = gen_range_triple in
         Pdb.Codeview_symbols.DefRangeRegister
           {
             register;
             may_have_no_name;
             range_offset;
             range_section;
             range_length;
           }))
    (fun input -> symbol_record_roundtrip input = input)

let test_defrange_fp_rel_full_scope_roundtrip =
  q_test "DefRangeFramePointerRelFullScope roundtrip"
    (QCheck.make
       QCheck.Gen.(
         map
           (fun offset ->
             Pdb.Codeview_symbols.DefRangeFramePointerRelFullScope { offset })
           gen_i32_offset))
    (fun input -> symbol_record_roundtrip input = input)

(** {2 Named Stream Map Round-trip} *)

let test_named_stream_map_roundtrip =
  q_test "named stream map roundtrip" ~count:100
    (QCheck.make
       QCheck.Gen.(
         let+ count = int_range 0 5 in
         List.init count (fun i ->
             let name = Printf.sprintf "/stream%d" i in
             (name, i + 1))))
    (fun entries ->
      let buf = Buffer.create 128 in
      Pdb.Named_stream_map.write buf entries;
      let bytes = Buffer.contents buf in
      let obj_buf = buffer_of_string bytes in
      let cur = Object.Buffer.cursor obj_buf in
      let result = Pdb.Named_stream_map.parse cur in
      List.length result = List.length entries
      && List.for_all
           (fun (name, idx) ->
             List.exists (fun (n, i) -> n = name && i = idx) result)
           entries)

(** {2 Test runner} *)

let () =
  let open Alcotest in
  run "CodeView QCheck"
    [
      ( "numeric_leaf",
        [ QCheck_alcotest.to_alcotest test_numeric_leaf_roundtrip ] );
      ( "type_properties",
        [ QCheck_alcotest.to_alcotest test_type_properties_roundtrip ] );
      ( "type_records",
        List.map QCheck_alcotest.to_alcotest
          [
            test_modifier_roundtrip;
            test_pointer_roundtrip;
            test_arglist_roundtrip;
            test_procedure_roundtrip;
            test_bitfield_roundtrip;
            test_func_id_roundtrip;
            test_string_id_roundtrip;
            test_enum_roundtrip;
            test_mfunction_roundtrip;
            test_array_roundtrip;
            test_class_roundtrip;
            test_structure_roundtrip;
            test_union_roundtrip;
            test_vtshape_roundtrip;
            test_methodlist_roundtrip;
            test_mfunc_id_roundtrip;
            test_buildinfo_type_roundtrip;
            test_udt_mod_src_line_roundtrip;
            test_substr_list_roundtrip;
            test_fieldlist_roundtrip;
            test_interface_roundtrip;
            test_udt_src_line_roundtrip;
            test_typeserver2_roundtrip;
            test_unknown_type_record_roundtrip;
          ] );
      ( "symbol_records",
        List.map QCheck_alcotest.to_alcotest
          [
            test_objname_roundtrip;
            test_pub32_roundtrip;
            test_udt_roundtrip;
            test_constant_roundtrip;
            test_bprel32_roundtrip;
            test_local_roundtrip;
            test_qc_lproc32;
            test_qc_gproc32id;
            test_qc_lproc32id;
            test_qc_gthread32;
            test_qc_lthread32;
            test_qc_regrel32;
            test_qc_register;
            test_qc_label32;
            test_qc_ldata32;
            test_qc_gdata32;
            test_qc_block32;
            test_qc_frameproc;
            test_compile3_roundtrip;
            test_buildinfo_sym_roundtrip;
            test_inlinesite_roundtrip;
            test_unamespace_roundtrip;
            test_envblock_roundtrip;
            test_unknown_symbol_record_roundtrip;
            test_defrange_fp_rel_roundtrip;
            test_defrange_register_rel_roundtrip;
            test_defrange_register_roundtrip;
            test_defrange_fp_rel_full_scope_roundtrip;
          ] );
      ( "named_stream_map",
        [ QCheck_alcotest.to_alcotest test_named_stream_map_roundtrip ] );
    ]
