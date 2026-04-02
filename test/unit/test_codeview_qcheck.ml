(** QCheck property-based tests for CodeView type and symbol records. *)

module Buffer = Stdlib.Buffer

let buffer_of_string s =
  let len = String.length s in
  let buf =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout len
  in
  for i = 0 to len - 1 do
    buf.{i} <- Char.code s.[i]
  done;
  buf

let u32 n = Unsigned.UInt32.of_int n

(** {2 Generators} *)

(** Generate a non-negative u32 value *)
let gen_u32 =
  QCheck.Gen.(map (fun n -> u32 (abs n mod 0xFFFFFF)) int)

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
  QCheck.Test.make ~name:"numeric leaf roundtrip" ~count:500
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
  QCheck.Test.make ~name:"type_properties roundtrip" ~count:500
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
  QCheck.Test.make ~name:"Modifier roundtrip" ~count:200
    (QCheck.make
       QCheck.Gen.(
         let+ modified_type = gen_u32 and+ modifiers = int_range 0 7 in
         (modified_type, modifiers)))
    (fun (modified_type, modifiers) ->
      let r =
        type_record_roundtrip
          (Pdb.Codeview_types.Modifier { modified_type; modifiers })
      in
      match r with
      | Pdb.Codeview_types.Modifier { modified_type = mt; modifiers = m } ->
          Unsigned.UInt32.equal mt modified_type && m = modifiers
      | _ -> false)

let test_pointer_roundtrip =
  QCheck.Test.make ~name:"Pointer roundtrip" ~count:200
    (QCheck.make
       QCheck.Gen.(
         let+ pointee_type = gen_u32 and+ attrs = gen_u32 in
         (pointee_type, attrs)))
    (fun (pointee_type, attrs) ->
      let r =
        type_record_roundtrip
          (Pdb.Codeview_types.Pointer { pointee_type; attrs })
      in
      match r with
      | Pdb.Codeview_types.Pointer { pointee_type = pt; attrs = a } ->
          Unsigned.UInt32.equal pt pointee_type && Unsigned.UInt32.equal a attrs
      | _ -> false)

let test_arglist_roundtrip =
  QCheck.Test.make ~name:"ArgList roundtrip" ~count:200
    (QCheck.make
       QCheck.Gen.(
         let+ count = int_range 0 10 in
         Array.init count (fun i -> u32 (0x1000 + i))))
    (fun args ->
      let r =
        type_record_roundtrip (Pdb.Codeview_types.ArgList { args })
      in
      match r with
      | Pdb.Codeview_types.ArgList { args = args' } ->
          Array.length args = Array.length args'
          && Array.for_all2 Unsigned.UInt32.equal args args'
      | _ -> false)

let test_procedure_roundtrip =
  QCheck.Test.make ~name:"Procedure roundtrip" ~count:200
    (QCheck.make
       QCheck.Gen.(
         let+ return_type = gen_u32
         and+ param_count = int_range 0 20
         and+ arg_list = gen_u32 in
         (return_type, param_count, arg_list)))
    (fun (return_type, param_count, arg_list) ->
      let r =
        type_record_roundtrip
          (Pdb.Codeview_types.Procedure
             {
               return_type;
               calling_conv = Pdb.Codeview_constants.NearC;
               param_count;
               arg_list;
             })
      in
      match r with
      | Pdb.Codeview_types.Procedure { return_type = rt; param_count = pc; arg_list = al; _ }
        ->
          Unsigned.UInt32.equal rt return_type
          && pc = param_count
          && Unsigned.UInt32.equal al arg_list
      | _ -> false)

let test_bitfield_roundtrip =
  QCheck.Test.make ~name:"Bitfield roundtrip" ~count:200
    (QCheck.make
       QCheck.Gen.(
         let+ underlying_type = gen_u32
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
          Unsigned.UInt32.equal ut underlying_type && l = length && p = position
      | _ -> false)

let test_func_id_roundtrip =
  QCheck.Test.make ~name:"FuncId roundtrip" ~count:200
    (QCheck.make
       QCheck.Gen.(
         let+ scope_id = gen_u32 and+ func_type = gen_u32 and+ name = gen_name in
         (scope_id, func_type, name)))
    (fun (scope_id, func_type, name) ->
      let r =
        type_record_roundtrip
          (Pdb.Codeview_types.FuncId { scope_id; func_type; name })
      in
      match r with
      | Pdb.Codeview_types.FuncId { scope_id = si; func_type = ft; name = n }
        ->
          Unsigned.UInt32.equal si scope_id
          && Unsigned.UInt32.equal ft func_type
          && n = name
      | _ -> false)

let test_string_id_roundtrip =
  QCheck.Test.make ~name:"StringId roundtrip" ~count:200
    (QCheck.make
       QCheck.Gen.(
         let+ id = gen_u32 and+ str = gen_name in
         (id, str)))
    (fun (id, str) ->
      let r =
        type_record_roundtrip (Pdb.Codeview_types.StringId { id; str })
      in
      match r with
      | Pdb.Codeview_types.StringId { id = i; str = s } ->
          Unsigned.UInt32.equal i id && s = str
      | _ -> false)

let test_enum_roundtrip =
  QCheck.Test.make ~name:"Enum roundtrip" ~count:200
    (QCheck.make
       QCheck.Gen.(
         let+ field_count = int_range 0 50
         and+ underlying_type = gen_u32
         and+ field_list = gen_u32
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
  QCheck.Test.make ~name:"ObjName roundtrip" ~count:200
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
  QCheck.Test.make ~name:"Pub32 roundtrip" ~count:200
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
  QCheck.Test.make ~name:"Udt roundtrip" ~count:200
    (QCheck.make
       QCheck.Gen.(
         let+ type_index = gen_u32 and+ name = gen_name in
         (type_index, name)))
    (fun (type_index, name) ->
      let r =
        symbol_record_roundtrip
          (Pdb.Codeview_symbols.Udt { type_index; name })
      in
      match r with
      | Pdb.Codeview_symbols.Udt { type_index = ti; name = n } ->
          Unsigned.UInt32.equal ti type_index && n = name
      | _ -> false)

let test_constant_roundtrip =
  QCheck.Test.make ~name:"Constant roundtrip" ~count:200
    (QCheck.make
       QCheck.Gen.(
         let+ type_index = gen_u32
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
          Unsigned.UInt32.equal ti type_index && v = value && n = name
      | _ -> false)

let test_bprel32_roundtrip =
  QCheck.Test.make ~name:"BPRel32 roundtrip" ~count:200
    (QCheck.make
       QCheck.Gen.(
         let+ offset = map Int32.of_int (int_range (-1000) 1000)
         and+ type_index = gen_u32
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
          o = offset && Unsigned.UInt32.equal ti type_index && n = name
      | _ -> false)

let test_local_roundtrip =
  QCheck.Test.make ~name:"Local roundtrip" ~count:200
    (QCheck.make
       QCheck.Gen.(
         let+ type_index = gen_u32
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
          Unsigned.UInt32.equal ti type_index && f = flags && n = name
      | _ -> false)

(** {2 Named Stream Map Round-trip} *)

let test_named_stream_map_roundtrip =
  QCheck.Test.make ~name:"named stream map roundtrip" ~count:100
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
          ] );
      ( "named_stream_map",
        [ QCheck_alcotest.to_alcotest test_named_stream_map_roundtrip ] );
    ]
