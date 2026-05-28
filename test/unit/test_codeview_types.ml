(** Tests for CodeView type records and numeric leaf encoding. *)

module Buffer = Stdlib.Buffer

open Test_support

(** {2 Numeric leaf tests} *)

let test_numeric_leaf_literal () =
  (* Values < 0x8000 are literal u16 *)
  List.iter
    (fun v ->
      let buf = Buffer.create 8 in
      Pdb.Codeview_types.write_numeric_leaf buf v;
      let bytes = Buffer.contents buf in
      let obj_buf = buffer_of_string bytes in
      let cur = Object.Buffer.cursor obj_buf in
      let result = Pdb.Codeview_types.parse_numeric_leaf cur in
      Alcotest.(check int64) (Printf.sprintf "literal %Ld" v) v result)
    [ 0L; 1L; 42L; 255L; 0x7FFFL ]

let test_numeric_leaf_char () =
  (* Negative values in [-128, -1] use LF_CHAR *)
  List.iter
    (fun v ->
      let buf = Buffer.create 8 in
      Pdb.Codeview_types.write_numeric_leaf buf v;
      let bytes = Buffer.contents buf in
      let obj_buf = buffer_of_string bytes in
      let cur = Object.Buffer.cursor obj_buf in
      let result = Pdb.Codeview_types.parse_numeric_leaf cur in
      Alcotest.(check int64) (Printf.sprintf "char %Ld" v) v result)
    [ -1L; -128L; -50L ]

let test_numeric_leaf_short () =
  (* Values in [-32768, -129] use LF_SHORT *)
  List.iter
    (fun v ->
      let buf = Buffer.create 8 in
      Pdb.Codeview_types.write_numeric_leaf buf v;
      let bytes = Buffer.contents buf in
      let obj_buf = buffer_of_string bytes in
      let cur = Object.Buffer.cursor obj_buf in
      let result = Pdb.Codeview_types.parse_numeric_leaf cur in
      Alcotest.(check int64) (Printf.sprintf "short %Ld" v) v result)
    [ -129L; -32768L; -1000L ]

let test_numeric_leaf_ushort () =
  (* Values in [0x8000, 0xFFFF] use LF_USHORT *)
  List.iter
    (fun v ->
      let buf = Buffer.create 8 in
      Pdb.Codeview_types.write_numeric_leaf buf v;
      let bytes = Buffer.contents buf in
      let obj_buf = buffer_of_string bytes in
      let cur = Object.Buffer.cursor obj_buf in
      let result = Pdb.Codeview_types.parse_numeric_leaf cur in
      Alcotest.(check int64) (Printf.sprintf "ushort %Ld" v) v result)
    [ 0x8000L; 0xFFFFL; 50000L ]

let test_numeric_leaf_long () =
  (* Larger values use LF_LONG/LF_ULONG *)
  List.iter
    (fun v ->
      let buf = Buffer.create 16 in
      Pdb.Codeview_types.write_numeric_leaf buf v;
      let bytes = Buffer.contents buf in
      let obj_buf = buffer_of_string bytes in
      let cur = Object.Buffer.cursor obj_buf in
      let result = Pdb.Codeview_types.parse_numeric_leaf cur in
      Alcotest.(check int64) (Printf.sprintf "long %Ld" v) v result)
    [ 0x10000L; 100000L; -100000L ]

(** {2 Type properties tests} *)

let test_type_properties_roundtrip () =
  let props =
    {
      Pdb.Codeview_types.packed = true;
      ctor = false;
      ovlops = true;
      is_nested = false;
      cnested = true;
      opassign = false;
      opcast = false;
      fwdref = true;
      scoped = false;
      has_unique_name = true;
      sealed = false;
      intrinsic = false;
    }
  in
  let bits = Pdb.Codeview_types.int_of_type_properties props in
  let props' = Pdb.Codeview_types.parse_type_properties bits in
  Alcotest.(check bool) "packed" true props'.packed;
  Alcotest.(check bool) "ovlops" true props'.ovlops;
  Alcotest.(check bool) "cnested" true props'.cnested;
  Alcotest.(check bool) "fwdref" true props'.fwdref;
  Alcotest.(check bool) "has_unique_name" true props'.has_unique_name;
  Alcotest.(check bool) "ctor" false props'.ctor;
  Alcotest.(check bool) "sealed" false props'.sealed

let test_type_properties_all_false () =
  let props = Pdb.Codeview_types.parse_type_properties 0 in
  Alcotest.(check int)
    "all false -> 0" 0
    (Pdb.Codeview_types.int_of_type_properties props)

let test_type_properties_all_true () =
  let bits = 0x0FFF in
  let props = Pdb.Codeview_types.parse_type_properties bits in
  let bits' = Pdb.Codeview_types.int_of_type_properties props in
  Alcotest.(check int) "all true roundtrip" bits bits'

(** {2 Type record round-trip tests} *)

let u32 n = Unsigned.UInt32.of_int n

let roundtrip_record name record check =
  let buf = Buffer.create 64 in
  Pdb.Codeview_types.write_type_record buf record;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  (* Read length prefix *)
  let rec_len = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int in
  let result = Pdb.Codeview_types.parse_type_record cur rec_len in
  check name result

let test_modifier_roundtrip () =
  roundtrip_record "modifier"
    (Pdb.Codeview_types.Modifier
       { modified_type = ti 0x0074; modifiers = 0x01 })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.Modifier { modified_type; modifiers } ->
          Alcotest.(check int)
            (name ^ " type") 0x0074
            (ti_to_int modified_type);
          Alcotest.(check int) (name ^ " mods") 0x01 modifiers
      | _ -> Alcotest.fail "expected Modifier")

let test_pointer_roundtrip () =
  roundtrip_record "pointer"
    (Pdb.Codeview_types.Pointer
       { pointee_type = ti 0x0074; attrs = u32 0x1000C })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.Pointer { pointee_type; attrs } ->
          Alcotest.(check int)
            (name ^ " pointee") 0x0074
            (ti_to_int pointee_type);
          Alcotest.(check int)
            (name ^ " attrs") 0x1000C
            (Unsigned.UInt32.to_int attrs)
      | _ -> Alcotest.fail "expected Pointer")

let test_procedure_roundtrip () =
  roundtrip_record "procedure"
    (Pdb.Codeview_types.Procedure
       {
         return_type = ti 0x0074;
         calling_conv = Pdb.Codeview_constants.NearC;
      options = 0;
         param_count = 2;
         arg_list = ti 0x1001;
       })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.Procedure { return_type; param_count; arg_list; _ }
        ->
          Alcotest.(check int)
            (name ^ " ret") 0x0074
            (ti_to_int return_type);
          Alcotest.(check int) (name ^ " params") 2 param_count;
          Alcotest.(check int)
            (name ^ " arglist") 0x1001
            (ti_to_int arg_list)
      | _ -> Alcotest.fail "expected Procedure")

let test_arglist_roundtrip () =
  roundtrip_record "arglist"
    (Pdb.Codeview_types.ArgList { args = [| ti 0x0074; ti 0x0075 |] })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.ArgList { args } ->
          Alcotest.(check int) (name ^ " count") 2 (Array.length args);
          Alcotest.(check int)
            (name ^ " arg0") 0x0074
            (ti_to_int args.(0));
          Alcotest.(check int)
            (name ^ " arg1") 0x0075
            (ti_to_int args.(1))
      | _ -> Alcotest.fail "expected ArgList")

let test_enum_roundtrip () =
  roundtrip_record "enum"
    (Pdb.Codeview_types.Enum
       {
         field_count = 3;
         properties = Pdb.Codeview_types.parse_type_properties 0;
         underlying_type = ti 0x0074;
         field_list = ti 0x1002;
         name = "Color";
         unique_name = Option.None;
       })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.Enum { field_count; name = n; _ } ->
          Alcotest.(check int) (name ^ " count") 3 field_count;
          Alcotest.(check string) (name ^ " name") "Color" n
      | _ -> Alcotest.fail "expected Enum")

let test_bitfield_roundtrip () =
  roundtrip_record "bitfield"
    (Pdb.Codeview_types.Bitfield
       { underlying_type = ti 0x0074; length = 5; position = 3 })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.Bitfield { length; position; _ } ->
          Alcotest.(check int) (name ^ " length") 5 length;
          Alcotest.(check int) (name ^ " position") 3 position
      | _ -> Alcotest.fail "expected Bitfield")

let test_func_id_roundtrip () =
  roundtrip_record "func_id"
    (Pdb.Codeview_types.FuncId
       { scope_id = ti 0; func_type = ti 0x1000; name = "main" })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.FuncId { name = n; func_type; _ } ->
          Alcotest.(check string) (name ^ " name") "main" n;
          Alcotest.(check int)
            (name ^ " type") 0x1000
            (ti_to_int func_type)
      | _ -> Alcotest.fail "expected FuncId")

let test_string_id_roundtrip () =
  roundtrip_record "string_id"
    (Pdb.Codeview_types.StringId { id = ti 0; str = "hello.c" })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.StringId { str; _ } ->
          Alcotest.(check string) (name ^ " str") "hello.c" str
      | _ -> Alcotest.fail "expected StringId")

let test_udt_src_line_roundtrip () =
  roundtrip_record "udt_src_line"
    (Pdb.Codeview_types.UdtSrcLine
       { udt = ti 0x1000; source = ti 0x1001; line = u32 42 })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.UdtSrcLine { line; _ } ->
          Alcotest.(check int) (name ^ " line") 42 (Unsigned.UInt32.to_int line)
      | _ -> Alcotest.fail "expected UdtSrcLine")

(** {2 Field list entry roundtrips} *)

(** Helper: wrap field entries in a FieldList, roundtrip, return parsed members *)
let roundtrip_fieldlist members =
  let buf = Buffer.create 128 in
  Pdb.Codeview_types.write_type_record buf
    (Pdb.Codeview_types.FieldList { members });
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let rec_len = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int in
  match Pdb.Codeview_types.parse_type_record cur rec_len with
  | Pdb.Codeview_types.FieldList { members = m } -> m
  | _ -> Alcotest.fail "expected FieldList"

let test_onemethod_roundtrip () =
  let members =
    roundtrip_fieldlist
      [
        Pdb.Codeview_types.OneMethod
          {
            attrs = 3;
            method_type = ti 0x1009;
            vftable_offset = Option.None;
            name = "doStuff";
          };
      ]
  in
  Alcotest.(check int) "one entry" 1 (List.length members);
  match List.hd members with
  | Pdb.Codeview_types.OneMethod { attrs; method_type; vftable_offset; name } ->
      Alcotest.(check int) "attrs" 3 attrs;
      Alcotest.(check int) "type" 0x1009
        (ti_to_int method_type);
      Alcotest.(check (option int)) "vft" Option.None vftable_offset;
      Alcotest.(check string) "name" "doStuff" name
  | _ -> Alcotest.fail "expected OneMethod"

let test_onemethod_virtual_roundtrip () =
  let members =
    roundtrip_fieldlist
      [
        Pdb.Codeview_types.OneMethod
          {
            attrs = 0x13;
            method_type = ti 0x100A;
            vftable_offset = Some 0;
            name = "virtualMethod";
          };
      ]
  in
  match List.hd members with
  | Pdb.Codeview_types.OneMethod { attrs; vftable_offset; name; _ } ->
      Alcotest.(check int) "attrs" 0x13 attrs;
      Alcotest.(check (option int)) "vft offset" (Some 0) vftable_offset;
      Alcotest.(check string) "name" "virtualMethod" name
  | _ -> Alcotest.fail "expected OneMethod"

let test_method_roundtrip () =
  let members =
    roundtrip_fieldlist
      [
        Pdb.Codeview_types.Method
          { count = 2; method_list = ti 0x100B; name = "overloaded" };
      ]
  in
  match List.hd members with
  | Pdb.Codeview_types.Method { count; method_list; name } ->
      Alcotest.(check int) "count" 2 count;
      Alcotest.(check int) "list" 0x100B
        (ti_to_int method_list);
      Alcotest.(check string) "name" "overloaded" name
  | _ -> Alcotest.fail "expected Method"

let test_baseclass_roundtrip () =
  let members =
    roundtrip_fieldlist
      [
        Pdb.Codeview_types.BaseClass
          { attrs = 3; base_type = ti 0x1007; offset = 0L };
      ]
  in
  match List.hd members with
  | Pdb.Codeview_types.BaseClass { attrs; base_type; offset } ->
      Alcotest.(check int) "attrs" 3 attrs;
      Alcotest.(check int) "base" 0x1007
        (ti_to_int base_type);
      Alcotest.(check int64) "offset" 0L offset
  | _ -> Alcotest.fail "expected BaseClass"

let test_vbaseclass_roundtrip () =
  let members =
    roundtrip_fieldlist
      [
        Pdb.Codeview_types.VBaseClass
          {
            attrs = 3;
            base_type = ti 0x1007;
            vbptr_type = ti 0x1008;
            vbptr_offset = 0L;
            vbtable_index = 1L;
          };
      ]
  in
  match List.hd members with
  | Pdb.Codeview_types.VBaseClass
      { attrs; base_type; vbptr_type; vbptr_offset; vbtable_index } ->
      Alcotest.(check int) "attrs" 3 attrs;
      Alcotest.(check int) "base" 0x1007
        (ti_to_int base_type);
      Alcotest.(check int) "vbptr" 0x1008
        (ti_to_int vbptr_type);
      Alcotest.(check int64) "vbptr_offset" 0L vbptr_offset;
      Alcotest.(check int64) "vbtable_index" 1L vbtable_index
  | _ -> Alcotest.fail "expected VBaseClass"

let test_nestedtype_roundtrip () =
  let members =
    roundtrip_fieldlist
      [
        Pdb.Codeview_types.NestedType
          { attrs = 0; nested_type = ti 0x1010; name = "InnerClass" };
      ]
  in
  match List.hd members with
  | Pdb.Codeview_types.NestedType { attrs; nested_type; name } ->
      Alcotest.(check int) "attrs" 0 attrs;
      Alcotest.(check int) "type" 0x1010
        (ti_to_int nested_type);
      Alcotest.(check string) "name" "InnerClass" name
  | _ -> Alcotest.fail "expected NestedType"

let test_vfunctab_roundtrip () =
  let members =
    roundtrip_fieldlist
      [ Pdb.Codeview_types.VFuncTab { vftable_type = ti 0x100A } ]
  in
  match List.hd members with
  | Pdb.Codeview_types.VFuncTab { vftable_type } ->
      Alcotest.(check int) "type" 0x100A
        (ti_to_int vftable_type)
  | _ -> Alcotest.fail "expected VFuncTab"

let test_staticmember_roundtrip () =
  let members =
    roundtrip_fieldlist
      [
        Pdb.Codeview_types.StaticMember
          { attrs = 3; field_type = ti 0x0074; name = "s_count" };
      ]
  in
  match List.hd members with
  | Pdb.Codeview_types.StaticMember { attrs; field_type; name } ->
      Alcotest.(check int) "attrs" 3 attrs;
      Alcotest.(check int) "type" 0x0074
        (ti_to_int field_type);
      Alcotest.(check string) "name" "s_count" name
  | _ -> Alcotest.fail "expected StaticMember"

let test_index_roundtrip () =
  let members =
    roundtrip_fieldlist
      [ Pdb.Codeview_types.Index { continuation = ti 0x1020 } ]
  in
  match List.hd members with
  | Pdb.Codeview_types.Index { continuation } ->
      Alcotest.(check int) "continuation" 0x1020
        (ti_to_int continuation)
  | _ -> Alcotest.fail "expected Index"

let test_mixed_fieldlist_roundtrip () =
  let members =
    roundtrip_fieldlist
      [
        Pdb.Codeview_types.BaseClass
          { attrs = 3; base_type = ti 0x1007; offset = 0L };
        Pdb.Codeview_types.VFuncTab { vftable_type = ti 0x100A };
        Pdb.Codeview_types.Member
          { attrs = 3; field_type = ti 0x0074; offset = 8L; name = "x" };
        Pdb.Codeview_types.Member
          { attrs = 3; field_type = ti 0x0074; offset = 12L; name = "y" };
        Pdb.Codeview_types.StaticMember
          { attrs = 3; field_type = ti 0x0074; name = "count" };
        Pdb.Codeview_types.OneMethod
          {
            attrs = 3;
            method_type = ti 0x1009;
            vftable_offset = Option.None;
            name = "getX";
          };
        Pdb.Codeview_types.NestedType
          { attrs = 0; nested_type = ti 0x1015; name = "Iterator" };
      ]
  in
  Alcotest.(check int) "7 entries" 7 (List.length members);
  (match List.nth members 0 with
  | Pdb.Codeview_types.BaseClass _ -> ()
  | _ -> Alcotest.fail "entry 0: expected BaseClass");
  (match List.nth members 1 with
  | Pdb.Codeview_types.VFuncTab _ -> ()
  | _ -> Alcotest.fail "entry 1: expected VFuncTab");
  (match List.nth members 2 with
  | Pdb.Codeview_types.Member { name; _ } ->
      Alcotest.(check string) "entry 2 name" "x" name
  | _ -> Alcotest.fail "entry 2: expected Member");
  (match List.nth members 4 with
  | Pdb.Codeview_types.StaticMember { name; _ } ->
      Alcotest.(check string) "entry 4 name" "count" name
  | _ -> Alcotest.fail "entry 4: expected StaticMember");
  (match List.nth members 5 with
  | Pdb.Codeview_types.OneMethod { name; _ } ->
      Alcotest.(check string) "entry 5 name" "getX" name
  | _ -> Alcotest.fail "entry 5: expected OneMethod");
  match List.nth members 6 with
  | Pdb.Codeview_types.NestedType { name; _ } ->
      Alcotest.(check string) "entry 6 name" "Iterator" name
  | _ -> Alcotest.fail "entry 6: expected NestedType"

(** {2 Previously untested type record roundtrips} *)

let test_mfunction_roundtrip () =
  roundtrip_record "mfunction"
    (Pdb.Codeview_types.MFunction
       {
         return_type = ti 0x0074;
         class_type = ti 0x1007;
         this_type = ti 0x1008;
         calling_conv = Pdb.Codeview_constants.ThisCall;
      options = 0;
         param_count = 2;
         arg_list = ti 0x1003;
         this_adjust = 0l;
       })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.MFunction
          { return_type; class_type; this_type; calling_conv; param_count;
            arg_list; this_adjust; _ } ->
          Alcotest.(check int) (name ^ " ret") 0x0074
            (ti_to_int return_type);
          Alcotest.(check int) (name ^ " class") 0x1007
            (ti_to_int class_type);
          Alcotest.(check int) (name ^ " this") 0x1008
            (ti_to_int this_type);
          Alcotest.(check bool) (name ^ " cc") true
            (calling_conv = Pdb.Codeview_constants.ThisCall);
          Alcotest.(check int) (name ^ " params") 2 param_count;
          Alcotest.(check int) (name ^ " arglist") 0x1003
            (ti_to_int arg_list);
          Alcotest.(check int) (name ^ " this_adjust") 0
            (Int32.to_int this_adjust)
      | _ -> Alcotest.fail "expected MFunction")

let test_array_roundtrip () =
  roundtrip_record "array"
    (Pdb.Codeview_types.Array
       {
         element_type = ti 0x0074;
         index_type = ti 0x0075;
         size = 40L;
         name = "int[10]";
       })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.Array { element_type; index_type; size; name = n } ->
          Alcotest.(check int) (name ^ " elem") 0x0074
            (ti_to_int element_type);
          Alcotest.(check int) (name ^ " idx") 0x0075
            (ti_to_int index_type);
          Alcotest.(check int64) (name ^ " size") 40L size;
          Alcotest.(check string) (name ^ " name") "int[10]" n
      | _ -> Alcotest.fail "expected Array")

let test_class_roundtrip () =
  roundtrip_record "class"
    (Pdb.Codeview_types.Class
       {
         field_count = 3;
         properties =
           Pdb.Codeview_types.parse_type_properties 0x0200;
         field_list = ti 0x100B;
         derived_from = ti 0;
         vtable_shape = ti 0x100A;
         size = 16L;
         name = "FooClass";
         unique_name = Some ".?AVFooClass@@";
       })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.Class
          { field_count; properties; vtable_shape; size; name = n;
            unique_name; _ } ->
          Alcotest.(check int) (name ^ " count") 3 field_count;
          Alcotest.(check bool) (name ^ " has_unique_name") true
            properties.has_unique_name;
          Alcotest.(check int) (name ^ " vtshape") 0x100A
            (ti_to_int vtable_shape);
          Alcotest.(check int64) (name ^ " size") 16L size;
          Alcotest.(check string) (name ^ " name") "FooClass" n;
          Alcotest.(check (option string)) (name ^ " unique")
            (Some ".?AVFooClass@@") unique_name
      | _ -> Alcotest.fail "expected Class")

let test_structure_roundtrip () =
  roundtrip_record "structure"
    (Pdb.Codeview_types.Structure
       {
         field_count = 2;
         properties = Pdb.Codeview_types.parse_type_properties 0;
         field_list = ti 0x1003;
         derived_from = ti 0;
         vtable_shape = ti 0;
         size = 8L;
         name = "Point";
         unique_name = Option.None;
       })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.Structure { field_count; size; name = n; _ } ->
          Alcotest.(check int) (name ^ " count") 2 field_count;
          Alcotest.(check int64) (name ^ " size") 8L size;
          Alcotest.(check string) (name ^ " name") "Point" n
      | _ -> Alcotest.fail "expected Structure")

let test_union_roundtrip () =
  roundtrip_record "union"
    (Pdb.Codeview_types.Union
       {
         field_count = 2;
         properties =
           Pdb.Codeview_types.parse_type_properties 0x0200;
         field_list = ti 0x1010;
         size = 4L;
         name = "MyUnion";
         unique_name = Some ".?ATMyUnion@@";
       })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.Union
          { field_count; size; name = n; unique_name; _ } ->
          Alcotest.(check int) (name ^ " count") 2 field_count;
          Alcotest.(check int64) (name ^ " size") 4L size;
          Alcotest.(check string) (name ^ " name") "MyUnion" n;
          Alcotest.(check (option string)) (name ^ " unique")
            (Some ".?ATMyUnion@@") unique_name
      | _ -> Alcotest.fail "expected Union")

let test_vtshape_roundtrip () =
  roundtrip_record "vtshape"
    (Pdb.Codeview_types.VTShape { descriptors = [| 0; 0; 0; 4 |] })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.VTShape { descriptors } ->
          Alcotest.(check int) (name ^ " count") 4 (Array.length descriptors);
          Alcotest.(check int) (name ^ " desc[0]") 0 descriptors.(0);
          Alcotest.(check int) (name ^ " desc[3]") 4 descriptors.(3)
      | _ -> Alcotest.fail "expected VTShape")

let test_methodlist_roundtrip () =
  roundtrip_record "methodlist"
    (Pdb.Codeview_types.MethodList
       {
         entries =
           [
             (* Vanilla method, attrs=3 (public), no vftable offset *)
             (3, ti 0x1009, Option.None);
             (* IntroducingVirtual method (kind=4), attrs=0x13, with vftable offset *)
             (0x13, ti 0x100A, Some 0);
           ];
       })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.MethodList { entries } ->
          Alcotest.(check int) (name ^ " count") 2 (List.length entries);
          let attrs0, type0, vft0 = List.nth entries 0 in
          Alcotest.(check int) (name ^ " e0 attrs") 3 attrs0;
          Alcotest.(check int) (name ^ " e0 type") 0x1009
            (ti_to_int type0);
          Alcotest.(check (option int)) (name ^ " e0 vft") Option.None vft0;
          let attrs1, type1, vft1 = List.nth entries 1 in
          Alcotest.(check int) (name ^ " e1 attrs") 0x13 attrs1;
          Alcotest.(check int) (name ^ " e1 type") 0x100A
            (ti_to_int type1);
          Alcotest.(check (option int)) (name ^ " e1 vft") (Some 0) vft1
      | _ -> Alcotest.fail "expected MethodList")

let test_mfunc_id_roundtrip () =
  roundtrip_record "mfunc_id"
    (Pdb.Codeview_types.MFuncId
       { parent_type = ti 0x1007; func_type = ti 0x1009; name = "method" })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.MFuncId { parent_type; func_type; name = n } ->
          Alcotest.(check int) (name ^ " parent") 0x1007
            (ti_to_int parent_type);
          Alcotest.(check int) (name ^ " type") 0x1009
            (ti_to_int func_type);
          Alcotest.(check string) (name ^ " name") "method" n
      | _ -> Alcotest.fail "expected MFuncId")

let test_buildinfo_type_roundtrip () =
  roundtrip_record "buildinfo"
    (Pdb.Codeview_types.BuildInfo
       { args = [| ti 0x1000; ti 0x1001; ti 0x1002; ti 0; ti 0x1003 |] })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.BuildInfo { args } ->
          Alcotest.(check int) (name ^ " count") 5 (Array.length args);
          Alcotest.(check int) (name ^ " arg0") 0x1000
            (ti_to_int args.(0));
          Alcotest.(check int) (name ^ " arg4") 0x1003
            (ti_to_int args.(4))
      | _ -> Alcotest.fail "expected BuildInfo")

let test_udt_mod_src_line_roundtrip () =
  roundtrip_record "udt_mod_src_line"
    (Pdb.Codeview_types.UdtModSrcLine
       { udt = ti 0x1004; source = ti 0x1001; line = u32 15; module_ = 0 })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.UdtModSrcLine { udt; line; module_; _ } ->
          Alcotest.(check int) (name ^ " udt") 0x1004
            (ti_to_int udt);
          Alcotest.(check int) (name ^ " line") 15
            (Unsigned.UInt32.to_int line);
          Alcotest.(check int) (name ^ " module") 0 module_
      | _ -> Alcotest.fail "expected UdtModSrcLine")

let test_substr_list_roundtrip () =
  roundtrip_record "substr_list"
    (Pdb.Codeview_types.SubstrList
       { strings = [| ti 0x1000; ti 0x1001 |] })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.SubstrList { strings } ->
          Alcotest.(check int) (name ^ " count") 2 (Array.length strings);
          Alcotest.(check int) (name ^ " s0") 0x1000
            (ti_to_int strings.(0))
      | _ -> Alcotest.fail "expected SubstrList")

let test_typeserver2_roundtrip () =
  let guid =
    {
      Pdb.Pdb_types.data1 = u32 0xDEADBEEF;
      data2 = Unsigned.UInt16.of_int 0x1234;
      data3 = Unsigned.UInt16.of_int 0x5678;
      data4 = "\x01\x02\x03\x04\x05\x06\x07\x08";
    }
  in
  roundtrip_record "typeserver2"
    (Pdb.Codeview_types.TypeServer2
       { guid; age = u32 7; name = "C:\\proj\\vc140.pdb" })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.TypeServer2 { guid = g; age; name = n } ->
          Alcotest.(check int)
            (name ^ " guid.data1") 0xDEADBEEF
            (Unsigned.UInt32.to_int g.data1);
          Alcotest.(check int)
            (name ^ " guid.data2") 0x1234
            (Unsigned.UInt16.to_int g.data2);
          Alcotest.(check int)
            (name ^ " guid.data3") 0x5678
            (Unsigned.UInt16.to_int g.data3);
          Alcotest.(check string)
            (name ^ " guid.data4")
            "\x01\x02\x03\x04\x05\x06\x07\x08" g.data4;
          Alcotest.(check int) (name ^ " age") 7 (Unsigned.UInt32.to_int age);
          Alcotest.(check string) (name ^ " name") "C:\\proj\\vc140.pdb" n
      | _ -> Alcotest.fail "expected TypeServer2")

(** {2 TPI stream round-trip} *)

let test_tpi_stream_roundtrip () =
  let records =
    [
      Pdb.Codeview_types.Modifier { modified_type = ti 0x0074; modifiers = 1 };
      Pdb.Codeview_types.Procedure
        {
          return_type = ti 0x0074;
          calling_conv = Pdb.Codeview_constants.NearC;
      options = 0;
          param_count = 0;
          arg_list = ti 0x1001;
        };
      Pdb.Codeview_types.ArgList { args = [||] };
    ]
  in
  let buf = Buffer.create 256 in
  Pdb.Tpi_write.write buf records;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let header = Pdb.Tpi.parse_header cur in
  Alcotest.(check int) "num records" 3 (Pdb.Tpi.num_type_records header);
  Alcotest.(check int)
    "type_index_begin" 0x1000
    (Unsigned.UInt32.to_int header.type_index_begin);
  Alcotest.(check int)
    "type_index_end" 0x1003
    (Unsigned.UInt32.to_int header.type_index_end);
  let parsed = Pdb.Tpi.parse_type_records cur header in
  let parsed_list = List.of_seq parsed in
  Alcotest.(check int) "parsed count" 3 (List.length parsed_list);
  (* Verify first record is a Modifier *)
  (match List.nth parsed_list 0 with
  | Pdb.Codeview_types.Modifier { modified_type; _ } ->
      Alcotest.(check int)
        "first record type" 0x0074
        (ti_to_int modified_type)
  | _ -> Alcotest.fail "expected Modifier as first record");
  (* Verify second record is a Procedure *)
  match List.nth parsed_list 1 with
  | Pdb.Codeview_types.Procedure { param_count; _ } ->
      Alcotest.(check int) "procedure params" 0 param_count
  | _ -> Alcotest.fail "expected Procedure as second record"

(** {2 Unknown record handling} *)

let write_u16_le buf v =
  Buffer.add_char buf (Char.chr (v land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xFF))

let test_unknown_type_record () =
  (* Construct a binary type record with an unrecognized leaf kind (0x9999).
     Format: u16 length, u16 leaf_kind, payload bytes *)
  let buf = Buffer.create 16 in
  let payload = "ABCD" in
  let rec_len = 2 + String.length payload in (* leaf kind + payload *)
  write_u16_le buf rec_len;
  write_u16_le buf 0x9999; (* unrecognized leaf kind *)
  Buffer.add_string buf payload;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let len = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int in
  let result = Pdb.Codeview_types.parse_type_record cur len in
  match result with
  | Pdb.Codeview_types.Unknown { kind; data } ->
      Alcotest.(check int) "kind" 0x9999 kind;
      Alcotest.(check int) "data length" 4 (String.length data);
      Alcotest.(check string) "data" "ABCD" data
  | _ -> Alcotest.fail "expected Unknown type record"

let test_unknown_type_roundtrip () =
  (* Write an Unknown record and read it back *)
  roundtrip_record "unknown"
    (Pdb.Codeview_types.Unknown { kind = 0xBEEF; data = "\x01\x02\x03" })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.Unknown { kind; data } ->
          Alcotest.(check int) (name ^ " kind") 0xBEEF kind;
          Alcotest.(check int) (name ^ " data len") 3 (String.length data)
      | _ -> Alcotest.fail "expected Unknown")

let test_tpi_hash_stream () =
  let records =
    [
      Pdb.Codeview_types.ArgList { args = [||] };
      Pdb.Codeview_types.Procedure
        {
          return_type = ti 0x0074;
          calling_conv = Pdb.Codeview_constants.NearC;
      options = 0;
          param_count = 0;
          arg_list = ti 0x1000;
        };
      Pdb.Codeview_types.Modifier { modified_type = ti 0x0074; modifiers = 1 };
    ]
  in
  let buf = Buffer.create 256 in
  let hash_bytes =
    Pdb.Tpi_write.write_with_hash buf records ~hash_stream_index:5
  in
  (* Parse the TPI stream back *)
  let tpi_buf = buffer_of_string (Buffer.contents buf) in
  let cur = Object.Buffer.cursor tpi_buf in
  let header = Pdb.Tpi.parse_header cur in
  Alcotest.(check int) "3 records" 3 (Pdb.Tpi.num_type_records header);
  Alcotest.(check int) "hash stream index" 5 header.hash_stream_index;
  (* The hash stream should have 3 * 4 = 12 bytes of hash values *)
  Alcotest.(check bool) "hash stream non-empty" true
    (String.length hash_bytes >= 12);
  (* Parse the records to verify they're still correct *)
  let parsed = List.of_seq (Pdb.Tpi.parse_type_records cur header) in
  Alcotest.(check int) "parsed 3" 3 (List.length parsed);
  match List.nth parsed 0 with
  | Pdb.Codeview_types.ArgList { args } ->
      Alcotest.(check int) "arglist" 0 (Array.length args)
  | _ -> Alcotest.fail "expected ArgList"

(** {2 Long-name truncation} *)

(** Round-trip a Structure with a huge name + huge unique_name and assert
    the parsed-back record matches LLVM's truncated form:
    Name = take_front(4064) + MD5_hex(orig_name);
    UniqueName = "??@" + MD5_hex(orig_unique) + "@" (36 chars). *)
let test_truncate_with_unique_name () =
  let orig_name = String.make 68229 'a' in
  let orig_unique = String.make 68228 'b' in
  let md5_hex s = Digest.to_hex (Digest.string s) in
  roundtrip_record "longname+unique"
    (Pdb.Codeview_types.Structure
       {
         field_count = 0;
         properties = Pdb.Codeview_types.parse_type_properties 0x0200;
         (* HasUniqueName *)
         field_list = ti 0;
         derived_from = ti 0;
         vtable_shape = ti 0;
         size = 1L;
         name = orig_name;
         unique_name = Some orig_unique;
       })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.Structure
          { name = parsed_name; unique_name = parsed_unique; _ } ->
          let expected_name = String.make 4064 'a' ^ md5_hex orig_name in
          let expected_unique = "??@" ^ md5_hex orig_unique ^ "@" in
          Alcotest.(check int)
            (name ^ " name length") 4096
            (String.length parsed_name);
          Alcotest.(check string)
            (name ^ " name") expected_name parsed_name;
          Alcotest.(check (option string))
            (name ^ " unique") (Some expected_unique) parsed_unique
      | _ -> Alcotest.fail "expected Structure")

(** Round-trip a Structure with a huge name and no unique_name. The
    record should fill MaxRecordLength = 0xFF00; the name is plain
    take_front(BytesLeft - 1) = take_front(65257). *)
let test_truncate_without_unique_name () =
  let orig_name = String.make 68229 'f' in
  roundtrip_record "longname"
    (Pdb.Codeview_types.Structure
       {
         field_count = 0;
         properties = Pdb.Codeview_types.parse_type_properties 0;
         field_list = ti 0;
         derived_from = ti 0;
         vtable_shape = ti 0;
         size = 8L;
         name = orig_name;
         unique_name = None;
       })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.Structure
          { name = parsed_name; unique_name = parsed_unique; _ } ->
          Alcotest.(check int)
            (name ^ " name length") 65257
            (String.length parsed_name);
          Alcotest.(check string)
            (name ^ " name") (String.make 65257 'f') parsed_name;
          Alcotest.(check (option string)) (name ^ " unique") None parsed_unique
      | _ -> Alcotest.fail "expected Structure")

(** A short name must pass through unchanged. *)
let test_truncate_short_name_passthrough () =
  roundtrip_record "short name"
    (Pdb.Codeview_types.Structure
       {
         field_count = 0;
         properties = Pdb.Codeview_types.parse_type_properties 0x0200;
         field_list = ti 0;
         derived_from = ti 0;
         vtable_shape = ti 0;
         size = 4L;
         name = "Foo";
         unique_name = Some ".?AUFoo@@";
       })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.Structure
          { name = parsed_name; unique_name = parsed_unique; _ } ->
          Alcotest.(check string) (name ^ " name") "Foo" parsed_name;
          Alcotest.(check (option string))
            (name ^ " unique") (Some ".?AUFoo@@") parsed_unique
      | _ -> Alcotest.fail "expected Structure")

let () =
  Alcotest.run "CodeView Types"
    [
      ( "numeric_leaf",
        [
          Alcotest.test_case "literal" `Quick test_numeric_leaf_literal;
          Alcotest.test_case "char" `Quick test_numeric_leaf_char;
          Alcotest.test_case "short" `Quick test_numeric_leaf_short;
          Alcotest.test_case "ushort" `Quick test_numeric_leaf_ushort;
          Alcotest.test_case "long" `Quick test_numeric_leaf_long;
        ] );
      ( "type_properties",
        [
          Alcotest.test_case "roundtrip" `Quick test_type_properties_roundtrip;
          Alcotest.test_case "all false" `Quick test_type_properties_all_false;
          Alcotest.test_case "all true" `Quick test_type_properties_all_true;
        ] );
      ( "type_record",
        [
          Alcotest.test_case "modifier" `Quick test_modifier_roundtrip;
          Alcotest.test_case "pointer" `Quick test_pointer_roundtrip;
          Alcotest.test_case "procedure" `Quick test_procedure_roundtrip;
          Alcotest.test_case "arglist" `Quick test_arglist_roundtrip;
          Alcotest.test_case "enum" `Quick test_enum_roundtrip;
          Alcotest.test_case "bitfield" `Quick test_bitfield_roundtrip;
          Alcotest.test_case "func_id" `Quick test_func_id_roundtrip;
          Alcotest.test_case "string_id" `Quick test_string_id_roundtrip;
          Alcotest.test_case "udt_src_line" `Quick test_udt_src_line_roundtrip;
        ] );
      ( "field_entries",
        [
          Alcotest.test_case "onemethod" `Quick test_onemethod_roundtrip;
          Alcotest.test_case "onemethod virtual" `Quick
            test_onemethod_virtual_roundtrip;
          Alcotest.test_case "method" `Quick test_method_roundtrip;
          Alcotest.test_case "baseclass" `Quick test_baseclass_roundtrip;
          Alcotest.test_case "vbaseclass" `Quick test_vbaseclass_roundtrip;
          Alcotest.test_case "nestedtype" `Quick test_nestedtype_roundtrip;
          Alcotest.test_case "vfunctab" `Quick test_vfunctab_roundtrip;
          Alcotest.test_case "staticmember" `Quick test_staticmember_roundtrip;
          Alcotest.test_case "index" `Quick test_index_roundtrip;
          Alcotest.test_case "mixed fieldlist" `Quick
            test_mixed_fieldlist_roundtrip;
        ] );
      ( "type_record_extended",
        [
          Alcotest.test_case "mfunction" `Quick test_mfunction_roundtrip;
          Alcotest.test_case "array" `Quick test_array_roundtrip;
          Alcotest.test_case "class" `Quick test_class_roundtrip;
          Alcotest.test_case "structure" `Quick test_structure_roundtrip;
          Alcotest.test_case "union" `Quick test_union_roundtrip;
          Alcotest.test_case "vtshape" `Quick test_vtshape_roundtrip;
          Alcotest.test_case "methodlist" `Quick test_methodlist_roundtrip;
          Alcotest.test_case "mfunc_id" `Quick test_mfunc_id_roundtrip;
          Alcotest.test_case "buildinfo_type" `Quick
            test_buildinfo_type_roundtrip;
          Alcotest.test_case "udt_mod_src_line" `Quick
            test_udt_mod_src_line_roundtrip;
          Alcotest.test_case "substr_list" `Quick test_substr_list_roundtrip;
          Alcotest.test_case "typeserver2" `Quick test_typeserver2_roundtrip;
        ] );
      ( "tpi_stream",
        [
          Alcotest.test_case "roundtrip" `Quick test_tpi_stream_roundtrip;
          Alcotest.test_case "hash stream" `Quick test_tpi_hash_stream;
        ] );
      ( "long_name_truncation",
        [
          Alcotest.test_case "with unique_name" `Quick
            test_truncate_with_unique_name;
          Alcotest.test_case "without unique_name" `Quick
            test_truncate_without_unique_name;
          Alcotest.test_case "short name passthrough" `Quick
            test_truncate_short_name_passthrough;
        ] );
      ( "unknown_records",
        [
          Alcotest.test_case "unknown type record" `Quick
            test_unknown_type_record;
          Alcotest.test_case "unknown type roundtrip" `Quick
            test_unknown_type_roundtrip;
        ] );
    ]
