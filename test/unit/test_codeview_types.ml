(** Tests for CodeView type records and numeric leaf encoding. *)

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
       { modified_type = u32 0x0074; modifiers = 0x01 })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.Modifier { modified_type; modifiers } ->
          Alcotest.(check int)
            (name ^ " type") 0x0074
            (Unsigned.UInt32.to_int modified_type);
          Alcotest.(check int) (name ^ " mods") 0x01 modifiers
      | _ -> Alcotest.fail "expected Modifier")

let test_pointer_roundtrip () =
  roundtrip_record "pointer"
    (Pdb.Codeview_types.Pointer
       { pointee_type = u32 0x0074; attrs = u32 0x1000C })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.Pointer { pointee_type; attrs } ->
          Alcotest.(check int)
            (name ^ " pointee") 0x0074
            (Unsigned.UInt32.to_int pointee_type);
          Alcotest.(check int)
            (name ^ " attrs") 0x1000C
            (Unsigned.UInt32.to_int attrs)
      | _ -> Alcotest.fail "expected Pointer")

let test_procedure_roundtrip () =
  roundtrip_record "procedure"
    (Pdb.Codeview_types.Procedure
       {
         return_type = u32 0x0074;
         calling_conv = Pdb.Codeview_constants.NearC;
         param_count = 2;
         arg_list = u32 0x1001;
       })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.Procedure { return_type; param_count; arg_list; _ }
        ->
          Alcotest.(check int)
            (name ^ " ret") 0x0074
            (Unsigned.UInt32.to_int return_type);
          Alcotest.(check int) (name ^ " params") 2 param_count;
          Alcotest.(check int)
            (name ^ " arglist") 0x1001
            (Unsigned.UInt32.to_int arg_list)
      | _ -> Alcotest.fail "expected Procedure")

let test_arglist_roundtrip () =
  roundtrip_record "arglist"
    (Pdb.Codeview_types.ArgList { args = [| u32 0x0074; u32 0x0075 |] })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.ArgList { args } ->
          Alcotest.(check int) (name ^ " count") 2 (Array.length args);
          Alcotest.(check int)
            (name ^ " arg0") 0x0074
            (Unsigned.UInt32.to_int args.(0));
          Alcotest.(check int)
            (name ^ " arg1") 0x0075
            (Unsigned.UInt32.to_int args.(1))
      | _ -> Alcotest.fail "expected ArgList")

let test_enum_roundtrip () =
  roundtrip_record "enum"
    (Pdb.Codeview_types.Enum
       {
         field_count = 3;
         properties = Pdb.Codeview_types.parse_type_properties 0;
         underlying_type = u32 0x0074;
         field_list = u32 0x1002;
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
       { underlying_type = u32 0x0074; length = 5; position = 3 })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.Bitfield { length; position; _ } ->
          Alcotest.(check int) (name ^ " length") 5 length;
          Alcotest.(check int) (name ^ " position") 3 position
      | _ -> Alcotest.fail "expected Bitfield")

let test_func_id_roundtrip () =
  roundtrip_record "func_id"
    (Pdb.Codeview_types.FuncId
       { scope_id = u32 0; func_type = u32 0x1000; name = "main" })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.FuncId { name = n; func_type; _ } ->
          Alcotest.(check string) (name ^ " name") "main" n;
          Alcotest.(check int)
            (name ^ " type") 0x1000
            (Unsigned.UInt32.to_int func_type)
      | _ -> Alcotest.fail "expected FuncId")

let test_string_id_roundtrip () =
  roundtrip_record "string_id"
    (Pdb.Codeview_types.StringId { id = u32 0; str = "hello.c" })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.StringId { str; _ } ->
          Alcotest.(check string) (name ^ " str") "hello.c" str
      | _ -> Alcotest.fail "expected StringId")

let test_udt_src_line_roundtrip () =
  roundtrip_record "udt_src_line"
    (Pdb.Codeview_types.UdtSrcLine
       { udt = u32 0x1000; source = u32 0x1001; line = u32 42 })
    (fun name r ->
      match r with
      | Pdb.Codeview_types.UdtSrcLine { line; _ } ->
          Alcotest.(check int) (name ^ " line") 42 (Unsigned.UInt32.to_int line)
      | _ -> Alcotest.fail "expected UdtSrcLine")

(** {2 TPI stream round-trip} *)

let test_tpi_stream_roundtrip () =
  let records =
    [
      Pdb.Codeview_types.Modifier { modified_type = u32 0x0074; modifiers = 1 };
      Pdb.Codeview_types.Procedure
        {
          return_type = u32 0x0074;
          calling_conv = Pdb.Codeview_constants.NearC;
          param_count = 0;
          arg_list = u32 0x1001;
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
        (Unsigned.UInt32.to_int modified_type)
  | _ -> Alcotest.fail "expected Modifier as first record");
  (* Verify second record is a Procedure *)
  match List.nth parsed_list 1 with
  | Pdb.Codeview_types.Procedure { param_count; _ } ->
      Alcotest.(check int) "procedure params" 0 param_count
  | _ -> Alcotest.fail "expected Procedure as second record"

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
      ( "tpi_stream",
        [ Alcotest.test_case "roundtrip" `Quick test_tpi_stream_roundtrip ] );
    ]
