(** Tests for CodeView symbol records. *)

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

let roundtrip_symbol name record check =
  let buf = Buffer.create 64 in
  Pdb.Codeview_symbols.write_symbol_record buf record;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let rec_len = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int in
  let result = Pdb.Codeview_symbols.parse_symbol_record cur rec_len in
  check name result

let test_end_roundtrip () =
  roundtrip_symbol "end" Pdb.Codeview_symbols.End (fun name r ->
      match r with
      | Pdb.Codeview_symbols.End -> ()
      | _ -> Alcotest.fail (name ^ ": expected End"))

let test_objname_roundtrip () =
  roundtrip_symbol "objname"
    (Pdb.Codeview_symbols.ObjName { signature = u32 0; name = "test.obj" })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.ObjName { name = n; _ } ->
          Alcotest.(check string) (name ^ " name") "test.obj" n
      | _ -> Alcotest.fail (name ^ ": expected ObjName"))

let test_pub32_roundtrip () =
  roundtrip_symbol "pub32"
    (Pdb.Codeview_symbols.Pub32
       { flags = u32 2; offset = u32 0x1000; segment = 1; name = "_main" })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.Pub32 { flags; offset; segment; name = n } ->
          Alcotest.(check int)
            (name ^ " flags") 2
            (Unsigned.UInt32.to_int flags);
          Alcotest.(check int)
            (name ^ " offset") 0x1000
            (Unsigned.UInt32.to_int offset);
          Alcotest.(check int) (name ^ " segment") 1 segment;
          Alcotest.(check string) (name ^ " name") "_main" n
      | _ -> Alcotest.fail (name ^ ": expected Pub32"))

let test_gproc32_roundtrip () =
  let proc : Pdb.Codeview_symbols.proc_record =
    {
      parent = u32 0;
      end_ = u32 100;
      next = u32 0;
      code_size = u32 50;
      debug_start = u32 5;
      debug_end = u32 45;
      type_index = u32 0x1000;
      offset = u32 0x2000;
      segment = 1;
      flags = 0;
      name = "main";
    }
  in
  roundtrip_symbol "gproc32" (Pdb.Codeview_symbols.GProc32 proc) (fun name r ->
      match r with
      | Pdb.Codeview_symbols.GProc32 p ->
          Alcotest.(check string) (name ^ " name") "main" p.name;
          Alcotest.(check int)
            (name ^ " code_size") 50
            (Unsigned.UInt32.to_int p.code_size);
          Alcotest.(check int)
            (name ^ " type") 0x1000
            (Unsigned.UInt32.to_int p.type_index);
          Alcotest.(check int) (name ^ " segment") 1 p.segment
      | _ -> Alcotest.fail (name ^ ": expected GProc32"))

let test_gdata32_roundtrip () =
  roundtrip_symbol "gdata32"
    (Pdb.Codeview_symbols.GData32
       {
         type_index = u32 0x0074;
         offset = u32 0x3000;
         segment = 2;
         name = "global_var";
       })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.GData32 d ->
          Alcotest.(check string) (name ^ " name") "global_var" d.name;
          Alcotest.(check int)
            (name ^ " type") 0x0074
            (Unsigned.UInt32.to_int d.type_index)
      | _ -> Alcotest.fail (name ^ ": expected GData32"))

let test_local_roundtrip () =
  roundtrip_symbol "local"
    (Pdb.Codeview_symbols.Local
       { type_index = u32 0x0074; flags = 0x01; name = "x" })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.Local { type_index; flags; name = n } ->
          Alcotest.(check string) (name ^ " name") "x" n;
          Alcotest.(check int) (name ^ " flags") 0x01 flags;
          Alcotest.(check int)
            (name ^ " type") 0x0074
            (Unsigned.UInt32.to_int type_index)
      | _ -> Alcotest.fail (name ^ ": expected Local"))

let test_udt_roundtrip () =
  roundtrip_symbol "udt"
    (Pdb.Codeview_symbols.Udt { type_index = u32 0x1005; name = "Point" })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.Udt { name = n; _ } ->
          Alcotest.(check string) (name ^ " name") "Point" n
      | _ -> Alcotest.fail (name ^ ": expected Udt"))

let test_constant_roundtrip () =
  roundtrip_symbol "constant"
    (Pdb.Codeview_symbols.Constant
       { type_index = u32 0x0074; value = 42L; name = "ANSWER" })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.Constant { value; name = n; _ } ->
          Alcotest.(check string) (name ^ " name") "ANSWER" n;
          Alcotest.(check int64) (name ^ " value") 42L value
      | _ -> Alcotest.fail (name ^ ": expected Constant"))

let test_bprel32_roundtrip () =
  roundtrip_symbol "bprel32"
    (Pdb.Codeview_symbols.BPRel32
       { offset = -8l; type_index = u32 0x0074; name = "argc" })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.BPRel32 { offset; name = n; _ } ->
          Alcotest.(check string) (name ^ " name") "argc" n;
          Alcotest.(check int) (name ^ " offset") (-8) (Int32.to_int offset)
      | _ -> Alcotest.fail (name ^ ": expected BPRel32"))

let test_regrel32_roundtrip () =
  roundtrip_symbol "regrel32"
    (Pdb.Codeview_symbols.RegRel32
       { offset = 16l; type_index = u32 0x0074; register = 334; name = "argv" })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.RegRel32 { offset; register; name = n; _ } ->
          Alcotest.(check string) (name ^ " name") "argv" n;
          Alcotest.(check int) (name ^ " offset") 16 (Int32.to_int offset);
          Alcotest.(check int) (name ^ " register") 334 register
      | _ -> Alcotest.fail (name ^ ": expected RegRel32"))

let test_buildinfo_roundtrip () =
  roundtrip_symbol "buildinfo"
    (Pdb.Codeview_symbols.BuildInfo { id = u32 0x1003 })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.BuildInfo { id } ->
          Alcotest.(check int) (name ^ " id") 0x1003 (Unsigned.UInt32.to_int id)
      | _ -> Alcotest.fail (name ^ ": expected BuildInfo"))

let test_unamespace_roundtrip () =
  roundtrip_symbol "unamespace"
    (Pdb.Codeview_symbols.UNamespace { name = "std" })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.UNamespace { name = n } ->
          Alcotest.(check string) (name ^ " name") "std" n
      | _ -> Alcotest.fail (name ^ ": expected UNamespace"))

let test_symbol_stream_roundtrip () =
  let symbols =
    [
      Pdb.Codeview_symbols.ObjName { signature = u32 0; name = "test.obj" };
      Pdb.Codeview_symbols.Pub32
        { flags = u32 0; offset = u32 0; segment = 1; name = "main" };
      Pdb.Codeview_symbols.End;
    ]
  in
  let buf = Buffer.create 256 in
  List.iter (fun s -> Pdb.Codeview_symbols.write_symbol_record buf s) symbols;
  let bytes = Buffer.contents buf in
  let total = String.length bytes in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let parsed = Pdb.Codeview_symbols.parse_symbol_stream cur total in
  let parsed_list = List.of_seq parsed in
  Alcotest.(check int) "parsed count" 3 (List.length parsed_list);
  (match List.nth parsed_list 0 with
  | Pdb.Codeview_symbols.ObjName { name; _ } ->
      Alcotest.(check string) "first name" "test.obj" name
  | _ -> Alcotest.fail "expected ObjName");
  (match List.nth parsed_list 1 with
  | Pdb.Codeview_symbols.Pub32 { name; _ } ->
      Alcotest.(check string) "second name" "main" name
  | _ -> Alcotest.fail "expected Pub32");
  match List.nth parsed_list 2 with
  | Pdb.Codeview_symbols.End -> ()
  | _ -> Alcotest.fail "expected End"

let () =
  Alcotest.run "CodeView Symbols"
    [
      ( "symbol_record",
        [
          Alcotest.test_case "end" `Quick test_end_roundtrip;
          Alcotest.test_case "objname" `Quick test_objname_roundtrip;
          Alcotest.test_case "pub32" `Quick test_pub32_roundtrip;
          Alcotest.test_case "gproc32" `Quick test_gproc32_roundtrip;
          Alcotest.test_case "gdata32" `Quick test_gdata32_roundtrip;
          Alcotest.test_case "local" `Quick test_local_roundtrip;
          Alcotest.test_case "udt" `Quick test_udt_roundtrip;
          Alcotest.test_case "constant" `Quick test_constant_roundtrip;
          Alcotest.test_case "bprel32" `Quick test_bprel32_roundtrip;
          Alcotest.test_case "regrel32" `Quick test_regrel32_roundtrip;
          Alcotest.test_case "buildinfo" `Quick test_buildinfo_roundtrip;
          Alcotest.test_case "unamespace" `Quick test_unamespace_roundtrip;
        ] );
      ( "symbol_stream",
        [ Alcotest.test_case "roundtrip" `Quick test_symbol_stream_roundtrip ]
      );
    ]
