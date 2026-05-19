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
let ti n = Pdb.Type_index.of_u32 (Unsigned.UInt32.of_int n)
let ti_to_int t = Unsigned.UInt32.to_int (Pdb.Type_index.to_u32 t)

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
      type_index = ti 0x1000;
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
            (ti_to_int p.type_index);
          Alcotest.(check int) (name ^ " segment") 1 p.segment
      | _ -> Alcotest.fail (name ^ ": expected GProc32"))

let test_gdata32_roundtrip () =
  roundtrip_symbol "gdata32"
    (Pdb.Codeview_symbols.GData32
       {
         type_index = ti 0x0074;
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
            (ti_to_int d.type_index)
      | _ -> Alcotest.fail (name ^ ": expected GData32"))

let test_local_roundtrip () =
  roundtrip_symbol "local"
    (Pdb.Codeview_symbols.Local
       { type_index = ti 0x0074; flags = 0x01; name = "x" })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.Local { type_index; flags; name = n } ->
          Alcotest.(check string) (name ^ " name") "x" n;
          Alcotest.(check int) (name ^ " flags") 0x01 flags;
          Alcotest.(check int)
            (name ^ " type") 0x0074
            (ti_to_int type_index)
      | _ -> Alcotest.fail (name ^ ": expected Local"))

let test_udt_roundtrip () =
  roundtrip_symbol "udt"
    (Pdb.Codeview_symbols.Udt { type_index = ti 0x1005; name = "Point" })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.Udt { name = n; _ } ->
          Alcotest.(check string) (name ^ " name") "Point" n
      | _ -> Alcotest.fail (name ^ ": expected Udt"))

let test_constant_roundtrip () =
  roundtrip_symbol "constant"
    (Pdb.Codeview_symbols.Constant
       { type_index = ti 0x0074; value = 42L; name = "ANSWER" })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.Constant { value; name = n; _ } ->
          Alcotest.(check string) (name ^ " name") "ANSWER" n;
          Alcotest.(check int64) (name ^ " value") 42L value
      | _ -> Alcotest.fail (name ^ ": expected Constant"))

let test_bprel32_roundtrip () =
  roundtrip_symbol "bprel32"
    (Pdb.Codeview_symbols.BPRel32
       { offset = -8l; type_index = ti 0x0074; name = "argc" })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.BPRel32 { offset; name = n; _ } ->
          Alcotest.(check string) (name ^ " name") "argc" n;
          Alcotest.(check int) (name ^ " offset") (-8) (Int32.to_int offset)
      | _ -> Alcotest.fail (name ^ ": expected BPRel32"))

let test_regrel32_roundtrip () =
  roundtrip_symbol "regrel32"
    (Pdb.Codeview_symbols.RegRel32
       { offset = 16l; type_index = ti 0x0074; register = 334; name = "argv" })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.RegRel32 { offset; register; name = n; _ } ->
          Alcotest.(check string) (name ^ " name") "argv" n;
          Alcotest.(check int) (name ^ " offset") 16 (Int32.to_int offset);
          Alcotest.(check int) (name ^ " register") 334 register
      | _ -> Alcotest.fail (name ^ ": expected RegRel32"))

let test_buildinfo_roundtrip () =
  roundtrip_symbol "buildinfo"
    (Pdb.Codeview_symbols.BuildInfo { id = ti 0x1003 })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.BuildInfo { id } ->
          Alcotest.(check int) (name ^ " id") 0x1003 (ti_to_int id)
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

let test_compile3_roundtrip () =
  roundtrip_symbol "compile3"
    (Pdb.Codeview_symbols.Compile3
       {
         flags = u32 0x00000100;
         machine = 0x8664;
         frontend_version = (19, 29, 30148, 0);
         backend_version = (19, 29, 30148, 0);
         version_string = "Microsoft (R) Optimizing Compiler";
       })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.Compile3
          { flags; machine; frontend_version; backend_version; version_string }
        ->
          Alcotest.(check int) (name ^ " flags") 0x00000100
            (Unsigned.UInt32.to_int flags);
          Alcotest.(check int) (name ^ " machine") 0x8664 machine;
          let fe_maj, fe_min, fe_bld, _ = frontend_version in
          Alcotest.(check int) (name ^ " fe_maj") 19 fe_maj;
          Alcotest.(check int) (name ^ " fe_min") 29 fe_min;
          Alcotest.(check int) (name ^ " fe_bld") 30148 fe_bld;
          let be_maj, _, _, _ = backend_version in
          Alcotest.(check int) (name ^ " be_maj") 19 be_maj;
          Alcotest.(check string) (name ^ " version")
            "Microsoft (R) Optimizing Compiler" version_string
      | _ -> Alcotest.fail (name ^ ": expected Compile3"))

let test_lproc32_roundtrip () =
  let proc : Pdb.Codeview_symbols.proc_record =
    {
      parent = u32 0;
      end_ = u32 200;
      next = u32 0;
      code_size = u32 30;
      debug_start = u32 3;
      debug_end = u32 27;
      type_index = ti 0x1001;
      offset = u32 0x5000;
      segment = 1;
      flags = 0;
      name = "helper";
    }
  in
  roundtrip_symbol "lproc32" (Pdb.Codeview_symbols.LProc32 proc)
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.LProc32 p ->
          Alcotest.(check string) (name ^ " name") "helper" p.name;
          Alcotest.(check int) (name ^ " code_size") 30
            (Unsigned.UInt32.to_int p.code_size)
      | _ -> Alcotest.fail (name ^ ": expected LProc32"))

let test_gproc32id_roundtrip () =
  let proc : Pdb.Codeview_symbols.proc_record =
    {
      parent = u32 0;
      end_ = u32 100;
      next = u32 0;
      code_size = u32 50;
      debug_start = u32 5;
      debug_end = u32 45;
      type_index = ti 0x1000;
      offset = u32 0x2000;
      segment = 1;
      flags = 0;
      name = "main";
    }
  in
  roundtrip_symbol "gproc32id" (Pdb.Codeview_symbols.GProc32Id proc)
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.GProc32Id p ->
          Alcotest.(check string) (name ^ " name") "main" p.name
      | _ -> Alcotest.fail (name ^ ": expected GProc32Id"))

let test_lproc32id_roundtrip () =
  let proc : Pdb.Codeview_symbols.proc_record =
    {
      parent = u32 0;
      end_ = u32 100;
      next = u32 0;
      code_size = u32 20;
      debug_start = u32 0;
      debug_end = u32 18;
      type_index = ti 0x1002;
      offset = u32 0x3000;
      segment = 1;
      flags = 0;
      name = "static_fn";
    }
  in
  roundtrip_symbol "lproc32id" (Pdb.Codeview_symbols.LProc32Id proc)
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.LProc32Id p ->
          Alcotest.(check string) (name ^ " name") "static_fn" p.name
      | _ -> Alcotest.fail (name ^ ": expected LProc32Id"))

let test_gthread32_roundtrip () =
  roundtrip_symbol "gthread32"
    (Pdb.Codeview_symbols.GThread32
       { type_index = ti 0x0074; offset = u32 0x4000; segment = 3;
         name = "tls_var" })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.GThread32 d ->
          Alcotest.(check string) (name ^ " name") "tls_var" d.name;
          Alcotest.(check int) (name ^ " segment") 3 d.segment
      | _ -> Alcotest.fail (name ^ ": expected GThread32"))

let test_lthread32_roundtrip () =
  roundtrip_symbol "lthread32"
    (Pdb.Codeview_symbols.LThread32
       { type_index = ti 0x0074; offset = u32 0x4010; segment = 3;
         name = "tls_local" })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.LThread32 d ->
          Alcotest.(check string) (name ^ " name") "tls_local" d.name
      | _ -> Alcotest.fail (name ^ ": expected LThread32"))

let test_defrange_fp_rel_roundtrip () =
  roundtrip_symbol "defrange_fp_rel"
    (Pdb.Codeview_symbols.DefRangeFramePointerRel
       { offset = -8l; range_offset = u32 0x10; range_section = 1;
         range_length = 20 })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.DefRangeFramePointerRel
          { offset; range_offset; range_section; range_length } ->
          Alcotest.(check int) (name ^ " offset") (-8) (Int32.to_int offset);
          Alcotest.(check int) (name ^ " range_offset") 0x10
            (Unsigned.UInt32.to_int range_offset);
          Alcotest.(check int) (name ^ " range_section") 1 range_section;
          Alcotest.(check int) (name ^ " range_length") 20 range_length
      | _ -> Alcotest.fail (name ^ ": expected DefRangeFramePointerRel"))

let test_defrange_register_rel_roundtrip () =
  roundtrip_symbol "defrange_reg_rel"
    (Pdb.Codeview_symbols.DefRangeRegisterRel
       { base_register = 334; offset = 16l; range_offset = u32 0x20;
         range_section = 1; range_length = 30 })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.DefRangeRegisterRel
          { base_register; offset; range_section; _ } ->
          Alcotest.(check int) (name ^ " reg") 334 base_register;
          Alcotest.(check int) (name ^ " offset") 16 (Int32.to_int offset);
          Alcotest.(check int) (name ^ " section") 1 range_section
      | _ -> Alcotest.fail (name ^ ": expected DefRangeRegisterRel"))

let test_defrange_register_roundtrip () =
  roundtrip_symbol "defrange_register"
    (Pdb.Codeview_symbols.DefRangeRegister
       { register = 17; may_have_no_name = 0; range_offset = u32 0x30;
         range_section = 1; range_length = 10 })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.DefRangeRegister { register; _ } ->
          Alcotest.(check int) (name ^ " register") 17 register
      | _ -> Alcotest.fail (name ^ ": expected DefRangeRegister"))

let test_defrange_fp_rel_full_scope_roundtrip () =
  roundtrip_symbol "defrange_fp_rel_full_scope"
    (Pdb.Codeview_symbols.DefRangeFramePointerRelFullScope { offset = -16l })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.DefRangeFramePointerRelFullScope { offset } ->
          Alcotest.(check int) (name ^ " offset") (-16) (Int32.to_int offset)
      | _ ->
          Alcotest.fail
            (name ^ ": expected DefRangeFramePointerRelFullScope"))

let test_block32_roundtrip () =
  roundtrip_symbol "block32"
    (Pdb.Codeview_symbols.Block32
       { parent = u32 0; end_ = u32 50; length = u32 20;
         offset = u32 0x100; segment = 1; name = "" })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.Block32 { length; offset; segment; _ } ->
          Alcotest.(check int) (name ^ " length") 20
            (Unsigned.UInt32.to_int length);
          Alcotest.(check int) (name ^ " offset") 0x100
            (Unsigned.UInt32.to_int offset);
          Alcotest.(check int) (name ^ " segment") 1 segment
      | _ -> Alcotest.fail (name ^ ": expected Block32"))

let test_inlinesite_roundtrip () =
  roundtrip_symbol "inlinesite"
    (Pdb.Codeview_symbols.InlineSite
       { parent = u32 0; end_ = u32 80; inlinee = ti 0x1000;
         annotations = "\x0B\x06\x02" })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.InlineSite { inlinee; annotations; _ } ->
          Alcotest.(check int) (name ^ " inlinee") 0x1000
            (ti_to_int inlinee);
          Alcotest.(check int) (name ^ " annotations len") 3
            (String.length annotations)
      | _ -> Alcotest.fail (name ^ ": expected InlineSite"))

let test_inlinesite_end_roundtrip () =
  roundtrip_symbol "inlinesite_end" Pdb.Codeview_symbols.InlineSiteEnd
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.InlineSiteEnd -> ()
      | _ -> Alcotest.fail (name ^ ": expected InlineSiteEnd"))

let test_proc_id_end_roundtrip () =
  roundtrip_symbol "proc_id_end" Pdb.Codeview_symbols.ProcIdEnd (fun name r ->
      match r with
      | Pdb.Codeview_symbols.ProcIdEnd -> ()
      | _ -> Alcotest.fail (name ^ ": expected ProcIdEnd"))

let test_frameproc_roundtrip () =
  roundtrip_symbol "frameproc"
    (Pdb.Codeview_symbols.FrameProc
       {
         total_frame_bytes = u32 48;
         padding_frame_bytes = u32 0;
         offset_to_padding = u32 0;
         callee_saved_reg_bytes = u32 16;
         exception_handler_offset = u32 0;
         exception_handler_section = 0;
         frame_proc_flags = u32 0x00114000;
       })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.FrameProc
          { total_frame_bytes; callee_saved_reg_bytes; frame_proc_flags; _ } ->
          Alcotest.(check int) (name ^ " frame") 48
            (Unsigned.UInt32.to_int total_frame_bytes);
          Alcotest.(check int) (name ^ " callee_saved") 16
            (Unsigned.UInt32.to_int callee_saved_reg_bytes);
          Alcotest.(check int) (name ^ " flags") 0x00114000
            (Unsigned.UInt32.to_int frame_proc_flags)
      | _ -> Alcotest.fail (name ^ ": expected FrameProc"))

let test_register_roundtrip () =
  roundtrip_symbol "register"
    (Pdb.Codeview_symbols.Register
       { type_index = ti 0x0074; register = 17; name = "eax_var" })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.Register { type_index; register; name = n } ->
          Alcotest.(check int) (name ^ " type") 0x0074
            (ti_to_int type_index);
          Alcotest.(check int) (name ^ " register") 17 register;
          Alcotest.(check string) (name ^ " name") "eax_var" n
      | _ -> Alcotest.fail (name ^ ": expected Register"))

let test_label32_roundtrip () =
  roundtrip_symbol "label32"
    (Pdb.Codeview_symbols.Label32
       { offset = u32 0x200; segment = 1; flags = 0; name = "$LN3" })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.Label32 { offset; segment; name = n; _ } ->
          Alcotest.(check int) (name ^ " offset") 0x200
            (Unsigned.UInt32.to_int offset);
          Alcotest.(check int) (name ^ " segment") 1 segment;
          Alcotest.(check string) (name ^ " name") "$LN3" n
      | _ -> Alcotest.fail (name ^ ": expected Label32"))

let test_ldata32_roundtrip () =
  roundtrip_symbol "ldata32"
    (Pdb.Codeview_symbols.LData32
       { type_index = ti 0x0074; offset = u32 0x6000; segment = 2;
         name = "static_var" })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.LData32 d ->
          Alcotest.(check string) (name ^ " name") "static_var" d.name;
          Alcotest.(check int) (name ^ " segment") 2 d.segment
      | _ -> Alcotest.fail (name ^ ": expected LData32"))

(** {2 Unknown record handling} *)

let write_u16_le buf v =
  Buffer.add_char buf (Char.chr (v land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xFF))

let test_envblock_roundtrip () =
  roundtrip_symbol "envblock"
    (Pdb.Codeview_symbols.EnvBlock
       {
         fields =
           [
             "cwd"; "C:\\project";
             "cl"; "C:\\msvc\\cl.exe";
             "cmd"; "-Zi -Od";
           ];
       })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.EnvBlock { fields } ->
          Alcotest.(check int) (name ^ " field count") 6 (List.length fields);
          Alcotest.(check string) (name ^ " field 0") "cwd" (List.nth fields 0);
          Alcotest.(check string) (name ^ " field 1") "C:\\project"
            (List.nth fields 1);
          Alcotest.(check string) (name ^ " field 4") "cmd" (List.nth fields 4);
          Alcotest.(check string) (name ^ " field 5") "-Zi -Od"
            (List.nth fields 5)
      | _ -> Alcotest.fail (name ^ ": expected EnvBlock"))

let test_envblock_empty () =
  roundtrip_symbol "envblock_empty"
    (Pdb.Codeview_symbols.EnvBlock { fields = [] })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.EnvBlock { fields } ->
          Alcotest.(check int) (name ^ " empty") 0 (List.length fields)
      | _ -> Alcotest.fail (name ^ ": expected EnvBlock"))

let test_unknown_symbol_record () =
  (* Construct a binary symbol record with an unrecognized kind (0xAAAA) *)
  let buf = Buffer.create 16 in
  let payload = "WXYZ" in
  let rec_len = 2 + String.length payload in
  write_u16_le buf rec_len;
  write_u16_le buf 0xAAAA;
  Buffer.add_string buf payload;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let len = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int in
  let result = Pdb.Codeview_symbols.parse_symbol_record cur len in
  match result with
  | Pdb.Codeview_symbols.Unknown { kind; data } ->
      Alcotest.(check int) "kind" 0xAAAA kind;
      Alcotest.(check int) "data length" 4 (String.length data);
      Alcotest.(check string) "data" "WXYZ" data
  | _ -> Alcotest.fail "expected Unknown symbol record"

let test_unknown_symbol_roundtrip () =
  (* CodeView symbol records are padded to 4-byte boundaries and the length
     field includes the padding (matching LLVM's wire format). For an
     [Unknown] record, the writer cannot distinguish padding from data, so
     the parser returns the full padded tail. A 2-byte payload becomes
     4 bytes (2 data + 2 padding). *)
  roundtrip_symbol "unknown"
    (Pdb.Codeview_symbols.Unknown { kind = 0xDEAD; data = "\xAA\xBB" })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.Unknown { kind; data } ->
          Alcotest.(check int) (name ^ " kind") 0xDEAD kind;
          Alcotest.(check int) (name ^ " data len") 4 (String.length data);
          Alcotest.(check char) (name ^ " data[0]") '\xAA' data.[0];
          Alcotest.(check char) (name ^ " data[1]") '\xBB' data.[1]
      | _ -> Alcotest.fail (name ^ ": expected Unknown"))

let test_unknown_symbol_empty_payload () =
  roundtrip_symbol "unknown_empty"
    (Pdb.Codeview_symbols.Unknown { kind = 0xFFFF; data = "" })
    (fun name r ->
      match r with
      | Pdb.Codeview_symbols.Unknown { kind; data } ->
          Alcotest.(check int) (name ^ " kind") 0xFFFF kind;
          Alcotest.(check int) (name ^ " data len") 0 (String.length data)
      | _ -> Alcotest.fail (name ^ ": expected Unknown"))

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
      ( "symbol_record_extended",
        [
          Alcotest.test_case "compile3" `Quick test_compile3_roundtrip;
          Alcotest.test_case "lproc32" `Quick test_lproc32_roundtrip;
          Alcotest.test_case "gproc32id" `Quick test_gproc32id_roundtrip;
          Alcotest.test_case "lproc32id" `Quick test_lproc32id_roundtrip;
          Alcotest.test_case "gthread32" `Quick test_gthread32_roundtrip;
          Alcotest.test_case "lthread32" `Quick test_lthread32_roundtrip;
          Alcotest.test_case "defrange_fp_rel" `Quick
            test_defrange_fp_rel_roundtrip;
          Alcotest.test_case "defrange_register_rel" `Quick
            test_defrange_register_rel_roundtrip;
          Alcotest.test_case "defrange_register" `Quick
            test_defrange_register_roundtrip;
          Alcotest.test_case "defrange_fp_rel_full_scope" `Quick
            test_defrange_fp_rel_full_scope_roundtrip;
          Alcotest.test_case "block32" `Quick test_block32_roundtrip;
          Alcotest.test_case "inlinesite" `Quick test_inlinesite_roundtrip;
          Alcotest.test_case "inlinesite_end" `Quick
            test_inlinesite_end_roundtrip;
          Alcotest.test_case "proc_id_end" `Quick test_proc_id_end_roundtrip;
          Alcotest.test_case "frameproc" `Quick test_frameproc_roundtrip;
          Alcotest.test_case "register" `Quick test_register_roundtrip;
          Alcotest.test_case "label32" `Quick test_label32_roundtrip;
          Alcotest.test_case "ldata32" `Quick test_ldata32_roundtrip;
        ] );
      ( "symbol_stream",
        [ Alcotest.test_case "roundtrip" `Quick test_symbol_stream_roundtrip ]
      );
      ( "envblock",
        [
          Alcotest.test_case "roundtrip" `Quick test_envblock_roundtrip;
          Alcotest.test_case "empty" `Quick test_envblock_empty;
        ] );
      ( "unknown_records",
        [
          Alcotest.test_case "unknown symbol from binary" `Quick
            test_unknown_symbol_record;
          Alcotest.test_case "unknown symbol roundtrip" `Quick
            test_unknown_symbol_roundtrip;
          Alcotest.test_case "unknown symbol empty payload" `Quick
            test_unknown_symbol_empty_payload;
        ] );
    ]
