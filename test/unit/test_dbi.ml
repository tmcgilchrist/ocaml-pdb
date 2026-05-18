(** Tests for DBI stream read/write. *)

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

let make_section_contrib ?(section = 1) ?(offset = 0l) ?(size = 100l)
    ?(module_index = 0) () : Pdb.Dbi.section_contribution =
  {
    section;
    offset;
    size;
    characteristics = u32 0x60000020;
    module_index;
    data_crc = u32 0;
    reloc_crc = u32 0;
  }

let make_module_info ?(name = "test.obj") ?(obj = "test.obj")
    ?(sym_stream = 0xFFFF) ?(sym_bytes = 0) () : Pdb.Dbi.module_info =
  {
    section_contrib = make_section_contrib ();
    flags = 0;
    module_sym_stream = sym_stream;
    sym_byte_size = sym_bytes;
    c11_byte_size = 0;
    c13_byte_size = 0;
    source_file_count = 0;
    module_name = name;
    obj_file_name = obj;
  }

let test_dbi_header_roundtrip () =
  let modules = [ make_module_info ~name:"simple.obj" () ] in
  let sc = [ make_section_contrib () ] in
  let buf = Buffer.create 256 in
  Pdb.Dbi_write.write buf modules sc ~source_files:[] ~machine:0x8664;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let dbi = Pdb.Dbi.parse cur in
  Alcotest.(check int) "machine" 0x8664 dbi.header.machine;
  Alcotest.(check int) "module count" 1 (Array.length dbi.modules);
  Alcotest.(check string) "module name" "simple.obj" dbi.modules.(0).module_name

let test_dbi_multiple_modules () =
  let modules =
    [
      make_module_info ~name:"a.obj" ~obj:"a.obj" ();
      make_module_info ~name:"b.obj" ~obj:"b.obj" ();
      make_module_info ~name:"* Linker *" ~obj:"" ();
    ]
  in
  let sc =
    [
      make_section_contrib ~module_index:0 ();
      make_section_contrib ~module_index:1 ~section:2 ();
    ]
  in
  let buf = Buffer.create 512 in
  Pdb.Dbi_write.write buf modules sc ~source_files:[] ~machine:0x14C;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let dbi = Pdb.Dbi.parse cur in
  Alcotest.(check int) "module count" 3 (Array.length dbi.modules);
  Alcotest.(check string) "module 0" "a.obj" dbi.modules.(0).module_name;
  Alcotest.(check string) "module 1" "b.obj" dbi.modules.(1).module_name;
  Alcotest.(check string) "module 2" "* Linker *" dbi.modules.(2).module_name;
  Alcotest.(check int) "machine" 0x14C dbi.header.machine;
  Alcotest.(check int) "sc count" 2 (Array.length dbi.section_contributions)

let test_dbi_section_contributions () =
  let sc =
    [
      make_section_contrib ~section:1 ~offset:0l ~size:50l ~module_index:0 ();
      make_section_contrib ~section:1 ~offset:50l ~size:100l ~module_index:1 ();
      make_section_contrib ~section:2 ~offset:0l ~size:200l ~module_index:0 ();
    ]
  in
  let modules = [ make_module_info (); make_module_info ~name:"b.obj" () ] in
  let buf = Buffer.create 512 in
  Pdb.Dbi_write.write buf modules sc ~source_files:[] ~machine:0x8664;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let dbi = Pdb.Dbi.parse cur in
  Alcotest.(check int) "sc count" 3 (Array.length dbi.section_contributions);
  Alcotest.(check int) "sc[0] section" 1 dbi.section_contributions.(0).section;
  Alcotest.(check int)
    "sc[1] offset" 50
    (Int32.to_int dbi.section_contributions.(1).offset);
  Alcotest.(check int)
    "sc[2] size" 200
    (Int32.to_int dbi.section_contributions.(2).size)

let test_dbi_empty () =
  let buf = Buffer.create 128 in
  Pdb.Dbi_write.write buf [] [] ~source_files:[] ~machine:0;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let dbi = Pdb.Dbi.parse cur in
  Alcotest.(check int) "empty modules" 0 (Array.length dbi.modules);
  Alcotest.(check int) "empty sc" 0 (Array.length dbi.section_contributions)

let test_dbi_obj_file_name () =
  let m =
    make_module_info ~name:"foo.obj" ~obj:"C:\\Users\\dev\\project\\foo.obj" ()
  in
  let buf = Buffer.create 256 in
  Pdb.Dbi_write.write buf [ m ] [] ~source_files:[] ~machine:0x8664;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let dbi = Pdb.Dbi.parse cur in
  Alcotest.(check string)
    "obj file name" "C:\\Users\\dev\\project\\foo.obj"
    dbi.modules.(0).obj_file_name

(** {2 DBI substreams and header fields} *)

let test_dbi_header_fields () =
  let modules =
    [ make_module_info ~name:"a.obj" ~sym_stream:5 ~sym_bytes:100 () ]
  in
  let sc = [ make_section_contrib ~section:1 ~offset:0l ~size:200l () ] in
  let buf = Buffer.create 512 in
  Pdb.Dbi_write.write buf modules sc ~source_files:[] ~machine:0x8664;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let dbi = Pdb.Dbi.parse cur in
  (* Verify header fields *)
  Alcotest.(check int) "machine" 0x8664 dbi.header.machine;
  Alcotest.(check int) "global_stream" 0xFFFF dbi.header.global_stream_index;
  Alcotest.(check int) "public_stream" 0xFFFF dbi.header.public_stream_index;
  Alcotest.(check int) "sym_record_stream" 0xFFFF dbi.header.sym_record_stream;
  (* Verify module fields preserved *)
  Alcotest.(check int) "mod sym_stream" 5 dbi.modules.(0).module_sym_stream;
  Alcotest.(check int) "mod sym_bytes" 100 dbi.modules.(0).sym_byte_size;
  (* Writer emits a 22-byte optional debug header (11 x u16, all 0xFFFF) *)
  Alcotest.(check int) "opt debug header size" 22
    dbi.header.optional_dbg_header_size

let test_dbi_module_c13_fields () =
  let m : Pdb.Dbi.module_info =
    {
      section_contrib = make_section_contrib ();
      flags = 0;
      module_sym_stream = 7;
      sym_byte_size = 200;
      c11_byte_size = 0;
      c13_byte_size = 300;
      source_file_count = 2;
      module_name = "test.obj";
      obj_file_name = "test.obj";
    }
  in
  let buf = Buffer.create 256 in
  Pdb.Dbi_write.write buf [ m ] [] ~source_files:[] ~machine:0x14C;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let dbi = Pdb.Dbi.parse cur in
  Alcotest.(check int) "c13_byte_size" 300 dbi.modules.(0).c13_byte_size;
  Alcotest.(check int) "source_file_count" 2
    dbi.modules.(0).source_file_count;
  Alcotest.(check int) "sym_byte_size" 200 dbi.modules.(0).sym_byte_size

let test_dbi_section_contrib_fields () =
  let sc : Pdb.Dbi.section_contribution =
    {
      section = 2;
      offset = 0x1000l;
      size = 0x500l;
      characteristics = Unsigned.UInt32.of_int 0x60000020;
      module_index = 1;
      data_crc = Unsigned.UInt32.of_int 0xDEADBEEF;
      reloc_crc = Unsigned.UInt32.of_int 0xCAFEBABE;
    }
  in
  let modules = [ make_module_info (); make_module_info ~name:"b.obj" () ] in
  let buf = Buffer.create 512 in
  Pdb.Dbi_write.write buf modules [ sc ] ~source_files:[] ~machine:0x8664;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let dbi = Pdb.Dbi.parse cur in
  Alcotest.(check int) "sc count" 1
    (Array.length dbi.section_contributions);
  let s = dbi.section_contributions.(0) in
  Alcotest.(check int) "sc section" 2 s.section;
  Alcotest.(check int) "sc offset" 0x1000 (Int32.to_int s.offset);
  Alcotest.(check int) "sc size" 0x500 (Int32.to_int s.size);
  Alcotest.(check int) "sc characteristics" 0x60000020
    (Unsigned.UInt32.to_int s.characteristics);
  Alcotest.(check int) "sc module_index" 1 s.module_index;
  Alcotest.(check int) "sc data_crc" 0xDEADBEEF
    (Unsigned.UInt32.to_int s.data_crc land 0xFFFFFFFF);
  Alcotest.(check int) "sc reloc_crc" 0xCAFEBABE
    (Unsigned.UInt32.to_int s.reloc_crc land 0xFFFFFFFF)

let test_dbi_version_signature () =
  let buf = Buffer.create 128 in
  Pdb.Dbi_write.write buf [] [] ~source_files:[] ~machine:0;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let dbi = Pdb.Dbi.parse cur in
  (* Version signature should be -1 *)
  Alcotest.(check int) "version_signature" (-1)
    (Int32.to_int dbi.header.version_signature);
  (* Version header should be V70 = 19990903 *)
  Alcotest.(check int) "version_header" 19990903
    (Unsigned.UInt32.to_int dbi.header.version_header)

let () =
  Alcotest.run "DBI Stream"
    [
      ( "dbi",
        [
          Alcotest.test_case "header roundtrip" `Quick test_dbi_header_roundtrip;
          Alcotest.test_case "multiple modules" `Quick test_dbi_multiple_modules;
          Alcotest.test_case "section contributions" `Quick
            test_dbi_section_contributions;
          Alcotest.test_case "empty" `Quick test_dbi_empty;
          Alcotest.test_case "obj file name" `Quick test_dbi_obj_file_name;
        ] );
      ( "substreams",
        [
          Alcotest.test_case "header fields" `Quick test_dbi_header_fields;
          Alcotest.test_case "module c13 fields" `Quick
            test_dbi_module_c13_fields;
          Alcotest.test_case "section contrib fields" `Quick
            test_dbi_section_contrib_fields;
          Alcotest.test_case "version signature" `Quick
            test_dbi_version_signature;
        ] );
    ]
