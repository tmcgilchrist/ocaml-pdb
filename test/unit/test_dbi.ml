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
  Pdb.Dbi_write.write buf modules sc ~machine:0x8664;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let dbi = Pdb.Dbi.parse cur in
  Alcotest.(check int) "machine" 0x8664 dbi.header.machine;
  Alcotest.(check int) "module count" 1 (Array.length dbi.modules);
  Alcotest.(check string) "module name" "simple.obj"
    dbi.modules.(0).module_name

let test_dbi_multiple_modules () =
  let modules =
    [
      make_module_info ~name:"a.obj" ~obj:"a.obj" ();
      make_module_info ~name:"b.obj" ~obj:"b.obj" ();
      make_module_info ~name:"* Linker *" ~obj:"" ();
    ]
  in
  let sc = [ make_section_contrib ~module_index:0 ();
             make_section_contrib ~module_index:1 ~section:2 () ] in
  let buf = Buffer.create 512 in
  Pdb.Dbi_write.write buf modules sc ~machine:0x14C;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let dbi = Pdb.Dbi.parse cur in
  Alcotest.(check int) "module count" 3 (Array.length dbi.modules);
  Alcotest.(check string) "module 0" "a.obj" dbi.modules.(0).module_name;
  Alcotest.(check string) "module 1" "b.obj" dbi.modules.(1).module_name;
  Alcotest.(check string) "module 2" "* Linker *"
    dbi.modules.(2).module_name;
  Alcotest.(check int) "machine" 0x14C dbi.header.machine;
  Alcotest.(check int) "sc count" 2
    (Array.length dbi.section_contributions)

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
  Pdb.Dbi_write.write buf modules sc ~machine:0x8664;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let dbi = Pdb.Dbi.parse cur in
  Alcotest.(check int) "sc count" 3
    (Array.length dbi.section_contributions);
  Alcotest.(check int) "sc[0] section" 1 dbi.section_contributions.(0).section;
  Alcotest.(check int) "sc[1] offset" 50
    (Int32.to_int dbi.section_contributions.(1).offset);
  Alcotest.(check int) "sc[2] size" 200
    (Int32.to_int dbi.section_contributions.(2).size)

let test_dbi_empty () =
  let buf = Buffer.create 128 in
  Pdb.Dbi_write.write buf [] [] ~machine:0;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let dbi = Pdb.Dbi.parse cur in
  Alcotest.(check int) "empty modules" 0 (Array.length dbi.modules);
  Alcotest.(check int) "empty sc" 0
    (Array.length dbi.section_contributions)

let test_dbi_obj_file_name () =
  let m =
    make_module_info ~name:"foo.obj"
      ~obj:"C:\\Users\\dev\\project\\foo.obj" ()
  in
  let buf = Buffer.create 256 in
  Pdb.Dbi_write.write buf [ m ] [] ~machine:0x8664;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let dbi = Pdb.Dbi.parse cur in
  Alcotest.(check string) "obj file name"
    "C:\\Users\\dev\\project\\foo.obj" dbi.modules.(0).obj_file_name

let () =
  Alcotest.run "DBI Stream"
    [
      ( "dbi",
        [
          Alcotest.test_case "header roundtrip" `Quick
            test_dbi_header_roundtrip;
          Alcotest.test_case "multiple modules" `Quick
            test_dbi_multiple_modules;
          Alcotest.test_case "section contributions" `Quick
            test_dbi_section_contributions;
          Alcotest.test_case "empty" `Quick test_dbi_empty;
          Alcotest.test_case "obj file name" `Quick test_dbi_obj_file_name;
        ] );
    ]
