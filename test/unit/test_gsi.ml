(** Tests for GSI/PSI hash table read/write. *)

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

let test_gsi_empty () =
  let buf = Buffer.create 64 in
  Pdb.Gsi_write.write_gsi buf [];
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let gsi = Pdb.Gsi.parse_gsi cur (String.length bytes) in
  Alcotest.(check int) "empty records" 0 (Array.length gsi.hash_records)

let test_gsi_single_entry () =
  let entries =
    [ { Pdb.Gsi_write.name = "main"; sym_offset = 0 } ]
  in
  let buf = Buffer.create 128 in
  Pdb.Gsi_write.write_gsi buf entries;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let gsi = Pdb.Gsi.parse_gsi cur (String.length bytes) in
  Alcotest.(check int) "one record" 1 (Array.length gsi.hash_records);
  (* The offset should be sym_offset + 1 = 1 *)
  Alcotest.(check int) "offset is sym_offset+1" 1
    (Unsigned.UInt32.to_int gsi.hash_records.(0).offset);
  Alcotest.(check int) "cref is 1" 1
    (Unsigned.UInt32.to_int gsi.hash_records.(0).cref)

let test_gsi_multiple_entries () =
  let entries =
    [
      { Pdb.Gsi_write.name = "main"; sym_offset = 0 };
      { Pdb.Gsi_write.name = "foo"; sym_offset = 50 };
      { Pdb.Gsi_write.name = "bar"; sym_offset = 100 };
      { Pdb.Gsi_write.name = "baz"; sym_offset = 150 };
    ]
  in
  let buf = Buffer.create 256 in
  Pdb.Gsi_write.write_gsi buf entries;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let gsi = Pdb.Gsi.parse_gsi cur (String.length bytes) in
  Alcotest.(check int) "four records" 4 (Array.length gsi.hash_records);
  (* All cref values should be 1 *)
  Array.iter
    (fun (hr : Pdb.Gsi.hash_record) ->
      Alcotest.(check int) "cref=1" 1 (Unsigned.UInt32.to_int hr.cref))
    gsi.hash_records;
  (* All offsets should be sym_offset + 1 for one of the entries *)
  let offsets =
    Array.to_list
      (Array.map
         (fun (hr : Pdb.Gsi.hash_record) -> Unsigned.UInt32.to_int hr.offset)
         gsi.hash_records)
  in
  (* Offsets are sym_offset+1: 1, 51, 101, 151 *)
  List.iter
    (fun expected ->
      Alcotest.(check bool)
        (Printf.sprintf "has offset %d" expected)
        true (List.mem expected offsets))
    [ 1; 51; 101; 151 ]

let test_gsi_hash_buckets_nonempty () =
  (* With entries, the hash_buckets array should be non-empty *)
  let entries =
    [
      { Pdb.Gsi_write.name = "alpha"; sym_offset = 0 };
      { Pdb.Gsi_write.name = "beta"; sym_offset = 20 };
    ]
  in
  let buf = Buffer.create 256 in
  Pdb.Gsi_write.write_gsi buf entries;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let gsi = Pdb.Gsi.parse_gsi cur (String.length bytes) in
  Alcotest.(check bool) "has buckets" true
    (Array.length gsi.hash_buckets > 0)

let test_publics_stream () =
  let symbols =
    [
      Pdb.Codeview_symbols.Pub32
        { flags = u32 2; offset = u32 0x1000; segment = 1; name = "_main" };
      Pdb.Codeview_symbols.Pub32
        { flags = u32 0; offset = u32 0x2000; segment = 1; name = "_foo" };
    ]
  in
  let buf = Buffer.create 512 in
  Pdb.Gsi_write.write_publics_stream buf symbols;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  (* Parse the publics header *)
  let ph = Pdb.Gsi.parse_publics_header cur in
  Alcotest.(check bool) "sym_hash_size > 0" true (ph.sym_hash_size > 0);
  Alcotest.(check bool) "addr_map_size > 0" true (ph.addr_map_size > 0);
  Alcotest.(check int) "num_thunks" 0 ph.num_thunks;
  (* Parse the GSI hash that follows *)
  let gsi = Pdb.Gsi.parse_gsi cur ph.sym_hash_size in
  Alcotest.(check int) "2 hash records" 2 (Array.length gsi.hash_records)

(** {2 build_gsi_streams tests} *)

let test_build_gsi_streams () =
  let publics =
    [
      Pdb.Codeview_symbols.Pub32
        { flags = u32 2; offset = u32 0x1000; segment = 1; name = "_main" };
      Pdb.Codeview_symbols.Pub32
        { flags = u32 0; offset = u32 0x2000; segment = 1; name = "_helper" };
    ]
  in
  let globals =
    [
      Pdb.Codeview_symbols.GData32
        { type_index = u32 0x0074; offset = u32 0x3000; segment = 2;
          name = "g_count" };
    ]
  in
  let streams = Pdb.Gsi_write.build_gsi_streams ~publics ~globals in
  (* Symbol record stream should contain all 3 records *)
  Alcotest.(check bool) "sym_record non-empty" true
    (String.length streams.sym_record_stream > 0);
  (* Parse the sym record stream to verify contents *)
  let sym_buf = buffer_of_string streams.sym_record_stream in
  let sym_cur = Object.Buffer.cursor sym_buf in
  let syms =
    List.of_seq
      (Pdb.Codeview_symbols.parse_symbol_stream sym_cur
         (String.length streams.sym_record_stream))
  in
  Alcotest.(check int) "3 symbol records" 3 (List.length syms);
  (* First two should be publics *)
  (match List.nth syms 0 with
  | Pdb.Codeview_symbols.Pub32 { name; _ } ->
      Alcotest.(check string) "pub 0" "_main" name
  | _ -> Alcotest.fail "expected Pub32");
  (* Third should be global *)
  (match List.nth syms 2 with
  | Pdb.Codeview_symbols.GData32 d ->
      Alcotest.(check string) "global" "g_count" d.name
  | _ -> Alcotest.fail "expected GData32");
  (* Publics stream should be parseable *)
  let pub_buf = buffer_of_string streams.publics_stream in
  let pub_cur = Object.Buffer.cursor pub_buf in
  let ph = Pdb.Gsi.parse_publics_header pub_cur in
  Alcotest.(check bool) "pub sym_hash > 0" true (ph.sym_hash_size > 0);
  (* Globals stream should be parseable *)
  let gbl_buf = buffer_of_string streams.globals_stream in
  let gbl_cur = Object.Buffer.cursor gbl_buf in
  let gsi = Pdb.Gsi.parse_gsi gbl_cur (String.length streams.globals_stream) in
  Alcotest.(check int) "1 global hash record" 1
    (Array.length gsi.hash_records)

let test_build_gsi_streams_empty () =
  let streams = Pdb.Gsi_write.build_gsi_streams ~publics:[] ~globals:[] in
  Alcotest.(check int) "empty sym record" 0
    (String.length streams.sym_record_stream)

let test_dbi_write_full_roundtrip () =
  let buf = Buffer.create 256 in
  Pdb.Dbi_write.write_full buf [] []
    ~machine:0x8664 ~global_stream:7 ~public_stream:8 ~sym_record_stream:9;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let dbi = Pdb.Dbi.parse cur in
  Alcotest.(check int) "global stream" 7 dbi.header.global_stream_index;
  Alcotest.(check int) "public stream" 8 dbi.header.public_stream_index;
  Alcotest.(check int) "sym record stream" 9 dbi.header.sym_record_stream

let () =
  Alcotest.run "GSI"
    [
      ( "gsi_write",
        [
          Alcotest.test_case "empty" `Quick test_gsi_empty;
          Alcotest.test_case "single entry" `Quick test_gsi_single_entry;
          Alcotest.test_case "multiple entries" `Quick test_gsi_multiple_entries;
          Alcotest.test_case "hash buckets" `Quick
            test_gsi_hash_buckets_nonempty;
        ] );
      ( "publics",
        [ Alcotest.test_case "publics stream" `Quick test_publics_stream ] );
      ( "build_gsi_streams",
        [
          Alcotest.test_case "with symbols" `Quick test_build_gsi_streams;
          Alcotest.test_case "empty" `Quick test_build_gsi_streams_empty;
        ] );
      ( "dbi_wiring",
        [
          Alcotest.test_case "write_full roundtrip" `Quick
            test_dbi_write_full_roundtrip;
        ] );
    ]
