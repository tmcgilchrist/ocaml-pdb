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
        [
          Alcotest.test_case "publics stream" `Quick test_publics_stream;
        ] );
    ]
