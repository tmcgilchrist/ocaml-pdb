(** Tests for PDB Named Stream Map and Hash Table. *)

module Buffer = Stdlib.Buffer

(** Helper to convert a string to an Object.Buffer.t *)
let buffer_of_string s =
  let len = String.length s in
  let buf =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout len
  in
  for i = 0 to len - 1 do
    buf.{i} <- Char.code s.[i]
  done;
  buf

let test_hash_table_roundtrip () =
  (* Write a hash table, read it back *)
  let entries = [ (0, 5); (10, 7); (20, 3) ] in
  let capacity = 8 in
  let buf = Buffer.create 64 in
  Pdb.Named_stream_map.write_hash_table buf entries capacity;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let result = Pdb.Named_stream_map.parse_hash_table cur in
  (* Should have same number of entries *)
  Alcotest.(check int) "entry count" 3 (List.length result);
  (* All original entries should be present (order may differ due to hashing) *)
  List.iter
    (fun (k, v) ->
      Alcotest.(check bool)
        (Printf.sprintf "contains (%d, %d)" k v)
        true
        (List.exists (fun (k', v') -> k = k' && v = v') result))
    entries

let test_hash_table_empty () =
  let buf = Buffer.create 32 in
  Pdb.Named_stream_map.write_hash_table buf [] 1;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let result = Pdb.Named_stream_map.parse_hash_table cur in
  Alcotest.(check int) "empty table" 0 (List.length result)

let test_named_stream_map_roundtrip () =
  (* Write a named stream map, read it back *)
  let entries = [ ("/names", 5); ("/LinkInfo", 8); ("/src/headerblock", 12) ] in
  let buf = Buffer.create 128 in
  Pdb.Named_stream_map.write buf entries;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let result = Pdb.Named_stream_map.parse cur in
  Alcotest.(check int) "entry count" 3 (List.length result);
  (* Verify all entries are present *)
  List.iter
    (fun (name, idx) ->
      Alcotest.(check bool)
        (Printf.sprintf "contains (%s, %d)" name idx)
        true
        (List.exists (fun (n, i) -> n = name && i = idx) result))
    entries

let test_named_stream_map_single () =
  let entries = [ ("/names", 42) ] in
  let buf = Buffer.create 64 in
  Pdb.Named_stream_map.write buf entries;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let result = Pdb.Named_stream_map.parse cur in
  Alcotest.(check int) "single entry count" 1 (List.length result);
  let name, idx = List.hd result in
  Alcotest.(check string) "name" "/names" name;
  Alcotest.(check int) "index" 42 idx

let test_named_stream_map_empty () =
  let entries = [] in
  let buf = Buffer.create 32 in
  Pdb.Named_stream_map.write buf entries;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let result = Pdb.Named_stream_map.parse cur in
  Alcotest.(check int) "empty map" 0 (List.length result)

let () =
  Alcotest.run "Named Stream Map"
    [
      ( "hash_table",
        [
          Alcotest.test_case "roundtrip" `Quick test_hash_table_roundtrip;
          Alcotest.test_case "empty" `Quick test_hash_table_empty;
        ] );
      ( "named_stream_map",
        [
          Alcotest.test_case "roundtrip" `Quick test_named_stream_map_roundtrip;
          Alcotest.test_case "single entry" `Quick test_named_stream_map_single;
          Alcotest.test_case "empty" `Quick test_named_stream_map_empty;
        ] );
    ]
