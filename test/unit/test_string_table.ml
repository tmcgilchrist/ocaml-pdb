(** Tests for PDB global string table (/names stream). *)

module Buffer = Stdlib.Buffer
open Test_support

let test_empty_table () =
  let t = Pdb.Pdb_string_table.create () in
  Alcotest.(check int) "count" 0 (Pdb.Pdb_string_table.count t);
  (* Write and read back *)
  let buf = Buffer.create 64 in
  Pdb.Pdb_string_table.write buf t;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let t' = Pdb.Pdb_string_table.parse cur in
  Alcotest.(check int) "parsed count" 0 (Pdb.Pdb_string_table.count t')

let test_single_string () =
  let t = Pdb.Pdb_string_table.create () in
  let off = Pdb.Pdb_string_table.add_string t "hello.c" in
  Alcotest.(check bool) "offset > 0" true (off > 0);
  Alcotest.(check int) "count" 1 (Pdb.Pdb_string_table.count t);
  (* Lookup *)
  Alcotest.(check (option int))
    "lookup" (Some off)
    (Pdb.Pdb_string_table.lookup t "hello.c");
  Alcotest.(check (option int))
    "lookup missing" Option.None
    (Pdb.Pdb_string_table.lookup t "missing.c");
  (* Round-trip *)
  let buf = Buffer.create 128 in
  Pdb.Pdb_string_table.write buf t;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let t' = Pdb.Pdb_string_table.parse cur in
  Alcotest.(check int) "parsed count" 1 (Pdb.Pdb_string_table.count t');
  Alcotest.(check (option int))
    "parsed lookup" (Some off)
    (Pdb.Pdb_string_table.lookup t' "hello.c")

let test_multiple_strings () =
  let t = Pdb.Pdb_string_table.create () in
  let off1 = Pdb.Pdb_string_table.add_string t "foo.c" in
  let off2 = Pdb.Pdb_string_table.add_string t "bar.h" in
  let off3 = Pdb.Pdb_string_table.add_string t "baz.cpp" in
  Alcotest.(check int) "count" 3 (Pdb.Pdb_string_table.count t);
  (* Offsets should all be different *)
  Alcotest.(check bool)
    "different offsets" true
    (off1 <> off2 && off2 <> off3 && off1 <> off3);
  (* Round-trip *)
  let buf = Buffer.create 256 in
  Pdb.Pdb_string_table.write buf t;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let t' = Pdb.Pdb_string_table.parse cur in
  Alcotest.(check int) "parsed count" 3 (Pdb.Pdb_string_table.count t');
  Alcotest.(check (option int))
    "foo.c" (Some off1)
    (Pdb.Pdb_string_table.lookup t' "foo.c");
  Alcotest.(check (option int))
    "bar.h" (Some off2)
    (Pdb.Pdb_string_table.lookup t' "bar.h");
  Alcotest.(check (option int))
    "baz.cpp" (Some off3)
    (Pdb.Pdb_string_table.lookup t' "baz.cpp")

let test_deduplication () =
  let t = Pdb.Pdb_string_table.create () in
  let off1 = Pdb.Pdb_string_table.add_string t "same.c" in
  let off2 = Pdb.Pdb_string_table.add_string t "same.c" in
  Alcotest.(check int) "same offset" off1 off2;
  Alcotest.(check int) "count still 1" 1 (Pdb.Pdb_string_table.count t)

let test_windows_paths () =
  let t = Pdb.Pdb_string_table.create () in
  let off1 =
    Pdb.Pdb_string_table.add_string t "C:\\Users\\dev\\project\\main.c"
  in
  let off2 =
    Pdb.Pdb_string_table.add_string t "C:\\Users\\dev\\project\\util.h"
  in
  Alcotest.(check int) "count" 2 (Pdb.Pdb_string_table.count t);
  let buf = Buffer.create 256 in
  Pdb.Pdb_string_table.write buf t;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let t' = Pdb.Pdb_string_table.parse cur in
  Alcotest.(check (option int))
    "main.c" (Some off1)
    (Pdb.Pdb_string_table.lookup t' "C:\\Users\\dev\\project\\main.c");
  Alcotest.(check (option int))
    "util.h" (Some off2)
    (Pdb.Pdb_string_table.lookup t' "C:\\Users\\dev\\project\\util.h")

let test_many_strings () =
  (* Test with enough strings to exercise hash table collisions *)
  let t = Pdb.Pdb_string_table.create () in
  let offsets =
    Array.init 50 (fun i ->
        let name = Printf.sprintf "file_%03d.c" i in
        Pdb.Pdb_string_table.add_string t name)
  in
  Alcotest.(check int) "count" 50 (Pdb.Pdb_string_table.count t);
  let buf = Buffer.create 2048 in
  Pdb.Pdb_string_table.write buf t;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let t' = Pdb.Pdb_string_table.parse cur in
  Alcotest.(check int) "parsed count" 50 (Pdb.Pdb_string_table.count t');
  (* Verify all lookups work *)
  Array.iteri
    (fun i expected_off ->
      let name = Printf.sprintf "file_%03d.c" i in
      Alcotest.(check (option int))
        (Printf.sprintf "lookup %s" name)
        (Some expected_off)
        (Pdb.Pdb_string_table.lookup t' name))
    offsets

(** A header whose [byte_size] claims more names-buffer bytes than the cursor
    actually contains must surface as Invalid_format. *)
let test_truncated_mid_names_buffer () =
  let buf = Buffer.create 12 in
  let put_u32 v =
    Buffer.add_char buf (Char.chr (v land 0xFF));
    Buffer.add_char buf (Char.chr ((v lsr 8) land 0xFF));
    Buffer.add_char buf (Char.chr ((v lsr 16) land 0xFF));
    Buffer.add_char buf (Char.chr ((v lsr 24) land 0xFF))
  in
  put_u32 0xEFFEEFFE;
  (* signature *)
  put_u32 1;
  (* hash_version *)
  put_u32 100;
  (* byte_size larger than what follows (0 bytes) *)
  let bytes = Buffer.contents buf in
  let cur = Object.Buffer.cursor (buffer_of_string bytes) in
  match Pdb.Pdb_string_table.parse cur with
  | _ -> Alcotest.fail "expected Invalid_format"
  | exception Object.Buffer.Invalid_format _ -> ()

let () =
  Alcotest.run "PDB String Table"
    [
      ( "string_table",
        [
          Alcotest.test_case "empty" `Quick test_empty_table;
          Alcotest.test_case "single string" `Quick test_single_string;
          Alcotest.test_case "multiple strings" `Quick test_multiple_strings;
          Alcotest.test_case "deduplication" `Quick test_deduplication;
          Alcotest.test_case "windows paths" `Quick test_windows_paths;
          Alcotest.test_case "many strings" `Quick test_many_strings;
        ] );
      ( "malformed",
        [
          Alcotest.test_case "truncated mid names buffer" `Quick
            test_truncated_mid_names_buffer;
        ] );
    ]
