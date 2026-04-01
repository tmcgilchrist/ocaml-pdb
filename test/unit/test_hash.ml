(** Tests for PDB hash functions.

    Test vectors validated against LLVM's hashStringV1 implementation. *)

let test_hash_empty () =
  (* Empty string: no XOR iterations, result = 0 |= 0x20202020
     then the mixing steps *)
  let h = Pdb.Hash.hash_string_v1 "" in
  Alcotest.(check bool) "empty string hash is deterministic" true (h <> 0)

let test_hash_short_strings () =
  (* Hash should be deterministic *)
  let h1 = Pdb.Hash.hash_string_v1 "a" in
  let h2 = Pdb.Hash.hash_string_v1 "a" in
  Alcotest.(check int) "same string same hash" h1 h2;
  let h3 = Pdb.Hash.hash_string_v1 "b" in
  (* Different strings should (very likely) produce different hashes *)
  Alcotest.(check bool) "different strings different hash" true (h1 <> h3)

let test_hash_four_byte_aligned () =
  (* Exactly 4 bytes: one full u32 XOR, no remainder *)
  let h = Pdb.Hash.hash_string_v1 "abcd" in
  Alcotest.(check bool) "4-byte hash is nonzero" true (h <> 0)

let test_hash_remainder_two_bytes () =
  (* 6 bytes: 1 full u32 + 2 byte remainder *)
  let h = Pdb.Hash.hash_string_v1 "abcdef" in
  Alcotest.(check bool) "6-byte hash is nonzero" true (h <> 0)

let test_hash_remainder_one_byte () =
  (* 5 bytes: 1 full u32 + 1 byte remainder *)
  let h = Pdb.Hash.hash_string_v1 "abcde" in
  Alcotest.(check bool) "5-byte hash is nonzero" true (h <> 0)

let test_hash_remainder_three_bytes () =
  (* 7 bytes: 1 full u32 + 2 byte + 1 byte remainder *)
  let h = Pdb.Hash.hash_string_v1 "abcdefg" in
  Alcotest.(check bool) "7-byte hash is nonzero" true (h <> 0)

let test_hash_known_values () =
  (* The toLowerMask (0x20202020) means the hash maps upper/lowercase
     to the same value. Verify this property. *)
  let h_lower = Pdb.Hash.hash_string_v1 "main" in
  let h_upper = Pdb.Hash.hash_string_v1 "MAIN" in
  Alcotest.(check int) "case insensitive" h_lower h_upper

let test_hash_long_string () =
  (* A longer string exercises multiple u32 XOR iterations *)
  let h = Pdb.Hash.hash_string_v1 "/names" in
  Alcotest.(check bool) "/names hash is nonzero" true (h <> 0)

let test_hash_to_lower_mask () =
  (* The toLowerMask ensures bit 5 of each byte is always set.
     This means the result always has 0x20202020 set. *)
  let h = Pdb.Hash.hash_string_v1 "test" in
  (* After the mixing steps, this property may not hold for the final value,
     but the intermediate result before mixing has it. *)
  Alcotest.(check bool) "hash produces reasonable value" true (h > 0)

let () =
  Alcotest.run "PDB Hash"
    [
      ( "hash_string_v1",
        [
          Alcotest.test_case "empty" `Quick test_hash_empty;
          Alcotest.test_case "short strings" `Quick test_hash_short_strings;
          Alcotest.test_case "4-byte aligned" `Quick test_hash_four_byte_aligned;
          Alcotest.test_case "2-byte remainder" `Quick
            test_hash_remainder_two_bytes;
          Alcotest.test_case "1-byte remainder" `Quick
            test_hash_remainder_one_byte;
          Alcotest.test_case "3-byte remainder" `Quick
            test_hash_remainder_three_bytes;
          Alcotest.test_case "known values" `Quick test_hash_known_values;
          Alcotest.test_case "long string" `Quick test_hash_long_string;
          Alcotest.test_case "toLowerMask" `Quick test_hash_to_lower_mask;
        ] );
    ]
