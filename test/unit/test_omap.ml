(** Tests for OMAP address-translation streams. *)

module Buffer = Stdlib.Buffer

open Test_support

let u32 n = Unsigned.UInt32.of_int n
let u32_to_int = Unsigned.UInt32.to_int

let sample : Pdb.Omap.t =
  [|
    { rva = u32 0x1000; rva_to = u32 0x4000 };
    { rva = u32 0x1100; rva_to = u32 0x4200 };
    { rva = u32 0x1200; rva_to = u32 0x0000 };
    (* unmapped *)
    { rva = u32 0x1300; rva_to = u32 0x4500 };
  |]

let test_roundtrip () =
  let buf = Buffer.create 64 in
  Pdb.Omap.write buf sample;
  let bytes = Buffer.contents buf in
  Alcotest.(check int) "8 bytes per entry" 32 (String.length bytes);
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let out = Pdb.Omap.parse cur (String.length bytes) in
  Alcotest.(check int) "count" 4 (Array.length out);
  Alcotest.(check int) "entry 0 rva" 0x1000 (u32_to_int out.(0).rva);
  Alcotest.(check int) "entry 0 rva_to" 0x4000 (u32_to_int out.(0).rva_to);
  Alcotest.(check int) "entry 3 rva_to" 0x4500 (u32_to_int out.(3).rva_to)

let test_lookup_exact () =
  Alcotest.(check (option int))
    "exact hit on entry 1" (Some 0x4200)
    (Option.map u32_to_int (Pdb.Omap.lookup sample (u32 0x1100)))

let test_lookup_interval () =
  (* 0x10A0 falls between 0x1000 and 0x1100; map via entry 0:
     rva_to=0x4000 + (0x10A0 - 0x1000) = 0x40A0. *)
  Alcotest.(check (option int))
    "interval mapped" (Some 0x40A0)
    (Option.map u32_to_int (Pdb.Omap.lookup sample (u32 0x10A0)))

let test_lookup_unmapped () =
  (* 0x1280 falls in entry 2's interval, which has rva_to=0 (unmapped). *)
  Alcotest.(check (option int))
    "unmapped interval" None
    (Option.map u32_to_int (Pdb.Omap.lookup sample (u32 0x1280)))

let test_lookup_below_first () =
  Alcotest.(check (option int))
    "below first entry" None
    (Option.map u32_to_int (Pdb.Omap.lookup sample (u32 0x500)))

let test_lookup_after_last () =
  (* 0x1500 falls after entry 3 (no upper bound), maps via entry 3:
     0x4500 + 0x200 = 0x4700. *)
  Alcotest.(check (option int))
    "above last entry" (Some 0x4700)
    (Option.map u32_to_int (Pdb.Omap.lookup sample (u32 0x1500)))

let test_empty () =
  let buf = Buffer.create 0 in
  Pdb.Omap.write buf [||];
  Alcotest.(check int) "empty bytes" 0 (Buffer.length buf);
  let obj_buf = buffer_of_string "" in
  let cur = Object.Buffer.cursor obj_buf in
  let out = Pdb.Omap.parse cur 0 in
  Alcotest.(check int) "empty array" 0 (Array.length out);
  Alcotest.(check (option int))
    "lookup on empty" None
    (Option.map u32_to_int (Pdb.Omap.lookup out (u32 0x1000)))

let test_truncated () =
  let obj_buf = buffer_of_string "\x00\x00\x00\x00" in
  (* 4 bytes — not multiple of 8 *)
  let cur = Object.Buffer.cursor obj_buf in
  match Pdb.Omap.parse cur 4 with
  | _ -> Alcotest.fail "expected Invalid_format"
  | exception Object.Buffer.Invalid_format _ -> ()

(** A [total_bytes] that is a multiple of 8 but exceeds what the cursor
    actually contains must surface as Invalid_format, not as a Bigarray
    bounds error. *)
let test_truncated_mid_entry () =
  let obj_buf = buffer_of_string (String.make 8 '\x00') in
  let cur = Object.Buffer.cursor obj_buf in
  match Pdb.Omap.parse cur 16 with
  | _ -> Alcotest.fail "expected Invalid_format"
  | exception Object.Buffer.Invalid_format _ -> ()

let () =
  Alcotest.run "OMAP"
    [
      ( "wire",
        [
          Alcotest.test_case "roundtrip" `Quick test_roundtrip;
          Alcotest.test_case "empty" `Quick test_empty;
          Alcotest.test_case "truncated" `Quick test_truncated;
          Alcotest.test_case "truncated mid-entry" `Quick
            test_truncated_mid_entry;
        ] );
      ( "lookup",
        [
          Alcotest.test_case "exact hit" `Quick test_lookup_exact;
          Alcotest.test_case "interval" `Quick test_lookup_interval;
          Alcotest.test_case "unmapped interval" `Quick test_lookup_unmapped;
          Alcotest.test_case "below first entry" `Quick test_lookup_below_first;
          Alcotest.test_case "above last entry" `Quick test_lookup_after_last;
        ] );
    ]
