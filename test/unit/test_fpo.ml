(** Tests for the old-style FPO_DATA stream. *)

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

let sample : Pdb.Fpo.t =
  [|
    {
      offset = u32 0x1000;
      size = u32 32;
      num_locals = u32 4;
      num_params = 2;
      attributes = 0x4042;
    };
    {
      offset = u32 0x1100;
      size = u32 64;
      num_locals = u32 0;
      num_params = 0;
      attributes = 0x0000;
    };
  |]

let test_roundtrip () =
  let buf = Buffer.create 64 in
  Pdb.Fpo.write buf sample;
  let bytes = Buffer.contents buf in
  Alcotest.(check int) "16 bytes per entry" 32 (String.length bytes);
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let out = Pdb.Fpo.parse cur (String.length bytes) in
  Alcotest.(check int) "count" 2 (Array.length out);
  Alcotest.(check int) "entry 0 offset" 0x1000
    (Unsigned.UInt32.to_int out.(0).offset);
  Alcotest.(check int) "entry 0 size" 32
    (Unsigned.UInt32.to_int out.(0).size);
  Alcotest.(check int) "entry 0 num_locals" 4
    (Unsigned.UInt32.to_int out.(0).num_locals);
  Alcotest.(check int) "entry 0 num_params" 2 out.(0).num_params;
  Alcotest.(check int) "entry 0 attributes" 0x4042 out.(0).attributes;
  Alcotest.(check int) "entry 1 offset" 0x1100
    (Unsigned.UInt32.to_int out.(1).offset);
  Alcotest.(check int) "entry 1 attributes" 0x0000 out.(1).attributes

let test_empty () =
  let buf = Buffer.create 0 in
  Pdb.Fpo.write buf [||];
  Alcotest.(check int) "empty bytes" 0 (Buffer.length buf);
  let cur = Object.Buffer.cursor (buffer_of_string "") in
  let out = Pdb.Fpo.parse cur 0 in
  Alcotest.(check int) "empty array" 0 (Array.length out)

let test_non_multiple () =
  let cur = Object.Buffer.cursor (buffer_of_string (String.make 12 '\000')) in
  match Pdb.Fpo.parse cur 12 with
  | _ -> Alcotest.fail "expected Invalid_format"
  | exception Object.Buffer.Invalid_format _ -> ()

let test_truncated () =
  let cur = Object.Buffer.cursor (buffer_of_string (String.make 8 '\000')) in
  match Pdb.Fpo.parse cur 16 with
  | _ -> Alcotest.fail "expected Invalid_format"
  | exception Object.Buffer.Invalid_format _ -> ()

let () =
  Alcotest.run "FPO"
    [
      ( "wire",
        [
          Alcotest.test_case "roundtrip" `Quick test_roundtrip;
          Alcotest.test_case "empty" `Quick test_empty;
          Alcotest.test_case "non-multiple-of-16" `Quick test_non_multiple;
          Alcotest.test_case "truncated" `Quick test_truncated;
        ] );
    ]
