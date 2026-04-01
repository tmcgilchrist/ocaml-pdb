(** Tests for MSF container read/write round-trip. *)

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

(** Helper to convert an Object.Buffer.t to a string *)
let string_of_buffer (buf : Object.Buffer.t) =
  let len = Bigarray.Array1.dim buf in
  let s = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set s i (Char.chr buf.{i})
  done;
  Bytes.to_string s

let test_roundtrip_empty_streams () =
  (* Create an MSF with two empty streams *)
  let builder = Pdb.Msf_write.create ~block_size:4096 in
  let _s0 = Pdb.Msf_write.add_empty_stream builder in
  let _s1 = Pdb.Msf_write.add_empty_stream builder in
  let msf_bytes = Pdb.Msf_write.finalize builder in
  let buf = buffer_of_string msf_bytes in
  let msf = Pdb.Msf.read buf in
  Alcotest.(check int) "stream count" 2 (Pdb.Msf.stream_count msf);
  let s0 = Pdb.Msf.get_stream_exn msf 0 in
  let s1 = Pdb.Msf.get_stream_exn msf 1 in
  Alcotest.(check int) "stream 0 size" 0 (Bigarray.Array1.dim s0);
  Alcotest.(check int) "stream 1 size" 0 (Bigarray.Array1.dim s1)

let test_roundtrip_with_data () =
  (* Create an MSF with streams containing known data *)
  let builder = Pdb.Msf_write.create ~block_size:512 in
  let data0 = "Hello, PDB!" in
  let data1 = String.make 1000 'X' in
  let data2 = "" in
  let _s0 = Pdb.Msf_write.add_stream builder data0 in
  let _s1 = Pdb.Msf_write.add_stream builder data1 in
  let _s2 = Pdb.Msf_write.add_stream builder data2 in
  let msf_bytes = Pdb.Msf_write.finalize builder in
  let buf = buffer_of_string msf_bytes in
  let msf = Pdb.Msf.read buf in
  Alcotest.(check int) "stream count" 3 (Pdb.Msf.stream_count msf);
  (* Verify stream 0 *)
  let s0 = Pdb.Msf.get_stream_exn msf 0 in
  Alcotest.(check int) "stream 0 size" (String.length data0)
    (Bigarray.Array1.dim s0);
  Alcotest.(check string) "stream 0 content" data0 (string_of_buffer s0);
  (* Verify stream 1 *)
  let s1 = Pdb.Msf.get_stream_exn msf 1 in
  Alcotest.(check int) "stream 1 size" (String.length data1)
    (Bigarray.Array1.dim s1);
  Alcotest.(check string) "stream 1 content" data1 (string_of_buffer s1);
  (* Verify stream 2 *)
  let s2 = Pdb.Msf.get_stream_exn msf 2 in
  Alcotest.(check int) "stream 2 size" 0 (Bigarray.Array1.dim s2)

let test_roundtrip_large_stream () =
  (* A stream larger than one block *)
  let builder = Pdb.Msf_write.create ~block_size:512 in
  let data = String.init 2000 (fun i -> Char.chr (i mod 256)) in
  let _s0 = Pdb.Msf_write.add_stream builder data in
  let msf_bytes = Pdb.Msf_write.finalize builder in
  let buf = buffer_of_string msf_bytes in
  let msf = Pdb.Msf.read buf in
  let s0 = Pdb.Msf.get_stream_exn msf 0 in
  Alcotest.(check int) "large stream size" 2000 (Bigarray.Array1.dim s0);
  Alcotest.(check string) "large stream content" data (string_of_buffer s0)

let test_superblock_fields () =
  let builder = Pdb.Msf_write.create ~block_size:4096 in
  let _s0 = Pdb.Msf_write.add_stream builder "test data" in
  let msf_bytes = Pdb.Msf_write.finalize builder in
  let buf = buffer_of_string msf_bytes in
  let msf = Pdb.Msf.read buf in
  let sb = Pdb.Msf.superblock msf in
  Alcotest.(check int) "block size" 4096
    (Unsigned.UInt32.to_int sb.block_size);
  Alcotest.(check int) "free block map block" 1
    (Unsigned.UInt32.to_int sb.free_block_map_block)

let test_magic_validation () =
  (* A buffer with wrong magic should fail *)
  let bad = buffer_of_string (String.make 4096 '\000') in
  Alcotest.check_raises "bad magic" (Object.Buffer.Invalid_format "Invalid MSF magic")
    (fun () -> ignore (Pdb.Msf.read bad))

let test_get_stream_out_of_range () =
  let builder = Pdb.Msf_write.create ~block_size:512 in
  let _s0 = Pdb.Msf_write.add_stream builder "data" in
  let msf_bytes = Pdb.Msf_write.finalize builder in
  let buf = buffer_of_string msf_bytes in
  let msf = Pdb.Msf.read buf in
  Alcotest.(check bool) "stream -1 is None" true
    (Option.is_none (Pdb.Msf.get_stream msf (-1)));
  Alcotest.(check bool) "stream 5 is None" true
    (Option.is_none (Pdb.Msf.get_stream msf 5))

let test_multiple_block_sizes () =
  (* Test with different valid block sizes *)
  List.iter
    (fun block_size ->
      let builder = Pdb.Msf_write.create ~block_size in
      let data = String.make (block_size + 100) 'A' in
      let _s = Pdb.Msf_write.add_stream builder data in
      let msf_bytes = Pdb.Msf_write.finalize builder in
      let buf = buffer_of_string msf_bytes in
      let msf = Pdb.Msf.read buf in
      let s = Pdb.Msf.get_stream_exn msf 0 in
      Alcotest.(check string)
        (Printf.sprintf "block_size=%d content" block_size)
        data (string_of_buffer s))
    [ 512; 1024; 2048; 4096 ]

let () =
  Alcotest.run "MSF"
    [
      ( "round-trip",
        [
          Alcotest.test_case "empty streams" `Quick test_roundtrip_empty_streams;
          Alcotest.test_case "with data" `Quick test_roundtrip_with_data;
          Alcotest.test_case "large stream" `Quick test_roundtrip_large_stream;
          Alcotest.test_case "superblock fields" `Quick test_superblock_fields;
          Alcotest.test_case "magic validation" `Quick test_magic_validation;
          Alcotest.test_case "out of range" `Quick test_get_stream_out_of_range;
          Alcotest.test_case "multiple block sizes" `Quick
            test_multiple_block_sizes;
        ] );
    ]
