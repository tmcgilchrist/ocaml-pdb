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
  Alcotest.(check int)
    "stream 0 size" (String.length data0) (Bigarray.Array1.dim s0);
  Alcotest.(check string) "stream 0 content" data0 (string_of_buffer s0);
  (* Verify stream 1 *)
  let s1 = Pdb.Msf.get_stream_exn msf 1 in
  Alcotest.(check int)
    "stream 1 size" (String.length data1) (Bigarray.Array1.dim s1);
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
  Alcotest.(check int) "block size" 4096 (Unsigned.UInt32.to_int sb.block_size);
  Alcotest.(check int)
    "free block map block" 1
    (Unsigned.UInt32.to_int sb.free_block_map_block)

let test_magic_validation () =
  (* A buffer with wrong magic should fail *)
  let bad = buffer_of_string (String.make 4096 '\000') in
  Alcotest.check_raises "bad magic"
    (Object.Buffer.Invalid_format "Invalid MSF magic") (fun () ->
      ignore (Pdb.Msf.read bad))

let test_get_stream_out_of_range () =
  let builder = Pdb.Msf_write.create ~block_size:512 in
  let _s0 = Pdb.Msf_write.add_stream builder "data" in
  let msf_bytes = Pdb.Msf_write.finalize builder in
  let buf = buffer_of_string msf_bytes in
  let msf = Pdb.Msf.read buf in
  Alcotest.(check bool)
    "stream -1 is None" true
    (Option.is_none (Pdb.Msf.get_stream msf (-1)));
  Alcotest.(check bool)
    "stream 5 is None" true
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

let test_too_small_for_superblock () =
  (* A file smaller than the superblock should fail *)
  let tiny = buffer_of_string (String.make 10 '\000') in
  Alcotest.check_raises "too small"
    (Object.Buffer.Invalid_format "MSF file too small for superblock")
    (fun () -> ignore (Pdb.Msf.read tiny))

let test_elf_magic_rejected () =
  (* An ELF file should be rejected with Invalid MSF magic *)
  let elf = buffer_of_string ("\x7fELF" ^ String.make 4092 '\000') in
  Alcotest.check_raises "ELF rejected"
    (Object.Buffer.Invalid_format "Invalid MSF magic")
    (fun () -> ignore (Pdb.Msf.read elf))

let test_invalid_block_size () =
  (* Construct a buffer with valid magic but invalid block size (256) *)
  let buf = Stdlib.Buffer.create 4096 in
  Stdlib.Buffer.add_string buf Pdb.Msf.msf_magic;
  (* block_size = 256 (invalid) *)
  Stdlib.Buffer.add_char buf '\x00';
  Stdlib.Buffer.add_char buf '\x01';
  Stdlib.Buffer.add_char buf '\x00';
  Stdlib.Buffer.add_char buf '\x00';
  (* Fill rest to at least magic_len + 24 bytes *)
  for _ = 1 to 20 do
    Stdlib.Buffer.add_char buf '\x00'
  done;
  (* Pad to reasonable size *)
  while Stdlib.Buffer.length buf < 512 do
    Stdlib.Buffer.add_char buf '\x00'
  done;
  let bytes = Stdlib.Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  Alcotest.check_raises "invalid block size"
    (Object.Buffer.Invalid_format "Invalid MSF block size: 256")
    (fun () -> ignore (Pdb.Msf.read obj_buf))

let test_invalid_block_size_builder () =
  (* The MSF builder should reject invalid block sizes *)
  Alcotest.check_raises "builder rejects 256"
    (Invalid_argument "MSF block_size must be 512, 1024, 2048, or 4096")
    (fun () -> ignore (Pdb.Msf_write.create ~block_size:256));
  Alcotest.check_raises "builder rejects 3000"
    (Invalid_argument "MSF block_size must be 512, 1024, 2048, or 4096")
    (fun () -> ignore (Pdb.Msf_write.create ~block_size:3000))

let test_multi_block_stream_directory () =
  (* Create enough streams/data that the stream directory requires
     multiple blocks. With block_size=512, we need the directory to
     exceed 512 bytes. Each stream adds 4 bytes (size) + 4 bytes per
     block to the directory. With 60 streams of ~600 bytes each,
     directory should be well over 512 bytes. *)
  let builder = Pdb.Msf_write.create ~block_size:512 in
  let expected = Array.init 60 (fun i ->
    let data = String.make 600 (Char.chr ((i mod 26) + 65)) in
    let _ = Pdb.Msf_write.add_stream builder data in
    data)
  in
  let msf_bytes = Pdb.Msf_write.finalize builder in
  let buf = buffer_of_string msf_bytes in
  let msf = Pdb.Msf.read buf in
  Alcotest.(check int) "stream count" 60 (Pdb.Msf.stream_count msf);
  (* Verify a few streams *)
  for i = 0 to 59 do
    let s = Pdb.Msf.get_stream_exn msf i in
    Alcotest.(check int)
      (Printf.sprintf "stream %d size" i)
      600 (Bigarray.Array1.dim s);
    Alcotest.(check string)
      (Printf.sprintf "stream %d content" i)
      expected.(i) (string_of_buffer s)
  done

let test_many_small_streams () =
  (* Test with many empty/tiny streams *)
  let builder = Pdb.Msf_write.create ~block_size:512 in
  for _ = 1 to 100 do
    ignore (Pdb.Msf_write.add_empty_stream builder)
  done;
  let msf_bytes = Pdb.Msf_write.finalize builder in
  let buf = buffer_of_string msf_bytes in
  let msf = Pdb.Msf.read buf in
  Alcotest.(check int) "100 streams" 100 (Pdb.Msf.stream_count msf);
  (* All should be empty *)
  for i = 0 to 99 do
    let s = Pdb.Msf.get_stream_exn msf i in
    Alcotest.(check int)
      (Printf.sprintf "stream %d empty" i)
      0 (Bigarray.Array1.dim s)
  done

let test_exact_block_boundary () =
  (* Stream whose size is exactly one block -- no partial block *)
  let builder = Pdb.Msf_write.create ~block_size:512 in
  let data = String.make 512 'X' in
  let _ = Pdb.Msf_write.add_stream builder data in
  let msf_bytes = Pdb.Msf_write.finalize builder in
  let buf = buffer_of_string msf_bytes in
  let msf = Pdb.Msf.read buf in
  let s = Pdb.Msf.get_stream_exn msf 0 in
  Alcotest.(check int) "exact block size" 512 (Bigarray.Array1.dim s);
  Alcotest.(check string) "content" data (string_of_buffer s)

let test_one_byte_stream () =
  let builder = Pdb.Msf_write.create ~block_size:4096 in
  let _ = Pdb.Msf_write.add_stream builder "X" in
  let msf_bytes = Pdb.Msf_write.finalize builder in
  let buf = buffer_of_string msf_bytes in
  let msf = Pdb.Msf.read buf in
  let s = Pdb.Msf.get_stream_exn msf 0 in
  Alcotest.(check int) "1 byte" 1 (Bigarray.Array1.dim s);
  Alcotest.(check string) "content" "X" (string_of_buffer s)

(** {2 Free Page Map tests} *)

let test_fpm_correctness () =
  (* Build an MSF and verify the FPM bitmap is correct:
     allocated blocks should have bit=0, free blocks should have bit=1. *)
  let builder = Pdb.Msf_write.create ~block_size:512 in
  let _ = Pdb.Msf_write.add_stream builder (String.make 1000 'A') in
  let _ = Pdb.Msf_write.add_stream builder (String.make 500 'B') in
  let msf_bytes = Pdb.Msf_write.finalize builder in
  let buf = buffer_of_string msf_bytes in
  let msf = Pdb.Msf.read buf in
  let sb = Pdb.Msf.superblock msf in
  let block_size = Unsigned.UInt32.to_int sb.block_size in
  let num_blocks = Unsigned.UInt32.to_int sb.num_blocks in
  (* Read FPM from block 1 *)
  let fpm_offset = block_size in
  (* Block 0 (superblock) must be allocated: bit 0 of byte 0 should be 0 *)
  let fpm_byte0 = Char.code msf_bytes.[fpm_offset] in
  Alcotest.(check int) "block 0 allocated" 0 (fpm_byte0 land 1);
  (* Block 1 (FPM0) must be allocated: bit 1 *)
  Alcotest.(check int) "block 1 allocated" 0 (fpm_byte0 land 2);
  (* Block 2 (FPM1) must be allocated: bit 2 *)
  Alcotest.(check int) "block 2 allocated" 0 (fpm_byte0 land 4);
  (* All blocks beyond num_blocks should be free (bit=1).
     Check the byte containing the last block. *)
  let last_block = num_blocks - 1 in
  let last_byte_idx = last_block / 8 in
  let last_bit_idx = last_block mod 8 in
  let last_fpm_byte = Char.code msf_bytes.[fpm_offset + last_byte_idx] in
  (* The last allocated block should have bit=0 *)
  Alcotest.(check int) "last block allocated" 0
    (last_fpm_byte land (1 lsl last_bit_idx));
  (* Byte after the last block's byte should have free bits *)
  if last_byte_idx + 1 < block_size then begin
    let beyond_byte = Char.code msf_bytes.[fpm_offset + last_byte_idx + 1] in
    Alcotest.(check int) "beyond blocks free" 0xFF beyond_byte
  end;
  (* Verify data still reads correctly *)
  let s0 = Pdb.Msf.get_stream_exn msf 0 in
  Alcotest.(check int) "stream 0 size" 1000 (Bigarray.Array1.dim s0)

let test_fpm_blocks_1_and_2_identical () =
  let builder = Pdb.Msf_write.create ~block_size:512 in
  let _ = Pdb.Msf_write.add_stream builder "data" in
  let msf_bytes = Pdb.Msf_write.finalize builder in
  (* FPM0 is at block 1, FPM1 is at block 2 *)
  let fpm0 = String.sub msf_bytes 512 512 in
  let fpm1 = String.sub msf_bytes 1024 512 in
  Alcotest.(check string) "FPM0 == FPM1" fpm0 fpm1

(** Derive the set of allocated blocks by walking the superblock, block map,
    and stream directory. Any block reachable from one of those structures
    must be marked allocated in the FPM; everything else must be free. *)
let test_fpm_full_bitmap () =
  let builder = Pdb.Msf_write.create ~block_size:512 in
  let _ = Pdb.Msf_write.add_stream builder (String.make 600 'A') in
  let _ = Pdb.Msf_write.add_empty_stream builder in
  let _ = Pdb.Msf_write.add_stream builder (String.make 1500 'B') in
  let _ = Pdb.Msf_write.add_stream builder "small" in
  let msf_bytes = Pdb.Msf_write.finalize builder in
  let buf = buffer_of_string msf_bytes in
  let msf = Pdb.Msf.read buf in
  let sb = Pdb.Msf.superblock msf in
  let block_size = Unsigned.UInt32.to_int sb.block_size in
  let num_blocks = Unsigned.UInt32.to_int sb.num_blocks in
  let block_map_addr = Unsigned.UInt32.to_int sb.block_map_addr in
  let num_dir_bytes = Unsigned.UInt32.to_int sb.num_directory_bytes in
  let div_ceil a b = (a + b - 1) / b in
  let num_dir_blocks = div_ceil num_dir_bytes block_size in
  let read_u32_at off =
    Char.code msf_bytes.[off]
    lor (Char.code msf_bytes.[off + 1] lsl 8)
    lor (Char.code msf_bytes.[off + 2] lsl 16)
    lor (Char.code msf_bytes.[off + 3] lsl 24)
  in
  let allocated = Array.make num_blocks false in
  let mark b =
    if b < 0 || b >= num_blocks then
      Alcotest.failf "block %d out of range (num_blocks=%d)" b num_blocks;
    allocated.(b) <- true
  in
  mark 0;
  (* superblock *)
  mark 1;
  (* FPM0 *)
  mark 2;
  (* FPM1 *)
  mark block_map_addr;
  (* Directory blocks listed in the block map *)
  let dir_blocks =
    Array.init num_dir_blocks (fun i ->
        read_u32_at ((block_map_addr * block_size) + (i * 4)))
  in
  Array.iter mark dir_blocks;
  (* Stream blocks listed in the directory *)
  (* Reassemble directory bytes (could span multiple blocks). *)
  let dir_bytes = Bytes.create num_dir_bytes in
  let remaining = ref num_dir_bytes in
  Array.iteri
    (fun i b ->
      let src = b * block_size in
      let dst = i * block_size in
      let n = min !remaining block_size in
      Bytes.blit_string msf_bytes src dir_bytes dst n;
      remaining := !remaining - n)
    dir_blocks;
  let dir = Bytes.unsafe_to_string dir_bytes in
  let read_dir_u32 off =
    Char.code dir.[off]
    lor (Char.code dir.[off + 1] lsl 8)
    lor (Char.code dir.[off + 2] lsl 16)
    lor (Char.code dir.[off + 3] lsl 24)
  in
  let num_streams = read_dir_u32 0 in
  let stream_sizes =
    Array.init num_streams (fun i -> read_dir_u32 (4 + (i * 4)))
  in
  let pos = ref (4 + (num_streams * 4)) in
  Array.iter
    (fun size ->
      if size > 0 && size <> 0xFFFFFFFF then
        let nb = div_ceil size block_size in
        for _ = 1 to nb do
          mark (read_dir_u32 !pos);
          pos := !pos + 4
        done)
    stream_sizes;
  (* Now compare against the FPM written at block 1. *)
  let fpm_off = 1 * block_size in
  for b = 0 to num_blocks - 1 do
    let byte = Char.code msf_bytes.[fpm_off + (b / 8)] in
    let bit = (byte lsr (b mod 8)) land 1 in
    let expected_free = if allocated.(b) then 0 else 1 in
    if bit <> expected_free then
      Alcotest.failf "block %d: FPM bit=%d, expected %d (allocated=%b)" b bit
        expected_free allocated.(b)
  done;
  (* Bits past num_blocks must be 1 (free) all the way to the end of the FPM block. *)
  for b = num_blocks to (block_size * 8) - 1 do
    let byte = Char.code msf_bytes.[fpm_off + (b / 8)] in
    let bit = (byte lsr (b mod 8)) land 1 in
    if bit <> 1 then
      Alcotest.failf "bit %d past num_blocks should be free, got %d" b bit
  done

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
      ( "edge_cases",
        [
          Alcotest.test_case "too small" `Quick test_too_small_for_superblock;
          Alcotest.test_case "ELF rejected" `Quick test_elf_magic_rejected;
          Alcotest.test_case "invalid block size" `Quick
            test_invalid_block_size;
          Alcotest.test_case "invalid block size builder" `Quick
            test_invalid_block_size_builder;
          Alcotest.test_case "multi-block directory" `Quick
            test_multi_block_stream_directory;
          Alcotest.test_case "many small streams" `Quick
            test_many_small_streams;
          Alcotest.test_case "exact block boundary" `Quick
            test_exact_block_boundary;
          Alcotest.test_case "one byte stream" `Quick test_one_byte_stream;
        ] );
      ( "fpm",
        [
          Alcotest.test_case "correctness" `Quick test_fpm_correctness;
          Alcotest.test_case "blocks 1 and 2 identical" `Quick
            test_fpm_blocks_1_and_2_identical;
          Alcotest.test_case "full bitmap matches reachable blocks" `Quick
            test_fpm_full_bitmap;
        ] );
    ]
