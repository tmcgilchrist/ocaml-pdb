(** MSF (Multi-Stream File) container reader.

    References:
    - LLVM: llvm/include/llvm/DebugInfo/MSF/MSFCommon.h
    - LLVM docs: https://llvm.org/docs/PDB/MsfFile.html *)

open Pdb_types

let msf_magic = "Microsoft C/C++ MSF 7.00\r\n\x1aDS\000\000\000"

type superblock = {
  block_size : u32;
  free_block_map_block : u32;
  num_blocks : u32;
  num_directory_bytes : u32;
  block_map_addr : u32;
}

type t = { sb : superblock; streams : Object.Buffer.t array }

let superblock t = t.sb
let stream_count t = Array.length t.streams

let get_stream t idx =
  if idx >= 0 && idx < Array.length t.streams then Some t.streams.(idx)
  else None

let get_stream_exn t idx =
  if idx >= 0 && idx < Array.length t.streams then t.streams.(idx)
  else
    Object.Buffer.invalid_format
      (Printf.sprintf "MSF stream index %d out of range (have %d streams)" idx
         (Array.length t.streams))

(** Read a little-endian u32 from a buffer at a given byte offset. *)
let read_u32_at (buf : Object.Buffer.t) (offset : int) : int =
  buf.{offset}
  lor (buf.{offset + 1} lsl 8)
  lor (buf.{offset + 2} lsl 16)
  lor (buf.{offset + 3} lsl 24)

(** Reassemble a stream from non-contiguous blocks into a contiguous buffer. *)
let reassemble_stream (buf : Object.Buffer.t) (block_size : int)
    (block_list : int array) (stream_size : int) : Object.Buffer.t =
  let result =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout stream_size
  in
  let remaining = ref stream_size in
  Array.iteri
    (fun i block_idx ->
      let src_offset = block_idx * block_size in
      let dst_offset = i * block_size in
      let copy_size = min !remaining block_size in
      for j = 0 to copy_size - 1 do
        result.{dst_offset + j} <- buf.{src_offset + j}
      done;
      remaining := !remaining - copy_size)
    block_list;
  result

let div_ceil a b = (a + b - 1) / b

let read (buf : Object.Buffer.t) : t =
  let buf_size = Bigarray.Array1.dim buf in
  (* Validate magic *)
  let magic_len = String.length msf_magic in
  if buf_size < magic_len + 24 then
    Object.Buffer.invalid_format "MSF file too small for superblock";
  for i = 0 to magic_len - 1 do
    if buf.{i} <> Char.code msf_magic.[i] then
      Object.Buffer.invalid_format "Invalid MSF magic"
  done;
  (* Parse superblock fields after magic (32 bytes) *)
  let block_size = read_u32_at buf (magic_len + 0) in
  let free_block_map_block = read_u32_at buf (magic_len + 4) in
  let num_blocks = read_u32_at buf (magic_len + 8) in
  let num_directory_bytes = read_u32_at buf (magic_len + 12) in
  (* Skip Unknown1 at +16 *)
  let block_map_addr = read_u32_at buf (magic_len + 20) in
  (* Validate block size *)
  (match block_size with
  | 512 | 1024 | 2048 | 4096 | 8192 | 16384 | 32768 -> ()
  | _ ->
      Object.Buffer.invalid_format
        (Printf.sprintf "Invalid MSF block size: %d" block_size));
  (* Validate file size *)
  if buf_size < num_blocks * block_size then
    Object.Buffer.invalid_format
      "MSF file size smaller than num_blocks * block_size";
  let sb =
    {
      block_size = Unsigned.UInt32.of_int block_size;
      free_block_map_block = Unsigned.UInt32.of_int free_block_map_block;
      num_blocks = Unsigned.UInt32.of_int num_blocks;
      num_directory_bytes = Unsigned.UInt32.of_int num_directory_bytes;
      block_map_addr = Unsigned.UInt32.of_int block_map_addr;
    }
  in
  (* Read the block map (array of block indices for the stream directory).
     The block map is at block [block_map_addr]. It contains the block
     indices where the stream directory is stored. *)
  let num_directory_blocks = div_ceil num_directory_bytes block_size in
  let block_map_offset = block_map_addr * block_size in
  let directory_block_list =
    Array.init num_directory_blocks (fun i ->
        read_u32_at buf (block_map_offset + (i * 4)))
  in
  (* Reassemble the stream directory *)
  let directory =
    reassemble_stream buf block_size directory_block_list num_directory_bytes
  in
  (* Parse the stream directory.
     Layout:
       u32 num_streams
       u32[num_streams] stream_sizes
       u32[...] block lists (concatenated, one per stream) *)
  let dir_cur = Object.Buffer.cursor directory in
  let num_streams = Object.Buffer.Read.u32 dir_cur |> Unsigned.UInt32.to_int in
  let stream_sizes =
    Array.init num_streams (fun _i ->
        Object.Buffer.Read.u32 dir_cur |> Unsigned.UInt32.to_int)
  in
  (* Read block lists for each stream *)
  let streams =
    Array.init num_streams (fun i ->
        let size = stream_sizes.(i) in
        if size = 0 || size = -1 (* 0xFFFFFFFF = unused stream *) then
          Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout 0
        else begin
          let num_blocks_for_stream = div_ceil size block_size in
          let block_list =
            Array.init num_blocks_for_stream (fun _j ->
                Object.Buffer.Read.u32 dir_cur |> Unsigned.UInt32.to_int)
          in
          reassemble_stream buf block_size block_list size
        end)
  in
  { sb; streams }
