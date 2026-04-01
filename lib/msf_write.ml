(** MSF (Multi-Stream File) container writer. *)

module Buffer = Stdlib.Buffer

type t = {
  block_size : int;
  mutable streams : string list;  (** In reverse order *)
}

let create ~block_size =
  (match block_size with
  | 512 | 1024 | 2048 | 4096 -> ()
  | _ -> invalid_arg "MSF block_size must be 512, 1024, 2048, or 4096");
  { block_size; streams = [] }

let add_stream t contents =
  let idx = List.length t.streams in
  t.streams <- contents :: t.streams;
  idx

let add_empty_stream t = add_stream t ""
let div_ceil a b = (a + b - 1) / b

let write_u32_le buf v =
  Buffer.add_char buf (Char.chr (v land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 16) land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 24) land 0xFF))

let finalize t =
  let streams = List.rev t.streams in
  let block_size = t.block_size in
  let num_streams = List.length streams in
  (* Calculate how many blocks each stream needs *)
  let stream_sizes = List.map String.length streams in
  let stream_block_counts =
    List.map
      (fun size -> if size = 0 then 0 else div_ceil size block_size)
      stream_sizes
  in
  (* Build the stream directory contents *)
  let dir_buf = Buffer.create 256 in
  write_u32_le dir_buf num_streams;
  List.iter (fun size -> write_u32_le dir_buf size) stream_sizes;
  (* We'll assign blocks sequentially starting after reserved blocks.
     Reserved: block 0 (superblock), block 1 (FPM0), block 2 (FPM1).
     Block 3+ is the block map, then directory blocks, then stream data. *)
  let total_stream_blocks = List.fold_left ( + ) 0 stream_block_counts in
  (* The directory itself also needs blocks *)
  let dir_content_size = Buffer.length dir_buf in
  (* We haven't written block lists yet; we need to know block assignments
     first. We'll calculate the directory size including block lists. *)
  let dir_size_with_blocks = dir_content_size + (total_stream_blocks * 4) in
  let num_directory_blocks = div_ceil dir_size_with_blocks block_size in
  (* The block map lists the directory's blocks. It takes
     num_directory_blocks * 4 bytes, fitting in one block for reasonable sizes. *)
  let block_map_blocks = div_ceil (num_directory_blocks * 4) block_size in
  (* Total reserved blocks before stream data:
     3 (superblock + FPM0 + FPM1) + block_map_blocks + num_directory_blocks *)
  let first_data_block = 3 + block_map_blocks + num_directory_blocks in
  (* Assign blocks to each stream *)
  let stream_block_lists =
    let next_block = ref first_data_block in
    List.map
      (fun count ->
        let blocks = Array.init count (fun i -> !next_block + i) in
        next_block := !next_block + count;
        blocks)
      stream_block_counts
  in
  (* Now write the block lists into the directory *)
  List.iter
    (fun blocks -> Array.iter (fun b -> write_u32_le dir_buf b) blocks)
    stream_block_lists;
  let directory_bytes = Buffer.contents dir_buf in
  let num_directory_bytes = String.length directory_bytes in
  (* Recalculate in case our estimate was slightly off *)
  let num_directory_blocks_actual = div_ceil num_directory_bytes block_size in
  assert (num_directory_blocks_actual = num_directory_blocks);
  let total_blocks = first_data_block + total_stream_blocks in
  (* Assign directory blocks: they come right after the block map *)
  let directory_block_start = 3 + block_map_blocks in
  let block_map_addr = 3 in
  (* Build the output *)
  let out = Buffer.create (total_blocks * block_size) in
  (* Block 0: Superblock *)
  Buffer.add_string out Msf.msf_magic;
  write_u32_le out block_size;
  write_u32_le out 1;
  (* FreeBlockMapBlock: always 1 *)
  write_u32_le out total_blocks;
  write_u32_le out num_directory_bytes;
  write_u32_le out 0;
  (* Unknown1 *)
  write_u32_le out block_map_addr;
  (* Pad block 0 to block_size *)
  let superblock_size = String.length Msf.msf_magic + 24 in
  for _ = 1 to block_size - superblock_size do
    Buffer.add_char out '\000'
  done;
  (* Block 1: FPM0 - mark all blocks as used (bit=1 means free).
     For simplicity, we mark everything as allocated (all zeros),
     except we could be more precise. PDB readers generally don't
     validate the FPM strictly. *)
  for _ = 1 to block_size do
    Buffer.add_char out '\000'
  done;
  (* Block 2: FPM1 - alternate FPM, also zeroed *)
  for _ = 1 to block_size do
    Buffer.add_char out '\000'
  done;
  (* Block 3+: Block map (directory block indices) *)
  for i = 0 to num_directory_blocks - 1 do
    write_u32_le out (directory_block_start + i)
  done;
  (* Pad block map block(s) *)
  let block_map_written = num_directory_blocks * 4 in
  let block_map_total = block_map_blocks * block_size in
  for _ = 1 to block_map_total - block_map_written do
    Buffer.add_char out '\000'
  done;
  (* Directory blocks *)
  Buffer.add_string out directory_bytes;
  let dir_pad = (num_directory_blocks * block_size) - num_directory_bytes in
  for _ = 1 to dir_pad do
    Buffer.add_char out '\000'
  done;
  (* Stream data blocks *)
  List.iter2
    (fun content blocks ->
      let len = String.length content in
      let num_blocks = Array.length blocks in
      if num_blocks > 0 then begin
        Buffer.add_string out content;
        let pad = (num_blocks * block_size) - len in
        for _ = 1 to pad do
          Buffer.add_char out '\000'
        done
      end)
    streams stream_block_lists;
  Buffer.contents out
