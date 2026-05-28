(** MSF (Multi-Stream File) container writer. *)

module Buffer = Stdlib.Buffer

open Binary_writer

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

(** A block at position [k * block_size + 1] or [k * block_size + 2] (for
    any [k >= 0]) is reserved as a Free Page Map block. The actual
    superblock at position 0 is not an FPM block. *)
let is_fpm_block block_size idx =
  if idx = 0 then false
  else
    let r = idx mod block_size in
    r = 1 || r = 2

(** Allocate [count] consecutive non-FPM block indices starting from
    [start]. Returns the assigned indices and the next free index. *)
let alloc_blocks block_size start count =
  let blocks = Array.make count 0 in
  let next = ref start in
  for i = 0 to count - 1 do
    while is_fpm_block block_size !next do
      incr next
    done;
    blocks.(i) <- !next;
    incr next
  done;
  (blocks, !next)

let finalize t =
  let streams = List.rev t.streams in
  let block_size = t.block_size in
  let num_streams = List.length streams in
  let stream_sizes = List.map String.length streams in
  let stream_block_counts =
    List.map
      (fun size -> if size = 0 then 0 else div_ceil size block_size)
      stream_sizes
  in
  let total_stream_blocks = List.fold_left ( + ) 0 stream_block_counts in
  (* Directory layout: u32 num_streams + u32 per stream size + u32 per
     stream block index. *)
  let num_directory_bytes =
    4 + (num_streams * 4) + (total_stream_blocks * 4)
  in
  let num_directory_blocks = div_ceil num_directory_bytes block_size in
  let block_map_blocks = div_ceil (num_directory_blocks * 4) block_size in
  (* Allocate the block map, directory blocks, and stream blocks via the
     FPM-aware allocator. Start from block 3 (after the superblock and
     the first FPM pair at positions 1 and 2). *)
  let block_map_block_idxs, after_block_map =
    alloc_blocks block_size 3 block_map_blocks
  in
  let directory_block_idxs, after_directory =
    alloc_blocks block_size after_block_map num_directory_blocks
  in
  let stream_block_lists, after_streams =
    let next = ref after_directory in
    let lists =
      List.map
        (fun count ->
          let blocks, n = alloc_blocks block_size !next count in
          next := n;
          blocks)
        stream_block_counts
    in
    (lists, !next)
  in
  let total_blocks = after_streams in
  (* Block map: just an array of directory-block indices. *)
  let block_map_buf = Buffer.create (num_directory_blocks * 4) in
  Array.iter (fun b -> write_u32_le block_map_buf b) directory_block_idxs;
  let block_map_bytes = Buffer.contents block_map_buf in
  (* Directory: num_streams, stream sizes, then each stream's block list. *)
  let dir_buf = Buffer.create num_directory_bytes in
  write_u32_le dir_buf num_streams;
  List.iter (fun size -> write_u32_le dir_buf size) stream_sizes;
  List.iter
    (fun blocks -> Array.iter (fun b -> write_u32_le dir_buf b) blocks)
    stream_block_lists;
  let directory_bytes = Buffer.contents dir_buf in
  (* Superblock. *)
  let block_map_addr =
    if block_map_blocks = 0 then 3 else block_map_block_idxs.(0)
  in
  let sb_buf = Buffer.create block_size in
  Buffer.add_string sb_buf Msf.msf_magic;
  write_u32_le sb_buf block_size;
  write_u32_le sb_buf 1;
  (* FreeBlockMapBlock: FPM0 (position 1) is active *)
  write_u32_le sb_buf total_blocks;
  write_u32_le sb_buf num_directory_bytes;
  write_u32_le sb_buf 0;
  (* Unknown1 *)
  write_u32_le sb_buf block_map_addr;
  let superblock_bytes = Buffer.contents sb_buf in
  (* Mark used blocks. Block 0 is the superblock; every FPM position in
     [0, total_blocks) is reserved; then block_map, directory, and each
     stream's data blocks. *)
  let used = Array.make total_blocks false in
  used.(0) <- true;
  for i = 0 to total_blocks - 1 do
    if is_fpm_block block_size i then used.(i) <- true
  done;
  Array.iter (fun b -> used.(b) <- true) block_map_block_idxs;
  Array.iter (fun b -> used.(b) <- true) directory_block_idxs;
  List.iter
    (fun blocks -> Array.iter (fun b -> used.(b) <- true) blocks)
    stream_block_lists;
  (* Build the FPM bitmap. Each FPM block covers [k*block_size*8,
     (k+1)*block_size*8) blocks of the file. *)
  let num_fpm_chunks = div_ceil total_blocks (block_size * 8) in
  let fpm_chunks =
    Array.init num_fpm_chunks (fun k ->
        let chunk = Bytes.make block_size '\xFF' in
        let lo = k * block_size * 8 in
        let hi = min ((k + 1) * block_size * 8) total_blocks in
        for b = lo to hi - 1 do
          if used.(b) then begin
            let local = b - lo in
            let byte_idx = local / 8 in
            let bit_idx = local mod 8 in
            let byte = Char.code (Bytes.get chunk byte_idx) in
            Bytes.set chunk byte_idx (Char.chr (byte land lnot (1 lsl bit_idx)))
          end
        done;
        chunk)
  in
  (* Assemble the output by placing each block's content at its assigned
     position. Unused positions remain zero-filled. *)
  let out = Bytes.make (total_blocks * block_size) '\000' in
  let put_block idx contents =
    let len = String.length contents in
    if len > 0 then Bytes.blit_string contents 0 out (idx * block_size) len
  in
  let put_chunked_block idx contents offset block_len =
    let avail = String.length contents - offset in
    let len = min block_len avail in
    if len > 0 then
      Bytes.blit_string contents offset out (idx * block_size) len
  in
  put_block 0 superblock_bytes;
  (* Both FPM mirrors get the same bytes. *)
  for k = 0 to num_fpm_chunks - 1 do
    let chunk = Bytes.unsafe_to_string fpm_chunks.(k) in
    let fpm0 = (k * block_size) + 1 in
    let fpm1 = (k * block_size) + 2 in
    if fpm0 < total_blocks then put_block fpm0 chunk;
    if fpm1 < total_blocks then put_block fpm1 chunk
  done;
  Array.iteri
    (fun i b ->
      put_chunked_block b block_map_bytes (i * block_size) block_size)
    block_map_block_idxs;
  Array.iteri
    (fun i b -> put_chunked_block b directory_bytes (i * block_size) block_size)
    directory_block_idxs;
  List.iter2
    (fun content blocks ->
      Array.iteri
        (fun i b -> put_chunked_block b content (i * block_size) block_size)
        blocks)
    streams stream_block_lists;
  Bytes.unsafe_to_string out
