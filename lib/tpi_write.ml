(** TPI/IPI stream writer.

    References:
    - LLVM: llvm/lib/DebugInfo/PDB/Native/TpiStreamBuilder.cpp *)

module Buffer = Stdlib.Buffer

(* TPI version: V80 = 20040203 *)
open Binary_writer

let tpi_version_v80 = 20040203
let tpi_header_size = 56
let first_type_index = 0x1000

let write buf records =
  (* Serialize all records to compute total bytes *)
  let rec_buf = Buffer.create 1024 in
  List.iter (fun r -> Codeview_types.write_type_record rec_buf r) records;
  let type_record_bytes = Buffer.length rec_buf in
  let num_records = List.length records in
  (* Write header (56 bytes) *)
  write_u32_le buf tpi_version_v80;
  write_u32_le buf tpi_header_size;
  write_u32_le buf first_type_index;
  (* TypeIndexBegin *)
  write_u32_le buf (first_type_index + num_records);
  (* TypeIndexEnd *)
  write_u32_le buf type_record_bytes;
  write_u16_le buf 0xFFFF;
  (* HashStreamIndex: -1 = no hash stream *)
  write_u16_le buf 0xFFFF;
  (* HashAuxStreamIndex *)
  write_u32_le buf 4;
  (* HashKeySize *)
  write_u32_le buf 0x40000;
  (* NumHashBuckets *)
  (* Three EmbeddedBuf fields (offset + length), all zero *)
  write_u32_le buf 0;
  write_u32_le buf 0;
  (* HashValueBuffer *)
  write_u32_le buf 0;
  write_u32_le buf 0;
  (* IndexOffsetBuffer *)
  write_u32_le buf 0;
  write_u32_le buf 0;
  (* HashAdjBuffer *)
  (* Write type records *)
  Buffer.add_string buf (Buffer.contents rec_buf)

let max_tpi_hash_buckets = 0x40000

(* 8KB boundary for TypeIndexOffset entries *)
let type_index_offset_interval = 8192

let write_with_hash buf records ~hash_stream_index =
  (* Serialize each record individually to get per-record bytes and offsets *)
  let per_record =
    List.map
      (fun r ->
        let rb = Buffer.create 64 in
        Codeview_types.write_type_record rb r;
        Buffer.contents rb)
      records
  in
  let num_records = List.length records in
  let type_record_bytes =
    List.fold_left (fun acc s -> acc + String.length s) 0 per_record
  in
  (* Build hash values: CRC32 of each record's bytes, folded into bucket range *)
  let bucket_count = max_tpi_hash_buckets - 1 in
  let hash_values =
    List.map
      (fun record_bytes ->
        let h = Hash.hash_buffer_v8 record_bytes in
        h mod bucket_count)
      per_record
  in
  (* Build TypeIndexOffset entries: one per ~8KB boundary *)
  let index_offsets = ref [] in
  let byte_offset = ref 0 in
  let next_boundary = ref type_index_offset_interval in
  List.iteri
    (fun i record_bytes ->
      if !byte_offset >= !next_boundary then begin
        index_offsets := (first_type_index + i, !byte_offset) :: !index_offsets;
        next_boundary := !byte_offset + type_index_offset_interval
      end;
      byte_offset := !byte_offset + String.length record_bytes)
    per_record;
  let index_offsets = List.rev !index_offsets in
  (* Calculate hash stream sizes *)
  let hash_values_size = num_records * 4 in
  let index_offsets_size = List.length index_offsets * 8 in
  (* Write TPI header *)
  write_u32_le buf tpi_version_v80;
  write_u32_le buf tpi_header_size;
  write_u32_le buf first_type_index;
  write_u32_le buf (first_type_index + num_records);
  write_u32_le buf type_record_bytes;
  write_u16_le buf hash_stream_index;
  write_u16_le buf 0xFFFF;
  (* HashAuxStreamIndex *)
  write_u32_le buf 4;
  (* HashKeySize *)
  write_u32_le buf (max_tpi_hash_buckets - 1);
  (* NumHashBuckets *)
  (* HashValueBuffer: offset=0, length=hash_values_size *)
  write_u32_le buf 0;
  write_u32_le buf hash_values_size;
  (* IndexOffsetBuffer: offset=hash_values_size, length=index_offsets_size *)
  write_u32_le buf hash_values_size;
  write_u32_le buf index_offsets_size;
  (* HashAdjBuffer: offset=end, length=0 *)
  write_u32_le buf (hash_values_size + index_offsets_size);
  write_u32_le buf 0;
  (* Write type records *)
  List.iter (Buffer.add_string buf) per_record;
  (* Build the hash stream *)
  let hash_buf = Buffer.create (hash_values_size + index_offsets_size) in
  List.iter (write_u32_le hash_buf) hash_values;
  List.iter
    (fun (ti, off) ->
      write_u32_le hash_buf ti;
      write_u32_le hash_buf off)
    index_offsets;
  Buffer.contents hash_buf
