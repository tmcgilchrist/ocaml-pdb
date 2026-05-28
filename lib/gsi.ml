(** Global/Public Symbol Index (GSI/PSI) reader.

    References:
    - LLVM: llvm/lib/DebugInfo/PDB/Native/GlobalsStream.cpp
    - LLVM: llvm/include/llvm/DebugInfo/PDB/Native/RawTypes.h *)

open Pdb_types

open Binary_writer

type hash_record = { offset : u32; cref : u32 }
type t = { hash_records : hash_record array; hash_buckets : u32 array }

type publics_header = {
  sym_hash_size : int;
  addr_map_size : int;
  num_thunks : int;
  size_of_thunk : int;
  isect_thunk_table : int;
  off_thunk_table : u32;
  num_sections : int;
}


let parse_publics_header (cur : Object.Buffer.cursor) : publics_header =
  (* PublicsStreamHeader is 28 bytes: 4 u32 + 2 u16 + 2 u32. *)
  Object.Buffer.ensure cur 28 "PSI publics header: truncated";
  let sym_hash_size = Unsigned.UInt32.to_int (read_u32 cur) in
  let addr_map_size = Unsigned.UInt32.to_int (read_u32 cur) in
  let num_thunks = Unsigned.UInt32.to_int (read_u32 cur) in
  let size_of_thunk = Unsigned.UInt32.to_int (read_u32 cur) in
  let isect_thunk_table = read_u16 cur in
  let _padding = read_u16 cur in
  let off_thunk_table = read_u32 cur in
  let num_sections = Unsigned.UInt32.to_int (read_u32 cur) in
  {
    sym_hash_size;
    addr_map_size;
    num_thunks;
    size_of_thunk;
    isect_thunk_table;
    off_thunk_table;
    num_sections;
  }

let parse_gsi (cur : Object.Buffer.cursor) (stream_size : int) : t =
  let _ = stream_size in
  (* GSIHashHeader is 16 bytes: VerSignature, VerHdr, HrSize, NumBuckets. *)
  Object.Buffer.ensure cur 16 "GSI hash header: truncated";
  let _ver_signature = read_u32 cur in
  let _ver_hdr = read_u32 cur in
  let hr_size = Unsigned.UInt32.to_int (read_u32 cur) in
  let buckets_byte_size = Unsigned.UInt32.to_int (read_u32 cur) in
  Object.Buffer.ensure cur (hr_size + buckets_byte_size)
    (Printf.sprintf
       "GSI: %d-byte hash record block + %d-byte buckets exceed stream end"
       hr_size buckets_byte_size);
  let num_records = hr_size / 8 in
  let hash_records =
    Array.init num_records (fun _ ->
        let offset = read_u32 cur in
        let cref = read_u32 cur in
        { offset; cref })
  in
  let num_bucket_words = buckets_byte_size / 4 in
  let hash_buckets = Array.init num_bucket_words (fun _ -> read_u32 cur) in
  { hash_records; hash_buckets }
