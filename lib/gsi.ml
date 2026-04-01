(** Global/Public Symbol Index (GSI/PSI) reader.

    References:
    - LLVM: llvm/lib/DebugInfo/PDB/Native/GlobalsStream.cpp
    - LLVM: llvm/include/llvm/DebugInfo/PDB/Native/RawTypes.h *)

open Pdb_types

type hash_record = {
  offset : u32;
  cref : u32;
}

type t = {
  hash_records : hash_record array;
  hash_buckets : u32 array;
}

type publics_header = {
  sym_hash_size : int;
  addr_map_size : int;
  num_thunks : int;
  size_of_thunk : int;
  isect_thunk_table : int;
  off_thunk_table : u32;
  num_sections : int;
}

let read_u16 cur = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int
let read_u32 cur = Object.Buffer.Read.u32 cur

let parse_publics_header (cur : Object.Buffer.cursor) : publics_header =
  let sym_hash_size = Unsigned.UInt32.to_int (read_u32 cur) in
  let addr_map_size = Unsigned.UInt32.to_int (read_u32 cur) in
  let num_thunks = Unsigned.UInt32.to_int (read_u32 cur) in
  let size_of_thunk = Unsigned.UInt32.to_int (read_u32 cur) in
  let isect_thunk_table = read_u16 cur in
  let _padding = read_u16 cur in
  let off_thunk_table = read_u32 cur in
  let num_sections = Unsigned.UInt32.to_int (read_u32 cur) in
  { sym_hash_size; addr_map_size; num_thunks; size_of_thunk;
    isect_thunk_table; off_thunk_table; num_sections }

let parse_gsi (cur : Object.Buffer.cursor) (stream_size : int) : t =
  let _ = stream_size in
  (* GSIHashHeader: VerSignature, VerHdr, HrSize, NumBuckets *)
  let _ver_signature = read_u32 cur in
  let _ver_hdr = read_u32 cur in
  let hr_size = Unsigned.UInt32.to_int (read_u32 cur) in
  let num_buckets = Unsigned.UInt32.to_int (read_u32 cur) in
  (* Hash records: HrSize bytes of (u32 offset, u32 cref) pairs *)
  let num_records = hr_size / 8 in
  let hash_records =
    Array.init num_records (fun _ ->
        let offset = read_u32 cur in
        let cref = read_u32 cur in
        { offset; cref })
  in
  (* Hash buckets: NumBuckets u32 values *)
  let hash_buckets = Array.init num_buckets (fun _ -> read_u32 cur) in
  { hash_records; hash_buckets }
