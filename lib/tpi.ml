(** TPI/IPI stream reader.

    References:
    - LLVM: llvm/include/llvm/DebugInfo/PDB/Native/RawTypes.h (TpiStreamHeader)
    - LLVM: llvm/lib/DebugInfo/PDB/Native/TpiStream.cpp *)

open Pdb_types

type header = {
  version : u32;
  header_size : u32;
  type_index_begin : u32;
  type_index_end : u32;
  type_record_bytes : u32;
  hash_stream_index : int;
  hash_aux_stream_index : int;
  hash_key_size : u32;
  num_hash_buckets : u32;
}

let parse_header (cur : Object.Buffer.cursor) : header =
  let version = Object.Buffer.Read.u32 cur in
  let header_size = Object.Buffer.Read.u32 cur in
  let type_index_begin = Object.Buffer.Read.u32 cur in
  let type_index_end = Object.Buffer.Read.u32 cur in
  let type_record_bytes = Object.Buffer.Read.u32 cur in
  let hash_stream_index =
    Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int
  in
  let hash_aux_stream_index =
    Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int
  in
  let hash_key_size = Object.Buffer.Read.u32 cur in
  let num_hash_buckets = Object.Buffer.Read.u32 cur in
  (* Skip the three EmbeddedBuf fields (8 bytes each = 24 bytes) *)
  let _hash_value_off = Object.Buffer.Read.u32 cur in
  let _hash_value_len = Object.Buffer.Read.u32 cur in
  let _index_offset_off = Object.Buffer.Read.u32 cur in
  let _index_offset_len = Object.Buffer.Read.u32 cur in
  let _hash_adj_off = Object.Buffer.Read.u32 cur in
  let _hash_adj_len = Object.Buffer.Read.u32 cur in
  {
    version;
    header_size;
    type_index_begin;
    type_index_end;
    type_record_bytes;
    hash_stream_index;
    hash_aux_stream_index;
    hash_key_size;
    num_hash_buckets;
  }

let num_type_records (h : header) : int =
  Unsigned.UInt32.to_int h.type_index_end
  - Unsigned.UInt32.to_int h.type_index_begin

let parse_type_records (cur : Object.Buffer.cursor) (h : header) :
    Codeview_types.type_record Seq.t =
  let total_bytes = Unsigned.UInt32.to_int h.type_record_bytes in
  let end_pos = cur.position + total_bytes in
  let rec next () =
    if cur.position >= end_pos then Seq.Nil
    else
      (* Each record: u16 length (not including the length field itself),
         then length bytes of payload starting with the leaf kind u16 *)
      let rec_len = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int in
      let record = Codeview_types.parse_type_record cur rec_len in
      (* Advance to the next 4-byte aligned position *)
      let record_end_unaligned = cur.position in
      let aligned = (record_end_unaligned + 3) land lnot 3 in
      if aligned > record_end_unaligned && aligned <= end_pos then
        Object.Buffer.seek cur aligned;
      Seq.Cons (record, next)
  in
  next
