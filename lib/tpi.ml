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

let parse_header cur =
  (* TpiStreamHeader is 56 bytes: 5 u32 + 2 u16 + 2 u32 + 6 u32 = 56. *)
  Object.Buffer.ensure cur 56 "TPI stream: truncated header";
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

let num_type_records h =
  Unsigned.UInt32.to_int h.type_index_end
  - Unsigned.UInt32.to_int h.type_index_begin

let parse_type_records (cur : Object.Buffer.cursor) h =
  let total_bytes = Unsigned.UInt32.to_int h.type_record_bytes in
  let end_pos = cur.position + total_bytes in
  let rec next () =
    if cur.position >= end_pos then Seq.Nil
    else begin
      (* Each record: u16 length (not including the length field itself),
         then length bytes of payload starting with the leaf kind u16. *)
      Object.Buffer.ensure cur 2 "TPI: truncated record length prefix";
      let rec_len = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int in
      if cur.position + rec_len > end_pos then
        Object.Buffer.invalid_format
          (Printf.sprintf
             "TPI: record length %d at offset %d overruns stream end" rec_len
             (cur.position - 2));
      let record = Codeview_types.parse_type_record cur rec_len in
      (* Advance to the next 4-byte aligned position. *)
      let record_end_unaligned = cur.position in
      let aligned = (record_end_unaligned + 3) land lnot 3 in
      if aligned > record_end_unaligned && aligned <= end_pos then
        Object.Buffer.seek cur aligned;
      Seq.Cons (record, next)
    end
  in
  next
