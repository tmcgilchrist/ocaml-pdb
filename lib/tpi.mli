(** TPI/IPI stream reader.

    The TPI stream (Stream 2) contains all CodeView type records. The IPI stream
    (Stream 4) has the same format but contains ID records (LF_FUNC_ID,
    LF_STRING_ID, etc.).

    Both streams share the same header and record layout. *)

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

val parse_header : Object.Buffer.cursor -> header
(** Parse the TPI/IPI stream header (56 bytes).
    @raise Object.Buffer.Invalid_format on truncated input. *)

val parse_type_records :
  Object.Buffer.cursor -> header -> Codeview_types.type_record Seq.t
(** [parse_type_records cur header] lazily iterates all type records in the
    TPI/IPI stream.
    @raise Object.Buffer.Invalid_format (during iteration) if a record's
    length prefix overruns the stream end or the record is internally
    malformed. *)

val num_type_records : header -> int
(** Number of type records: [type_index_end - type_index_begin]. *)
