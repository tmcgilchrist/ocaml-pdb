(** TPI/IPI stream writer.

    Counterpart to {!Tpi}. The TPI stream (index 2) holds CodeView type records;
    the IPI stream (index 4) holds ID records (LF_FUNC_ID, LF_STRING_ID, ...);
    both share the same on-disk layout. *)

val write : Stdlib.Buffer.t -> Codeview_types.type_record list -> unit
(** [write buf records] serializes a complete TPI/IPI stream including the
    header and all type records. Hash stream index is set to 0xFFFF. *)

val write_with_hash :
  Stdlib.Buffer.t ->
  Codeview_types.type_record list ->
  hash_stream_index:int ->
  string
(** [write_with_hash buf records ~hash_stream_index] serializes a TPI/IPI stream
    with the given hash stream index in the header, and returns the hash stream
    bytes (for a separate MSF stream). The hash stream contains per-record hash
    values and type index offset entries. *)
