(** DBI (Debug Information) stream writer. *)

val write :
  Stdlib.Buffer.t ->
  Dbi.module_info list ->
  Dbi.section_contribution list ->
  machine:int ->
  unit
(** [write buf modules section_contribs ~machine] serializes a DBI stream
    with no global/public/symrecord stream references (all set to 0xFFFF). *)

val write_full :
  Stdlib.Buffer.t ->
  Dbi.module_info list ->
  Dbi.section_contribution list ->
  machine:int ->
  global_stream:int ->
  public_stream:int ->
  sym_record_stream:int ->
  unit
(** [write_full buf modules section_contribs ~machine ~global_stream
    ~public_stream ~sym_record_stream] serializes a DBI stream with
    the given stream indices for global symbols, public symbols, and
    the symbol record stream. *)
