(** DBI (Debug Information) stream writer. *)

val write :
  Stdlib.Buffer.t ->
  Dbi.module_info list ->
  Dbi.section_contribution list ->
  source_files:string list list ->
  machine:int ->
  unit
(** [write buf modules section_contribs ~source_files ~machine] serializes a
    DBI stream with no global/public/symrecord stream references (all
    0xFFFF). [source_files] lists the source filenames for each module in
    module order (use [[]] when none); the FileInfo substream is built from
    this and each module's recorded [source_file_count] is overridden to
    match. *)

val write_full :
  Stdlib.Buffer.t ->
  Dbi.module_info list ->
  Dbi.section_contribution list ->
  source_files:string list list ->
  machine:int ->
  global_stream:int ->
  public_stream:int ->
  sym_record_stream:int ->
  unit
(** [write_full] is like [write] but also records the given stream indices
    in the DBI header for the globals, publics, and symbol record streams. *)
