(** DBI (Debug Information) stream writer. *)

val write :
  Stdlib.Buffer.t ->
  Dbi.module_info list ->
  Dbi.section_contribution list ->
  source_files:string list list ->
  machine:int ->
  ?global_stream:int ->
  ?public_stream:int ->
  ?sym_record_stream:int ->
  ?optional_debug_header:Dbi.optional_debug_header ->
  unit ->
  unit
(** [write buf modules section_contribs ~source_files ~machine ?global_stream
    ?public_stream ?sym_record_stream ?optional_debug_header ()] serializes
    a DBI stream.

    [source_files] lists the source filenames for each module in module
    order (use [[]] when none); the FileInfo substream is built from this
    and each module's recorded [source_file_count] is overridden to match.

    The optional [global_stream], [public_stream], and [sym_record_stream]
    record the corresponding stream indices in the DBI header. They each
    default to [0xFFFF] meaning "absent."

    [optional_debug_header] populates the 11-field optional-debug-header
    substream pointing at FPO / OMAP / xdata / pdata / etc. streams. All
    fields default to [0xFFFF] when this argument is omitted. *)
