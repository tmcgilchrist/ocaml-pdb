(** Global/Public Symbol Index (GSI/PSI) writer.

    Builds the hash tables needed for global and public symbol lookup. *)

type symbol_entry = {
  name : string;
  sym_offset : int;
}
(** A symbol with its name and offset into the symbol record stream. *)

val write_gsi : Stdlib.Buffer.t -> symbol_entry list -> unit
(** [write_gsi buf entries] writes a GSI hash stream (used for both
    the global and public symbol hash tables). *)

val write_publics_stream :
  Stdlib.Buffer.t -> Codeview_symbols.symbol_record list -> unit
(** [write_publics_stream buf symbols] writes a complete publics stream
    including the publics header, GSI hash table, and address map.
    [symbols] should be the public symbol records (S_PUB32). *)

type gsi_streams = {
  sym_record_stream : string;
  globals_stream : string;
  publics_stream : string;
}
(** The three streams needed for global/public symbol support. *)

val build_gsi_streams :
  publics:Codeview_symbols.symbol_record list ->
  globals:Codeview_symbols.symbol_record list ->
  gsi_streams
(** [build_gsi_streams ~publics ~globals] builds the symbol record stream
    (public records followed by global records), the publics hash stream,
    and the globals hash stream. These should be added as MSF streams and
    their indices wired into the DBI header. *)
