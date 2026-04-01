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
