(** C13 debug subsections (line info, file checksums, string table).

    These subsections appear in module debug streams, referenced
    from the DBI stream's module info entries. *)

open Pdb_types

type line_entry = {
  offset : u32;
  line_start : int;
  delta_line_end : int;
  is_statement : bool;
}

type line_block = {
  file_index : u32;
  lines : line_entry array;
}

type lines_subsection = {
  contrib_offset : u32;
  contrib_segment : int;
  flags : int;
  contrib_size : u32;
  blocks : line_block array;
}

type checksum_kind =
  | None
  | MD5
  | SHA1
  | SHA256

type file_checksum_entry = {
  file_name_offset : u32;
  checksum_kind : checksum_kind;
  checksum : string;
}

type inlinee_line = {
  inlinee : u32;
  file_id : u32;
  source_line : u32;
}

type subsection =
  | Lines of lines_subsection
  | FileChecksums of file_checksum_entry array
  | StringTable of string array
  | InlineeLines of inlinee_line array
  | Unknown of { kind : int; data : string }

val parse_subsections : Object.Buffer.cursor -> int -> subsection Seq.t
(** [parse_subsections cur total_bytes] lazily iterates C13 subsections. *)

val write_subsection : Stdlib.Buffer.t -> subsection -> unit
(** [write_subsection buf sub] serializes a single subsection. *)
