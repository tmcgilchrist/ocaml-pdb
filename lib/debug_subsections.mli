(** C13 debug subsections (line info, file checksums, string table).

    These subsections appear in module debug streams, referenced from the DBI
    stream's module info entries. *)

open Pdb_types

type line_entry = {
  offset : u32;
  line_start : int;
  delta_line_end : int;
  is_statement : bool;
}

type column_entry = { start_column : int; end_column : int }

type line_block = {
  file_index : u32;
  lines : line_entry array;
  columns : column_entry array option;
}

type lines_subsection = {
  contrib_offset : u32;
  contrib_segment : int;
  flags : int;
  contrib_size : u32;
  blocks : line_block array;
}

type checksum_kind = None | MD5 | SHA1 | SHA256

type file_checksum_entry = {
  file_name_offset : u32;
  checksum_kind : checksum_kind;
  checksum : string;
}

type inlinee_line = { inlinee : u32; file_id : u32; source_line : u32 }

type frame_data_entry = {
  rva_start : u32;
  code_size : u32;
  local_size : u32;
  params_size : u32;
  max_stack_size : u32;
  frame_func : u32;
  prolog_size : int;
  saved_regs_size : int;
  flags : u32;
}

type cross_module_export = { local : u32; global : u32 }
(** Maps a TypeIndex local to one module onto a global TypeIndex shared across
    modules in the same PDB. *)

type cross_module_import = {
  module_name_offset : u32;
      (** Offset into the /names string table of the source module. *)
  references : u32 array;
      (** TypeIndexes in the source module that this module references. *)
}

type subsection =
  | Lines of lines_subsection
  | FileChecksums of file_checksum_entry array
  | StringTable of string array
  | InlineeLines of inlinee_line array
  | FrameData of frame_data_entry array
  | CrossModuleExports of cross_module_export array
  | CrossModuleImports of cross_module_import array
  | Unknown of { kind : int; data : string }

val parse_subsections : Object.Buffer.cursor -> int -> subsection Seq.t
(** [parse_subsections cur total_bytes] lazily iterates C13 subsections. Raises
    [Object.Buffer.Invalid_format] (during iteration) if a subsection's
    kind/size header is truncated or its declared size overruns the
    [total_bytes] window. *)

val write_subsection : Stdlib.Buffer.t -> subsection -> unit
(** [write_subsection buf sub] serializes a single subsection. *)
