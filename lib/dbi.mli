(** DBI (Debug Information) stream reader.

    The DBI stream is always at stream index 3 in the MSF container. It is the
    central directory connecting modules, section contributions, and debug
    substreams. *)

open Pdb_types

(** Section contribution: which bytes of a PE section came from which
    module. Used by the debugger to map addresses back to source files. *)
type section_contribution = {
  section : int;  (** PE section index (1-based). *)
  offset : int32;  (** Byte offset within the section. *)
  size : int32;  (** Number of bytes contributed. *)
  characteristics : u32;  (** Copy of the section's COFF characteristics. *)
  module_index : int;  (** Index into {!t.modules}. *)
  data_crc : u32;  (** CRC of the contribution's data bytes. *)
  reloc_crc : u32;  (** CRC of the contribution's relocations. *)
}

(** Per-module metadata describing one compilation unit. *)
type module_info = {
  section_contrib : section_contribution;
      (** The first section contribution recorded for this module. *)
  flags : int;
  module_sym_stream : int;
      (** MSF stream containing this module's symbol records, or
          0xFFFF if absent. *)
  sym_byte_size : int;
      (** Size of the symbol payload within [module_sym_stream]. *)
  c11_byte_size : int;
      (** Size of the legacy C11 line-info payload (typically 0). *)
  c13_byte_size : int;
      (** Size of the C13 debug-subsection payload following the
          symbols in [module_sym_stream]. *)
  source_file_count : int;  (** Number of source files in the FileInfo substream. *)
  module_name : string;  (** Logical module name, usually an object-file path. *)
  obj_file_name : string;  (** Object file path; may differ from [module_name]. *)
}

(** DBI stream's 64-byte header. *)
type header = {
  version_signature : int32;
      (** Always -1 in well-formed PDBs. *)
  version_header : u32;
      (** Format version (typically [V70 = 19990903]). *)
  age : u32;  (** Matches {!Pdb_stream.t.age}. *)
  global_stream_index : int;
      (** MSF stream with the GSI hash table, or 0xFFFF. *)
  build_number : int;
  public_stream_index : int;
      (** MSF stream with the PSI hash table, or 0xFFFF. *)
  pdb_dll_version : int;
  sym_record_stream : int;
      (** MSF stream containing the concatenated public+global symbol
          records that GSI/PSI index into. *)
  pdb_dll_rbld : int;
  mod_info_size : int;
      (** Byte size of the module-info substream. *)
  section_contribution_size : int;
      (** Byte size of the section-contribution substream. *)
  section_map_size : int;
      (** Byte size of the section-map substream. *)
  file_info_size : int;
      (** Byte size of the FileInfo source-files substream. *)
  type_server_map_size : int;
      (** Byte size of the type-server map (LF_TYPESERVER2 lookup). *)
  mfc_type_server_index : u32;
      (** Index of the MFC type server (or 0 if none). *)
  optional_dbg_header_size : int;
      (** Byte size of the optional-debug-header substream. *)
  ec_substream_size : int;
      (** Byte size of the EC (Edit-and-Continue) substream. *)
  flags : int;
  machine : int;
      (** PE COFF machine type (e.g. 0x8664 for AMD64). *)
}

(** Optional debug header: a fixed-length array of stream indices
    pointing at auxiliary streams. [0xFFFF] means "absent." *)
type optional_debug_header = {
  fpo_data : int;  (** Old-style FPO stream (see {!Fpo}). *)
  exception_data : int;
  fixup_data : int;
  omap_to_src : int;  (** OMAP target → source (see {!Omap}). *)
  omap_from_src : int;  (** OMAP source → target (see {!Omap}). *)
  section_header : int;  (** Copy of the linked image's COFF section headers. *)
  token_rid_map : int;
  xdata : int;  (** Copy of the .xdata section. *)
  pdata : int;  (** Copy of the .pdata section. *)
  new_fpo_data : int;  (** New-style FPO stream (same wire layout as the C13 FrameData subsection). *)
  original_section_header : int;
}

type t = {
  header : header;
  modules : module_info array;
  section_contributions : section_contribution array;
  optional_debug_header : optional_debug_header option;
}

val parse : Object.Buffer.cursor -> t
(** [parse cur] reads the DBI stream from the cursor position.
    @raise Object.Buffer.Invalid_format on truncated input. *)

val module_symbols :
  Msf.t -> module_info -> Codeview_symbols.symbol_record Seq.t
(** [module_symbols msf mod_info] lazily iterates the symbol records of
    a module's symbol stream. *)
