(** DBI (Debug Information) stream reader.

    The DBI stream is always at stream index 3 in the MSF container. It is the
    central directory connecting modules, section contributions, and debug
    substreams. *)

open Pdb_types

type section_contribution = {
  section : int;
  offset : int32;
  size : int32;
  characteristics : u32;
  module_index : int;
  data_crc : u32;
  reloc_crc : u32;
}

type module_info = {
  section_contrib : section_contribution;
  flags : int;
  module_sym_stream : int;
  sym_byte_size : int;
  c11_byte_size : int;
  c13_byte_size : int;
  source_file_count : int;
  module_name : string;
  obj_file_name : string;
}

type header = {
  version_signature : int32;
  version_header : u32;
  age : u32;
  global_stream_index : int;
  build_number : int;
  public_stream_index : int;
  pdb_dll_version : int;
  sym_record_stream : int;
  pdb_dll_rbld : int;
  mod_info_size : int;
  section_contribution_size : int;
  section_map_size : int;
  file_info_size : int;
  type_server_map_size : int;
  mfc_type_server_index : u32;
  optional_dbg_header_size : int;
  ec_substream_size : int;
  flags : int;
  machine : int;
}

type optional_debug_header = {
  fpo_data : int;
  exception_data : int;
  fixup_data : int;
  omap_to_src : int;
  omap_from_src : int;
  section_header : int;
  token_rid_map : int;
  xdata : int;
  pdata : int;
  new_fpo_data : int;
  original_section_header : int;
}

type t = {
  header : header;
  modules : module_info array;
  section_contributions : section_contribution array;
  optional_debug_header : optional_debug_header option;
}

val parse : Object.Buffer.cursor -> t
(** [parse cur] reads the DBI stream from the cursor position. *)

val module_symbols :
  Msf.t -> module_info -> Codeview_symbols.symbol_record Seq.t
(** [module_symbols msf mod_info] reads symbol records for a module. *)
