(** DBI (Debug Information) stream reader.

    References:
    - LLVM: llvm/include/llvm/DebugInfo/PDB/Native/RawTypes.h
    - LLVM: llvm/lib/DebugInfo/PDB/Native/DbiStream.cpp
    - LLVM docs: https://llvm.org/docs/PDB/DbiStream.html *)

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

let read_u16 cur = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int
let read_u32 cur = Object.Buffer.Read.u32 cur
let read_i32 cur = Unsigned.UInt32.to_int32 (read_u32 cur)

let read_cstring (cur : Object.Buffer.cursor) : string =
  match Object.Buffer.Read.zero_string cur () with
  | Some s -> s
  | Option.None -> ""

(* SectionContrib is 28 bytes *)
let parse_section_contribution (cur : Object.Buffer.cursor) :
    section_contribution =
  let section = read_u16 cur in
  let _padding = read_u16 cur in
  let offset = read_i32 cur in
  let size = read_i32 cur in
  let characteristics = read_u32 cur in
  let module_index = read_u16 cur in
  let _padding2 = read_u16 cur in
  let data_crc = read_u32 cur in
  let reloc_crc = read_u32 cur in
  { section; offset; size; characteristics; module_index; data_crc; reloc_crc }

let parse_module_info (cur : Object.Buffer.cursor) : module_info =
  let _mod = read_u32 cur in
  (* unused pointer field *)
  let section_contrib = parse_section_contribution cur in
  let flags = read_u16 cur in
  let module_sym_stream = read_u16 cur in
  let sym_byte_size = Unsigned.UInt32.to_int (read_u32 cur) in
  let c11_byte_size = Unsigned.UInt32.to_int (read_u32 cur) in
  let c13_byte_size = Unsigned.UInt32.to_int (read_u32 cur) in
  let source_file_count = read_u16 cur in
  let _padding1 = read_u16 cur in
  let _file_name_offs = read_u32 cur in
  let _src_file_name_ni = read_u32 cur in
  let _pdb_file_path_ni = read_u32 cur in
  let module_name = read_cstring cur in
  let obj_file_name = read_cstring cur in
  (* Align to 4 bytes *)
  let pos = cur.position in
  let aligned = (pos + 3) land lnot 3 in
  if aligned > pos then Object.Buffer.seek cur aligned;
  {
    section_contrib;
    flags;
    module_sym_stream;
    sym_byte_size;
    c11_byte_size;
    c13_byte_size;
    source_file_count;
    module_name;
    obj_file_name;
  }

let parse_header (cur : Object.Buffer.cursor) : header =
  (* DbiStreamHeader is 64 bytes. *)
  Object.Buffer.ensure cur 64 "DBI stream: truncated header";
  let version_signature = read_i32 cur in
  let version_header = read_u32 cur in
  let age = read_u32 cur in
  let global_stream_index = read_u16 cur in
  let build_number = read_u16 cur in
  let public_stream_index = read_u16 cur in
  let pdb_dll_version = read_u16 cur in
  let sym_record_stream = read_u16 cur in
  let pdb_dll_rbld = read_u16 cur in
  let mod_info_size = Int32.to_int (read_i32 cur) in
  let section_contribution_size = Int32.to_int (read_i32 cur) in
  let section_map_size = Int32.to_int (read_i32 cur) in
  let file_info_size = Int32.to_int (read_i32 cur) in
  let type_server_map_size = Int32.to_int (read_i32 cur) in
  let mfc_type_server_index = read_u32 cur in
  let optional_dbg_header_size = Int32.to_int (read_i32 cur) in
  let ec_substream_size = Int32.to_int (read_i32 cur) in
  let flags = read_u16 cur in
  let machine = read_u16 cur in
  let _reserved = read_u32 cur in
  {
    version_signature;
    version_header;
    age;
    global_stream_index;
    build_number;
    public_stream_index;
    pdb_dll_version;
    sym_record_stream;
    pdb_dll_rbld;
    mod_info_size;
    section_contribution_size;
    section_map_size;
    file_info_size;
    type_server_map_size;
    mfc_type_server_index;
    optional_dbg_header_size;
    ec_substream_size;
    flags;
    machine;
  }

let parse_optional_debug_header (cur : Object.Buffer.cursor) (size : int) :
    optional_debug_header option =
  if size < 22 then Option.None (* need at least 11 * u16 = 22 bytes *)
  else
    Some
      {
        fpo_data = read_u16 cur;
        exception_data = read_u16 cur;
        fixup_data = read_u16 cur;
        omap_to_src = read_u16 cur;
        omap_from_src = read_u16 cur;
        section_header = read_u16 cur;
        token_rid_map = read_u16 cur;
        xdata = read_u16 cur;
        pdata = read_u16 cur;
        new_fpo_data = read_u16 cur;
        original_section_header = read_u16 cur;
      }

let parse (cur : Object.Buffer.cursor) : t =
  let h = parse_header cur in
  (* Parse module info substream *)
  let mod_start = cur.position in
  let mod_end = mod_start + h.mod_info_size in
  let modules = ref [] in
  while cur.position < mod_end do
    let m = parse_module_info cur in
    modules := m :: !modules
  done;
  let modules = Array.of_list (List.rev !modules) in
  (* Parse section contribution substream *)
  let sc_end = cur.position + h.section_contribution_size in
  let section_contributions =
    if h.section_contribution_size > 4 then begin
      (* First u32 is the version *)
      let _version = read_u32 cur in
      let sc_entry_size = 28 in
      (* SectionContrib is 28 bytes *)
      let remaining = sc_end - cur.position in
      let count = remaining / sc_entry_size in
      let scs = Array.init count (fun _ -> parse_section_contribution cur) in
      scs
    end
    else [||]
  in
  (* Skip to end of section contributions *)
  if cur.position < sc_end then Object.Buffer.seek cur sc_end;
  (* Skip section map *)
  let sm_end = cur.position + h.section_map_size in
  if cur.position < sm_end then Object.Buffer.seek cur sm_end;
  (* Skip file info *)
  let fi_end = cur.position + h.file_info_size in
  if cur.position < fi_end then Object.Buffer.seek cur fi_end;
  (* Skip type server map *)
  let ts_end = cur.position + h.type_server_map_size in
  if cur.position < ts_end then Object.Buffer.seek cur ts_end;
  (* Skip EC substream *)
  let ec_end = cur.position + h.ec_substream_size in
  if cur.position < ec_end then Object.Buffer.seek cur ec_end;
  (* Parse optional debug header *)
  let optional_debug_header =
    parse_optional_debug_header cur h.optional_dbg_header_size
  in
  { header = h; modules; section_contributions; optional_debug_header }

let module_symbols (msf : Msf.t) (m : module_info) :
    Codeview_symbols.symbol_record Seq.t =
  if m.module_sym_stream = 0xFFFF || m.sym_byte_size = 0 then Seq.empty
  else
    match Msf.get_stream msf m.module_sym_stream with
    | Option.None -> Seq.empty
    | Some stream ->
        let cur = Object.Buffer.cursor stream in
        (* The first 4 bytes are a signature (CV_SIGNATURE_C13 = 4) *)
        let _sig = read_u32 cur in
        let sym_bytes = m.sym_byte_size - 4 in
        if sym_bytes > 0 then Codeview_symbols.parse_symbol_stream cur sym_bytes
        else Seq.empty
