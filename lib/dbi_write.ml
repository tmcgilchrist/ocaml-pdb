(** DBI (Debug Information) stream writer.

    References:
    - LLVM: llvm/lib/DebugInfo/PDB/Native/DbiStreamBuilder.cpp *)

module Buffer = Stdlib.Buffer

open Binary_writer

let write_section_contribution buf (sc : Dbi.section_contribution) =
  write_u16_le buf sc.section;
  write_u16_le buf 0;
  (* padding *)
  write_i32_le buf sc.offset;
  write_i32_le buf sc.size;
  write_u32_le buf (Unsigned.UInt32.to_int sc.characteristics);
  write_u16_le buf sc.module_index;
  write_u16_le buf 0;
  (* padding2 *)
  write_u32_le buf (Unsigned.UInt32.to_int sc.data_crc);
  write_u32_le buf (Unsigned.UInt32.to_int sc.reloc_crc)

let write_module_info buf (m : Dbi.module_info) =
  write_u32_le buf 0;
  (* Mod (unused pointer) *)
  write_section_contribution buf m.section_contrib;
  write_u16_le buf m.flags;
  write_u16_le buf m.module_sym_stream;
  write_u32_le buf m.sym_byte_size;
  write_u32_le buf m.c11_byte_size;
  write_u32_le buf m.c13_byte_size;
  write_u16_le buf m.source_file_count;
  write_u16_le buf 0;
  (* padding *)
  write_u32_le buf 0;
  (* FileNameOffs *)
  write_u32_le buf 0;
  (* SrcFileNameNI *)
  write_u32_le buf 0;
  (* PdbFilePathNI *)
  write_cstring buf m.module_name;
  write_cstring buf m.obj_file_name;
  write_padding_to_align buf 4

(* DBI version: V70 = 19990903 *)
let dbi_version_v70 = 19990903

let absent_optional_debug_header : Dbi.optional_debug_header =
  {
    fpo_data = 0xFFFF;
    exception_data = 0xFFFF;
    fixup_data = 0xFFFF;
    omap_to_src = 0xFFFF;
    omap_from_src = 0xFFFF;
    section_header = 0xFFFF;
    token_rid_map = 0xFFFF;
    xdata = 0xFFFF;
    pdata = 0xFFFF;
    new_fpo_data = 0xFFFF;
    original_section_header = 0xFFFF;
  }

let write (buf : Buffer.t) (modules : Dbi.module_info list)
    (section_contribs : Dbi.section_contribution list) ~source_files ~machine
    ?(global_stream = 0xFFFF) ?(public_stream = 0xFFFF)
    ?(sym_record_stream = 0xFFFF)
    ?(optional_debug_header = absent_optional_debug_header) () : unit =
  let num_modules = List.length modules in
  (* [source_files = []] means "caller did not supply source filenames";
     leave each module_info's [source_file_count] alone and emit a minimal
     FileInfo substream with zero-count entries. When at least one module's
     list is supplied, normalize to length [num_modules] and override the
     corresponding [source_file_count]s so the FileInfo substream and the
     module info stay consistent. *)
  let supplied_source_files = source_files <> [] in
  let source_files =
    if not supplied_source_files then List.init num_modules (fun _ -> [])
    else
      let n = List.length source_files in
      if n >= num_modules then
        let rec take k = function
          | _ when k = 0 -> []
          | [] -> []
          | x :: rest -> x :: take (k - 1) rest
        in
        take num_modules source_files
      else source_files @ List.init (num_modules - n) (fun _ -> [])
  in
  let modules =
    if not supplied_source_files then modules
    else
      List.map2
        (fun (m : Dbi.module_info) files ->
          { m with source_file_count = List.length files })
        modules source_files
  in
  (* Build substreams *)
  let mod_buf = Buffer.create 256 in
  List.iter (write_module_info mod_buf) modules;
  let mod_info_size = Buffer.length mod_buf in
  let sc_buf = Buffer.create 128 in
  (* Section contribution version (0xeffe0000 + 19970605 = Ver60) *)
  write_u32_le sc_buf 0xF12EBA2D;
  (* SC version *)
  List.iter (write_section_contribution sc_buf) section_contribs;
  let sc_size = Buffer.length sc_buf in
  (* Build FileInfo substream. Layout per LLVM DbiModuleList::initialize:
       NumModules        : u16
       NumSourceFiles    : u16  (deprecated; truncates total to u16 range,
                                 but llvm-pdbutil uses sum of ModFileCounts)
       ModIndices        : u16[NumModules]  (per-module file list base)
       ModFileCounts     : u16[NumModules]
       FileNameOffsets   : u32[total]       (offsets into NamesBuffer)
       NamesBuffer       : null-terminated strings, padded to 4 bytes
     ModIndices[i] gives the offset (in FileNameOffsets entries) where
     module i's source files begin; ModFileCounts[i] the count. *)
  let total_files =
    List.fold_left (fun acc l -> acc + List.length l) 0 source_files
  in
  let file_info_buf = Buffer.create 64 in
  write_u16_le file_info_buf num_modules;
  write_u16_le file_info_buf (total_files land 0xFFFF);
  (* ModIndices: per-module starting index into FileNameOffsets *)
  let running = ref 0 in
  List.iter
    (fun files ->
      write_u16_le file_info_buf (!running land 0xFFFF);
      running := !running + List.length files)
    source_files;
  (* ModFileCounts *)
  List.iter
    (fun files ->
      write_u16_le file_info_buf (List.length files land 0xFFFF))
    source_files;
  (* Build NamesBuffer and per-file offsets. The same filename string is
     deduplicated by offset (LLVM's DbiStreamBuilder does the same). *)
  let names_buf = Buffer.create 64 in
  let name_offsets : (string, int) Hashtbl.t = Hashtbl.create 8 in
  let offset_of name =
    match Hashtbl.find_opt name_offsets name with
    | Some off -> off
    | None ->
        let off = Buffer.length names_buf in
        Buffer.add_string names_buf name;
        Buffer.add_char names_buf '\000';
        Hashtbl.add name_offsets name off;
        off
  in
  let file_offsets =
    List.concat_map (fun files -> List.map offset_of files) source_files
  in
  List.iter (fun off -> write_u32_le file_info_buf off) file_offsets;
  Buffer.add_string file_info_buf (Buffer.contents names_buf);
  (* Pad FileInfo substream to 4-byte alignment so the next substream
     (TypeServerMap / EC) starts aligned. *)
  let fi_unaligned = Buffer.length file_info_buf in
  let fi_pad = (4 - (fi_unaligned mod 4)) mod 4 in
  for _ = 1 to fi_pad do
    Buffer.add_char file_info_buf '\000'
  done;
  let file_info_size = Buffer.length file_info_buf in
  (* Build OptionalDbgHeader: 11 x u16 stream indices. *)
  let opt_dbg_buf = Buffer.create 22 in
  let h = optional_debug_header in
  List.iter
    (write_u16_le opt_dbg_buf)
    [
      h.fpo_data;
      h.exception_data;
      h.fixup_data;
      h.omap_to_src;
      h.omap_from_src;
      h.section_header;
      h.token_rid_map;
      h.xdata;
      h.pdata;
      h.new_fpo_data;
      h.original_section_header;
    ];
  let opt_dbg_size = Buffer.length opt_dbg_buf in
  (* Build EC substream: an empty-but-valid PDB string table.
     llvm-pdbutil's dumpModules unconditionally looks up name index 0 via
     DbiStream::getECName, which requires this substream to parse. Without it
     we get "Stream Error: The stream is too short". Minimum table is 25
     bytes: 12-byte header + 1-byte strings ("\\0") + 4-byte bucket count (=1)
     + 4-byte bucket[0] (=0) + 4-byte name count (=0). *)
  let ec_buf = Buffer.create 25 in
  write_u32_le ec_buf 0xEFFEEFFE;
  (* PDBStringTableSignature *)
  write_u32_le ec_buf 1;
  (* HashVersion = 1 *)
  write_u32_le ec_buf 1;
  (* ByteSize of strings buffer *)
  Buffer.add_char ec_buf '\000';
  (* strings: single null byte = empty string at offset 0 *)
  write_u32_le ec_buf 1;
  (* NumBuckets *)
  write_u32_le ec_buf 0;
  (* buckets[0] = 0 (no string in this bucket) *)
  write_u32_le ec_buf 0;
  (* NameCount = 0 *)
  let ec_size = Buffer.length ec_buf in
  (* Write DBI header (64 bytes) *)
  write_i32_le buf (-1l);
  (* VersionSignature *)
  write_u32_le buf dbi_version_v70;
  write_u32_le buf 1;
  (* Age *)
  write_u16_le buf global_stream;
  (* GlobalSymbolStreamIndex *)
  write_u16_le buf 0;
  (* BuildNumber *)
  write_u16_le buf public_stream;
  (* PublicSymbolStreamIndex *)
  write_u16_le buf 0;
  (* PdbDllVersion *)
  write_u16_le buf sym_record_stream;
  (* SymRecordStreamIndex *)
  write_u16_le buf 0;
  (* PdbDllRbld *)
  write_i32_le buf (Int32.of_int mod_info_size);
  write_i32_le buf (Int32.of_int sc_size);
  write_i32_le buf 0l;
  (* SectionMapSize *)
  write_i32_le buf (Int32.of_int file_info_size);
  (* FileInfoSize *)
  write_i32_le buf 0l;
  (* TypeServerSize *)
  write_u32_le buf 0;
  (* MFCTypeServerIndex *)
  write_i32_le buf (Int32.of_int opt_dbg_size);
  (* OptionalDbgHdrSize *)
  write_i32_le buf (Int32.of_int ec_size);
  (* ECSubstreamSize *)
  write_u16_le buf 0;
  (* Flags *)
  write_u16_le buf machine;
  write_u32_le buf 0;
  (* Reserved *)
  (* Write substreams *)
  Buffer.add_string buf (Buffer.contents mod_buf);
  Buffer.add_string buf (Buffer.contents sc_buf);
  (* SectionMap: empty *)
  Buffer.add_string buf (Buffer.contents file_info_buf);
  (* TypeServer: empty *)
  (* MFCTypeServer: nothing *)
  Buffer.add_string buf (Buffer.contents ec_buf);
  Buffer.add_string buf (Buffer.contents opt_dbg_buf)

