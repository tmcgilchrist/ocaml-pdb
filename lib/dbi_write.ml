(** DBI (Debug Information) stream writer.

    References:
    - LLVM: llvm/lib/DebugInfo/PDB/Native/DbiStreamBuilder.cpp *)

module Buffer = Stdlib.Buffer

let write_u16_le buf v =
  Buffer.add_char buf (Char.chr (v land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xFF))

let write_u32_le buf v =
  Buffer.add_char buf (Char.chr (v land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 16) land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 24) land 0xFF))

let write_i32_le buf (v : int32) = write_u32_le buf (Int32.to_int v)

let write_cstring buf s =
  Buffer.add_string buf s;
  Buffer.add_char buf '\000'

let write_padding_to_align buf alignment =
  let pos = Buffer.length buf in
  let align = (alignment - (pos mod alignment)) mod alignment in
  for _ = 1 to align do
    Buffer.add_char buf '\000'
  done

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

let write_full (buf : Buffer.t) (modules : Dbi.module_info list)
    (section_contribs : Dbi.section_contribution list) ~machine ~global_stream
    ~public_stream ~sym_record_stream : unit =
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
  (* Build FileInfo substream: NumModules(u16), NumSourceFiles(u16),
     then ModIndices[N] and ModFileCounts[N] arrays (both u16 each) *)
  let num_modules = List.length modules in
  let file_info_buf = Buffer.create 32 in
  write_u16_le file_info_buf num_modules;
  write_u16_le file_info_buf 0;
  (* NumSourceFiles *)
  for _ = 1 to num_modules do
    write_u16_le file_info_buf 0
    (* ModIndices *)
  done;
  for _ = 1 to num_modules do
    write_u16_le file_info_buf 0
    (* ModFileCounts *)
  done;
  let file_info_size = Buffer.length file_info_buf in
  (* Build OptionalDbgHeader: 11 x u16 stream indices, all 0xFFFF *)
  let opt_dbg_buf = Buffer.create 22 in
  for _ = 1 to 11 do
    write_u16_le opt_dbg_buf 0xFFFF
  done;
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

let write buf modules section_contribs ~machine =
  write_full buf modules section_contribs ~machine ~global_stream:0xFFFF
    ~public_stream:0xFFFF ~sym_record_stream:0xFFFF
