(** High-level PDB file builder.

    Assembles a complete PDB file, handling stream layout and cross-referencing
    automatically.

    Stream layout produced by [finalize]:
    - Stream 0: empty (old directory)
    - Stream 1: PDB Info Stream
    - Stream 2: TPI Stream
    - Stream 3: DBI Stream
    - Stream 4: IPI Stream
    - Stream 5..5+N-1: per-module symbol/debug streams (one per module)
    - Stream 5+N: symbol record stream (publics + globals concatenated)
    - Stream 5+N+1: globals hash stream
    - Stream 5+N+2: publics hash stream
    - Stream 5+N+3: /names stream *)

open Pdb_types
module Buffer = Stdlib.Buffer
open Binary_writer

type machine = I386 | AMD64 | ARM | ARM64

let machine_to_int = function
  | I386 -> 0x014C
  | AMD64 -> 0x8664
  | ARM -> 0x01C0
  | ARM64 -> 0xAA64

type module_desc = {
  name : string;
  obj_file : string;
  symbols : Codeview_symbols.symbol_record list;
  subsections : Debug_subsections.subsection list;
  section_contrib : Dbi.section_contribution option;
  source_files : string list;
      (** Source filenames associated with this compilation unit. Goes into the
          DBI FileInfo substream and is reported by llvm-pdbutil's [--files] /
          [--modules] (# files) output. Use [[]] for none. *)
}

type t = {
  guid : guid;
  age : int;
  machine : machine;
  mutable tpi_records : Codeview_types.type_record list;
  mutable tpi_next_index : int;
  mutable ipi_records : Codeview_types.type_record list;
  mutable ipi_next_index : int;
  mutable modules : module_desc list;
  mutable publics : Codeview_symbols.symbol_record list;
  mutable globals : Codeview_symbols.symbol_record list;
  string_table : Pdb_string_table.t;
}

let default_guid =
  {
    data1 = Unsigned.UInt32.zero;
    data2 = Unsigned.UInt16.zero;
    data3 = Unsigned.UInt16.zero;
    data4 = "\x00\x00\x00\x00\x00\x00\x00\x00";
  }

let create ?(guid = default_guid) ?(age = 1) machine =
  {
    guid;
    age;
    machine;
    tpi_records = [];
    tpi_next_index = 0x1000;
    ipi_records = [];
    ipi_next_index = 0x1000;
    modules = [];
    publics = [];
    globals = [];
    string_table = Pdb_string_table.create ();
  }

let add_type t record =
  let idx = t.tpi_next_index in
  t.tpi_records <- record :: t.tpi_records;
  t.tpi_next_index <- idx + 1;
  Type_index.user (Unsigned.UInt32.of_int idx)

let add_id t record =
  let idx = t.ipi_next_index in
  t.ipi_records <- record :: t.ipi_records;
  t.ipi_next_index <- idx + 1;
  Type_index.user (Unsigned.UInt32.of_int idx)

let add_module t desc = t.modules <- desc :: t.modules
let add_public t sym = t.publics <- sym :: t.publics
let add_global t sym = t.globals <- sym :: t.globals
let add_string t str = Pdb_string_table.add_string t.string_table str

(* CV_SIGNATURE_C13 *)
let cv_signature_c13 = 4

let finalize t =
  let modules = List.rev t.modules in
  let tpi_records = List.rev t.tpi_records in
  let ipi_records = List.rev t.ipi_records in
  let publics = List.rev t.publics in
  let globals = List.rev t.globals in
  (* A module gets its own debug stream only if it has content
     (symbols or C13 subsections). An empty module records
     module_sym_stream = 0xFFFF, matching llvm-pdbutil's yaml2pdb. *)
  let module_has_stream (m : module_desc) =
    m.symbols <> [] || m.subsections <> []
  in
  (* Build per-module stream bodies, only for modules that need one.
     Layout per LLVM's ModuleDebugStreamRef::reloadSerialize:
       [SymbolsSubstream | C11Lines | C13Lines | u32 GlobalRefsSize | globals]
     SymbolsSubstream begins with the 4-byte CV signature. We don't emit
     globals refs, so we always write GlobalRefsSize = 0. *)
  let module_streams =
    List.filter_map
      (fun (m : module_desc) ->
        if not (module_has_stream m) then Option.None
        else
          let buf = Buffer.create 256 in
          (* CV signature + symbol records *)
          write_u32_le buf cv_signature_c13;
          List.iter
            (fun sym -> Codeview_symbols.write_symbol_record buf sym)
            m.symbols;
          (* C13 debug subsections *)
          List.iter
            (fun sub -> Debug_subsections.write_subsection buf sub)
            m.subsections;
          (* GlobalRefs trailer (size = 0, no body) *)
          write_u32_le buf 0;
          Some (Buffer.contents buf))
      modules
  in
  let num_module_streams = List.length module_streams in
  (* Build GSI/PSI streams *)
  let gsi = Gsi_write.build_gsi_streams ~publics ~globals in
  (* Build /names stream *)
  let names_buf = Buffer.create 256 in
  Pdb_string_table.write names_buf t.string_table;
  let names_bytes = Buffer.contents names_buf in
  (* Assign stream indices. LLVM's PDBs always allocate an empty
     /LinkInfo named stream at index 5 (used for incremental link
     metadata); matching that layout keeps stream-index references in
     llvm-pdbutil dumps consistent with yaml2pdb output.
       0: empty, 1: PDB info, 2: TPI, 3: DBI, 4: IPI
       5: /LinkInfo (empty)
       6..6+N-1: module streams
       6+N: sym record, 6+N+1: globals, 6+N+2: publics, 6+N+3: /names *)
  let link_info_stream_idx = 5 in
  let first_module_stream = 6 in
  let sym_record_stream_idx = first_module_stream + num_module_streams in
  let globals_stream_idx = sym_record_stream_idx + 1 in
  let publics_stream_idx = globals_stream_idx + 1 in
  let names_stream_idx = publics_stream_idx + 1 in
  (* Build TPI stream *)
  let tpi_buf = Buffer.create 512 in
  Tpi_write.write tpi_buf tpi_records;
  let tpi_bytes = Buffer.contents tpi_buf in
  (* Build IPI stream *)
  let ipi_buf = Buffer.create 256 in
  Tpi_write.write ipi_buf ipi_records;
  let ipi_bytes = Buffer.contents ipi_buf in
  (* Build DBI stream *)
  let default_sc : Dbi.section_contribution =
    {
      section = 0;
      offset = 0l;
      size = 0l;
      characteristics = Unsigned.UInt32.zero;
      module_index = 0;
      data_crc = Unsigned.UInt32.zero;
      reloc_crc = Unsigned.UInt32.zero;
    }
  in
  (* Walk modules and assign module debug stream indices to those that
     have content; others record 0xFFFF (no stream). *)
  let module_infos =
    let next_stream = ref first_module_stream in
    List.mapi
      (fun i (m : module_desc) ->
        let module_sym_stream, sym_byte_size, c13_byte_size =
          if module_has_stream m then begin
            let stream_idx = !next_stream in
            incr next_stream;
            let sym_buf = Buffer.create 64 in
            (* sym_byte_size includes the 4-byte CV signature *)
            write_u32_le sym_buf cv_signature_c13;
            List.iter
              (fun sym -> Codeview_symbols.write_symbol_record sym_buf sym)
              m.symbols;
            let sym_size = Buffer.length sym_buf in
            let sub_buf = Buffer.create 64 in
            List.iter
              (fun sub -> Debug_subsections.write_subsection sub_buf sub)
              m.subsections;
            (stream_idx, sym_size, Buffer.length sub_buf)
          end
          else (0xFFFF, 0, 0)
        in
        let sc =
          match m.section_contrib with
          | Some sc -> { sc with module_index = i }
          | Option.None -> { default_sc with module_index = i }
        in
        ({
           Dbi.section_contrib = sc;
           flags = 0;
           module_sym_stream;
           sym_byte_size;
           c11_byte_size = 0;
           c13_byte_size;
           source_file_count = 0;
           module_name = m.name;
           obj_file_name = m.obj_file;
         }
          : Dbi.module_info))
      modules
  in
  let section_contribs =
    List.filter_map (fun (m : module_desc) -> m.section_contrib) modules
  in
  let dbi_buf = Buffer.create 512 in
  let has_gsi =
    String.length gsi.sym_record_stream > 0
    || List.length publics > 0
    || List.length globals > 0
  in
  let source_files_per_module =
    List.map (fun (m : module_desc) -> m.source_files) modules
  in
  if has_gsi then
    Dbi_write.write dbi_buf module_infos section_contribs
      ~source_files:source_files_per_module ~machine:(machine_to_int t.machine)
      ~global_stream:globals_stream_idx ~public_stream:publics_stream_idx
      ~sym_record_stream:sym_record_stream_idx ()
  else
    Dbi_write.write dbi_buf module_infos section_contribs
      ~source_files:source_files_per_module ~machine:(machine_to_int t.machine)
      ();
  let dbi_bytes = Buffer.contents dbi_buf in
  (* Build PDB Info Stream *)
  let named_streams =
    [ ("/names", names_stream_idx); ("/LinkInfo", link_info_stream_idx) ]
  in
  let info_buf = Buffer.create 128 in
  Pdb_stream_write.write info_buf
    {
      version = VC70;
      signature = Unsigned.UInt32.zero;
      age = Unsigned.UInt32.of_int t.age;
      guid = t.guid;
      named_streams;
      features = [ ContainsIdStream ];
    };
  let info_bytes = Buffer.contents info_buf in
  (* Assemble MSF *)
  let msf = Msf_write.create ~block_size:4096 in
  let _s0 = Msf_write.add_empty_stream msf in
  (* stream 0 *)
  let _s1 = Msf_write.add_stream msf info_bytes in
  (* stream 1 *)
  let _s2 = Msf_write.add_stream msf tpi_bytes in
  (* stream 2 *)
  let _s3 = Msf_write.add_stream msf dbi_bytes in
  (* stream 3 *)
  let _s4 = Msf_write.add_stream msf ipi_bytes in
  (* stream 4 *)
  let _s5 = Msf_write.add_empty_stream msf in
  (* stream 5: /LinkInfo (empty) *)
  ignore link_info_stream_idx;
  (* Module streams *)
  List.iter
    (fun stream_bytes -> ignore (Msf_write.add_stream msf stream_bytes))
    module_streams;
  (* GSI/PSI streams *)
  let _sym_s = Msf_write.add_stream msf gsi.sym_record_stream in
  let _gbl_s = Msf_write.add_stream msf gsi.globals_stream in
  let _pub_s = Msf_write.add_stream msf gsi.publics_stream in
  (* /names stream *)
  let _names_s = Msf_write.add_stream msf names_bytes in
  Msf_write.finalize msf
