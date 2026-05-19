[@@@warning "-26"]

(** LLVM-equivalence tests.

    For each scenario, this driver:
    1. Locates the corresponding LLVM YAML fixture (in llvm-project source tree)
    2. Runs [llvm-pdbutil yaml2pdb] on it to produce a reference PDB
    3. Programmatically builds an equivalent PDB via [Pdb.Pdb_builder]
    4. Dumps both with [llvm-pdbutil dump <subcmd>]
    5. Diffs the text output, failing the test on any mismatch

    Tests skip cleanly when [llvm-pdbutil] is not on PATH or the LLVM source
    tree cannot be found.

    The LLVM YAML files are not copied into this repo — they are used as a
    living specification of what each scenario should look like. *)

module Buffer = Stdlib.Buffer

(** {1 Environment discovery} *)

let has_llvm_pdbutil () =
  try
    let ic = Unix.open_process_in "llvm-pdbutil --version 2>/dev/null" in
    let _ = input_line ic in
    Unix.close_process_in ic = Unix.WEXITED 0
  with _ -> false

(** Resolve the LLVM PDB Inputs directory.
    Order: [LLVM_PROJECT_DIR] env var, then a known local path. *)
let llvm_pdb_inputs () =
  let candidates =
    match Sys.getenv_opt "LLVM_PROJECT_DIR" with
    | Some d -> [ Filename.concat d "llvm/test/DebugInfo/PDB/Inputs" ]
    | None ->
        [
          "/home/tsmc/projects/oxcaml-name-mangling/llvm-project/llvm/test/DebugInfo/PDB/Inputs";
        ]
  in
  List.find_opt Sys.file_exists candidates

(** {1 Process helpers} *)

let run_command cmd =
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 1024 in
  (try
     while true do
       Buffer.add_channel buf ic 4096
     done
   with End_of_file -> ());
  let _ = Unix.close_process_in ic in
  Buffer.contents buf

let write_file path bytes =
  let oc = open_out_bin path in
  output_string oc bytes;
  close_out oc

(** {1 Output normalization}

    [llvm-pdbutil dump] embeds the input file path in some headers, and the
    test produces two temp files. Replace both temp paths with a stable
    placeholder so diffs only reflect content differences. *)
let normalize ~ref_path ~our_path output =
  let q s = Str.quote s in
  output
  |> Str.global_replace (Str.regexp (q ref_path)) "<PDB>"
  |> Str.global_replace (Str.regexp (q our_path)) "<PDB>"

(** {1 Scenario runner} *)

type scenario = {
  name : string;
  (** Short identifier (also used in temp filenames) *)
  yaml : string;  (** YAML file name in LLVM PDB Inputs dir *)
  dump_args : string;  (** [llvm-pdbutil dump] subcommand args *)
  build : unit -> string;  (** Build the equivalent PDB byte string *)
}

let run_scenario s =
  if not (has_llvm_pdbutil ()) then Alcotest.skip ()
  else
    match llvm_pdb_inputs () with
    | None -> Alcotest.skip ()
    | Some inputs ->
        let yaml_path = Filename.concat inputs s.yaml in
        if not (Sys.file_exists yaml_path) then Alcotest.skip ()
        else begin
          let ref_pdb =
            Filename.temp_file ("llvm_equiv_ref_" ^ s.name ^ "_") ".pdb"
          in
          let our_pdb =
            Filename.temp_file ("llvm_equiv_our_" ^ s.name ^ "_") ".pdb"
          in
          (* Generate the reference PDB from the LLVM YAML *)
          let yaml2pdb_cmd =
            Printf.sprintf "llvm-pdbutil yaml2pdb %s --pdb=%s 2>&1"
              (Filename.quote yaml_path) (Filename.quote ref_pdb)
          in
          let yaml2pdb_out = run_command yaml2pdb_cmd in
          if not (Sys.file_exists ref_pdb)
             || (let st = Unix.stat ref_pdb in
                 st.st_size = 0)
          then begin
            Sys.remove our_pdb;
            Alcotest.failf "yaml2pdb produced no output for %s:\n%s" s.yaml
              yaml2pdb_out
          end;
          (* Generate our PDB *)
          write_file our_pdb (s.build ());
          (* Dump both and diff *)
          let dump path =
            run_command
              (Printf.sprintf "llvm-pdbutil dump %s %s 2>&1" s.dump_args
                 (Filename.quote path))
          in
          let ref_dump = normalize ~ref_path:ref_pdb ~our_path:our_pdb (dump ref_pdb) in
          let our_dump = normalize ~ref_path:ref_pdb ~our_path:our_pdb (dump our_pdb) in
          (* Set OCAML_PDB_KEEP_TEMP=1 to leave the temp PDBs on disk for
             inspection with llvm-pdbutil. *)
          (match Sys.getenv_opt "OCAML_PDB_KEEP_TEMP" with
          | None ->
              Sys.remove ref_pdb;
              Sys.remove our_pdb
          | Some _ ->
              Printf.eprintf "kept: ref=%s our=%s\n" ref_pdb our_pdb);
          Alcotest.(check string)
            (Printf.sprintf "%s dump matches LLVM reference" s.name)
            ref_dump our_dump
        end

(** {1 Scenarios} *)

(** Equivalent of [objfilename.yaml]: one DBI module with name + obj path,
    no module stream content. *)
let build_objfilename () =
  let b = Pdb.Pdb_builder.create Pdb.Pdb_builder.AMD64 in
  Pdb.Pdb_builder.add_module b
    {
      name = "C:\\src\\test.obj";
      obj_file = "C:\\src\\test.obj";
      symbols = [];
      subsections = [];
      section_contrib = None;
      source_files = [];
    };
  Pdb.Pdb_builder.finalize b

let objfilename_scenario =
  {
    name = "objfilename";
    yaml = "objfilename.yaml";
    dump_args = "--modules";
    build = build_objfilename;
  }

(** Equivalent of [one-symbol.yaml]: one DBI module containing a single
    S_OBJNAME symbol. Exercises module symbol stream layout. *)
let build_one_symbol () =
  let b = Pdb.Pdb_builder.create Pdb.Pdb_builder.AMD64 in
  Pdb.Pdb_builder.add_module b
    {
      name = "one-symbol.yaml";
      obj_file = "one-symbol.yaml";
      symbols =
        [
          Pdb.Codeview_symbols.ObjName
            {
              signature = Unsigned.UInt32.zero;
              name = "c:\\foo\\one-symbol.yaml";
            };
        ];
      subsections = [];
      section_contrib = None;
      source_files = [];
    };
  Pdb.Pdb_builder.finalize b

let one_symbol_scenario =
  {
    name = "one_symbol";
    yaml = "one-symbol.yaml";
    dump_args = "--modules --symbols";
    build = build_one_symbol;
  }

(** Equivalent of [merge-types-1.yaml]: 8 TPI records spanning LF_POINTER,
    LF_STRUCTURE (forward ref with unique name), LF_ARGLIST, LF_PROCEDURE.
    Type indices are referenced by both basic-type values (e.g. 0x75 for
    [unsigned]) and user-defined indices (>= 0x1000). *)
let build_merge_types () =
  let b = Pdb.Pdb_builder.create Pdb.Pdb_builder.AMD64 in
  let u = Unsigned.UInt32.of_int in
  let t n = Pdb.Type_index.of_u32 (Unsigned.UInt32.of_int n) in
  let ptr_attrs = Pdb.Type_index.near32_pointer_attrs in
  (* 0x1000: uint32_t* *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Pointer
         { pointee_type = Pdb.Type_index.uint32; attrs = ptr_attrs })
  in
  (* 0x1001: int64_t* *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Pointer
         { pointee_type = Pdb.Type_index.int64; attrs = ptr_attrs })
  in
  (* 0x1002: struct OnlyInMerge1 (forward ref + has-unique-name) *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Structure
         {
           field_count = 0;
           properties = Pdb.Codeview_types.parse_type_properties 0x0280;
           field_list = t 0;
           derived_from = t 0;
           vtable_shape = t 0;
           size = 0L;
           name = "OnlyInMerge1";
           unique_name = Some "OnlyInMerge1";
         })
  in
  (* 0x1003: uint32_t** *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Pointer
         { pointee_type = t 0x1000; attrs = ptr_attrs })
  in
  (* 0x1004: uint32_t triple-ptr *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Pointer
         { pointee_type = t 0x1003; attrs = ptr_attrs })
  in
  (* 0x1005: int64_t* (second copy) *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Pointer
         { pointee_type = t 0x1001; attrs = ptr_attrs })
  in
  (* 0x1006: (uint32_t, uint32_t*, uint32_t ptr-ptr) *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.ArgList
         { args =
             [| Pdb.Type_index.uint32; t 0x1000; t 0x1003 |] })
  in
  (* 0x1007: uint32_t (uint32_t, uint32_t*, uint32_t ptr-ptr) *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Procedure
         {
           return_type = Pdb.Type_index.uint32;
           calling_conv = Pdb.Codeview_constants.NearC;
           options = 0;
           param_count = 0;
           arg_list = t 0x1006;
         })
  in
  Pdb.Pdb_builder.finalize b

let merge_types_scenario =
  {
    name = "merge_types";
    yaml = "merge-types-1.yaml";
    dump_args = "--types";
    build = build_merge_types;
  }

(** Decode an even-length hex string into raw bytes. *)
let hex_decode (s : string) : string =
  let nibble c =
    match c with
    | '0' .. '9' -> Char.code c - Char.code '0'
    | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
    | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
    | _ -> invalid_arg "hex_decode"
  in
  let n = String.length s / 2 in
  String.init n (fun i ->
      Char.chr ((nibble s.[2 * i] lsl 4) lor nibble s.[(2 * i) + 1]))

(** Equivalent of [debug-subsections.yaml] (subset matching [-l] and [--il]
    dump output). The fixture defines four modules:
      0. Foo.obj, 1. Bar.obj — exercise CrossModuleExports/Imports
         subsections (not currently implemented in our writer), so we
         emit them as empty modules. Only their DBI entries matter for
         [-l] / [--il] output, which shows just the module header.
      2. empty.obj — FileChecksums + Lines + InlineeLines, the path that
         actually matters for source-level debugging.
      3. ObjFileSubsections — StringTable/Symbols/FrameData subsections
         (also empty for our purposes here).
    Filenames referenced by the line info are registered in /names so
    file_name_offset matches LLVM's. *)
let build_debug_subsections () =
  let b = Pdb.Pdb_builder.create Pdb.Pdb_builder.AMD64 in
  let u = Unsigned.UInt32.of_int in
  let t n = Pdb.Type_index.of_u32 (Unsigned.UInt32.of_int n) in
  let empty_cpp =
    "d:\\src\\llvm\\test\\debuginfo\\pdb\\inputs\\empty.cpp"
  in
  let winerror_h =
    "f:\\dd\\externalapis\\windows\\10\\sdk\\inc\\winerror.h"
  in
  let empty_cpp_off = Pdb.Pdb_builder.add_string b empty_cpp in
  let winerror_h_off = Pdb.Pdb_builder.add_string b winerror_h in
  (* File checksum table entries are aligned to 4 bytes: each MD5 entry
     is 4 (file_name_offset) + 1 (size) + 1 (kind) + 16 (digest) = 22
     bytes, padded to 24. Block file_index / inlinee file_id fields
     point at byte offsets into this table. *)
  let empty_cpp_md5 = hex_decode "A0A5BD0D3ECD93FC29D19DE826FBF4BC" in
  let winerror_h_md5 = hex_decode "1154D69F5B2650196E1FC34F4134E56B" in
  let file_checksums =
    Pdb.Debug_subsections.FileChecksums
      [|
        {
          file_name_offset = u empty_cpp_off;
          checksum_kind = MD5;
          checksum = empty_cpp_md5;
        };
        {
          file_name_offset = u winerror_h_off;
          checksum_kind = MD5;
          checksum = winerror_h_md5;
        };
      |]
  in
  let lines =
    Pdb.Debug_subsections.Lines
      {
        contrib_offset = u 100016;
        contrib_segment = 1;
        flags = 0;
        contrib_size = u 10;
        blocks =
          [|
            {
              file_index = u 0;
              (* empty.cpp checksum entry starts at offset 0 *)
              lines =
                [|
                  {
                    offset = u 0;
                    line_start = 5;
                    delta_line_end = 0;
                    is_statement = true;
                  };
                  {
                    offset = u 3;
                    line_start = 6;
                    delta_line_end = 0;
                    is_statement = true;
                  };
                  {
                    offset = u 8;
                    line_start = 7;
                    delta_line_end = 0;
                    is_statement = true;
                  };
                |];
              columns = None;
            };
          |];
      }
  in
  let inlinee_lines =
    Pdb.Debug_subsections.InlineeLines
      [|
        {
          inlinee = u 22767;
          file_id = u 24;
          (* winerror.h checksum entry at byte offset 24 *)
          source_line = u 26950;
        };
      |]
  in
  (* The LLVM fixture gives Foo/Bar/ObjFileSubsections debug streams
     populated with subsection kinds we don't write (CrossModule*,
     StringTable in subsections, FrameData). Without an allocated module
     stream, llvm-pdbutil's [-l] dump iterator reuses the previously
     opened module's subsections (see SymbolGroup::initializeForPdb in
     InputFile.cpp), so empty.obj's lines would erroneously re-render
     for these placeholder modules. We allocate an empty stream for each
     by giving them a single zero-length subsection of a kind the [-l]
     and [--il] dumpers ignore. 0xF7 is CrossModuleExports — picked
     because it matches the kind LLVM uses for Foo/Bar in this fixture. *)
  let placeholder_subsection =
    Pdb.Debug_subsections.Unknown { kind = 0xF7; data = "" }
  in
  (* Module 0: Foo.obj — empty stream so the dump iterator resets. *)
  Pdb.Pdb_builder.add_module b
    {
      name = "Foo.obj";
      obj_file = "Foo.obj";
      symbols = [];
      subsections = [ placeholder_subsection ];
      section_contrib = None;
      source_files = [];
    };
  (* Module 1: Bar.obj — likewise *)
  Pdb.Pdb_builder.add_module b
    {
      name = "Bar.obj";
      obj_file = "Bar.obj";
      symbols = [];
      subsections = [ placeholder_subsection ];
      section_contrib = None;
      source_files = [];
    };
  (* Module 2: empty.obj with the line info *)
  Pdb.Pdb_builder.add_module b
    {
      name = "d:\\src\\llvm\\test\\DebugInfo\\PDB\\Inputs\\empty.obj";
      obj_file = "d:\\src\\llvm\\test\\DebugInfo\\PDB\\Inputs\\empty.obj";
      symbols = [];
      subsections = [ file_checksums; lines; inlinee_lines ];
      section_contrib = None;
      source_files = [];
    };
  (* Module 3: ObjFileSubsections — likewise empty *)
  Pdb.Pdb_builder.add_module b
    {
      name = "ObjFileSubsections";
      obj_file = "ObjFileSubsections";
      symbols = [];
      subsections = [ placeholder_subsection ];
      section_contrib = None;
      source_files = [];
    };
  Pdb.Pdb_builder.finalize b

let debug_subsections_scenario =
  {
    name = "debug_subsections";
    yaml = "debug-subsections.yaml";
    dump_args = "-l --il";
    build = build_debug_subsections;
  }

(** Equivalent of [source-names-1.yaml]: one DBI module with a single
    source file recorded in the FileInfo substream (no module debug
    stream, no FileChecksums subsection). Exercises the DBI FileInfo
    substream's NumSourceFiles / ModFileCounts / FileNameOffsets /
    NamesBuffer layout. *)
let build_source_names () =
  let b = Pdb.Pdb_builder.create Pdb.Pdb_builder.AMD64 in
  Pdb.Pdb_builder.add_module b
    {
      name = "C:\\src\\test.obj";
      obj_file = "C:\\src\\test.obj";
      symbols = [];
      subsections = [];
      section_contrib = None;
      source_files = [ "C:\\src\\test.c" ];
    };
  Pdb.Pdb_builder.finalize b

let source_names_scenario =
  {
    name = "source_names";
    yaml = "source-names-1.yaml";
    dump_args = "--modules --files";
    build = build_source_names;
  }

(** Equivalent of [source-names-2.yaml]: companion to [source-names-1.yaml]
    with a different source filename (.cc instead of .c). The pair is used
    by LLVM's PDB-merging tests; for us each is an independent validation
    of the DBI FileInfo NamesBuffer. *)
let build_source_names_2 () =
  let b = Pdb.Pdb_builder.create Pdb.Pdb_builder.AMD64 in
  Pdb.Pdb_builder.add_module b
    {
      name = "C:\\src\\test.obj";
      obj_file = "C:\\src\\test.obj";
      symbols = [];
      subsections = [];
      section_contrib = None;
      source_files = [ "C:\\src\\test.cc" ];
    };
  Pdb.Pdb_builder.finalize b

let source_names_2_scenario =
  {
    name = "source_names_2";
    yaml = "source-names-2.yaml";
    dump_args = "--modules --files";
    build = build_source_names_2;
  }

(** Equivalent of [unknown-symbol.yaml]: one DBI module containing a single
    symbol record of kind S_RESERVED1 (0x0001) with 8 bytes of payload.
    llvm-pdbutil prints the unrecognized kind with its size but no field
    interpretation, exercising the [Unknown] symbol fallback path. *)
let build_unknown_symbol () =
  let b = Pdb.Pdb_builder.create Pdb.Pdb_builder.AMD64 in
  Pdb.Pdb_builder.add_module b
    {
      name = "unknown-symbol.yaml";
      obj_file = "unknown-symbol.yaml";
      symbols =
        [
          Pdb.Codeview_symbols.Unknown
            {
              kind = 0x101c;
              (* S_RESERVED1 (S_COMPILE is 0x0001) *)
              data = hex_decode "123456789ABCDEF0";
            };
        ];
      subsections = [];
      section_contrib = None;
      source_files = [];
    };
  Pdb.Pdb_builder.finalize b

let unknown_symbol_scenario =
  {
    name = "unknown_symbol";
    yaml = "unknown-symbol.yaml";
    dump_args = "--modules --symbols";
    build = build_unknown_symbol;
  }

(** Equivalent of [longname-truncation.yaml]: two LF_STRUCTURE records with
    very long names (68k+ chars), exercising LLVM's truncation logic in
    [TypeRecordMapping::mapNameAndUniqueName]. Two cases:

    - Record 0x1000 has both Name and UniqueName. LLVM caps the on-disk
      record at ~4156 bytes: Name = take_front(4064) + MD5_hex(Name);
      UniqueName = "??@" + MD5_hex(UniqueName) + "@".
    - Record 0x1001 has Name only. The simple truncation
      [Name.take_front(maxFieldLength() - 1)] runs and the record fills
      MaxRecordLength = 0xFF00 = 65280 bytes.

    The original 68k-char strings are passed in; the writer truncates
    them on the way out, matching LLVM byte-for-byte. *)
let build_longname_truncation () =
  let b = Pdb.Pdb_builder.create Pdb.Pdb_builder.AMD64 in
  let t n = Pdb.Type_index.of_u32 (Unsigned.UInt32.of_int n) in
  (* YAML inputs:
       Record 0x1000: Name = 68229 'a's, UniqueName = 68228 'b's, Size = 1
       Record 0x1001: Name = 68229 'f's, no UniqueName,           Size = 8 *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Structure
         {
           field_count = 0;
           properties = Pdb.Codeview_types.parse_type_properties 0x0200;
           (* HasUniqueName *)
           field_list = t 0;
           derived_from = t 0;
           vtable_shape = t 0;
           size = 1L;
           name = String.make 68229 'a';
           unique_name = Some (String.make 68228 'b');
         })
  in
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Structure
         {
           field_count = 0;
           properties = Pdb.Codeview_types.parse_type_properties 0x0000;
           field_list = t 0;
           derived_from = t 0;
           vtable_shape = t 0;
           size = 8L;
           name = String.make 68229 'f';
           unique_name = None;
         })
  in
  Pdb.Pdb_builder.finalize b

let longname_truncation_scenario =
  {
    name = "longname_truncation";
    yaml = "longname-truncation.yaml";
    dump_args = "--types";
    build = build_longname_truncation;
  }

(** Equivalent of [merge-types-2.yaml]: companion to merge-types-1 with a
    different ordering of LF_POINTER chains and a struct forward
    reference (OnlyInMerge2). Exercises TPI coverage for the same record
    kinds but at different type indices, validating that our builder
    handles forward references to user-defined indices in any order. *)
let build_merge_types_2 () =
  let b = Pdb.Pdb_builder.create Pdb.Pdb_builder.AMD64 in
  let u = Unsigned.UInt32.of_int in
  let t n = Pdb.Type_index.of_u32 (Unsigned.UInt32.of_int n) in
  let ptr_attrs = Pdb.Type_index.near32_pointer_attrs in
  (* 0x1000: uint32_t* *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Pointer
         { pointee_type = Pdb.Type_index.uint32; attrs = ptr_attrs })
  in
  (* 0x1001: uint32_t ptr-ptr *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Pointer
         { pointee_type = t 0x1000; attrs = ptr_attrs })
  in
  (* 0x1002: uint32_t triple-ptr *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Pointer
         { pointee_type = t 0x1001; attrs = ptr_attrs })
  in
  (* 0x1003: (uint32_t, uint32_t*, uint32_t ptr-ptr) *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.ArgList
         { args =
             [| Pdb.Type_index.uint32; t 0x1000; t 0x1001 |] })
  in
  (* 0x1004: uint32_t (uint32_t, uint32_t*, uint32_t ptr-ptr) *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Procedure
         {
           return_type = Pdb.Type_index.uint32;
           calling_conv = Pdb.Codeview_constants.NearC;
           options = 0;
           param_count = 0;
           arg_list = t 0x1003;
         })
  in
  (* 0x1005: int64_t* *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Pointer
         { pointee_type = Pdb.Type_index.int64; attrs = ptr_attrs })
  in
  (* 0x1006: int64_t ptr-ptr *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Pointer
         { pointee_type = t 0x1005; attrs = ptr_attrs })
  in
  (* 0x1007: struct OnlyInMerge2 (forward ref + has-unique-name) *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Structure
         {
           field_count = 0;
           properties = Pdb.Codeview_types.parse_type_properties 0x0280;
           field_list = t 0;
           derived_from = t 0;
           vtable_shape = t 0;
           size = 0L;
           name = "OnlyInMerge2";
           unique_name = Some "OnlyInMerge2";
         })
  in
  Pdb.Pdb_builder.finalize b

let merge_types_2_scenario =
  {
    name = "merge_types_2";
    yaml = "merge-types-2.yaml";
    dump_args = "--types";
    build = build_merge_types_2;
  }

(** Equivalent of [merge-ids-1.yaml]: 7 IPI records exercising LF_STRING_ID
    (with and without a parent ID) and LF_SUBSTR_LIST. Validates that
    [Pdb_builder.add_id] emits IPI records byte-for-byte equivalent to
    yaml2pdb output. *)
let build_merge_ids () =
  let b = Pdb.Pdb_builder.create Pdb.Pdb_builder.AMD64 in
  let u = Unsigned.UInt32.of_int in
  let t n = Pdb.Type_index.of_u32 (Unsigned.UInt32.of_int n) in
  (* 0x1000: 'One' *)
  let _ =
    Pdb.Pdb_builder.add_id b
      (Pdb.Codeview_types.StringId { id = t 0; str = "One" })
  in
  (* 0x1001: 'Two' *)
  let _ =
    Pdb.Pdb_builder.add_id b
      (Pdb.Codeview_types.StringId { id = t 0; str = "Two" })
  in
  (* 0x1002: 'OnlyInFirst' *)
  let _ =
    Pdb.Pdb_builder.add_id b
      (Pdb.Codeview_types.StringId { id = t 0; str = "OnlyInFirst" })
  in
  (* 0x1003: 'SubOne' *)
  let _ =
    Pdb.Pdb_builder.add_id b
      (Pdb.Codeview_types.StringId { id = t 0; str = "SubOne" })
  in
  (* 0x1004: 'SubTwo' *)
  let _ =
    Pdb.Pdb_builder.add_id b
      (Pdb.Codeview_types.StringId { id = t 0; str = "SubTwo" })
  in
  (* 0x1005: LF_SUBSTR_LIST [0x1003, 0x1004] *)
  let _ =
    Pdb.Pdb_builder.add_id b
      (Pdb.Codeview_types.SubstrList
         { strings = [| t 0x1003; t 0x1004 |] })
  in
  (* 0x1006: 'Main' with parent id = 0x1005 *)
  let _ =
    Pdb.Pdb_builder.add_id b
      (Pdb.Codeview_types.StringId { id = t 0x1005; str = "Main" })
  in
  Pdb.Pdb_builder.finalize b

let merge_ids_scenario =
  {
    name = "merge_ids";
    yaml = "merge-ids-1.yaml";
    dump_args = "--ids";
    build = build_merge_ids;
  }

(** Equivalent of [merge-ids-2.yaml]: companion to merge-ids-1 with a
    different ordering of LF_STRING_ID records and an LF_SUBSTR_LIST
    that references them out of order. *)
let build_merge_ids_2 () =
  let b = Pdb.Pdb_builder.create Pdb.Pdb_builder.AMD64 in
  let u = Unsigned.UInt32.of_int in
  let t n = Pdb.Type_index.of_u32 (Unsigned.UInt32.of_int n) in
  (* 0x1000: 'SubTwo' *)
  let _ =
    Pdb.Pdb_builder.add_id b
      (Pdb.Codeview_types.StringId { id = t 0; str = "SubTwo" })
  in
  (* 0x1001: 'OnlyInSecond' *)
  let _ =
    Pdb.Pdb_builder.add_id b
      (Pdb.Codeview_types.StringId { id = t 0; str = "OnlyInSecond" })
  in
  (* 0x1002: 'SubOne' *)
  let _ =
    Pdb.Pdb_builder.add_id b
      (Pdb.Codeview_types.StringId { id = t 0; str = "SubOne" })
  in
  (* 0x1003: LF_SUBSTR_LIST [0x1002, 0x1000] (SubOne, SubTwo) *)
  let _ =
    Pdb.Pdb_builder.add_id b
      (Pdb.Codeview_types.SubstrList
         { strings = [| t 0x1002; t 0x1000 |] })
  in
  (* 0x1004: 'One' *)
  let _ =
    Pdb.Pdb_builder.add_id b
      (Pdb.Codeview_types.StringId { id = t 0; str = "One" })
  in
  (* 0x1005: 'Main' with parent id = 0x1003 *)
  let _ =
    Pdb.Pdb_builder.add_id b
      (Pdb.Codeview_types.StringId { id = t 0x1003; str = "Main" })
  in
  Pdb.Pdb_builder.finalize b

let merge_ids_2_scenario =
  {
    name = "merge_ids_2";
    yaml = "merge-ids-2.yaml";
    dump_args = "--ids";
    build = build_merge_ids_2;
  }

(** Equivalent of [merge-ids-and-types-1.yaml]: combined TPI + IPI scenario.
    The TPI defines a [FooBar] struct, an [int main(int, char-ptr-ptr)] procedure,
    and a member function. The IPI references them via LF_FUNC_ID,
    LF_MFUNC_ID, and LF_UDT_MOD_SRC_LINE — exercising cross-stream type
    references. Also exercises the new FunctionOptions field on
    LF_MFUNCTION (Constructor = 0x02). *)
let build_merge_ids_and_types () =
  let b = Pdb.Pdb_builder.create Pdb.Pdb_builder.AMD64 in
  let u = Unsigned.UInt32.of_int in
  let t n = Pdb.Type_index.of_u32 (Unsigned.UInt32.of_int n) in
  let ptr_attrs = Pdb.Type_index.near32_pointer_attrs in
  (* TPI 0x1000: char** *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Pointer
         { pointee_type = Pdb.Type_index.char_ptr32; attrs = ptr_attrs })
  in
  (* TPI 0x1001: field list with one LF_MEMBER (public void *FooMember) *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.FieldList
         {
           members =
             [
               Pdb.Codeview_types.Member
                 {
                   attrs = 3;
                   (* public *)
                   field_type = Pdb.Type_index.void_ptr32;
                   offset = 0L;
                   name = "FooMember";
                 };
             ];
         })
  in
  (* TPI 0x1002: (int, char ptr-ptr) *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.ArgList
         { args = [| Pdb.Type_index.int32; t 0x1000 |] })
  in
  (* TPI 0x1003: struct FooBar (HasUniqueName) *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Structure
         {
           field_count = 1;
           properties = Pdb.Codeview_types.parse_type_properties 0x0200;
           field_list = t 0x1001;
           derived_from = t 0;
           vtable_shape = t 0;
           size = 4L;
           name = "FooBar";
           unique_name = Some "FooBar";
         })
  in
  (* TPI 0x1004: FooBar* *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Pointer
         { pointee_type = t 0x1003; attrs = ptr_attrs })
  in
  (* TPI 0x1005: (int) *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.ArgList { args = [| Pdb.Type_index.int32 |] })
  in
  (* TPI 0x1006: LF_MFUNCTION void(int) on FooBar, ThisCall + Constructor *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.MFunction
         {
           return_type = Pdb.Type_index.void;
           class_type = t 0x1003;
           this_type = t 0x1004;
           calling_conv = Pdb.Codeview_constants.ThisCall;
           options = 0x02;
           (* Constructor *)
           param_count = 1;
           arg_list = t 0x1005;
           this_adjust = 0l;
         })
  in
  (* TPI 0x1007: LF_PROCEDURE int(int, char ptr-ptr) *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Procedure
         {
           return_type = Pdb.Type_index.int32;
           calling_conv = Pdb.Codeview_constants.NearC;
           options = 0;
           param_count = 2;
           arg_list = t 0x1002;
         })
  in
  (* IPI 0x1000: LF_FUNC_ID 'main' referencing TPI 0x1007 *)
  let _ =
    Pdb.Pdb_builder.add_id b
      (Pdb.Codeview_types.FuncId
         { scope_id = t 0; func_type = t 0x1007; name = "main" })
  in
  (* IPI 0x1001: LF_MFUNC_ID 'FooMethod' on TPI 0x1003 -> TPI 0x1006 *)
  let _ =
    Pdb.Pdb_builder.add_id b
      (Pdb.Codeview_types.MFuncId
         { parent_type = t 0x1003; func_type = t 0x1006; name = "FooMethod" })
  in
  (* IPI 0x1002: LF_UDT_MOD_SRC_LINE referencing TPI 0x1003 *)
  let _ =
    Pdb.Pdb_builder.add_id b
      (Pdb.Codeview_types.UdtModSrcLine
         { udt = t 0x1003; source = t 0; line = u 0; module_ = 0 })
  in
  Pdb.Pdb_builder.finalize b

let merge_ids_and_types_scenario =
  {
    name = "merge_ids_and_types";
    yaml = "merge-ids-and-types-1.yaml";
    dump_args = "--types --ids";
    build = build_merge_ids_and_types;
  }

(** Equivalent of [merge-ids-and-types-2.yaml]: the companion file to
    merge-ids-and-types-1, designed by LLVM to exercise PDB type merging.
    It shares some records with file 1 (so they'd merge) and introduces
    new variants (main2, foo, FooMethod2). For us each is an independent
    end-to-end validation of TPI + IPI writes. *)
let build_merge_ids_and_types_2 () =
  let b = Pdb.Pdb_builder.create Pdb.Pdb_builder.AMD64 in
  let u = Unsigned.UInt32.of_int in
  let t n = Pdb.Type_index.of_u32 (Unsigned.UInt32.of_int n) in
  let ptr_attrs = Pdb.Type_index.near32_pointer_attrs in
  (* TPI 0x1000: (int) *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.ArgList { args = [| Pdb.Type_index.int32 |] })
  in
  (* TPI 0x1001: field list with public void *FooMember *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.FieldList
         {
           members =
             [
               Pdb.Codeview_types.Member
                 {
                   attrs = 3;
                   field_type = Pdb.Type_index.void_ptr32;
                   offset = 0L;
                   name = "FooMember";
                 };
             ];
         })
  in
  (* TPI 0x1002: char ptr-ptr *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Pointer
         { pointee_type = Pdb.Type_index.char_ptr32; attrs = ptr_attrs })
  in
  (* TPI 0x1003: (int, char ptr-ptr) *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.ArgList
         { args = [| Pdb.Type_index.int32; t 0x1002 |] })
  in
  (* TPI 0x1004: struct FooBar (HasUniqueName) *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Structure
         {
           field_count = 1;
           properties = Pdb.Codeview_types.parse_type_properties 0x0200;
           field_list = t 0x1001;
           derived_from = t 0;
           vtable_shape = t 0;
           size = 4L;
           name = "FooBar";
           unique_name = Some "FooBar";
         })
  in
  (* TPI 0x1005: void (int, char ptr-ptr) *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Procedure
         {
           return_type = Pdb.Type_index.void;
           calling_conv = Pdb.Codeview_constants.NearC;
           options = 0;
           param_count = 2;
           arg_list = t 0x1003;
         })
  in
  (* TPI 0x1006: FooBar* *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Pointer
         { pointee_type = t 0x1004; attrs = ptr_attrs })
  in
  (* TPI 0x1007: int (int, char ptr-ptr) *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Procedure
         {
           return_type = Pdb.Type_index.int32;
           calling_conv = Pdb.Codeview_constants.NearC;
           options = 0;
           param_count = 2;
           arg_list = t 0x1003;
         })
  in
  (* TPI 0x1008: LF_MFUNCTION void(int) on FooBar, ThisCall + Constructor *)
  let _ =
    Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.MFunction
         {
           return_type = Pdb.Type_index.void;
           class_type = t 0x1004;
           this_type = t 0x1006;
           calling_conv = Pdb.Codeview_constants.ThisCall;
           options = 0x02;
           param_count = 1;
           arg_list = t 0x1000;
           this_adjust = 0l;
         })
  in
  (* IPI 0x1000: LF_UDT_MOD_SRC_LINE for FooBar *)
  let _ =
    Pdb.Pdb_builder.add_id b
      (Pdb.Codeview_types.UdtModSrcLine
         { udt = t 0x1004; source = t 0; line = u 0; module_ = 0 })
  in
  (* IPI 0x1001: LF_FUNC_ID 'main2' -> TPI 0x1007 *)
  let _ =
    Pdb.Pdb_builder.add_id b
      (Pdb.Codeview_types.FuncId
         { scope_id = t 0; func_type = t 0x1007; name = "main2" })
  in
  (* IPI 0x1002: LF_FUNC_ID 'foo' -> TPI 0x1005 *)
  let _ =
    Pdb.Pdb_builder.add_id b
      (Pdb.Codeview_types.FuncId
         { scope_id = t 0; func_type = t 0x1005; name = "foo" })
  in
  (* IPI 0x1003: LF_MFUNC_ID 'FooMethod2' on TPI 0x1004 -> TPI 0x1008 *)
  let _ =
    Pdb.Pdb_builder.add_id b
      (Pdb.Codeview_types.MFuncId
         { parent_type = t 0x1004; func_type = t 0x1008; name = "FooMethod2" })
  in
  (* IPI 0x1004: LF_FUNC_ID 'main' -> TPI 0x1007 *)
  let _ =
    Pdb.Pdb_builder.add_id b
      (Pdb.Codeview_types.FuncId
         { scope_id = t 0; func_type = t 0x1007; name = "main" })
  in
  Pdb.Pdb_builder.finalize b

let merge_ids_and_types_2_scenario =
  {
    name = "merge_ids_and_types_2";
    yaml = "merge-ids-and-types-2.yaml";
    dump_args = "--types --ids";
    build = build_merge_ids_and_types_2;
  }

(** {1 Suite} *)

let test_of_scenario s =
  Alcotest.test_case s.name `Quick (fun () -> run_scenario s)

let () =
  Alcotest.run "LLVM Equivalence"
    [
      ( "scenarios",
        [
          test_of_scenario objfilename_scenario;
          test_of_scenario one_symbol_scenario;
          test_of_scenario merge_types_scenario;
          test_of_scenario debug_subsections_scenario;
          test_of_scenario source_names_scenario;
          test_of_scenario source_names_2_scenario;
          test_of_scenario unknown_symbol_scenario;
          test_of_scenario longname_truncation_scenario;
          test_of_scenario merge_types_2_scenario;
          test_of_scenario merge_ids_scenario;
          test_of_scenario merge_ids_2_scenario;
          test_of_scenario merge_ids_and_types_scenario;
          test_of_scenario merge_ids_and_types_2_scenario;
        ] );
    ]
