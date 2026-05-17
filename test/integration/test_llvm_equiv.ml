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
    };
  Pdb.Pdb_builder.finalize b

let objfilename_scenario =
  {
    name = "objfilename";
    yaml = "objfilename.yaml";
    dump_args = "--modules";
    build = build_objfilename;
  }

(** {1 Suite} *)

let test_of_scenario s =
  Alcotest.test_case s.name `Quick (fun () -> run_scenario s)

let () =
  Alcotest.run "LLVM Equivalence"
    [ ("scenarios", [ test_of_scenario objfilename_scenario ]) ]
