(** Tests for the high-level PDB builder. *)

module Buffer = Stdlib.Buffer

let buffer_of_string s =
  let len = String.length s in
  let buf =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout len
  in
  for i = 0 to len - 1 do
    buf.{i} <- Char.code s.[i]
  done;
  buf

let u32 n = Unsigned.UInt32.of_int n

let has_llvm_pdbutil () =
  try
    let ic = Unix.open_process_in "llvm-pdbutil --version 2>/dev/null" in
    let _ = input_line ic in
    Unix.close_process_in ic = Unix.WEXITED 0
  with _ -> false

let run_command cmd =
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_char buf (input_char ic)
     done
   with End_of_file -> ());
  let _ = Unix.close_process_in ic in
  Buffer.contents buf

let test_minimal_pdb () =
  (* Build the simplest possible PDB *)
  let b = Pdb.Pdb_builder.create Pdb.Pdb_builder.AMD64 in
  let pdb_bytes = Pdb.Pdb_builder.finalize b in
  (* Read it back *)
  let buf = buffer_of_string pdb_bytes in
  let msf = Pdb.Msf.read buf in
  Alcotest.(check bool) "has streams" true (Pdb.Msf.stream_count msf >= 5);
  (* Check PDB info *)
  let s1 = Pdb.Msf.get_stream_exn msf 1 in
  let info = Pdb.Pdb_stream.read (Object.Buffer.cursor s1) in
  Alcotest.(check int) "age" 1 (Unsigned.UInt32.to_int info.age);
  Alcotest.(check bool) "has ContainsIdStream" true
    (List.mem Pdb.Pdb_stream.ContainsIdStream info.features);
  (* Check TPI has 0 records *)
  let s2 = Pdb.Msf.get_stream_exn msf 2 in
  let tpi_h = Pdb.Tpi.parse_header (Object.Buffer.cursor s2) in
  Alcotest.(check int) "0 TPI records" 0 (Pdb.Tpi.num_type_records tpi_h)

let test_pdb_with_types () =
  let b = Pdb.Pdb_builder.create Pdb.Pdb_builder.AMD64 in
  (* Add some types *)
  let _arglist = Pdb.Pdb_builder.add_type b
    (Pdb.Codeview_types.ArgList { args = [||] }) in
  let _proc = Pdb.Pdb_builder.add_type b
    (Pdb.Codeview_types.Procedure
       { return_type = u32 0x0074; calling_conv = Pdb.Codeview_constants.NearC;
         param_count = 0; arg_list = u32 0x1000 }) in
  (* Add an IPI record *)
  let _func_id = Pdb.Pdb_builder.add_id b
    (Pdb.Codeview_types.FuncId
       { scope_id = u32 0; func_type = u32 0x1001; name = "main" }) in
  let pdb_bytes = Pdb.Pdb_builder.finalize b in
  let buf = buffer_of_string pdb_bytes in
  let msf = Pdb.Msf.read buf in
  (* Check TPI *)
  let s2 = Pdb.Msf.get_stream_exn msf 2 in
  let cur2 = Object.Buffer.cursor s2 in
  let tpi_h = Pdb.Tpi.parse_header cur2 in
  Alcotest.(check int) "2 TPI records" 2 (Pdb.Tpi.num_type_records tpi_h);
  let tpi_records = List.of_seq (Pdb.Tpi.parse_type_records cur2 tpi_h) in
  (match List.nth tpi_records 0 with
  | Pdb.Codeview_types.ArgList { args } ->
      Alcotest.(check int) "arglist" 0 (Array.length args)
  | _ -> Alcotest.fail "expected ArgList");
  (* Check IPI *)
  let s4 = Pdb.Msf.get_stream_exn msf 4 in
  let cur4 = Object.Buffer.cursor s4 in
  let ipi_h = Pdb.Tpi.parse_header cur4 in
  Alcotest.(check int) "1 IPI record" 1 (Pdb.Tpi.num_type_records ipi_h);
  let ipi_records = List.of_seq (Pdb.Tpi.parse_type_records cur4 ipi_h) in
  match List.nth ipi_records 0 with
  | Pdb.Codeview_types.FuncId { name; _ } ->
      Alcotest.(check string) "func_id name" "main" name
  | _ -> Alcotest.fail "expected FuncId"

let test_pdb_with_module () =
  let b = Pdb.Pdb_builder.create Pdb.Pdb_builder.AMD64 in
  let _proc_ti = Pdb.Pdb_builder.add_type b
    (Pdb.Codeview_types.Procedure
       { return_type = u32 0x0074; calling_conv = Pdb.Codeview_constants.NearC;
         param_count = 0; arg_list = u32 0 }) in
  Pdb.Pdb_builder.add_module b
    {
      name = "test.obj";
      obj_file = "test.obj";
      symbols =
        [
          Pdb.Codeview_symbols.ObjName
            { signature = u32 0; name = "test.obj" };
          Pdb.Codeview_symbols.Compile3
            {
              flags = u32 0;
              machine = 0x8664;
              frontend_version = (1, 0, 0, 0);
              backend_version = (1, 0, 0, 0);
              version_string = "ocaml-pdb";
            };
        ];
      subsections = [];
      section_contrib = Option.None;
    };
  let pdb_bytes = Pdb.Pdb_builder.finalize b in
  let buf = buffer_of_string pdb_bytes in
  let msf = Pdb.Msf.read buf in
  (* Check DBI *)
  let s3 = Pdb.Msf.get_stream_exn msf 3 in
  let dbi = Pdb.Dbi.parse (Object.Buffer.cursor s3) in
  Alcotest.(check int) "1 module" 1 (Array.length dbi.modules);
  Alcotest.(check string) "module name" "test.obj"
    dbi.modules.(0).module_name;
  Alcotest.(check int) "machine" 0x8664 dbi.header.machine;
  (* Read module symbols *)
  let syms = List.of_seq (Pdb.Dbi.module_symbols msf dbi.modules.(0)) in
  Alcotest.(check int) "2 module symbols" 2 (List.length syms);
  (match List.nth syms 0 with
  | Pdb.Codeview_symbols.ObjName { name; _ } ->
      Alcotest.(check string) "objname" "test.obj" name
  | _ -> Alcotest.fail "expected ObjName");
  match List.nth syms 1 with
  | Pdb.Codeview_symbols.Compile3 { version_string; _ } ->
      Alcotest.(check string) "version" "ocaml-pdb" version_string
  | _ -> Alcotest.fail "expected Compile3"

let test_pdb_with_publics () =
  let b = Pdb.Pdb_builder.create Pdb.Pdb_builder.AMD64 in
  Pdb.Pdb_builder.add_public b
    (Pdb.Codeview_symbols.Pub32
       { flags = u32 2; offset = u32 0x1000; segment = 1; name = "_main" });
  Pdb.Pdb_builder.add_global b
    (Pdb.Codeview_symbols.GData32
       { type_index = u32 0x0074; offset = u32 0x2000; segment = 2;
         name = "g_var" });
  let pdb_bytes = Pdb.Pdb_builder.finalize b in
  let buf = buffer_of_string pdb_bytes in
  let msf = Pdb.Msf.read buf in
  let s3 = Pdb.Msf.get_stream_exn msf 3 in
  let dbi = Pdb.Dbi.parse (Object.Buffer.cursor s3) in
  (* Verify stream indices are set (not 0xFFFF) *)
  Alcotest.(check bool) "global stream set" true
    (dbi.header.global_stream_index <> 0xFFFF);
  Alcotest.(check bool) "public stream set" true
    (dbi.header.public_stream_index <> 0xFFFF);
  Alcotest.(check bool) "sym record stream set" true
    (dbi.header.sym_record_stream <> 0xFFFF)

let test_pdb_with_strings () =
  let b = Pdb.Pdb_builder.create Pdb.Pdb_builder.AMD64 in
  let off = Pdb.Pdb_builder.add_string b "test.c" in
  Alcotest.(check bool) "offset > 0" true (off > 0);
  let pdb_bytes = Pdb.Pdb_builder.finalize b in
  let buf = buffer_of_string pdb_bytes in
  let msf = Pdb.Msf.read buf in
  (* Find /names stream via PDB info *)
  let s1 = Pdb.Msf.get_stream_exn msf 1 in
  let info = Pdb.Pdb_stream.read (Object.Buffer.cursor s1) in
  let names_idx =
    List.assoc "/names" info.named_streams
  in
  let names_stream = Pdb.Msf.get_stream_exn msf names_idx in
  let st =
    Pdb.Pdb_string_table.parse (Object.Buffer.cursor names_stream)
  in
  Alcotest.(check (option int)) "lookup test.c" (Some off)
    (Pdb.Pdb_string_table.lookup st "test.c")

let test_llvm_pdbutil_validates () =
  if not (has_llvm_pdbutil ()) then Alcotest.skip ()
  else begin
    let b = Pdb.Pdb_builder.create Pdb.Pdb_builder.AMD64 in
    let _ = Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.ArgList { args = [||] }) in
    let _ = Pdb.Pdb_builder.add_type b
      (Pdb.Codeview_types.Procedure
         { return_type = u32 0x0074;
           calling_conv = Pdb.Codeview_constants.NearC;
           param_count = 0; arg_list = u32 0x1000 }) in
    Pdb.Pdb_builder.add_module b
      {
        name = "main.obj";
        obj_file = "main.obj";
        symbols =
          [
            Pdb.Codeview_symbols.ObjName
              { signature = u32 0; name = "main.obj" };
          ];
        subsections = [];
        section_contrib = Option.None;
      };
    Pdb.Pdb_builder.add_public b
      (Pdb.Codeview_symbols.Pub32
         { flags = u32 2; offset = u32 0; segment = 1; name = "_main" });
    let _ = Pdb.Pdb_builder.add_string b "main.c" in
    let pdb_bytes = Pdb.Pdb_builder.finalize b in
    let tmpfile = Filename.temp_file "pdb_builder_" ".pdb" in
    let oc = open_out_bin tmpfile in
    output_string oc pdb_bytes;
    close_out oc;
    let output =
      run_command
        (Printf.sprintf "llvm-pdbutil dump --summary --types %s 2>&1" tmpfile)
    in
    Sys.remove tmpfile;
    Alcotest.(check bool) "has Block Size" true
      (try
         ignore (Str.search_forward (Str.regexp "Block Size") output 0);
         true
       with Not_found -> false);
    Alcotest.(check bool) "has types" true
      (try
         ignore (Str.search_forward (Str.regexp "Has Types: true") output 0);
         true
       with Not_found -> false);
    Alcotest.(check bool) "has LF_PROCEDURE" true
      (try
         ignore (Str.search_forward (Str.regexp "LF_PROCEDURE") output 0);
         true
       with Not_found -> false)
  end

let () =
  Alcotest.run "PDB Builder"
    [
      ( "builder",
        [
          Alcotest.test_case "minimal" `Quick test_minimal_pdb;
          Alcotest.test_case "with types" `Quick test_pdb_with_types;
          Alcotest.test_case "with module" `Quick test_pdb_with_module;
          Alcotest.test_case "with publics" `Quick test_pdb_with_publics;
          Alcotest.test_case "with strings" `Quick test_pdb_with_strings;
          Alcotest.test_case "llvm-pdbutil validates" `Quick
            test_llvm_pdbutil_validates;
        ] );
    ]
