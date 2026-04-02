(** Cross-validation: write a PDB with ocaml-pdb, validate with llvm-pdbutil.

    This test constructs a PDB file programmatically, writes it to a temp file,
    runs llvm-pdbutil dump on it, and checks the output contains expected
    strings. This validates write-path compatibility with the LLVM reference. *)

module Buffer = Stdlib.Buffer

let u32 n = Unsigned.UInt32.of_int n

(** Check if llvm-pdbutil is available *)
let has_llvm_pdbutil () =
  try
    let ic = Unix.open_process_in "llvm-pdbutil --version 2>/dev/null" in
    let _ = input_line ic in
    let status = Unix.close_process_in ic in
    status = Unix.WEXITED 0
  with _ -> false

(** Run a command and return stdout *)
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

(** Build a minimal but valid PDB file *)
let build_test_pdb () : string =
  let msf = Pdb.Msf_write.create ~block_size:4096 in
  (* Stream 0: Old directory (empty) *)
  let _s0 = Pdb.Msf_write.add_empty_stream msf in
  (* Stream 1: PDB Info Stream *)
  let info_buf = Buffer.create 256 in
  Pdb.Pdb_stream_write.write info_buf
    {
      version = VC70;
      signature = u32 0;
      age = u32 1;
      guid =
        {
          data1 = u32 0x11223344;
          data2 = Unsigned.UInt16.of_int 0x5566;
          data3 = Unsigned.UInt16.of_int 0x7788;
          data4 = "\x99\xAA\xBB\xCC\xDD\xEE\xFF\x00";
        };
      named_streams = [];
      features = [ ContainsIdStream ];
    };
  let _s1 = Pdb.Msf_write.add_stream msf (Buffer.contents info_buf) in
  (* Stream 2: TPI Stream *)
  let tpi_buf = Buffer.create 256 in
  Pdb.Tpi_write.write tpi_buf
    [
      (* 0x1000: empty arglist *)
      Pdb.Codeview_types.ArgList { args = [||] };
      (* 0x1001: int main(void) *)
      Pdb.Codeview_types.Procedure
        {
          return_type = u32 0x0074;
          calling_conv = Pdb.Codeview_constants.NearC;
          param_count = 0;
          arg_list = u32 0x1000;
        };
      (* 0x1002: struct Point fwd ref *)
      Pdb.Codeview_types.Structure
        {
          field_count = 0;
          properties = Pdb.Codeview_types.parse_type_properties 0x0280;
          field_list = u32 0;
          derived_from = u32 0;
          vtable_shape = u32 0;
          size = 0L;
          name = "Point";
          unique_name = Some ".?AUPoint@@";
        };
    ];
  let _s2 = Pdb.Msf_write.add_stream msf (Buffer.contents tpi_buf) in
  (* Stream 3: DBI Stream *)
  let dbi_buf = Buffer.create 256 in
  Pdb.Dbi_write.write dbi_buf [] [] ~machine:0x8664;
  let _s3 = Pdb.Msf_write.add_stream msf (Buffer.contents dbi_buf) in
  (* Stream 4: IPI Stream (empty) *)
  let ipi_buf = Buffer.create 128 in
  Pdb.Tpi_write.write ipi_buf [];
  let _s4 = Pdb.Msf_write.add_stream msf (Buffer.contents ipi_buf) in
  Pdb.Msf_write.finalize msf

let test_llvm_pdbutil_summary () =
  if not (has_llvm_pdbutil ()) then
    Alcotest.skip ()
  else begin
    let pdb_bytes = build_test_pdb () in
    let tmpfile = Filename.temp_file "ocaml_pdb_test_" ".pdb" in
    let oc = open_out_bin tmpfile in
    output_string oc pdb_bytes;
    close_out oc;
    let output =
      run_command
        (Printf.sprintf "llvm-pdbutil dump --summary %s 2>&1" tmpfile)
    in
    Sys.remove tmpfile;
    (* Verify llvm-pdbutil can read the file and reports key fields *)
    Alcotest.(check bool) "contains Block Size" true
      (String.length output > 0
       && (try
             ignore (Str.search_forward (Str.regexp "Block Size") output 0);
             true
           with Not_found -> false));
    Alcotest.(check bool) "contains Number of streams" true
      (try
         ignore
           (Str.search_forward (Str.regexp "Number of streams") output 0);
         true
       with Not_found -> false);
    Alcotest.(check bool) "contains Age: 1" true
      (try
         ignore (Str.search_forward (Str.regexp "Age: 1") output 0);
         true
       with Not_found -> false);
    Alcotest.(check bool) "contains Has Types: true" true
      (try
         ignore (Str.search_forward (Str.regexp "Has Types: true") output 0);
         true
       with Not_found -> false)
  end

let test_llvm_pdbutil_types () =
  if not (has_llvm_pdbutil ()) then
    Alcotest.skip ()
  else begin
    let pdb_bytes = build_test_pdb () in
    let tmpfile = Filename.temp_file "ocaml_pdb_test_" ".pdb" in
    let oc = open_out_bin tmpfile in
    output_string oc pdb_bytes;
    close_out oc;
    let output =
      run_command
        (Printf.sprintf "llvm-pdbutil dump --types %s 2>&1" tmpfile)
    in
    Sys.remove tmpfile;
    (* Verify type records are readable *)
    Alcotest.(check bool) "contains LF_ARGLIST" true
      (try
         ignore (Str.search_forward (Str.regexp "LF_ARGLIST") output 0);
         true
       with Not_found -> false);
    Alcotest.(check bool) "contains LF_PROCEDURE" true
      (try
         ignore (Str.search_forward (Str.regexp "LF_PROCEDURE") output 0);
         true
       with Not_found -> false);
    Alcotest.(check bool) "contains Point" true
      (try
         ignore (Str.search_forward (Str.regexp "Point") output 0);
         true
       with Not_found -> false);
    Alcotest.(check bool) "contains 3 records" true
      (try
         ignore
           (Str.search_forward (Str.regexp "Showing 3 records") output 0);
         true
       with Not_found -> false)
  end

let test_llvm_pdbutil_roundtrip_read () =
  if not (has_llvm_pdbutil ()) then
    Alcotest.skip ()
  else begin
    (* Write PDB, read back with our library, verify consistency *)
    let pdb_bytes = build_test_pdb () in
    let tmpfile = Filename.temp_file "ocaml_pdb_test_" ".pdb" in
    let oc = open_out_bin tmpfile in
    output_string oc pdb_bytes;
    close_out oc;
    (* Read it back with our parser *)
    let buf = Object.Buffer.parse tmpfile in
    let msf = Pdb.Msf.read buf in
    Sys.remove tmpfile;
    (* Verify our reader can parse what our writer produced *)
    Alcotest.(check bool) "has 5+ streams" true
      (Pdb.Msf.stream_count msf >= 5);
    (* Check PDB info stream *)
    let stream1 = Pdb.Msf.get_stream_exn msf 1 in
    let cur1 = Object.Buffer.cursor stream1 in
    let info = Pdb.Pdb_stream.read cur1 in
    Alcotest.(check int) "age" 1 (Unsigned.UInt32.to_int info.age);
    (* Check TPI *)
    let stream2 = Pdb.Msf.get_stream_exn msf 2 in
    let cur2 = Object.Buffer.cursor stream2 in
    let header = Pdb.Tpi.parse_header cur2 in
    Alcotest.(check int) "3 type records" 3 (Pdb.Tpi.num_type_records header);
    let records = List.of_seq (Pdb.Tpi.parse_type_records cur2 header) in
    (match List.nth records 2 with
    | Pdb.Codeview_types.Structure { name; _ } ->
        Alcotest.(check string) "struct name" "Point" name
    | _ -> Alcotest.fail "expected Structure")
  end

let () =
  Alcotest.run "Cross Validation"
    [
      ( "llvm-pdbutil",
        [
          Alcotest.test_case "summary" `Quick test_llvm_pdbutil_summary;
          Alcotest.test_case "types" `Quick test_llvm_pdbutil_types;
          Alcotest.test_case "roundtrip read" `Quick
            test_llvm_pdbutil_roundtrip_read;
        ] );
    ]
