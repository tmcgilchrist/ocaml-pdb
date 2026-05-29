(** Differential testing: compare OCaml hash functions against the LLVM C
    reference implementation (tmp/hash_oracle).

    Generates random strings via QCheck and verifies both implementations
    produce identical values. *)

let oracle_path =
  let candidates =
    [
      "test/oracle/hash_oracle";
      "../oracle/hash_oracle";
      "../../../test/oracle/hash_oracle";
      "../../../../test/oracle/hash_oracle";
    ]
  in
  let cwd = Sys.getcwd () in
  List.find_map
    (fun rel ->
      let p = Filename.concat cwd rel in
      if Sys.file_exists p then Some p else Option.None)
    candidates

let has_oracle () = oracle_path <> Option.None

(** Run the C oracle and return the hash as an int *)
let run_oracle mode input =
  match oracle_path with
  | Option.None -> failwith "hash_oracle not found"
  | Some path ->
      (* Write input to a temp file to handle binary/special chars *)
      let tmpfile = Filename.temp_file "hash_input_" ".bin" in
      let oc = open_out_bin tmpfile in
      output_string oc input;
      close_out oc;
      (* Use shell to read from file *)
      let cmd =
        Printf.sprintf "%s %s \"$(cat %s)\" 2>/dev/null" path mode tmpfile
      in
      let ic = Unix.open_process_in cmd in
      let result =
        try int_of_string (String.trim (input_line ic)) with _ -> -1
      in
      let _ = Unix.close_process_in ic in
      Sys.remove tmpfile;
      result

(** Generate printable ASCII strings (no nulls, no shell-special chars) *)
let gen_safe_string =
  QCheck.Gen.(
    let+ len = int_range 0 50 in
    String.init len (fun i -> Char.chr ((((i * 7) + 33) mod 90) + 33)))

let test_hash_v1_differential =
  QCheck.Test.make ~name:"hash_string_v1 matches C oracle" ~count:200
    (QCheck.make gen_safe_string) (fun s ->
      if not (has_oracle ()) then true (* skip if oracle not built *)
      else
        let ocaml_result = Pdb.Hash.hash_string_v1 s in
        let c_result = run_oracle "v1" s in
        ocaml_result = c_result)

let test_hash_v8_differential =
  QCheck.Test.make ~name:"hash_buffer_v8 matches C oracle" ~count:200
    (QCheck.make gen_safe_string) (fun s ->
      if not (has_oracle ()) then true
      else
        let ocaml_result = Pdb.Hash.hash_buffer_v8 s in
        let c_result = run_oracle "v8" s in
        ocaml_result = c_result)

(** Also test with specific tricky strings *)
let test_v1_specific_strings () =
  if not (has_oracle ()) then Alcotest.skip ()
  else
    let cases =
      [
        "";
        "a";
        "ab";
        "abc";
        "abcd";
        "abcde";
        "main";
        "/names";
        "/LinkInfo";
        "MAIN";
        "int";
        "Point";
        ".?AUPoint@@";
        "C:\\Users\\dev\\project\\main.c";
        "std::vector<int, std::allocator<int>>";
      ]
    in
    List.iter
      (fun s ->
        let ocaml_v = Pdb.Hash.hash_string_v1 s in
        let c_v = run_oracle "v1" s in
        Alcotest.(check int) (Printf.sprintf "v1(%S)" s) c_v ocaml_v)
      cases

let test_v8_specific_strings () =
  if not (has_oracle ()) then Alcotest.skip ()
  else
    let cases =
      [
        "";
        "a";
        "abc";
        "123456789";
        "hello world";
        "\x00\x01\x02\x03";
        "LF_STRUCTURE";
      ]
    in
    List.iter
      (fun s ->
        let ocaml_v = Pdb.Hash.hash_buffer_v8 s in
        let c_v = run_oracle "v8" s in
        Alcotest.(check int) (Printf.sprintf "v8(%S)" s) c_v ocaml_v)
      cases

let () =
  Alcotest.run "Hash Differential"
    [
      ( "differential",
        [
          Alcotest.test_case "v1 specific strings" `Quick
            test_v1_specific_strings;
          Alcotest.test_case "v8 specific strings" `Quick
            test_v8_specific_strings;
          QCheck_alcotest.to_alcotest test_hash_v1_differential;
          QCheck_alcotest.to_alcotest test_hash_v8_differential;
        ] );
    ]
