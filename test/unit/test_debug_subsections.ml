(** Tests for C13 debug subsections. *)

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

let test_lines_roundtrip () =
  let lines : Pdb.Debug_subsections.lines_subsection =
    {
      contrib_offset = u32 0;
      contrib_segment = 1;
      flags = 0;
      contrib_size = u32 100;
      blocks =
        [|
          {
            file_index = u32 0;
            lines =
              [|
                {
                  offset = u32 0;
                  line_start = 10;
                  delta_line_end = 0;
                  is_statement = true;
                };
                {
                  offset = u32 5;
                  line_start = 11;
                  delta_line_end = 0;
                  is_statement = true;
                };
                {
                  offset = u32 20;
                  line_start = 12;
                  delta_line_end = 0;
                  is_statement = true;
                };
              |];
            columns = Option.None;
          };
        |];
    }
  in
  let buf = Buffer.create 128 in
  Pdb.Debug_subsections.write_subsection buf (Lines lines);
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let total = String.length bytes in
  let subs = Pdb.Debug_subsections.parse_subsections cur total in
  let sub_list = List.of_seq subs in
  Alcotest.(check int) "one subsection" 1 (List.length sub_list);
  match List.hd sub_list with
  | Pdb.Debug_subsections.Lines ls ->
      Alcotest.(check int) "segment" 1 ls.contrib_segment;
      Alcotest.(check int)
        "contrib_size" 100
        (Unsigned.UInt32.to_int ls.contrib_size);
      Alcotest.(check int) "one block" 1 (Array.length ls.blocks);
      let block = ls.blocks.(0) in
      Alcotest.(check int) "three lines" 3 (Array.length block.lines);
      Alcotest.(check int) "line 0 start" 10 block.lines.(0).line_start;
      Alcotest.(check int) "line 1 start" 11 block.lines.(1).line_start;
      Alcotest.(check int) "line 2 start" 12 block.lines.(2).line_start;
      Alcotest.(check bool) "is_statement" true block.lines.(0).is_statement
  | _ -> Alcotest.fail "expected Lines subsection"

let test_file_checksums_roundtrip () =
  let checksums =
    [|
      {
        Pdb.Debug_subsections.file_name_offset = u32 0;
        checksum_kind = MD5;
        checksum = String.make 16 '\xAB';
      };
      {
        file_name_offset = u32 10;
        checksum_kind = SHA1;
        checksum = String.make 20 '\xCD';
      };
    |]
  in
  let buf = Buffer.create 128 in
  Pdb.Debug_subsections.write_subsection buf (FileChecksums checksums);
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let subs =
    Pdb.Debug_subsections.parse_subsections cur (String.length bytes)
  in
  let sub_list = List.of_seq subs in
  Alcotest.(check int) "one subsection" 1 (List.length sub_list);
  match List.hd sub_list with
  | Pdb.Debug_subsections.FileChecksums entries ->
      Alcotest.(check int) "two entries" 2 (Array.length entries);
      Alcotest.(check int)
        "entry 0 name offset" 0
        (Unsigned.UInt32.to_int entries.(0).file_name_offset);
      Alcotest.(check int)
        "entry 0 checksum len" 16
        (String.length entries.(0).checksum);
      Alcotest.(check int)
        "entry 1 name offset" 10
        (Unsigned.UInt32.to_int entries.(1).file_name_offset);
      Alcotest.(check int)
        "entry 1 checksum len" 20
        (String.length entries.(1).checksum)
  | _ -> Alcotest.fail "expected FileChecksums subsection"

let test_string_table_roundtrip () =
  let strings = [| "foo.c"; "bar.h"; "baz.cpp" |] in
  let buf = Buffer.create 64 in
  Pdb.Debug_subsections.write_subsection buf (StringTable strings);
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let subs =
    Pdb.Debug_subsections.parse_subsections cur (String.length bytes)
  in
  let sub_list = List.of_seq subs in
  Alcotest.(check int) "one subsection" 1 (List.length sub_list);
  match List.hd sub_list with
  | Pdb.Debug_subsections.StringTable strs ->
      Alcotest.(check int) "three strings" 3 (Array.length strs);
      Alcotest.(check string) "string 0" "foo.c" strs.(0);
      Alcotest.(check string) "string 1" "bar.h" strs.(1);
      Alcotest.(check string) "string 2" "baz.cpp" strs.(2)
  | _ -> Alcotest.fail "expected StringTable subsection"

let test_inlinee_lines_roundtrip () =
  let entries =
    [|
      {
        Pdb.Debug_subsections.inlinee = u32 0x1000;
        file_id = u32 0;
        source_line = u32 42;
      };
      { inlinee = u32 0x1001; file_id = u32 1; source_line = u32 100 };
    |]
  in
  let buf = Buffer.create 64 in
  Pdb.Debug_subsections.write_subsection buf (InlineeLines entries);
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let subs =
    Pdb.Debug_subsections.parse_subsections cur (String.length bytes)
  in
  let sub_list = List.of_seq subs in
  Alcotest.(check int) "one subsection" 1 (List.length sub_list);
  match List.hd sub_list with
  | Pdb.Debug_subsections.InlineeLines ils ->
      Alcotest.(check int) "two entries" 2 (Array.length ils);
      Alcotest.(check int)
        "entry 0 line" 42
        (Unsigned.UInt32.to_int ils.(0).source_line);
      Alcotest.(check int)
        "entry 1 inlinee" 0x1001
        (Unsigned.UInt32.to_int ils.(1).inlinee)
  | _ -> Alcotest.fail "expected InlineeLines subsection"

let test_multiple_subsections () =
  let buf = Buffer.create 256 in
  Pdb.Debug_subsections.write_subsection buf (StringTable [| "hello.c" |]);
  Pdb.Debug_subsections.write_subsection buf
    (FileChecksums
       [| { file_name_offset = u32 0; checksum_kind = None; checksum = "" } |]);
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let subs =
    Pdb.Debug_subsections.parse_subsections cur (String.length bytes)
  in
  let sub_list = List.of_seq subs in
  Alcotest.(check int) "two subsections" 2 (List.length sub_list);
  (match List.nth sub_list 0 with
  | Pdb.Debug_subsections.StringTable _ -> ()
  | _ -> Alcotest.fail "expected StringTable first");
  match List.nth sub_list 1 with
  | Pdb.Debug_subsections.FileChecksums _ -> ()
  | _ -> Alcotest.fail "expected FileChecksums second"

(** {2 Column info tests} *)

let test_lines_with_columns () =
  let lines : Pdb.Debug_subsections.lines_subsection =
    {
      contrib_offset = u32 0;
      contrib_segment = 1;
      flags = 0; (* writer will set HaveColumns automatically *)
      contrib_size = u32 50;
      blocks =
        [|
          {
            file_index = u32 0;
            lines =
              [|
                {
                  offset = u32 0;
                  line_start = 5;
                  delta_line_end = 0;
                  is_statement = true;
                };
                {
                  offset = u32 10;
                  line_start = 6;
                  delta_line_end = 0;
                  is_statement = true;
                };
                {
                  offset = u32 25;
                  line_start = 7;
                  delta_line_end = 0;
                  is_statement = true;
                };
              |];
            columns =
              Some
                [|
                  { Pdb.Debug_subsections.start_column = 3; end_column = 15 };
                  { start_column = 5; end_column = 20 };
                  { start_column = 3; end_column = 10 };
                |];
          };
        |];
    }
  in
  let buf = Buffer.create 128 in
  Pdb.Debug_subsections.write_subsection buf (Lines lines);
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let subs = Pdb.Debug_subsections.parse_subsections cur (String.length bytes) in
  let sub_list = List.of_seq subs in
  Alcotest.(check int) "one subsection" 1 (List.length sub_list);
  match List.hd sub_list with
  | Pdb.Debug_subsections.Lines ls ->
      Alcotest.(check int) "HaveColumns flag" 1 (ls.flags land 0x0001);
      Alcotest.(check int) "one block" 1 (Array.length ls.blocks);
      let block = ls.blocks.(0) in
      Alcotest.(check int) "three lines" 3 (Array.length block.lines);
      Alcotest.(check int) "line 0" 5 block.lines.(0).line_start;
      Alcotest.(check int) "line 1" 6 block.lines.(1).line_start;
      (* Verify columns *)
      (match block.columns with
      | Some cols ->
          Alcotest.(check int) "three cols" 3 (Array.length cols);
          Alcotest.(check int) "col 0 start" 3 cols.(0).start_column;
          Alcotest.(check int) "col 0 end" 15 cols.(0).end_column;
          Alcotest.(check int) "col 1 start" 5 cols.(1).start_column;
          Alcotest.(check int) "col 1 end" 20 cols.(1).end_column;
          Alcotest.(check int) "col 2 start" 3 cols.(2).start_column;
          Alcotest.(check int) "col 2 end" 10 cols.(2).end_column
      | Option.None -> Alcotest.fail "expected columns")
  | _ -> Alcotest.fail "expected Lines subsection"

let test_lines_no_columns_parsed_as_none () =
  (* Lines without column info should have columns = None *)
  let lines : Pdb.Debug_subsections.lines_subsection =
    {
      contrib_offset = u32 0;
      contrib_segment = 1;
      flags = 0;
      contrib_size = u32 20;
      blocks =
        [|
          {
            file_index = u32 0;
            lines =
              [|
                {
                  offset = u32 0;
                  line_start = 1;
                  delta_line_end = 0;
                  is_statement = true;
                };
              |];
            columns = Option.None;
          };
        |];
    }
  in
  let buf = Buffer.create 64 in
  Pdb.Debug_subsections.write_subsection buf (Lines lines);
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let subs = Pdb.Debug_subsections.parse_subsections cur (String.length bytes) in
  let sub_list = List.of_seq subs in
  match List.hd sub_list with
  | Pdb.Debug_subsections.Lines ls ->
      Alcotest.(check int) "no HaveColumns" 0 (ls.flags land 0x0001);
      let block = ls.blocks.(0) in
      Alcotest.(check bool) "no columns" true
        (block.columns = Option.None)
  | _ -> Alcotest.fail "expected Lines subsection"

let () =
  Alcotest.run "Debug Subsections"
    [
      ( "subsection",
        [
          Alcotest.test_case "lines roundtrip" `Quick test_lines_roundtrip;
          Alcotest.test_case "file checksums" `Quick
            test_file_checksums_roundtrip;
          Alcotest.test_case "string table" `Quick test_string_table_roundtrip;
          Alcotest.test_case "inlinee lines" `Quick test_inlinee_lines_roundtrip;
          Alcotest.test_case "multiple subsections" `Quick
            test_multiple_subsections;
        ] );
      ( "columns",
        [
          Alcotest.test_case "lines with columns" `Quick
            test_lines_with_columns;
          Alcotest.test_case "lines without columns" `Quick
            test_lines_no_columns_parsed_as_none;
        ] );
    ]
