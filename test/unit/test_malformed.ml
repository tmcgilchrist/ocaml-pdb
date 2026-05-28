(** Tests for malformed/corrupt input handling.

    Verifies the library raises appropriate exceptions rather than
    producing garbage or crashing on invalid input. *)

module Buffer = Stdlib.Buffer

open Test_support

let write_u16_le buf v =
  Buffer.add_char buf (Char.chr (v land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xFF))

let write_u32_le buf v =
  Buffer.add_char buf (Char.chr (v land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 16) land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 24) land 0xFF))

(** {2 Truncated type records} *)

(** [bad_format] matches any [Object.Buffer.Invalid_format _] exception
    regardless of its message string — the exact wording is not part of
    the public contract. *)
let bad_format f =
  match f () with
  | _ -> Alcotest.fail "expected Invalid_format, function returned"
  | exception Object.Buffer.Invalid_format _ -> ()

let test_truncated_modifier () =
  (* LF_MODIFIER needs at least 6 bytes (2 kind + 4 type), give it only 4 *)
  let buf = Buffer.create 8 in
  write_u16_le buf 4; (* length = 4 bytes *)
  write_u16_le buf 0x1001; (* LF_MODIFIER *)
  write_u16_le buf 0x0074; (* partial: only 2 of 4 bytes for modified_type *)
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let len = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int in
  bad_format (fun () -> Pdb.Codeview_types.parse_type_record cur len)

let test_truncated_procedure () =
  (* LF_PROCEDURE needs 12 bytes payload, give it only 6 *)
  let buf = Buffer.create 12 in
  write_u16_le buf 6;
  write_u16_le buf 0x1008; (* LF_PROCEDURE *)
  write_u32_le buf 0x0074; (* return_type only *)
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let len = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int in
  bad_format (fun () -> Pdb.Codeview_types.parse_type_record cur len)

(** {2 Truncated symbol records} *)

let test_truncated_gproc32 () =
  (* S_GPROC32 needs ~35+ bytes, give it only 8 *)
  let buf = Buffer.create 16 in
  write_u16_le buf 8;
  write_u16_le buf 0x1110; (* S_GPROC32 *)
  write_u32_le buf 0; (* parent only *)
  write_u16_le buf 0; (* partial end_ *)
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let len = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int in
  bad_format (fun () -> Pdb.Codeview_symbols.parse_symbol_record cur len)

let test_truncated_pub32 () =
  (* S_PUB32 needs at least 10 bytes, give it only 4 *)
  let buf = Buffer.create 8 in
  write_u16_le buf 4;
  write_u16_le buf 0x110e; (* S_PUB32 *)
  write_u16_le buf 0; (* partial flags *)
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let len = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int in
  bad_format (fun () -> Pdb.Codeview_symbols.parse_symbol_record cur len)

(** {2 Truncated numeric leaf} *)

let test_truncated_numeric_leaf () =
  (* A numeric leaf tag 0x8003 (LF_LONG) but no following 4 bytes *)
  let buf = Buffer.create 4 in
  write_u16_le buf 0x8003;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  bad_format (fun () -> Pdb.Codeview_types.parse_numeric_leaf cur)

(** {2 Zero-length and edge-case records} *)

let test_zero_length_type_record () =
  (* A type record with length = 2 (just the kind, no payload).
     This should parse as Unknown with empty data. *)
  let buf = Buffer.create 8 in
  write_u16_le buf 2; (* length = 2, just the leaf kind *)
  write_u16_le buf 0x9999; (* unknown kind *)
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let len = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int in
  let result = Pdb.Codeview_types.parse_type_record cur len in
  match result with
  | Pdb.Codeview_types.Unknown { kind; data } ->
      Alcotest.(check int) "kind" 0x9999 kind;
      Alcotest.(check int) "empty data" 0 (String.length data)
  | _ -> Alcotest.fail "expected Unknown"

let test_zero_length_symbol_record () =
  let buf = Buffer.create 8 in
  write_u16_le buf 2;
  write_u16_le buf 0xAAAA;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let len = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int in
  let result = Pdb.Codeview_symbols.parse_symbol_record cur len in
  match result with
  | Pdb.Codeview_symbols.Unknown { kind; data } ->
      Alcotest.(check int) "kind" 0xAAAA kind;
      Alcotest.(check int) "empty data" 0 (String.length data)
  | _ -> Alcotest.fail "expected Unknown"

(** {2 MSF with corrupt stream sizes} *)

let test_msf_truncated_stream () =
  (* Build a valid MSF, then truncate the file so stream data is missing *)
  let builder = Pdb.Msf_write.create ~block_size:512 in
  let _ = Pdb.Msf_write.add_stream builder (String.make 1000 'X') in
  let msf_bytes = Pdb.Msf_write.finalize builder in
  (* Truncate to just the first 2 blocks (superblock + FPM) *)
  let truncated = String.sub msf_bytes 0 1024 in
  let obj_buf = buffer_of_string truncated in
  (* This should fail because num_blocks * block_size > file size *)
  Alcotest.check_raises "truncated MSF"
    (Object.Buffer.Invalid_format
       "MSF file size smaller than num_blocks * block_size")
    (fun () -> ignore (Pdb.Msf.read obj_buf))

(** {2 TPI header edge cases} *)

let test_tpi_empty_stream () =
  (* A TPI stream with 0 type records should parse cleanly *)
  let buf = Buffer.create 128 in
  Pdb.Tpi_write.write buf [];
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let header = Pdb.Tpi.parse_header cur in
  Alcotest.(check int) "0 records" 0 (Pdb.Tpi.num_type_records header);
  let records = List.of_seq (Pdb.Tpi.parse_type_records cur header) in
  Alcotest.(check int) "parsed 0" 0 (List.length records)

(** {2 Truncated headers} *)

(* Any reader pointed at an empty cursor should raise Invalid_format
   rather than letting Bigarray's Invalid_argument leak through. *)
let empty_cursor () =
  Object.Buffer.cursor
    (Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout 0)

let test_truncated_tpi_header () =
  bad_format (fun () -> Pdb.Tpi.parse_header (empty_cursor ()))

let test_truncated_dbi_header () =
  bad_format (fun () -> Pdb.Dbi.parse (empty_cursor ()))

let test_truncated_pdb_info_header () =
  bad_format (fun () -> Pdb.Pdb_stream.read (empty_cursor ()))

let test_truncated_gsi_header () =
  bad_format (fun () -> ignore (Pdb.Gsi.parse_gsi (empty_cursor ()) 0))

let test_truncated_publics_header () =
  bad_format (fun () ->
      ignore (Pdb.Gsi.parse_publics_header (empty_cursor ())))

let test_truncated_string_table_header () =
  bad_format (fun () -> ignore (Pdb.Pdb_string_table.parse (empty_cursor ())))

(** /names string table with the wrong signature must be rejected. *)
let test_string_table_bad_signature () =
  let buf = Buffer.create 12 in
  write_u32_le buf 0xDEADBEEF;
  (* not 0xEFFEEFFE *)
  write_u32_le buf 1;
  (* hash_version *)
  write_u32_le buf 0;
  (* byte_size *)
  let obj_buf = buffer_of_string (Buffer.contents buf) in
  bad_format (fun () ->
      ignore (Pdb.Pdb_string_table.parse (Object.Buffer.cursor obj_buf)))

(** A TPI header that claims more record bytes than the stream actually
    contains must fail in the per-record loop, not as a Bigarray error. *)
let test_tpi_overruns_stream () =
  let buf = Buffer.create 128 in
  Pdb.Tpi_write.write buf [];
  let bytes = Buffer.contents buf in
  (* Patch type_record_bytes (header offset 16, u32) to claim 100 extra
     bytes of records that aren't actually present. *)
  let bytes = Bytes.of_string bytes in
  Bytes.set bytes 16 (Char.chr 100);
  Bytes.set bytes 17 '\000';
  Bytes.set bytes 18 '\000';
  Bytes.set bytes 19 '\000';
  let obj_buf = buffer_of_string (Bytes.unsafe_to_string bytes) in
  let cur = Object.Buffer.cursor obj_buf in
  let header = Pdb.Tpi.parse_header cur in
  bad_format (fun () ->
      ignore (List.of_seq (Pdb.Tpi.parse_type_records cur header)))

let () =
  Alcotest.run "Malformed Input"
    [
      ( "truncated_types",
        [
          Alcotest.test_case "truncated modifier" `Quick test_truncated_modifier;
          Alcotest.test_case "truncated procedure" `Quick
            test_truncated_procedure;
        ] );
      ( "truncated_symbols",
        [
          Alcotest.test_case "truncated gproc32" `Quick test_truncated_gproc32;
          Alcotest.test_case "truncated pub32" `Quick test_truncated_pub32;
        ] );
      ( "truncated_numeric",
        [
          Alcotest.test_case "truncated numeric leaf" `Quick
            test_truncated_numeric_leaf;
        ] );
      ( "zero_length",
        [
          Alcotest.test_case "zero-length type" `Quick
            test_zero_length_type_record;
          Alcotest.test_case "zero-length symbol" `Quick
            test_zero_length_symbol_record;
        ] );
      ( "msf_corrupt",
        [
          Alcotest.test_case "truncated stream" `Quick test_msf_truncated_stream;
        ] );
      ( "tpi_edge",
        [
          Alcotest.test_case "empty TPI" `Quick test_tpi_empty_stream;
          Alcotest.test_case "TPI overruns stream" `Quick
            test_tpi_overruns_stream;
        ] );
      ( "truncated_headers",
        [
          Alcotest.test_case "TPI header" `Quick test_truncated_tpi_header;
          Alcotest.test_case "DBI header" `Quick test_truncated_dbi_header;
          Alcotest.test_case "PDB info header" `Quick
            test_truncated_pdb_info_header;
          Alcotest.test_case "GSI header" `Quick test_truncated_gsi_header;
          Alcotest.test_case "publics header" `Quick
            test_truncated_publics_header;
          Alcotest.test_case "string table header" `Quick
            test_truncated_string_table_header;
        ] );
      ( "format_validation",
        [
          Alcotest.test_case "/names bad signature" `Quick
            test_string_table_bad_signature;
        ] );
    ]
