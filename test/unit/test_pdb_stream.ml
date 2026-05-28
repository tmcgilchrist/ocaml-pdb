(** Tests for PDB Info Stream (Stream 1) read/write. *)

module Buffer = Stdlib.Buffer

open Test_support

let test_pdb_version_roundtrip () =
  let versions = [ Pdb.Pdb_stream.VC70; VC80; VC110; VC140; Unknown 12345 ] in
  List.iter
    (fun v ->
      let n = Pdb.Pdb_stream.pdb_version_to_int v in
      let v' = Pdb.Pdb_stream.int_to_pdb_version n in
      let n' = Pdb.Pdb_stream.pdb_version_to_int v' in
      Alcotest.(check int)
        (Printf.sprintf "version roundtrip %s"
           (Pdb.Pdb_stream.string_of_pdb_version v))
        n n')
    versions

let make_test_guid () : Pdb.Pdb_types.guid =
  {
    data1 = Unsigned.UInt32.of_int 0x12345678;
    data2 = Unsigned.UInt16.of_int 0xABCD;
    data3 = Unsigned.UInt16.of_int 0xEF01;
    data4 = "\x01\x02\x03\x04\x05\x06\x07\x08";
  }

let test_pdb_stream_roundtrip () =
  let info : Pdb.Pdb_stream.t =
    {
      version = VC140;
      signature = Unsigned.UInt32.of_int 0x5F3A1234;
      age = Unsigned.UInt32.of_int 1;
      guid = make_test_guid ();
      named_streams = [ ("/names", 5); ("/LinkInfo", 8) ];
      features = [ ContainsIdStream ];
    }
  in
  let buf = Buffer.create 256 in
  Pdb.Pdb_stream_write.write buf info;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let info' = Pdb.Pdb_stream.read cur in
  (* Verify header fields *)
  Alcotest.(check int)
    "version" 20140508
    (Pdb.Pdb_stream.pdb_version_to_int info'.version);
  Alcotest.(check int)
    "signature" 0x5F3A1234
    (Unsigned.UInt32.to_int info'.signature);
  Alcotest.(check int) "age" 1 (Unsigned.UInt32.to_int info'.age);
  (* Verify GUID *)
  Alcotest.(check int)
    "guid.data1" 0x12345678
    (Unsigned.UInt32.to_int info'.guid.data1);
  Alcotest.(check int)
    "guid.data2" 0xABCD
    (Unsigned.UInt16.to_int info'.guid.data2);
  Alcotest.(check int)
    "guid.data3" 0xEF01
    (Unsigned.UInt16.to_int info'.guid.data3);
  Alcotest.(check string)
    "guid.data4" "\x01\x02\x03\x04\x05\x06\x07\x08" info'.guid.data4;
  (* Verify named streams *)
  Alcotest.(check int) "named stream count" 2 (List.length info'.named_streams);
  Alcotest.(check bool)
    "has /names" true
    (List.exists (fun (n, i) -> n = "/names" && i = 5) info'.named_streams);
  Alcotest.(check bool)
    "has /LinkInfo" true
    (List.exists (fun (n, i) -> n = "/LinkInfo" && i = 8) info'.named_streams);
  (* Verify features *)
  Alcotest.(check bool)
    "has ContainsIdStream" true
    (List.mem Pdb.Pdb_stream.ContainsIdStream info'.features)

let test_pdb_stream_no_features () =
  let info : Pdb.Pdb_stream.t =
    {
      version = VC70;
      signature = Unsigned.UInt32.of_int 0;
      age = Unsigned.UInt32.of_int 1;
      guid = make_test_guid ();
      named_streams = [];
      features = [];
    }
  in
  let buf = Buffer.create 128 in
  Pdb.Pdb_stream_write.write buf info;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let cur = Object.Buffer.cursor obj_buf in
  let info' = Pdb.Pdb_stream.read cur in
  Alcotest.(check int)
    "version VC70" 20000404
    (Pdb.Pdb_stream.pdb_version_to_int info'.version);
  Alcotest.(check int) "no features" 0 (List.length info'.features);
  Alcotest.(check int) "no named streams" 0 (List.length info'.named_streams)

let test_guid_string () =
  let g = make_test_guid () in
  let s = Pdb.Pdb_types.string_of_guid g in
  (* Should produce something like {12345678-ABCD-EF01-0102030405060708} *)
  Alcotest.(check bool)
    "guid string starts with {" true
    (String.length s > 0 && s.[0] = '{');
  Alcotest.(check bool)
    "guid string ends with }" true
    (s.[String.length s - 1] = '}')

let test_pdb_stream_full_roundtrip_through_msf () =
  (* Build a complete MSF with a PDB info stream at index 1 *)
  let info : Pdb.Pdb_stream.t =
    {
      version = VC140;
      signature = Unsigned.UInt32.of_int 42;
      age = Unsigned.UInt32.of_int 1;
      guid = make_test_guid ();
      named_streams = [ ("/names", 3) ];
      features = [ ContainsIdStream ];
    }
  in
  let info_buf = Buffer.create 256 in
  Pdb.Pdb_stream_write.write info_buf info;
  let info_bytes = Buffer.contents info_buf in
  (* Build MSF: stream 0 (old directory, empty), stream 1 (PDB info) *)
  let builder = Pdb.Msf_write.create ~block_size:4096 in
  let _s0 = Pdb.Msf_write.add_empty_stream builder in
  let _s1 = Pdb.Msf_write.add_stream builder info_bytes in
  let msf_bytes = Pdb.Msf_write.finalize builder in
  (* Read back *)
  let msf_buf = buffer_of_string msf_bytes in
  let msf = Pdb.Msf.read msf_buf in
  let stream1 = Pdb.Msf.get_stream_exn msf 1 in
  let cur = Object.Buffer.cursor stream1 in
  let info' = Pdb.Pdb_stream.read cur in
  Alcotest.(check int)
    "version through MSF" 20140508
    (Pdb.Pdb_stream.pdb_version_to_int info'.version);
  Alcotest.(check int) "age through MSF" 1 (Unsigned.UInt32.to_int info'.age);
  Alcotest.(check bool)
    "named stream /names through MSF" true
    (List.exists (fun (n, i) -> n = "/names" && i = 3) info'.named_streams)

let () =
  Alcotest.run "PDB Stream"
    [
      ( "pdb_version",
        [ Alcotest.test_case "roundtrip" `Quick test_pdb_version_roundtrip ] );
      ( "pdb_stream",
        [
          Alcotest.test_case "roundtrip" `Quick test_pdb_stream_roundtrip;
          Alcotest.test_case "no features" `Quick test_pdb_stream_no_features;
          Alcotest.test_case "guid string" `Quick test_guid_string;
          Alcotest.test_case "full MSF roundtrip" `Quick
            test_pdb_stream_full_roundtrip_through_msf;
        ] );
    ]
