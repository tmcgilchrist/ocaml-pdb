(** QCheck property-based tests for the high-level PDB builder.

    Each property drives the builder with arbitrary but valid inputs,
    finalizes the resulting byte string, and verifies an end-to-end
    invariant by parsing it back through [Msf] and the per-stream
    readers. *)

module Buffer = Stdlib.Buffer

open Test_support

let default_count =
  match Sys.getenv_opt "QCHECK_COUNT" with
  | Some s -> ( try int_of_string s with _ -> 100)
  | None -> 100

let q_test ?(count = default_count) name gen f =
  QCheck.Test.make ~name ~count gen f

(** Drop duplicates by [key], preserving order of first occurrence. *)
let dedup_by ~key xs =
  let seen = Hashtbl.create 16 in
  List.filter
    (fun x ->
      let k = key x in
      if Hashtbl.mem seen k then false
      else begin
        Hashtbl.add seen k ();
        true
      end)
    xs

let serialize_type r =
  let buf = Buffer.create 32 in
  Pdb.Codeview_types.write_type_record buf r;
  Buffer.contents buf

(** {2 Generators} *)

(** Lowercase identifier of length 1..16. *)
let gen_ident =
  QCheck.Gen.(
    let* len = int_range 1 16 in
    string_size ~gen:(map (fun n -> Char.chr (97 + (n mod 26))) nat)
      (return len))

(** Generate a small ArgList type record. Distinct [n] yields a distinct
    wire form. *)
let gen_arglist =
  QCheck.Gen.(
    let* n = int_range 0 4 in
    return
      (Pdb.Codeview_types.ArgList
         { args = Array.init n (fun i -> ti (0x0074 + i)) }))

let gen_pub32 =
  QCheck.Gen.(
    let* name = gen_ident in
    let* off = int_range 0x1000 0xFFFF in
    let* seg = int_range 1 4 in
    return
      (Pdb.Codeview_symbols.Pub32
         { flags = u32 0; offset = u32 off; segment = seg; name = "_" ^ name }))

(** {2 Properties} *)

let arglist_arb = QCheck.(make Gen.(list_size (int_range 0 8) gen_arglist))
let pub32_arb = QCheck.(make Gen.(list_size (int_range 0 8) gen_pub32))
let ident_arb = QCheck.(make Gen.(list_size (int_range 0 8) gen_ident))

let prop_add_type_dense_monotonic =
  q_test "add_type assigns 0x1000.. for distinct records" arglist_arb
    (fun records ->
      let uniq = dedup_by ~key:serialize_type records in
      let b = Pdb.Pdb_builder.create Pdb.Pdb_builder.AMD64 in
      List.mapi
        (fun i r ->
          let idx = Pdb.Pdb_builder.add_type b r in
          Unsigned.UInt32.to_int (Pdb.Type_index.to_u32 idx) = 0x1000 + i)
        uniq
      |> List.for_all (fun ok -> ok))

let prop_finalize_deterministic =
  q_test "finalize is deterministic" arglist_arb (fun records ->
      let build () =
        let b = Pdb.Pdb_builder.create Pdb.Pdb_builder.AMD64 in
        List.iter (fun r -> ignore (Pdb.Pdb_builder.add_type b r)) records;
        Pdb.Pdb_builder.finalize b
      in
      build () = build ())

let prop_types_roundtrip =
  q_test "TPI round-trips added types" arglist_arb (fun records ->
      let b = Pdb.Pdb_builder.create Pdb.Pdb_builder.AMD64 in
      List.iter (fun r -> ignore (Pdb.Pdb_builder.add_type b r)) records;
      let pdb_bytes = Pdb.Pdb_builder.finalize b in
      let buf = buffer_of_string pdb_bytes in
      let msf = Pdb.Msf.read buf in
      let s2 = Pdb.Msf.get_stream_exn msf 2 in
      let cur = Object.Buffer.cursor s2 in
      let h = Pdb.Tpi.parse_header cur in
      let parsed = List.of_seq (Pdb.Tpi.parse_type_records cur h) in
      (* Mirror the merger's identity (wire form) and look up via a
         set to keep membership O(1) under shrinking. *)
      let parsed_set = Hashtbl.create (List.length parsed) in
      List.iter
        (fun r -> Hashtbl.replace parsed_set (serialize_type r) ())
        parsed;
      List.for_all
        (fun r -> Hashtbl.mem parsed_set (serialize_type r))
        records)

let pub_name = function
  | Pdb.Codeview_symbols.Pub32 { name; _ } -> name
  | _ -> assert false

let prop_publics_roundtrip =
  q_test "publics round-trip through publics stream" pub32_arb (fun publics ->
      let uniq = dedup_by ~key:pub_name publics in
      let b = Pdb.Pdb_builder.create Pdb.Pdb_builder.AMD64 in
      List.iter (Pdb.Pdb_builder.add_public b) uniq;
      let pdb_bytes = Pdb.Pdb_builder.finalize b in
      let buf = buffer_of_string pdb_bytes in
      let msf = Pdb.Msf.read buf in
      let s3 = Pdb.Msf.get_stream_exn msf 3 in
      let dbi = Pdb.Dbi.parse (Object.Buffer.cursor s3) in
      let sym_stream_idx = dbi.header.sym_record_stream in
      if uniq = [] then sym_stream_idx = 0xFFFF
      else
        let sym_stream = Pdb.Msf.get_stream_exn msf sym_stream_idx in
        let sym_cur = Object.Buffer.cursor sym_stream in
        let syms =
          List.of_seq
            (Pdb.Codeview_symbols.parse_symbol_stream sym_cur
               (Bigarray.Array1.dim sym_stream))
        in
        let parsed_names = Hashtbl.create (List.length syms) in
        List.iter
          (function
            | Pdb.Codeview_symbols.Pub32 { name; _ } ->
                Hashtbl.replace parsed_names name ()
            | _ -> ())
          syms;
        List.for_all (fun p -> Hashtbl.mem parsed_names (pub_name p)) uniq)

let prop_strings_roundtrip =
  q_test "strings round-trip through /names" ident_arb (fun names ->
      let uniq = dedup_by ~key:Fun.id names in
      let b = Pdb.Pdb_builder.create Pdb.Pdb_builder.AMD64 in
      let added = List.map (fun n -> (n, Pdb.Pdb_builder.add_string b n)) uniq in
      let pdb_bytes = Pdb.Pdb_builder.finalize b in
      let buf = buffer_of_string pdb_bytes in
      let msf = Pdb.Msf.read buf in
      let s1 = Pdb.Msf.get_stream_exn msf 1 in
      let info = Pdb.Pdb_stream.parse (Object.Buffer.cursor s1) in
      match List.assoc_opt "/names" info.named_streams with
      | None -> uniq = []
      | Some idx ->
          let names_stream = Pdb.Msf.get_stream_exn msf idx in
          let st =
            Pdb.Pdb_string_table.parse (Object.Buffer.cursor names_stream)
          in
          List.for_all
            (fun (n, off) -> Pdb.Pdb_string_table.lookup st n = Some off)
            added)

let () =
  Alcotest.run "PDB Builder properties"
    [
      ( "builder/qcheck",
        List.map QCheck_alcotest.to_alcotest
          [
            prop_add_type_dense_monotonic;
            prop_finalize_deterministic;
            prop_types_roundtrip;
            prop_publics_roundtrip;
            prop_strings_roundtrip;
          ] );
    ]
