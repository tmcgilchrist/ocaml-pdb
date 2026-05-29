(** Type deduplication for TPI/IPI streams.

    Deduplicates type records by hashing their serialized bytes. Records with
    identical wire format get the same TypeIndex.

    References:
    - LLVM: llvm/lib/DebugInfo/CodeView/MergingTypeTableBuilder.cpp *)

module Buffer = Stdlib.Buffer

type t = {
  mutable records : Codeview_types.type_record list;  (** reverse order *)
  mutable next_index : int;
  seen : (string, Type_index.t) Hashtbl.t;  (** serialized bytes -> TypeIndex *)
}

let create () = { records = []; next_index = 0x1000; seen = Hashtbl.create 128 }

(* Serialize a type record to bytes for dedup comparison.
   We serialize without the length prefix since the content is what matters. *)
let record_key record =
  let buf = Buffer.create 64 in
  Codeview_types.write_type_record buf record;
  Buffer.contents buf

let insert t record =
  let key = record_key record in
  match Hashtbl.find_opt t.seen key with
  | Some existing_idx -> existing_idx
  | None ->
      let idx = Type_index.user (Unsigned.UInt32.of_int t.next_index) in
      Hashtbl.replace t.seen key idx;
      t.records <- record :: t.records;
      t.next_index <- t.next_index + 1;
      idx

let records t = List.rev t.records
let count t = List.length t.records

let find_index t record =
  let key = record_key record in
  Hashtbl.find_opt t.seen key

(* {2 Cross-compilation-unit merging} *)

type cross = { types : t; ids : t }

let create_cross () = { types = create (); ids = create () }
let cross_types c = records c.types
let cross_ids c = records c.ids

(** Build a remap closure over an array of already-merged indices for the
    current compilation unit. Index [j] (TypeIndex [0x1000 + j]) maps to
    [remap.(j)]. Simple/None indices and any out-of-range user index (a forward
    reference, which does not occur in a well-formed stream) pass through
    unchanged. *)
let remap_of remap ti =
  match ti with
  | Type_index.Simple _ -> ti
  | Type_index.User u ->
      let j = Unsigned.UInt32.to_int u - 0x1000 in
      if j >= 0 && j < Array.length remap then remap.(j) else ti

let merge_types c records =
  let n = List.length records in
  let remap = Array.make n (Type_index.user Unsigned.UInt32.zero) in
  let f = remap_of remap in
  (* Type records carry only TPI references, so [id_ref] is never invoked;
     passing [f] for it as well just keeps [map_type_indices] total. *)
  List.iteri
    (fun j record ->
      let remapped =
        Codeview_types.map_type_indices ~type_ref:f ~id_ref:f record
      in
      remap.(j) <- insert c.types remapped)
    records;
  remap

let merge_ids c ~type_remap records =
  let n = List.length records in
  let id_remap = Array.make n (Type_index.user Unsigned.UInt32.zero) in
  let type_ref = remap_of type_remap and id_ref = remap_of id_remap in
  List.iteri
    (fun j record ->
      let remapped = Codeview_types.map_type_indices ~type_ref ~id_ref record in
      id_remap.(j) <- insert c.ids remapped)
    records;
  id_remap
