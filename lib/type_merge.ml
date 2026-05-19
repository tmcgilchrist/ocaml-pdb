(** Type deduplication for TPI/IPI streams.

    Deduplicates type records by hashing their serialized bytes. Records
    with identical wire format get the same TypeIndex.

    References:
    - LLVM: llvm/lib/DebugInfo/CodeView/MergingTypeTableBuilder.cpp *)

module Buffer = Stdlib.Buffer

type t = {
  mutable records : Codeview_types.type_record list; (** reverse order *)
  mutable next_index : int;
  seen : (string, Type_index.t) Hashtbl.t; (** serialized bytes -> TypeIndex *)
}

let create () =
  { records = []; next_index = 0x1000; seen = Hashtbl.create 128 }

(** Serialize a type record to bytes for dedup comparison.
    We serialize without the length prefix since the content is what matters. *)
let record_key (record : Codeview_types.type_record) : string =
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
