(** PDB Named Stream Map.

    References:
    - LLVM: llvm/lib/DebugInfo/PDB/Native/NamedStreamMap.cpp
    - LLVM: llvm/include/llvm/DebugInfo/PDB/Native/HashTable.h
    - LLVM: llvm/lib/DebugInfo/PDB/Native/HashTable.cpp *)

open Binary_writer

type t = (string * int) list

module Buffer = Stdlib.Buffer

(** {2 Sparse bit vector serialization}

    Format: u32 num_words, then num_words x u32 words. Bit i of word j
    represents index (j*32 + i). *)

let parse_bit_vector cur =
  let num_words = Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int in
  let bits = Array.make (num_words * 32) false in
  for i = 0 to num_words - 1 do
    let word = Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int in
    for j = 0 to 31 do
      if word land (1 lsl j) <> 0 then bits.((i * 32) + j) <- true
    done
  done;
  bits

let write_bit_vector buf present capacity =
  (* Find the highest set bit to determine how many words we need *)
  let max_set =
    let r = ref (-1) in
    Array.iteri (fun i v -> if v then r := i) present;
    !r
  in
  let num_words = if max_set < 0 then 0 else (max_set / 32) + 1 in
  let _ = capacity in
  write_u32_le buf num_words;
  for i = 0 to num_words - 1 do
    let word = ref 0 in
    for j = 0 to 31 do
      let idx = (i * 32) + j in
      if idx < Array.length present && present.(idx) then
        word := !word lor (1 lsl j)
    done;
    write_u32_le buf !word
  done

(** {2 PDB Hash Table} *)

let parse_hash_table cur =
  let size = Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int in
  let _capacity = Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int in
  let present = parse_bit_vector cur in
  let _deleted = parse_bit_vector cur in
  (* Read entries for each present bucket *)
  let entries = ref [] in
  let count = ref 0 in
  for i = 0 to Array.length present - 1 do
    if present.(i) && !count < size then begin
      let key = Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int in
      let value = Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int in
      entries := (key, value) :: !entries;
      incr count
    end
  done;
  List.rev !entries

(** Generic placement: slot for a given key is [(key mod capacity)] with
    linear probing. Suitable for tables where the key already represents
    the hash. The named stream map uses a different placement function
    (see [write_named_hash_table_body]). *)
let write_hash_table buf entries capacity =
  let size = List.length entries in
  write_u32_le buf size;
  write_u32_le buf capacity;
  let buckets = Array.make capacity None in
  let present = Array.make capacity false in
  List.iter
    (fun (key, value) ->
      let start = key mod capacity in
      let rec find_slot i =
        let idx = (start + i) mod capacity in
        if not present.(idx) then idx else find_slot (i + 1)
      in
      let idx = find_slot 0 in
      present.(idx) <- true;
      buckets.(idx) <- Some (key, value))
    entries;
  (* Write present bit vector *)
  write_bit_vector buf present capacity;
  (* Write empty deleted bit vector *)
  let deleted = Array.make capacity false in
  write_bit_vector buf deleted capacity;
  (* Write key-value pairs in bucket order *)
  for i = 0 to capacity - 1 do
    match buckets.(i) with
    | Some (key, value) ->
        write_u32_le buf key;
        write_u32_le buf value
    | None -> ()
  done

(** {2 Named Stream Map} *)

(** Read a null-terminated string from a string at the given offset. *)
let string_at_offset str_buf offset =
  let len = String.length str_buf in
  let end_pos = ref offset in
  while !end_pos < len && str_buf.[!end_pos] <> '\000' do
    incr end_pos
  done;
  String.sub str_buf offset (!end_pos - offset)

let parse cur =
  (* Read string buffer *)
  let str_buf_size = Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int in
  let str_buf = Object.Buffer.Read.fixed_string cur str_buf_size in
  (* Read hash table: maps string_offset -> stream_index *)
  let ht_entries = parse_hash_table cur in
  (* Resolve string offsets to actual names *)
  List.map
    (fun (str_offset, stream_idx) ->
      let name = string_at_offset str_buf str_offset in
      (name, stream_idx))
    ht_entries

(** Write the named-stream hash table. Slot placement uses
    [hash_v1(name) mod capacity] with linear probing so that LLVM's
    lookup — which computes the same [hashStringV1] on the queried name —
    finds entries at the expected bucket. *)
let write_named_hash_table buf entries capacity =
  let size = List.length entries in
  write_u32_le buf size;
  write_u32_le buf capacity;
  let buckets = Array.make capacity None in
  let present = Array.make capacity false in
  List.iter
    (fun (name, key, value) ->
      let h = Hash.hash_string_v1 name land 0xFFFFFFFF in
      let start = h mod capacity in
      let rec find_slot i =
        let idx = (start + i) mod capacity in
        if not present.(idx) then idx else find_slot (i + 1)
      in
      let idx = find_slot 0 in
      present.(idx) <- true;
      buckets.(idx) <- Some (key, value))
    entries;
  write_bit_vector buf present capacity;
  let deleted = Array.make capacity false in
  write_bit_vector buf deleted capacity;
  for i = 0 to capacity - 1 do
    match buckets.(i) with
    | Some (key, value) ->
        write_u32_le buf key;
        write_u32_le buf value
    | None -> ()
  done

let write buf entries =
  (* Build the string buffer alongside (name, offset, stream_idx) triples
     so the hash table can be placed by [hash_v1(name) mod capacity]. *)
  let str_buf = Buffer.create 64 in
  let triples =
    List.map
      (fun (name, stream_idx) ->
        let offset = Buffer.length str_buf in
        Buffer.add_string str_buf name;
        Buffer.add_char str_buf '\000';
        (name, offset, stream_idx))
      entries
  in
  let str_bytes = Buffer.contents str_buf in
  write_u32_le buf (String.length str_bytes);
  Buffer.add_string buf str_bytes;
  let size = List.length entries in
  let capacity = max 1 ((size * 3 / 2) + 1) in
  write_named_hash_table buf triples capacity
