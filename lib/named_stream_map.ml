(** PDB Named Stream Map.

    References:
    - LLVM: llvm/lib/DebugInfo/PDB/Native/NamedStreamMap.cpp
    - LLVM: llvm/include/llvm/DebugInfo/PDB/Native/HashTable.h
    - LLVM: llvm/lib/DebugInfo/PDB/Native/HashTable.cpp *)

type t = (string * int) list

module Buffer = Stdlib.Buffer

let write_u32_le buf v =
  Buffer.add_char buf (Char.chr (v land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 16) land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 24) land 0xFF))

(** {2 Sparse bit vector serialization}

    Format: u32 num_words, then num_words x u32 words. Bit i of word j
    represents index (j*32 + i). *)

let parse_bit_vector (cur : Object.Buffer.cursor) : bool array =
  let num_words = Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int in
  let bits = Array.make (num_words * 32) false in
  for i = 0 to num_words - 1 do
    let word = Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int in
    for j = 0 to 31 do
      if word land (1 lsl j) <> 0 then bits.((i * 32) + j) <- true
    done
  done;
  bits

let write_bit_vector (buf : Buffer.t) (present : bool array) (capacity : int) =
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

let parse_hash_table (cur : Object.Buffer.cursor) : (int * int) list =
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

let write_hash_table (buf : Buffer.t) (entries : (int * int) list)
    (capacity : int) : unit =
  let size = List.length entries in
  write_u32_le buf size;
  write_u32_le buf capacity;
  (* Build present bit vector by hashing keys into buckets.
     Use linear probing to place entries. *)
  let buckets = Array.make capacity None in
  let present = Array.make capacity false in
  List.iter
    (fun (key, value) ->
      (* For the named stream map, the key is a string offset.
         The hash is computed from the string, but at this level we just
         need to place entries. We use a simple distribution. *)
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
let string_at_offset (str_buf : string) (offset : int) : string =
  let len = String.length str_buf in
  let end_pos = ref offset in
  while !end_pos < len && str_buf.[!end_pos] <> '\000' do
    incr end_pos
  done;
  String.sub str_buf offset (!end_pos - offset)

let parse (cur : Object.Buffer.cursor) : t =
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

let write (buf : Buffer.t) (entries : t) : unit =
  (* Build the string buffer *)
  let str_buf = Buffer.create 64 in
  let offsets =
    List.map
      (fun (name, stream_idx) ->
        let offset = Buffer.length str_buf in
        Buffer.add_string str_buf name;
        Buffer.add_char str_buf '\000';
        (offset, stream_idx))
      entries
  in
  let str_bytes = Buffer.contents str_buf in
  (* Write string buffer size + contents *)
  write_u32_le buf (String.length str_bytes);
  Buffer.add_string buf str_bytes;
  (* Write hash table.
     Capacity should be at least 2/3 larger than size to respect load factor. *)
  let size = List.length entries in
  let capacity = max 1 ((size * 3 / 2) + 1) in
  write_hash_table buf offsets capacity
