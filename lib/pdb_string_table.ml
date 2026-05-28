(** PDB global string table (/names stream).

    References:
    - LLVM: llvm/lib/DebugInfo/PDB/Native/PDBStringTable.cpp
    - LLVM: llvm/lib/DebugInfo/PDB/Native/PDBStringTableBuilder.cpp
    - LLVM: llvm/include/llvm/DebugInfo/PDB/Native/RawTypes.h *)

module Buffer = Stdlib.Buffer

(* PDB string table constants *)
open Binary_writer

let pdb_string_table_signature = 0xEFFEEFFE
let pdb_string_table_hash_version_1 = 1

type t = {
  mutable names_buf : Buffer.t;
  mutable offsets : (string, int) Hashtbl.t;
  mutable count : int;
}

let create () =
  let t =
    { names_buf = Buffer.create 256; offsets = Hashtbl.create 64; count = 0 }
  in
  (* The names buffer starts with a null byte (offset 0 = empty string) *)
  Buffer.add_char t.names_buf '\000';
  t

let add_string t str =
  match Hashtbl.find_opt t.offsets str with
  | Some offset -> offset
  | None ->
      let offset = Buffer.length t.names_buf in
      Buffer.add_string t.names_buf str;
      Buffer.add_char t.names_buf '\000';
      Hashtbl.replace t.offsets str offset;
      t.count <- t.count + 1;
      offset

let lookup t str = Hashtbl.find_opt t.offsets str
let count t = t.count

(* Writing helpers *)
let write (buf : Buffer.t) (t : t) : unit =
  let names_bytes = Buffer.contents t.names_buf in
  let byte_size = String.length names_bytes in
  (* Compute bucket count: use ~2x the number of strings for load factor *)
  let bucket_count = max 1 (t.count * 2 + 1) in
  (* Build hash table: buckets[hash % bucket_count] = offset *)
  let buckets = Array.make bucket_count 0 in
  Hashtbl.iter
    (fun str offset ->
      let h = Hash.hash_string_v1 str in
      let h = if h < 0 then h + max_int + 1 else h in
      let start = h mod bucket_count in
      (* Linear probe for empty slot *)
      let rec find_slot i =
        let idx = (start + i) mod bucket_count in
        if buckets.(idx) = 0 then idx
        else find_slot (i + 1)
      in
      let idx = find_slot 0 in
      buckets.(idx) <- offset)
    t.offsets;
  (* Write header *)
  write_u32_le buf pdb_string_table_signature;
  write_u32_le buf pdb_string_table_hash_version_1;
  write_u32_le buf byte_size;
  (* Write names buffer *)
  Buffer.add_string buf names_bytes;
  (* Write hash table *)
  write_u32_le buf bucket_count;
  Array.iter (write_u32_le buf) buckets;
  (* Write epilogue: string count *)
  write_u32_le buf t.count

let parse (cur : Object.Buffer.cursor) : t =
  (* PDBStringTableHeader: u32 signature, u32 hash_version, u32 byte_size. *)
  Object.Buffer.ensure cur 12 "/names string table: truncated header";
  let signature = Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int in
  if signature <> pdb_string_table_signature then
    Object.Buffer.invalid_format
      (Printf.sprintf
         "/names string table: bad signature 0x%08x (expected 0x%08x)" signature
         pdb_string_table_signature);
  let _hash_version =
    Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int
  in
  let byte_size =
    Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int
  in
  Object.Buffer.ensure cur byte_size
    (Printf.sprintf "/names string table: names buffer (%d bytes) overruns"
       byte_size);
  let names_bytes = Object.Buffer.Read.fixed_string cur byte_size in
  (* Read hash table *)
  Object.Buffer.ensure cur 4 "/names string table: missing bucket_count";
  let bucket_count =
    Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int
  in
  Object.Buffer.ensure cur
    ((bucket_count * 4) + 4)
    (Printf.sprintf
       "/names string table: %d buckets + epilogue exceed stream end"
       bucket_count);
  let _buckets =
    Array.init bucket_count (fun _ ->
        Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int)
  in
  (* Read epilogue *)
  let string_count =
    Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int
  in
  (* Reconstruct the table *)
  let t =
    {
      names_buf = Buffer.create byte_size;
      offsets = Hashtbl.create (string_count * 2);
      count = string_count;
    }
  in
  Buffer.add_string t.names_buf names_bytes;
  (* Walk the names buffer to rebuild the offset map *)
  let pos = ref 1 in
  (* skip leading null *)
  while !pos < byte_size do
    let start = !pos in
    while !pos < byte_size && names_bytes.[!pos] <> '\000' do
      incr pos
    done;
    if start < !pos then begin
      let str = String.sub names_bytes start (!pos - start) in
      Hashtbl.replace t.offsets str start
    end;
    incr pos
  done;
  t
