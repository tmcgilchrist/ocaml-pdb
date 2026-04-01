(** Global/Public Symbol Index (GSI/PSI) reader.

    The GSI and PSI streams provide hash-based lookup for global and public
    symbols respectively. *)

open Pdb_types

type hash_record = {
  offset : u32;  (** Offset in the symbol record stream *)
  cref : u32;  (** Reference count *)
}

type t = { hash_records : hash_record array; hash_buckets : u32 array }

val parse_gsi : Object.Buffer.cursor -> int -> t
(** [parse_gsi cur stream_size] parses a GSI or PSI hash table. *)

type publics_header = {
  sym_hash_size : int;
  addr_map_size : int;
  num_thunks : int;
  size_of_thunk : int;
  isect_thunk_table : int;
  off_thunk_table : u32;
  num_sections : int;
}

val parse_publics_header : Object.Buffer.cursor -> publics_header
(** [parse_publics_header cur] parses the publics stream header. *)
