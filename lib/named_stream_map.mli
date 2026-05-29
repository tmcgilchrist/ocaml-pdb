(** PDB Named Stream Map.

    A mapping from stream names (e.g., "/names", "/LinkInfo") to stream indices.
    Stored in the PDB Info Stream (Stream 1) using a PDB hash table with a
    string buffer. *)

type t = (string * int) list
(** Association list of (stream_name, stream_index) pairs. *)

val parse : Object.Buffer.cursor -> t
(** [parse cur] reads a named stream map from the cursor. Format: u32
    string_buffer_size, string_buffer, then a PDB hash table mapping string
    offsets to stream indices.
    Raises [Object.Buffer.Invalid_format] on truncated input. *)

val write : Stdlib.Buffer.t -> t -> unit
(** [write buf entries] serializes a named stream map. *)

(** {2 PDB Hash Table}

    Low-level PDB hash table used by the named stream map and other PDB
    structures. Uses open addressing with linear probing. *)

val parse_hash_table : Object.Buffer.cursor -> (int * int) list
(** [parse_hash_table cur] reads a PDB hash table and returns (key, value)
    pairs. *)

val write_hash_table : Stdlib.Buffer.t -> (int * int) list -> int -> unit
(** [write_hash_table buf entries capacity] writes a PDB hash table with the
    given capacity. *)
