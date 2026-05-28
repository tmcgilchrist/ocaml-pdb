(** Global/Public Symbol Index (GSI/PSI) reader.

    The GSI and PSI streams provide hash-based lookup for global and
    public symbols respectively. Both share the same on-disk
    {!hash_record}/{!hash_buckets} table format; the PSI stream is
    prefixed with an additional {!publics_header} carrying address-map
    and incremental-link thunk metadata. *)

open Pdb_types

type hash_record = {
  offset : u32;  (** Byte offset of this symbol within the shared symbol record stream. *)
  cref : u32;  (** Reference count (always 1 in practice). *)
}

type t = {
  hash_records : hash_record array;
      (** All symbols' records, in the order they appear in the stream. *)
  hash_buckets : u32 array;
      (** Bit-vector header followed by per-bucket offsets into
          {!hash_records}. Each present bucket holds a [u32] offset
          into the [hash_records] array (in bytes; divide by
          [sizeof (hash_record) = 12] for the index). *)
}

val parse_gsi : Object.Buffer.cursor -> int -> t
(** [parse_gsi cur total_bytes] parses a GSI or PSI hash table from the
    cursor; [total_bytes] is the size of the stream in bytes.
    @raise Object.Buffer.Invalid_format on a truncated stream. *)

type publics_header = {
  sym_hash_size : int;
      (** Byte size of the hash table that follows this header (passed
          to {!parse_gsi}). *)
  addr_map_size : int;
      (** Byte size of the address-to-symbol map immediately after the
          hash table. *)
  num_thunks : int;
      (** Count of incremental-link thunks. *)
  size_of_thunk : int;  (** Byte size of each thunk record. *)
  isect_thunk_table : int;
      (** Section index of the thunk table within the linked image. *)
  off_thunk_table : u32;  (** Byte offset of the thunk table in that section. *)
  num_sections : int;
      (** Number of section-map entries following the thunk table. *)
}

val parse_publics_header : Object.Buffer.cursor -> publics_header
(** [parse_publics_header cur] parses the 28-byte header prefix of the
    PSI stream.
    @raise Object.Buffer.Invalid_format on truncated input. *)
