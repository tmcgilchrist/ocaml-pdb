(** Type deduplication for TPI/IPI streams.

    When building a PDB, identical type records should map to the same
    TypeIndex. This module provides local deduplication: records with
    identical serialized bytes get the same index.

    This is sufficient for single-compilation-unit use (e.g., a compiler
    emitting one PDB). Cross-compilation-unit merging (where TypeIndex
    references differ but types are structurally identical) requires
    global hashing, which is not yet implemented. *)

open Pdb_types

type t
(** A deduplicating type table. *)

val create : unit -> t
(** Create an empty type table. *)

val insert : t -> Codeview_types.type_record -> u32
(** [insert t record] inserts a type record into the table. If an
    identical record was already inserted, returns the existing
    TypeIndex. Otherwise assigns a new index (starting from 0x1000). *)

val records : t -> Codeview_types.type_record list
(** [records t] returns all unique records in insertion order. *)

val count : t -> int
(** [count t] returns the number of unique records. *)

val find_index : t -> Codeview_types.type_record -> u32 option
(** [find_index t record] looks up the TypeIndex for an identical record,
    or returns [None] if not found. *)
