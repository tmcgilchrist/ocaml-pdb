(** Type deduplication for TPI/IPI streams.

    When building a PDB, identical type records should map to the same
    TypeIndex. This module provides two levels:

    - {b Local} deduplication ({!t}, {!insert}): records with identical
      serialized bytes get the same index. Sufficient within a single
      compilation unit, where references to the same type carry the same numeric
      TypeIndex.

    - {b Cross-compilation-unit} merging ({!cross}, {!merge_types},
      {!merge_ids}): records from independently-numbered streams are rewritten
      onto a shared numbering before dedup, so structurally identical types from
      different units collapse to one entry even though their original reference
      indices differed. This mirrors LLVM's [MergingTypeTableBuilder]:
      references are remapped via {!Codeview_types.map_type_indices} rather than
      content-hashed, which avoids a BLAKE3 dependency at the cost of
      re-serialising each record. *)

type t
(** A deduplicating type table. *)

val create : unit -> t
(** Create an empty type table. *)

val insert : t -> Codeview_types.type_record -> Type_index.t
(** [insert t record] inserts a type record into the table. If an identical
    record was already inserted, returns the existing TypeIndex. Otherwise
    assigns a new index (starting from 0x1000). *)

val records : t -> Codeview_types.type_record list
(** [records t] returns all unique records in insertion order. *)

val count : t -> int
(** [count t] returns the number of unique records. *)

val find_index : t -> Codeview_types.type_record -> Type_index.t option
(** [find_index t record] looks up the TypeIndex for an identical record, or
    returns [None] if not found. *)

(** {2 Cross-compilation-unit merging} *)

type cross
(** A pair of merged tables: one for TPI (type) records and one for IPI (id)
    records, sharing a single numbering across compilation units. *)

val create_cross : unit -> cross
(** Create an empty cross-unit merger. *)

val merge_types : cross -> Codeview_types.type_record list -> Type_index.t array
(** [merge_types c records] merges one compilation unit's type records (the TPI
    records, ordered so each references only earlier records). Each record's TPI
    references are rewritten onto the shared numbering before dedup. Returns an
    array mapping this unit's local positions (position [j] = local TypeIndex
    [0x1000 + j]) to the merged {!Type_index.t}, for use when remapping the
    unit's symbols and ids. *)

val merge_ids :
  cross ->
  type_remap:Type_index.t array ->
  Codeview_types.type_record list ->
  Type_index.t array
(** [merge_ids c ~type_remap records] merges one unit's IPI (id) records.
    [type_remap] is the array returned by {!merge_types} for the same unit, used
    to rewrite the ids' references into the TPI stream; the ids' references into
    the IPI stream are rewritten via the id mapping built up as records are
    merged. Returns the local-to-merged id map. *)

val cross_types : cross -> Codeview_types.type_record list
(** Merged TPI records in assignment order, references already rewritten onto
    the shared numbering. *)

val cross_ids : cross -> Codeview_types.type_record list
(** Merged IPI records in assignment order, references already rewritten onto
    the shared numbering. *)
