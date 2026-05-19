(** A CodeView TypeIndex, symbolically distinguished between built-in
    primitives and user-defined records.

    Values < 0x1000 in the wire format encode a built-in type as
    [(mode lsl 8) lor kind]. Values >= 0x1000 are positional indices into
    the TPI or IPI stream. [Type_index.t] preserves this distinction:
    decoded simple types appear as [Simple], everything else as [User]. *)

open Pdb_types

type t =
  | Simple of {
      kind : Codeview_constants.simple_type_kind;
      mode : Codeview_constants.simple_type_mode;
    }  (** Built-in primitive (TypeIndex < 0x1000). *)
  | User of u32
      (** TypeIndex >= 0x1000 (positional in TPI/IPI) -- or a < 0x1000
          value whose kind or mode byte we don't recognise. *)

(** {2 Serialisation} *)

val to_u32 : t -> u32
val of_u32 : u32 -> t

(** {2 Builders} *)

val simple :
  ?mode:Codeview_constants.simple_type_mode ->
  Codeview_constants.simple_type_kind ->
  t
(** [simple ?mode kind] builds a [Simple] value. Default mode is
    [Direct] (not a pointer). *)

val user : u32 -> t
(** [user n] wraps a positional TypeIndex (the value is the literal
    wire-format integer, e.g. [user (Unsigned.UInt32.of_int 0x1001)]). *)

(** {2 Predicates} *)

val is_simple : t -> bool
(** [true] iff this is a [Simple]. *)

val is_none : t -> bool
(** [true] for [Simple {kind=None; mode=Direct}] (TypeIndex 0). The PDB
    format uses this as the "no type" sentinel for optional references. *)

(** {2 Convenience values for common primitives}

    Shorthands for the simple types you reach for most often. Equivalent
    to [simple ?mode kind]; use whichever reads better. *)

val void : t
val int32 : t
val uint32 : t
val int64 : t
val uint64 : t
val char_ptr32 : t
val void_ptr32 : t

(** {2 LF_POINTER attribute values}
    TODO Move closer to Pointer type?
    Not a TypeIndex -- this is the value for the [attrs] field of an
    [Codeview_types.Pointer] record. Kept here because most callers
    reach for both a pointee type and an attrs value together. *)

val near32_pointer_attrs : u32
(** 0x800A: 32-bit near pointer of size 4. *)

val near64_pointer_attrs : u32
(** 0x1000C: 64-bit near pointer of size 8. *)
