(** A CodeView TypeIndex, symbolically distinguished between built-in
    primitives and user-defined records.

    Values < 0x1000 in the wire format encode a built-in type as
    [(mode lsl 8) lor kind]. Values >= 0x1000 are positional indices into
    the TPI or IPI stream. {!t} preserves this distinction: decoded
    simple types appear as {!Simple}, everything else as {!User}. *)

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
(** Encode to the on-disk u32 representation. *)

val of_u32 : u32 -> t
(** Decode the on-disk u32. Values [>= 0x1000] become {!User}; values
    [< 0x1000] are decoded into {!Simple} when both the kind and mode
    bytes are recognised, otherwise the raw value is wrapped as {!User}
    so the round-trip stays lossless. *)

(** {2 Builders} *)

val simple :
  ?mode:Codeview_constants.simple_type_mode ->
  Codeview_constants.simple_type_kind ->
  t
(** [simple ?mode kind] builds a {!Simple} value. Default mode is
    [Direct] (not a pointer). *)

val user : u32 -> t
(** [user n] wraps a positional TypeIndex (the value is the literal
    wire-format integer, e.g. [user (Unsigned.UInt32.of_int 0x1001)]). *)

(** {2 Predicates} *)

val is_simple : t -> bool
(** [true] iff this is a {!Simple}. *)

val is_none : t -> bool
(** [true] for [Simple {kind=None; mode=Direct}] (TypeIndex 0). The PDB
    format uses this as the "no type" sentinel for optional references. *)

(** {2 Convenience values for common primitives}

    Shorthands for the simple types you reach for most often. Each is
    equivalent to a particular [simple ?mode kind] call; use whichever
    form reads better at the call site. *)

val void : t
(** [simple Void]. *)

val int32 : t
(** [simple Int32] -- raw value [0x0074]. *)

val uint32 : t
(** [simple UInt32] -- raw value [0x0075]. *)

val int64 : t
(** [simple Int64] -- raw value [0x0076]. *)

val uint64 : t
(** [simple UInt64] -- raw value [0x0077]. *)

val char_ptr32 : t
(** [simple ~mode:NearPointer32 NarrowCharacter] -- a 32-bit pointer to
    [char]. *)

val void_ptr32 : t
(** [simple ~mode:NearPointer32 Void] -- a 32-bit [void*]. *)

(** {2 LF_POINTER attribute values}

    Not TypeIndexes -- the values for the [attrs] field of an
    {!Codeview_types.Pointer} record. They live alongside {!t} because
    callers reach for both a pointee type and a pointer-attrs word
    together; placing them in {!Codeview_types} would create a circular
    dependency with this module. *)

val near32_pointer_attrs : u32
(** [0x800A]: 32-bit near pointer of size 4. *)

val near64_pointer_attrs : u32
(** [0x1000C]: 64-bit near pointer of size 8. *)
