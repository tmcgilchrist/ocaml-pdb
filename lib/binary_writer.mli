(** Little-endian write helpers and a handful of read aliases.

    Internal-only conveniences over {!Stdlib.Buffer.t} and
    {!Object.Buffer.Read}. Every PDB writer needs the same set of
    [write_u8 / write_u16_le / write_u32_le / write_u64_le / write_i32_le]
    primitives; collecting them in one module avoids a copy per file. The
    [read_*] aliases unwrap the {!Object.Buffer.Read} return types
    ({!Unsigned.UInt16.t}, etc.) into plain ints where most callers want
    them. *)

(** {2 Writes} *)

val write_u8 : Stdlib.Buffer.t -> int -> unit
(** Append a single byte. Only the low 8 bits of the argument are used. *)

val write_u16_le : Stdlib.Buffer.t -> int -> unit
(** Append a little-endian 16-bit integer. Only the low 16 bits are used. *)

val write_u32_le : Stdlib.Buffer.t -> int -> unit
(** Append a little-endian 32-bit integer. Only the low 32 bits are used. *)

val write_u64_le : Stdlib.Buffer.t -> int64 -> unit
(** Append a little-endian 64-bit integer. *)

val write_i32_le : Stdlib.Buffer.t -> int32 -> unit
(** Append a little-endian signed 32-bit integer. *)

val write_cstring : Stdlib.Buffer.t -> string -> unit
(** Append the bytes of [s] followed by a null terminator. *)

val write_padding_to_align : Stdlib.Buffer.t -> int -> unit
(** [write_padding_to_align buf alignment] appends zero bytes until the
    buffer's length is a multiple of [alignment]. *)

(** {2 Reads}

    Thin wrappers that read from an {!Object.Buffer.cursor} and unwrap
    the unsigned-integer return type into [int]. *)

val read_u8 : Object.Buffer.cursor -> int
val read_u16 : Object.Buffer.cursor -> int
val read_u32 : Object.Buffer.cursor -> Pdb_types.u32
val read_i32 : Object.Buffer.cursor -> int32

val read_cstring : Object.Buffer.cursor -> string
(** Read a null-terminated string, returning [""] if the cursor is at the
    terminator already or past it. *)
