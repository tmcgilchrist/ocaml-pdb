(** PDB global string table (/names stream).

    The /names stream is a global string table used by file checksums
    and other debug subsections. Strings are stored null-terminated in a
    contiguous buffer, referenced by byte offset. A hash table provides
    fast lookup by string content. *)

type t
(** A PDB string table. *)

val create : unit -> t
(** Create an empty string table. *)

val add_string : t -> string -> int
(** [add_string t str] adds a string to the table and returns its byte
    offset. If the string was already added, returns the existing offset. *)

val write : Stdlib.Buffer.t -> t -> unit
(** [write buf t] serializes the string table to the /names stream format:
    header (signature + version + byte size), names buffer, hash table,
    string count epilogue. *)

val parse : Object.Buffer.cursor -> t
(** [parse cur] reads a /names stream from the cursor position.
    Raises [Object.Buffer.Invalid_format] on truncated input or when the
    leading signature is not [0xEFFEEFFE]. *)

val lookup : t -> string -> int option
(** [lookup t str] finds the byte offset of a string, or [None]. *)

val count : t -> int
(** Number of strings in the table. *)
