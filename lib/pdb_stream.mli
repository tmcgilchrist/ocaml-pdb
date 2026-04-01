(** PDB Info Stream (Stream 1) reader.

    The PDB Info Stream is always at stream index 1 in the MSF container.
    It contains the PDB version, a unique identifier (GUID + age), and
    the named stream map which maps stream names to indices. *)

open Pdb_types

type pdb_version =
  | VC70
  | VC80
  | VC110
  | VC140
  | Unknown of int

type feature =
  | ContainsIdStream
  | NoTypeMerging
  | MinimalDebugInfo

type t = {
  version : pdb_version;
  signature : u32;
  age : u32;
  guid : guid;
  named_streams : Named_stream_map.t;
  features : feature list;
}

val read : Object.Buffer.cursor -> t
(** [read cur] parses the PDB Info Stream from the cursor position. *)

val pdb_version_to_int : pdb_version -> int
val int_to_pdb_version : int -> pdb_version
val string_of_pdb_version : pdb_version -> string
