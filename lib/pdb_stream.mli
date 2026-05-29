(** PDB Info Stream (Stream 1) reader.

    The PDB Info Stream is always at stream index 1 in the MSF container. It
    contains the PDB version, a unique identifier (GUID + age), and the named
    stream map which maps stream names to indices. *)

open Pdb_types

(** PDB toolchain version recorded in the info stream's [version] field. The
    integer values come from Microsoft's [cvinfo.h] [PDB_IMPL_VER_*] constants
    -- they look like dates (e.g. VC70 = 20000404). [Unknown] holds the raw
    integer for any version this library does not recognise. *)
type pdb_version = VC70 | VC80 | VC110 | VC140 | Unknown of int

(** Feature signatures listed at the end of the info stream. Their main purpose
    is to tell consumers whether the IPI stream is present ([ContainsIdStream])
    and whether the TPI has been pre-deduplicated ([NoTypeMerging]) or stripped
    down to a minimal subset ([MinimalDebugInfo]). *)
type feature = ContainsIdStream | NoTypeMerging | MinimalDebugInfo

type t = {
  version : pdb_version;  (** PDB format version (typically [VC70]). *)
  signature : u32;
      (** Originally a [time_t] of when the PDB was created. Modern toolchains
          often set it to zero. *)
  age : u32;
      (** Bumped on every incremental link; the matching PE executable's debug
          directory carries the same age so the debugger can reject stale PDBs.
      *)
  guid : guid;
      (** Identifies this PDB. Combined with [age], used to match a PDB to its
          PE executable. *)
  named_streams : Named_stream_map.t;
      (** Maps named streams (e.g. ["/names"], ["/UDTSRCLINEUNDONE"]) to MSF
          stream indices. *)
  features : feature list;
}
(** Parsed contents of the PDB Info Stream. *)

val parse : Object.Buffer.cursor -> t
(** [parse cur] parses the PDB Info Stream from the cursor position. Raises
    [Object.Buffer.Invalid_format] if the stream is truncated. *)

val pdb_version_to_int : pdb_version -> int
(** Map a {!pdb_version} to its raw integer value. *)

val int_to_pdb_version : int -> pdb_version
(** Recognise an integer version value; unrecognised values become {!Unknown}.
*)

val string_of_pdb_version : pdb_version -> string
(** Human-readable name, e.g. ["VC70"], ["Unknown(12345)"]. *)
