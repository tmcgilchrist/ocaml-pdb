(** High-level PDB file builder.

    Assembles a complete PDB file from structured inputs. Handles stream
    index assignment and cross-referencing between DBI, TPI, IPI, GSI/PSI,
    module streams, and the /names string table automatically. *)

open Pdb_types

(** Target machine architecture for the PDB. *)
type machine =
  | I386 (** x86 32-bit *)
  | AMD64 (** x86-64 *)
  | ARM (** ARM 32-bit *)
  | ARM64 (** ARM 64-bit (AArch64) *)

type module_desc = {
  name : string;
  obj_file : string;
  symbols : Codeview_symbols.symbol_record list;
  subsections : Debug_subsections.subsection list;
  section_contrib : Dbi.section_contribution option;
  source_files : string list;
      (** Source filenames for this compilation unit. Goes into the DBI
          FileInfo substream and is reported by llvm-pdbutil's [--files] /
          [--modules] (# files) output. Use [[]] when none. *)
}
(** Description of a compilation module. *)

type t
(** A PDB file builder. *)

val create : ?guid:guid -> ?age:int -> machine -> t
(** [create ~guid ~age machine] creates a new PDB builder.

    @param guid A 16-byte unique identifier linking the PDB to its PE
      executable. The PE's debug directory stores the same GUID so the
      debugger can verify it loaded the correct PDB. If omitted, a
      zero GUID is used (suitable for testing; production use should
      generate one by hashing the PDB contents or using a random UUID).

    @param age Incremental link counter. Starts at 1 for a fresh build;
      incremental links bump it. The PE also stores the age so the debugger
      can reject stale PDBs. Defaults to 1. *)

val add_type : t -> Codeview_types.type_record -> u32
(** [add_type t record] adds a type record to the TPI stream.
    Returns the assigned TypeIndex (starting from 0x1000). *)

val add_id : t -> Codeview_types.type_record -> u32
(** [add_id t record] adds an ID record to the IPI stream.
    Returns the assigned TypeIndex (starting from 0x1000). *)

val add_module : t -> module_desc -> unit
(** [add_module t desc] adds a compilation module with its symbols
    and debug subsections. *)

val add_public : t -> Codeview_symbols.symbol_record -> unit
(** [add_public t sym] adds a public symbol (typically S_PUB32). *)

val add_global : t -> Codeview_symbols.symbol_record -> unit
(** [add_global t sym] adds a global symbol (S_GDATA32, S_CONSTANT, etc.). *)

val add_string : t -> string -> int
(** [add_string t str] adds a string to the /names table.
    Returns the byte offset. *)

val finalize : t -> string
(** [finalize t] produces the complete PDB file as a byte string.
    The builder should not be used after this call. *)

val machine_to_int : machine -> int
(** [machine_to_int m] returns the raw COFF machine constant. *)
