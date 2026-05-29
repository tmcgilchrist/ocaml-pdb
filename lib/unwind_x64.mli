(** x86-64 Windows unwind information (.xdata).

    This module parses and writes UNWIND_INFO structures from the .xdata
    section of PE files. These are the Windows equivalent of DWARF
    .eh_frame / .debug_frame CFI (Call Frame Information).

    Each non-leaf function has an UNWIND_INFO describing its prolog
    operations (register saves, stack allocation, frame pointer setup).
    The Windows unwinder uses this to restore the caller's state. *)

open Pdb_types

(** x86-64 register encoding for unwind codes. *)
type register =
  | RAX | RCX | RDX | RBX | RSP | RBP | RSI | RDI
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

(** Individual unwind operation describing one prolog instruction. *)
type unwind_code =
  | PushNonVol of { code_offset : int; reg : register }
  | AllocSmall of { code_offset : int; size : int }
  | AllocLarge of { code_offset : int; size : int }
  | SetFPReg of { code_offset : int }
  | SaveNonVol of { code_offset : int; reg : register; offset : int }
  | SaveNonVolFar of { code_offset : int; reg : register; offset : int }
  | SaveXMM128 of { code_offset : int; reg : int; offset : int }
  | SaveXMM128Far of { code_offset : int; reg : int; offset : int }
  | PushMachFrame of { code_offset : int; error_code : bool }

(** Flags on an UNWIND_INFO structure. *)
type unwind_flags = {
  exception_handler : bool;
  termination_handler : bool;
  chain_info : bool;
}

(** A parsed UNWIND_INFO structure from .xdata. *)
type unwind_info = {
  version : int;
  flags : unwind_flags;
  size_of_prolog : int;
  frame_register : register option;
  frame_offset : int;
  unwind_codes : unwind_code list;
  exception_handler : u32 option;
}

val parse : Object.Buffer.cursor -> unwind_info
(** [parse cur] parses an UNWIND_INFO structure from the cursor.
    Raises [Object.Buffer.Invalid_format] on a truncated header, missing
    codes, or missing trailing exception handler RVA. *)

val write : Stdlib.Buffer.t -> unwind_info -> unit
(** [write buf info] serializes an UNWIND_INFO structure. *)

val register_to_int : register -> int
(** Encode a {!register} to its 4-bit wire value (0..15). *)

val int_to_register : int -> register
(** Decode the 4-bit wire value back to a {!register}.
    Raises [Invalid_argument] on values outside [0..15]. *)
