(** ARM64 (AArch64) Windows unwind information (.xdata).

    This module parses and writes ARM64 UNWIND_INFO structures from the .xdata
    section of PE files. The ARM64 format is substantially different from
    x86-64: unwind codes are variable-length (1-4 bytes) and encode operations
    specific to the AArch64 instruction set.

    References:
    - Microsoft:
      https://docs.microsoft.com/en-us/cpp/build/arm64-exception-handling *)

open Pdb_types

(** ARM64 unwind operation. Each describes one prolog instruction. *)
type unwind_code =
  | AllocSmall of { size : int }
  | AllocMedium of { size : int }
  | AllocLarge of { size : int }
  | SaveFPLR of { offset : int }
  | SaveFPLRX of { offset : int }
  | SaveR19R20X of { offset : int }
  | SaveRegP of { reg : int; offset : int }
  | SaveRegPX of { reg : int; offset : int }
  | SaveReg of { reg : int; offset : int }
  | SaveRegX of { reg : int; offset : int }
  | SaveLRPair of { reg : int; offset : int }
  | SaveFRegP of { reg : int; offset : int }
  | SaveFRegPX of { reg : int; offset : int }
  | SaveFReg of { reg : int; offset : int }
  | SaveFRegX of { reg : int; offset : int }
  | SetFP
  | AddFP of { offset : int }
  | Nop
  | End
  | SaveNext
  | PACSignLR
  | TrapFrame
  | MachineFrame
  | Context
  | ClearUnwoundToCall

type unwind_info = {
  function_length : int;
  has_exception_data : bool;
  codes : unwind_code list;
  exception_handler : u32 option;
}
(** ARM64 .xdata header information. *)

val parse : Object.Buffer.cursor -> unwind_info
(** [parse cur] parses an ARM64 UNWIND_INFO from .xdata. Raises
    [Object.Buffer.Invalid_format] on a truncated header or trailing section. *)

val write : Stdlib.Buffer.t -> unwind_info -> unit
(** [write buf info] serializes ARM64 UNWIND_INFO. *)
