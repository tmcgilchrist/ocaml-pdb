(** Windows structured exception handling / stack unwinding.

    This module provides a unified entry point for Windows unwind information
    (.xdata section), re-exporting the architecture-specific implementations.

    Windows x86-64 and ARM64 use fundamentally different unwind formats: x86-64
    uses 2-byte slot-based codes with a fixed header, while ARM64 uses
    variable-length byte codes with a bitfield header. Unlike DWARF CFI (which
    is one format with architecture-parameterized registers), these are
    genuinely distinct formats that share only the high-level purpose.

    Consumers should use {!module-X64} or {!module-Arm64} directly based on the
    target architecture. The {!arch} type and {!parse} function provide a
    convenient dispatch when the architecture is determined at runtime. *)

(** Target architecture for unwind information. *)
type arch = X64 | Arm64

(** Architecture-tagged unwind information. *)
type t =
  | X64_unwind of Unwind_x64.unwind_info
  | Arm64_unwind of Unwind_arm64.unwind_info

val parse : arch -> Object.Buffer.cursor -> t
(** [parse arch cur] parses unwind information from the cursor, dispatching to
    the appropriate architecture-specific parser. *)

val write : Stdlib.Buffer.t -> t -> unit
(** [write buf info] serializes unwind information using the appropriate
    architecture-specific writer. *)

module X64 = Unwind_x64
(** x86-64 unwind information. *)

module Arm64 = Unwind_arm64
(** ARM64 (AArch64) unwind information. *)
