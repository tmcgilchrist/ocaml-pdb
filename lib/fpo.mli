(** Old-style FPO (Frame Pointer Omission) data stream.

    A flat array of 16-byte [FPO_DATA] records describing the stack frame
    layout for each function in an x86 binary. The stream index is recorded
    in {!Dbi.optional_debug_header.fpo_data}; the newer per-block
    {!Debug_subsections.FrameData} subsection (kind 0xf5) supersedes this
    format on x86-64 and ARM64, and a corresponding [new_fpo_data] stream
    is referenced from {!Dbi.optional_debug_header.new_fpo_data}.

    This module covers only the {b old} FPO_DATA format. The new format
    shares its wire layout with the C13 [FrameData] subsection -- callers
    that want the new-FPO contents can reuse {!Debug_subsections.parse_subsections}
    on that stream's bytes.

    TODO Move the LLVM reference into the fpo.ml and cross link to
    the PE/COFF spec.
    Format reference: Microsoft's [PE/COFF spec], LLVM's
    [llvm/include/llvm/Object/COFF.h] ([FpoData] struct). *)

open Pdb_types

type entry = {
  offset : u32;  (** [ulOffStart]: RVA of the function's first instruction. *)
  size : u32;  (** [cbProcSize]: function body size in bytes. *)
  num_locals : u32;  (** [cdwLocals]: locals area size in dwords. *)
  num_params : int;  (** [cdwParams]: params area size in dwords. *)
  attributes : int;
      (** Packed u16 of prolog size, saved-reg count, SEH/EBP flags and
          frame type. Kept raw; LLVM's accessor breakdown of these bits is
          known to be inconsistent with Microsoft's [cvinfo.h], so callers
          that need a particular field should decode it themselves. *)
}

type t = entry array

val parse : Object.Buffer.cursor -> int -> t
(** [parse cur total_bytes] reads [total_bytes / 16] entries. Raises
    [Object.Buffer.Invalid_format] on truncated input or when
    [total_bytes] is not a multiple of 16. *)

val write : Stdlib.Buffer.t -> t -> unit
(** [write buf t] appends the stream's bytes to [buf]. *)
