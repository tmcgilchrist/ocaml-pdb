(** OMAP address-translation streams.

    OMAP streams appear in PDBs for binaries that have been reordered after
    initial compilation (typically by Microsoft's binary basic-block tool
    BBT, profile-guided optimisation, or post-link rebasing). They let a
    debugger translate between the original RVA recorded in symbol/line
    information and the post-transform RVA in the executable.

    Two OMAP streams are referenced from the DBI optional debug header:
    {!Dbi.optional_debug_header.omap_to_src} maps {b current} (executable)
    RVAs back to {b source} (original) RVAs; {!Dbi.optional_debug_header.omap_from_src}
    maps the other direction. Both have the same on-disk format: a flat,
    sorted array of 8-byte {!entry} records.

    The OMAP wire format has no LLVM reference -- LLVM never emits it.
    The canonical specification is Microsoft's [cvinfo.h] and the
    [microsoft/microsoft-pdb] source dump. *)

open Pdb_types

type entry = { rva : u32; rva_to : u32 }
(** A single address mapping. [rva] is the start of an interval in the
    source coordinate space; [rva_to] is its image in the target space.
    [rva_to = 0] is the sentinel "unmapped" -- the address range was
    eliminated by the transform. *)

type t = entry array
(** An OMAP stream, as a flat array of entries sorted by [rva]. *)

val parse : Object.Buffer.cursor -> int -> t
(** [parse cur total_bytes] reads [total_bytes / 8] entries from the
    cursor. Raises {!Object.Buffer.Invalid_format} on truncated input. *)

val write : Stdlib.Buffer.t -> t -> unit
(** [write buf t] appends the OMAP stream's bytes to [buf]. The entries
    are written in the order given; callers must ensure they are sorted
    by [rva] for {!lookup} to work. *)

val lookup : t -> u32 -> u32 option
(** [lookup t rva] returns the translated address for [rva], or [None]
    if [rva] falls below the first entry or lands in an unmapped
    interval ([rva_to = 0]). When [rva] equals an [entry.rva] exactly
    it returns that entry's [rva_to]; when it falls between two entries
    it returns [entry.rva_to + (rva - entry.rva)] for the preceding
    entry. *)
