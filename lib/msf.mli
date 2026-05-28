(** MSF (Multi-Stream File) container reader.

    PDB files use the MSF format, a page-based virtual filesystem. The file is
    divided into fixed-size blocks (typically 4096 bytes). Logical streams are
    mapped onto sequences of these blocks, which need not be contiguous. *)

open Pdb_types

(** {2 Superblock}

    The superblock occupies the first block of the file and contains metadata
    about the MSF layout. *)

type superblock = {
  block_size : u32;  (** Bytes per block; must be 512/1024/2048/4096. *)
  free_block_map_block : u32;
      (** Index of the active Free Page Map block (always 1 or 2). *)
  num_blocks : u32;  (** Total number of blocks in the file. *)
  num_directory_bytes : u32;
      (** Byte size of the stream directory, used to determine how many
          directory blocks the block map points at. *)
  block_map_addr : u32;
      (** Block index of the block map (an array of directory block
          indices). *)
}

val msf_magic : string
(** The 32-byte magic string at the start of every MSF/PDB file:
    ["Microsoft C/C++ MSF 7.00\r\n\x1aDS\x00\x00\x00"] *)

(** {2 Parsed MSF} *)

type t
(** A parsed MSF container with all streams reassembled. *)

val read : Object.Buffer.t -> t
(** [read buffer] parses an MSF file from [buffer], validating the
    superblock and reassembling all streams from their block lists.
    @raise Object.Buffer.Invalid_format on malformed input. *)

val superblock : t -> superblock
(** The validated superblock. *)

val stream_count : t -> int
(** Number of logical streams (including any zero-byte ones). *)

val get_stream : t -> int -> Object.Buffer.t option
(** [get_stream t idx] returns the reassembled bytes of stream [idx], or
    [None] if [idx] is out of range. *)

val get_stream_exn : t -> int -> Object.Buffer.t
(** Like {!get_stream} but raises {!Object.Buffer.Invalid_format} on an
    out-of-range index. *)
