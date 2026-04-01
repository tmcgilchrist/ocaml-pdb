(** MSF (Multi-Stream File) container writer.

    Builds an MSF file from a collection of streams. Each stream is accumulated
    as bytes, then laid out into pages when finalized. *)

type t
(** An MSF file builder. *)

val create : block_size:int -> t
(** [create ~block_size] creates a new MSF builder. [block_size] must be 512,
    1024, 2048, or 4096. *)

val add_stream : t -> string -> int
(** [add_stream t contents] adds a stream with the given contents. Returns the
    stream index (0-based). *)

val add_empty_stream : t -> int
(** [add_empty_stream t] adds an empty stream. Returns the stream index. *)

val finalize : t -> string
(** [finalize t] produces the complete MSF file as a string. The builder should
    not be used after this call. *)
