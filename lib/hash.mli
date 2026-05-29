(** PDB hash functions.

    These correspond to the hash functions defined in PDB/include/misc.h in the
    Microsoft PDB source. *)

val hash_string_v1 : string -> int
(** [hash_string_v1 str] computes the PDB V1 hash of [str]. Corresponds to
    [Hasher::lhashPbCb] in the Microsoft PDB source. Used for the named stream
    map and TPI/IPI hash tables. *)

val hash_buffer_v8 : string -> int
(** [hash_buffer_v8 data] computes the CRC32 hash of [data] with initial value 0
    (JamCRC). Used for TPI/IPI type record hashing. Corresponds to [SigForPbCb]
    / [hashBufferV8] in the PDB source. *)
