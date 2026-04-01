(** PDB hash functions.

    These correspond to the hash functions defined in PDB/include/misc.h in the
    Microsoft PDB source. *)

val hash_string_v1 : string -> int
(** [hash_string_v1 str] computes the PDB V1 hash of [str]. Corresponds to
    [Hasher::lhashPbCb] in the Microsoft PDB source. Used for the named stream
    map and TPI/IPI hash tables. *)
