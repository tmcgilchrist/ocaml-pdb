(** PDB Info Stream (Stream 1) writer.

    Counterpart to {!Pdb_stream}. Emits version, signature, age, GUID,
    named stream map, and any feature signatures. *)

val write : Stdlib.Buffer.t -> Pdb_stream.t -> unit
(** [write buf info] serializes a PDB Info Stream. *)
