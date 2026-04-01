(** PDB Info Stream (Stream 1) writer. *)

val write : Stdlib.Buffer.t -> Pdb_stream.t -> unit
(** [write buf info] serializes a PDB Info Stream. *)
