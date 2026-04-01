(** TPI/IPI stream writer. *)

val write : Stdlib.Buffer.t -> Codeview_types.type_record list -> unit
(** [write buf records] serializes a complete TPI/IPI stream including the
    header and all type records. *)
