(** DBI (Debug Information) stream writer. *)

val write :
  Stdlib.Buffer.t ->
  Dbi.module_info list ->
  Dbi.section_contribution list ->
  machine:int ->
  unit
(** [write buf modules section_contribs ~machine] serializes a DBI stream. *)
