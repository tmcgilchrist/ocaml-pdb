(** Shared test helpers. *)

(** Build an [Object.Buffer.t] from a string. The buffer's contents are
    the bytes of [s], in the same order. *)
let buffer_of_string s =
  let len = String.length s in
  let buf =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout len
  in
  for i = 0 to len - 1 do
    buf.{i} <- Char.code s.[i]
  done;
  buf

(** Extract the contents of an [Object.Buffer.t] as a string. *)
let string_of_buffer (buf : Object.Buffer.t) =
  let len = Bigarray.Array1.dim buf in
  let s = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set s i (Char.chr buf.{i})
  done;
  Bytes.to_string s

(** Build a {!Pdb.Type_index.t} from a raw int wire value. *)
let ti n = Pdb.Type_index.of_u32 (Unsigned.UInt32.of_int n)

(** Unwrap a {!Pdb.Type_index.t} back to an int (for assertions). *)
let ti_to_int t = Unsigned.UInt32.to_int (Pdb.Type_index.to_u32 t)
