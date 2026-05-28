(** OMAP address-translation stream reader/writer.

    Wire format: a flat array of 8-byte [{ u32 rva; u32 rva_to }] entries
    sorted by [rva]. See the .mli for the semantics; the format is
    Microsoft's, not LLVM's (LLVM never emits OMAP). *)

open Pdb_types
module Buffer = Stdlib.Buffer

open Binary_writer

type entry = { rva : u32; rva_to : u32 }
type t = entry array

let parse (cur : Object.Buffer.cursor) (total_bytes : int) : t =
  if total_bytes mod 8 <> 0 then
    Object.Buffer.invalid_format
      (Printf.sprintf "OMAP stream: %d bytes is not a multiple of 8"
         total_bytes);
  Object.Buffer.ensure cur total_bytes
    (Printf.sprintf "OMAP stream: %d bytes overrun cursor" total_bytes);
  let n = total_bytes / 8 in
  Array.init n (fun _ ->
      let rva = Object.Buffer.Read.u32 cur in
      let rva_to = Object.Buffer.Read.u32 cur in
      { rva; rva_to })

let write (buf : Buffer.t) (t : t) : unit =
  Array.iter
    (fun e ->
      write_u32_le buf (Unsigned.UInt32.to_int e.rva);
      write_u32_le buf (Unsigned.UInt32.to_int e.rva_to))
    t

(** Binary-search for the largest index [i] such that [t.(i).rva <= rva].
    Returns [-1] if no such entry exists. *)
let find_floor t rva =
  let rva = Unsigned.UInt32.to_int rva in
  let n = Array.length t in
  let lo = ref 0 and hi = ref (n - 1) and result = ref (-1) in
  while !lo <= !hi do
    let mid = (!lo + !hi) / 2 in
    let mid_rva = Unsigned.UInt32.to_int t.(mid).rva in
    if mid_rva <= rva then begin
      result := mid;
      lo := mid + 1
    end
    else hi := mid - 1
  done;
  !result

let lookup (t : t) (rva : u32) : u32 option =
  match find_floor t rva with
  | -1 -> None
  | i ->
      let entry = t.(i) in
      let rva_to_int = Unsigned.UInt32.to_int entry.rva_to in
      if rva_to_int = 0 then None
      else
        let delta =
          Unsigned.UInt32.to_int rva - Unsigned.UInt32.to_int entry.rva
        in
        Some (Unsigned.UInt32.of_int (rva_to_int + delta))
