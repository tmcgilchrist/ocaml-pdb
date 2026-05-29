(** Old-style FPO_DATA stream reader/writer.

    Each entry is exactly 16 bytes:
    [{ u32 offset; u32 size; u32 num_locals; u16 num_params; u16 attributes }]. *)

open Pdb_types
module Buffer = Stdlib.Buffer

open Binary_writer

type entry = {
  offset : u32;
  size : u32;
  num_locals : u32;
  num_params : int;
  attributes : int;
}

type t = entry array

let parse cur total_bytes =
  if total_bytes mod 16 <> 0 then
    Object.Buffer.invalid_format
      (Printf.sprintf "FPO stream: %d bytes is not a multiple of 16"
         total_bytes);
  Object.Buffer.ensure cur total_bytes
    (Printf.sprintf "FPO stream: %d bytes overrun cursor" total_bytes);
  let n = total_bytes / 16 in
  Array.init n (fun _ ->
      let offset = Object.Buffer.Read.u32 cur in
      let size = Object.Buffer.Read.u32 cur in
      let num_locals = Object.Buffer.Read.u32 cur in
      let num_params =
        Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int
      in
      let attributes =
        Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int
      in
      { offset; size; num_locals; num_params; attributes })

let write buf (t : t) =
  Array.iter
    (fun e ->
      write_u32_le buf (Unsigned.UInt32.to_int e.offset);
      write_u32_le buf (Unsigned.UInt32.to_int e.size);
      write_u32_le buf (Unsigned.UInt32.to_int e.num_locals);
      write_u16_le buf e.num_params;
      write_u16_le buf e.attributes)
    t
