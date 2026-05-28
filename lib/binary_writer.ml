(** Little-endian write helpers and read aliases. See the .mli. *)

module Buffer = Stdlib.Buffer

let write_u8 buf v = Buffer.add_char buf (Char.chr (v land 0xFF))

let write_u16_le buf v =
  Buffer.add_char buf (Char.chr (v land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xFF))

let write_u32_le buf v =
  Buffer.add_char buf (Char.chr (v land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 16) land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 24) land 0xFF))

let write_u64_le buf v =
  for i = 0 to 7 do
    Buffer.add_char buf
      (Char.chr
         (Int64.to_int
            (Int64.logand (Int64.shift_right_logical v (i * 8)) 0xFFL)))
  done

let write_i32_le buf (v : int32) = write_u32_le buf (Int32.to_int v)

let write_cstring buf s =
  Buffer.add_string buf s;
  Buffer.add_char buf '\000'

let write_padding_to_align buf alignment =
  let pos = Buffer.length buf in
  let pad = (alignment - (pos mod alignment)) mod alignment in
  for _ = 1 to pad do
    Buffer.add_char buf '\000'
  done

let read_u8 cur = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int
let read_u16 cur = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int
let read_u32 cur = Object.Buffer.Read.u32 cur
let read_i32 cur = Unsigned.UInt32.to_int32 (read_u32 cur)

let read_cstring (cur : Object.Buffer.cursor) : string =
  match Object.Buffer.Read.zero_string cur () with
  | Some s -> s
  | Option.None -> ""
