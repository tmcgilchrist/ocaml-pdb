(** Common aliases to make more explicit the nature of values being read. *)

type u8 = Unsigned.UInt8.t
type u16 = Unsigned.UInt16.t
type u32 = Unsigned.UInt32.t
type u64 = Unsigned.UInt64.t
type i32 = Signed.Int32.t
type i64 = Signed.Int64.t

(** {2 Type Index}

    A type index identifies a type record in the TPI or IPI stream. Values below
    0x1000 are "simple types" (builtin primitives like int, float). Values >=
    0x1000 are indices into the type record array. *)

type type_index = u32

let type_index_of_u32 (v : u32) : type_index = v
let u32_of_type_index (ti : type_index) : u32 = ti
let first_non_simple : type_index = Unsigned.UInt32.of_int 0x1000

let is_simple_type (ti : type_index) : bool =
  Unsigned.UInt32.compare ti first_non_simple < 0

let type_index_to_array_index (ti : type_index) : int =
  Unsigned.UInt32.to_int ti - 0x1000

(** {2 GUID} *)

type guid = {
  data1 : u32;
  data2 : u16;
  data3 : u16;
  data4 : string;  (** 8 bytes *)
}

let string_of_guid g =
  Printf.sprintf "{%08X-%04X-%04X-%s}"
    (Unsigned.UInt32.to_int g.data1)
    (Unsigned.UInt16.to_int g.data2)
    (Unsigned.UInt16.to_int g.data3)
    (String.concat ""
       (List.init (String.length g.data4) (fun i ->
            Printf.sprintf "%02X" (Char.code g.data4.[i]))))
