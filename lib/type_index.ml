(** A CodeView TypeIndex, symbolically distinguished between built-in primitives
    and user-defined records. *)

open Pdb_types

type t =
  | Simple of {
      kind : Codeview_constants.simple_type_kind;
      mode : Codeview_constants.simple_type_mode;
    }
  | User of u32

let first_non_simple_int = 0x1000

let to_u32 = function
  | Simple { kind; mode } ->
      let mode_bits = Codeview_constants.int_of_simple_type_mode mode in
      let kind_bits = Codeview_constants.int_of_simple_type_kind kind in
      Unsigned.UInt32.of_int ((mode_bits lsl 8) lor kind_bits)
  | User v -> v

let of_u32 v =
  let n = Unsigned.UInt32.to_int v in
  if n >= first_non_simple_int then User v
  else
    match
      ( Codeview_constants.simple_type_kind_of_int (n land 0xFF),
        Codeview_constants.simple_type_mode_of_int ((n lsr 8) land 0x7) )
    with
    | kind, mode -> Simple { kind; mode }
    | exception _ -> User v

let simple ?(mode = Codeview_constants.Direct) kind = Simple { kind; mode }
let user v = User v
let is_simple = function Simple _ -> true | User _ -> false

let is_none = function
  | Simple { kind = Codeview_constants.None; mode = Codeview_constants.Direct }
    ->
      true
  | _ -> false

let near32_pointer_attrs =
  Unsigned.UInt32.of_int Codeview_constants.near32_pointer_attrs

let near64_pointer_attrs =
  Unsigned.UInt32.of_int Codeview_constants.near64_pointer_attrs

let void = simple Codeview_constants.Void
let int32 = simple Codeview_constants.Int32
let uint32 = simple Codeview_constants.UInt32
let int64 = simple Codeview_constants.Int64
let uint64 = simple Codeview_constants.UInt64

let char_ptr32 =
  simple ~mode:Codeview_constants.NearPointer32
    Codeview_constants.NarrowCharacter

let void_ptr32 =
  simple ~mode:Codeview_constants.NearPointer32 Codeview_constants.Void
