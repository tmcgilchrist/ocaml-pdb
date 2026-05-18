(** CodeView type record definitions and parsing.

    Type records appear in the TPI (Stream 2) and IPI (Stream 4) streams. Each
    record has a 2-byte length prefix, a 2-byte leaf kind, then payload. *)

open Pdb_types

(** {2 Numeric Leaf Encoding}

    Variable-width integer encoding used in type record payloads. Values <
    0x8000 are literal u16 values. Values >= 0x8000 are tag bytes selecting a
    wider format. *)

val parse_numeric_leaf : Object.Buffer.cursor -> int64
val write_numeric_leaf : Stdlib.Buffer.t -> int64 -> unit

(** {2 Type Properties}

    Bit flags describing properties of a class, structure, or union. *)

type type_properties = {
  packed : bool;
  ctor : bool;
  ovlops : bool;
  is_nested : bool;
  cnested : bool;
  opassign : bool;
  opcast : bool;
  fwdref : bool;
  scoped : bool;
  has_unique_name : bool;
  sealed : bool;
  intrinsic : bool;
}

val parse_type_properties : int -> type_properties
val int_of_type_properties : type_properties -> int

(** {2 Type Record} *)

type class_record = {
  field_count : int;
  properties : type_properties;
  field_list : u32;
  derived_from : u32;
  vtable_shape : u32;
  size : int64;
  name : string;
  unique_name : string option;
}

type field_entry =
  | Member of { attrs : int; field_type : u32; offset : int64; name : string }
  | Enumerate of { attrs : int; value : int64; name : string }
  | OneMethod of {
      attrs : int;
      method_type : u32;
      vftable_offset : int option;
      name : string;
    }
  | Method of { count : int; method_list : u32; name : string }
  | BaseClass of { attrs : int; base_type : u32; offset : int64 }
  | VBaseClass of {
      attrs : int;
      base_type : u32;
      vbptr_type : u32;
      vbptr_offset : int64;
      vbtable_index : int64;
    }
  | NestedType of { attrs : int; nested_type : u32; name : string }
  | VFuncTab of { vftable_type : u32 }
  | StaticMember of { attrs : int; field_type : u32; name : string }
  | Index of { continuation : u32 }

type type_record =
  | Modifier of { modified_type : u32; modifiers : int }
  | Pointer of { pointee_type : u32; attrs : u32 }
  | Procedure of {
      return_type : u32;
      calling_conv : Codeview_constants.calling_convention;
      options : int;
          (** FunctionOptions byte: 0x01=CxxReturnUdt, 0x02=Constructor,
              0x04=ConstructorWithVirtualBases. *)
      param_count : int;
      arg_list : u32;
    }
  | MFunction of {
      return_type : u32;
      class_type : u32;
      this_type : u32;
      calling_conv : Codeview_constants.calling_convention;
      options : int;  (** FunctionOptions byte; same encoding as Procedure. *)
      param_count : int;
      arg_list : u32;
      this_adjust : int32;
    }
  | ArgList of { args : u32 array }
  | FieldList of { members : field_entry list }
  | Array of {
      element_type : u32;
      index_type : u32;
      size : int64;
      name : string;
    }
  | Class of class_record
  | Structure of class_record
  | Interface of class_record
  | Union of {
      field_count : int;
      properties : type_properties;
      field_list : u32;
      size : int64;
      name : string;
      unique_name : string option;
    }
  | Enum of {
      field_count : int;
      properties : type_properties;
      underlying_type : u32;
      field_list : u32;
      name : string;
      unique_name : string option;
    }
  | Bitfield of { underlying_type : u32; length : int; position : int }
  | VTShape of { descriptors : int array }
  | MethodList of { entries : (int * u32 * int option) list }
  (* IPI records *)
  | FuncId of { scope_id : u32; func_type : u32; name : string }
  | MFuncId of { parent_type : u32; func_type : u32; name : string }
  | StringId of { id : u32; str : string }
  | BuildInfo of { args : u32 array }
  | UdtSrcLine of { udt : u32; source : u32; line : u32 }
  | UdtModSrcLine of { udt : u32; source : u32; line : u32; module_ : int }
  | SubstrList of { strings : u32 array }
  | Unknown of { kind : int; data : string }

val parse_type_record : Object.Buffer.cursor -> int -> type_record
(** [parse_type_record cur record_length] parses a single type record. The
    cursor should be positioned after the length prefix but at the leaf kind
    u16. [record_length] is the remaining payload bytes. *)

val write_type_record : Stdlib.Buffer.t -> type_record -> unit
(** [write_type_record buf record] serializes a type record including the length
    prefix and leaf kind. *)
