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

(** {2 Record-size limits}

    CodeView caps every type or symbol record at [max_record_length]
    bytes including the 2-byte length prefix and 2-byte leaf/symbol kind.
    Writers that emit a trailing variable-length field (typically a
    name) should consult {!bytes_remaining} before writing it and
    truncate as needed. *)

val max_record_length : int
(** 0xFF00 = 65280. Per LLVM's
    [llvm/include/llvm/DebugInfo/CodeView/RecordSerialization.h]. *)

val bytes_remaining : Stdlib.Buffer.t -> int
(** [bytes_remaining rec_buf] returns the number of payload bytes still
    available before the in-progress record would exceed
    {!max_record_length}. [rec_buf] holds the 2-byte leaf/symbol kind
    followed by the fields written so far; the 2-byte length prefix is
    added when the record is flushed. Equivalent to LLVM's
    [CodeViewRecordIO::maxFieldLength()]. *)

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
  field_list : Type_index.t;
  derived_from : Type_index.t;
  vtable_shape : Type_index.t;
  size : int64;
  name : string;
  unique_name : string option;
}

type field_entry =
  | Member of {
      attrs : int;
      field_type : Type_index.t;
      offset : int64;
      name : string;
    }
  | Enumerate of { attrs : int; value : int64; name : string }
  | OneMethod of {
      attrs : int;
      method_type : Type_index.t;
      vftable_offset : int option;
      name : string;
    }
  | Method of { count : int; method_list : Type_index.t; name : string }
  | BaseClass of { attrs : int; base_type : Type_index.t; offset : int64 }
  | VBaseClass of {
      attrs : int;
      base_type : Type_index.t;
      vbptr_type : Type_index.t;
      vbptr_offset : int64;
      vbtable_index : int64;
    }
  | NestedType of {
      attrs : int;
      nested_type : Type_index.t;
      name : string;
    }
  | VFuncTab of { vftable_type : Type_index.t }
  | StaticMember of {
      attrs : int;
      field_type : Type_index.t;
      name : string;
    }
  | Index of { continuation : Type_index.t }

type type_record =
  | Modifier of { modified_type : Type_index.t; modifiers : int }
  | Pointer of { pointee_type : Type_index.t; attrs : u32 }
      (** [attrs] is the raw bit-encoded LF_POINTER attribute word; see
          {!Type_index.near32_pointer_attrs}. *)
  | Procedure of {
      return_type : Type_index.t;
      calling_conv : Codeview_constants.calling_convention;
      options : int;
          (** FunctionOptions byte: 0x01=CxxReturnUdt, 0x02=Constructor,
              0x04=ConstructorWithVirtualBases. *)
      param_count : int;
      arg_list : Type_index.t;
    }
  | MFunction of {
      return_type : Type_index.t;
      class_type : Type_index.t;
      this_type : Type_index.t;
      calling_conv : Codeview_constants.calling_convention;
      options : int;  (** FunctionOptions byte; same encoding as Procedure. *)
      param_count : int;
      arg_list : Type_index.t;
      this_adjust : int32;
    }
  | ArgList of { args : Type_index.t array }
  | FieldList of { members : field_entry list }
  | Array of {
      element_type : Type_index.t;
      index_type : Type_index.t;
      size : int64;
      name : string;
    }
  | Class of class_record
  | Structure of class_record
  | Interface of class_record
  | Union of {
      field_count : int;
      properties : type_properties;
      field_list : Type_index.t;
      size : int64;
      name : string;
      unique_name : string option;
    }
  | Enum of {
      field_count : int;
      properties : type_properties;
      underlying_type : Type_index.t;
      field_list : Type_index.t;
      name : string;
      unique_name : string option;
    }
  | Bitfield of {
      underlying_type : Type_index.t;
      length : int;
      position : int;
    }
  | VTShape of { descriptors : int array }
  | MethodList of {
      entries : (int * Type_index.t * int option) list;
          (** Each entry: [(attrs, method_type, vftable_offset)]. *)
    }
  (* IPI records *)
  | FuncId of {
      scope_id : Type_index.t;
      func_type : Type_index.t;
      name : string;
    }
  | MFuncId of {
      parent_type : Type_index.t;
      func_type : Type_index.t;
      name : string;
    }
  | StringId of { id : Type_index.t; str : string }
  | BuildInfo of { args : Type_index.t array }
  | UdtSrcLine of { udt : Type_index.t; source : Type_index.t; line : u32 }
  | UdtModSrcLine of {
      udt : Type_index.t;
      source : Type_index.t;
      line : u32;
      module_ : int;
    }
  | SubstrList of { strings : Type_index.t array }
  | TypeServer2 of { guid : guid; age : u32; name : string }
      (** LF_TYPESERVER2 (0x1515): reference to an external PDB file
          containing this module's type information, used by MSVC's
          [/Zi] compile-with-shared-type-server flag. Carries no
          TypeIndex references. *)
  | Unknown of { kind : int; data : string }

val parse_type_record : Object.Buffer.cursor -> int -> type_record
(** [parse_type_record cur record_length] parses a single type record. The
    cursor should be positioned after the length prefix but at the leaf kind
    u16. [record_length] is the remaining payload bytes. *)

val write_type_record : Stdlib.Buffer.t -> type_record -> unit
(** [write_type_record buf record] serializes a type record including the length
    prefix and leaf kind. *)

val map_type_indices :
  type_ref:(Type_index.t -> Type_index.t) ->
  id_ref:(Type_index.t -> Type_index.t) ->
  type_record ->
  type_record
(** [map_type_indices ~type_ref ~id_ref record] returns [record] with every
    TypeIndex reference remapped: [type_ref] is applied to references into
    the TPI stream and [id_ref] to references into the IPI stream. The
    TPI/IPI classification matches LLVM's [discoverTypeIndices]. Used by
    cross-compilation-unit type merging to rewrite references onto a shared
    numbering. Non-reference fields (names, sizes, attribute words) are left
    unchanged. *)
