(** CodeView type record definitions and parsing.

    References:
    - LLVM: llvm/include/llvm/DebugInfo/CodeView/TypeRecord.h
    - LLVM: llvm/lib/DebugInfo/CodeView/RecordSerialization.cpp
    - LLVM: llvm/lib/DebugInfo/CodeView/TypeRecordMapping.cpp *)

open Pdb_types
module Buffer = Stdlib.Buffer

(** {2 Numeric Leaf Encoding} *)

(* Numeric leaf tag constants *)
let lf_numeric = 0x8000
let lf_char = 0x8000
let lf_short = 0x8001
let lf_ushort = 0x8002
let lf_long = 0x8003
let lf_ulong = 0x8004
let lf_quadword = 0x8009
let lf_uquadword = 0x800a

let parse_numeric_leaf (cur : Object.Buffer.cursor) : int64 =
  let tag = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int in
  if tag < lf_numeric then Int64.of_int tag
  else
    match tag with
    | t when t = lf_char ->
        let v = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
        (* Sign-extend from 8 bits *)
        if v > 0x7F then Int64.of_int (v - 0x100) else Int64.of_int v
    | t when t = lf_short ->
        let v = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int in
        if v > 0x7FFF then Int64.of_int (v - 0x10000) else Int64.of_int v
    | t when t = lf_ushort ->
        let v = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int in
        Int64.of_int v
    | t when t = lf_long ->
        let v = Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int32 in
        Int64.of_int32 v
    | t when t = lf_ulong ->
        let v = Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int in
        Int64.of_int v
    | t when t = lf_quadword ->
        let v = Object.Buffer.Read.u64 cur in
        Unsigned.UInt64.to_int64 v
    | t when t = lf_uquadword ->
        let v = Object.Buffer.Read.u64 cur in
        Unsigned.UInt64.to_int64 v
    | _ ->
        Object.Buffer.invalid_format
          (Printf.sprintf "Unknown numeric leaf tag: 0x%04x" tag)

let write_u16_le buf v =
  Buffer.add_char buf (Char.chr (v land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xFF))

let write_u32_le buf v =
  Buffer.add_char buf (Char.chr (v land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 16) land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 24) land 0xFF))

let write_i32_le buf (v : int32) = write_u32_le buf (Int32.to_int v)

let write_u64_le buf v =
  for i = 0 to 7 do
    Buffer.add_char buf
      (Char.chr
         (Int64.to_int
            (Int64.logand (Int64.shift_right_logical v (i * 8)) 0xFFL)))
  done

let write_numeric_leaf (buf : Buffer.t) (v : int64) : unit =
  if v >= 0L && v < Int64.of_int lf_numeric then
    write_u16_le buf (Int64.to_int v)
  else if v >= -128L && v <= 127L then begin
    write_u16_le buf lf_char;
    Buffer.add_char buf (Char.chr (Int64.to_int v land 0xFF))
  end
  else if v >= -32768L && v <= 32767L then begin
    write_u16_le buf lf_short;
    write_u16_le buf (Int64.to_int v land 0xFFFF)
  end
  else if v >= 0L && v <= 0xFFFFL then begin
    write_u16_le buf lf_ushort;
    write_u16_le buf (Int64.to_int v)
  end
  else if v >= Int64.of_int32 Int32.min_int && v <= Int64.of_int32 Int32.max_int
  then begin
    write_u16_le buf lf_long;
    write_u32_le buf (Int64.to_int v land 0xFFFFFFFF)
  end
  else if v >= 0L && v <= 0xFFFFFFFFL then begin
    write_u16_le buf lf_ulong;
    write_u32_le buf (Int64.to_int v)
  end
  else begin
    write_u16_le buf lf_quadword;
    write_u64_le buf v
  end

(** {2 Type Properties} *)

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

let parse_type_properties bits =
  {
    packed = bits land 0x0001 <> 0;
    ctor = bits land 0x0002 <> 0;
    ovlops = bits land 0x0004 <> 0;
    is_nested = bits land 0x0008 <> 0;
    cnested = bits land 0x0010 <> 0;
    opassign = bits land 0x0020 <> 0;
    opcast = bits land 0x0040 <> 0;
    fwdref = bits land 0x0080 <> 0;
    scoped = bits land 0x0100 <> 0;
    has_unique_name = bits land 0x0200 <> 0;
    sealed = bits land 0x0400 <> 0;
    intrinsic = bits land 0x0800 <> 0;
  }

let int_of_type_properties p =
  (if p.packed then 0x0001 else 0)
  lor (if p.ctor then 0x0002 else 0)
  lor (if p.ovlops then 0x0004 else 0)
  lor (if p.is_nested then 0x0008 else 0)
  lor (if p.cnested then 0x0010 else 0)
  lor (if p.opassign then 0x0020 else 0)
  lor (if p.opcast then 0x0040 else 0)
  lor (if p.fwdref then 0x0080 else 0)
  lor (if p.scoped then 0x0100 else 0)
  lor (if p.has_unique_name then 0x0200 else 0)
  lor (if p.sealed then 0x0400 else 0)
  lor if p.intrinsic then 0x0800 else 0

(** {2 Type Records} *)

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
  | Procedure of {
      return_type : Type_index.t;
      calling_conv : Codeview_constants.calling_convention;
      options : int;
      param_count : int;
      arg_list : Type_index.t;
    }
  | MFunction of {
      return_type : Type_index.t;
      class_type : Type_index.t;
      this_type : Type_index.t;
      calling_conv : Codeview_constants.calling_convention;
      options : int;
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
    }
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
  | Unknown of { kind : int; data : string }

(** Read a null-terminated string from cursor *)
let read_cstring (cur : Object.Buffer.cursor) : string =
  match Object.Buffer.Read.zero_string cur () with
  | Some s -> s
  | Option.None -> ""

(** Skip padding bytes (0xf0-0xff) *)
let skip_padding (cur : Object.Buffer.cursor) (end_pos : int) : unit =
  while cur.position < end_pos && cur.buffer.{cur.position} >= 0xf0 do
    Object.Buffer.advance cur 1
  done

let read_u16 cur = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int
let read_u32 cur = Object.Buffer.Read.u32 cur
let read_type_index cur = Type_index.of_u32 (read_u32 cur)
let write_type_index buf ti =
  write_u32_le buf (Unsigned.UInt32.to_int (Type_index.to_u32 ti))

let parse_class_record (cur : Object.Buffer.cursor) (end_pos : int) :
    class_record =
  let field_count = read_u16 cur in
  let properties = parse_type_properties (read_u16 cur) in
  let field_list = read_type_index cur in
  let derived_from = read_type_index cur in
  let vtable_shape = read_type_index cur in
  let size = parse_numeric_leaf cur in
  let name = read_cstring cur in
  let unique_name =
    if properties.has_unique_name && cur.position < end_pos then
      Some (read_cstring cur)
    else Option.None
  in
  {
    field_count;
    properties;
    field_list;
    derived_from;
    vtable_shape;
    size;
    name;
    unique_name;
  }

let parse_field_entry (cur : Object.Buffer.cursor) (end_pos : int) : field_entry
    =
  let kind = read_u16 cur in
  match kind with
  | 0x150d (* LF_MEMBER *) ->
      let attrs = read_u16 cur in
      let field_type = read_type_index cur in
      let offset = parse_numeric_leaf cur in
      let name = read_cstring cur in
      Member { attrs; field_type; offset; name }
  | 0x1502 (* LF_ENUMERATE *) ->
      let attrs = read_u16 cur in
      let value = parse_numeric_leaf cur in
      let name = read_cstring cur in
      Enumerate { attrs; value; name }
  | 0x1511 (* LF_ONEMETHOD *) ->
      let attrs = read_u16 cur in
      let method_type = read_type_index cur in
      let method_kind = (attrs lsr 2) land 0x07 in
      let vftable_offset =
        if method_kind = 0x04 || method_kind = 0x06 then
          Some (Unsigned.UInt32.to_int (read_u32 cur))
        else Option.None
      in
      let name = read_cstring cur in
      OneMethod { attrs; method_type; vftable_offset; name }
  | 0x150f (* LF_METHOD *) ->
      let count = read_u16 cur in
      let method_list = read_type_index cur in
      let name = read_cstring cur in
      Method { count; method_list; name }
  | 0x1400 (* LF_BCLASS *) | 0x151a (* LF_BINTERFACE *) ->
      let attrs = read_u16 cur in
      let base_type = read_type_index cur in
      let offset = parse_numeric_leaf cur in
      BaseClass { attrs; base_type; offset }
  | 0x1401 (* LF_VBCLASS *) | 0x1402 (* LF_IVBCLASS *) ->
      let attrs = read_u16 cur in
      let base_type = read_type_index cur in
      let vbptr_type = read_type_index cur in
      let vbptr_offset = parse_numeric_leaf cur in
      let vbtable_index = parse_numeric_leaf cur in
      VBaseClass { attrs; base_type; vbptr_type; vbptr_offset; vbtable_index }
  | 0x1510 (* LF_NESTTYPE *) ->
      let attrs = read_u16 cur in
      let nested_type = read_type_index cur in
      let name = read_cstring cur in
      NestedType { attrs; nested_type; name }
  | 0x1409 (* LF_VFUNCTAB *) ->
      let _padding = read_u16 cur in
      let vftable_type = read_type_index cur in
      VFuncTab { vftable_type }
  | 0x150e (* LF_STMEMBER *) ->
      let attrs = read_u16 cur in
      let field_type = read_type_index cur in
      let name = read_cstring cur in
      StaticMember { attrs; field_type; name }
  | 0x1404 (* LF_INDEX *) ->
      let _padding = read_u16 cur in
      let continuation = read_type_index cur in
      Index { continuation }
  | _ ->
      (* Skip to end for unknown field entries *)
      let remaining = end_pos - cur.position in
      let data = Object.Buffer.Read.fixed_string cur remaining in
      ignore data;
      Member
        {
          attrs = 0;
          field_type = Type_index.of_u32 Unsigned.UInt32.zero;
          offset = 0L;
          name = Printf.sprintf "<unknown field 0x%04x>" kind;
        }

let parse_type_record (cur : Object.Buffer.cursor) (record_data_len : int) :
    type_record =
  let start_pos = cur.position in
  let end_pos = start_pos + record_data_len in
  let kind = read_u16 cur in
  match kind with
  | 0x1001 (* LF_MODIFIER *) ->
      let modified_type = read_type_index cur in
      let modifiers = read_u16 cur in
      Modifier { modified_type; modifiers }
  | 0x1002 (* LF_POINTER *) ->
      let pointee_type = read_type_index cur in
      let attrs = read_u32 cur in
      Pointer { pointee_type; attrs }
  | 0x1008 (* LF_PROCEDURE *) ->
      let return_type = read_type_index cur in
      let cc = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
      let calling_conv = Codeview_constants.calling_convention_of_int cc in
      let options = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
      let param_count = read_u16 cur in
      let arg_list = read_type_index cur in
      Procedure { return_type; calling_conv; options; param_count; arg_list }
  | 0x1009 (* LF_MFUNCTION *) ->
      let return_type = read_type_index cur in
      let class_type = read_type_index cur in
      let this_type = read_type_index cur in
      let cc = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
      let calling_conv = Codeview_constants.calling_convention_of_int cc in
      let options = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
      let param_count = read_u16 cur in
      let arg_list = read_type_index cur in
      let this_adjust =
        Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int32
      in
      MFunction
        {
          return_type;
          class_type;
          this_type;
          calling_conv;
          options;
          param_count;
          arg_list;
          this_adjust;
        }
  | 0x1201 (* LF_ARGLIST *) ->
      let count = read_u32 cur |> Unsigned.UInt32.to_int in
      let args = Array.init count (fun _ -> read_type_index cur) in
      ArgList { args }
  | 0x1203 (* LF_FIELDLIST *) ->
      let members = ref [] in
      while cur.position < end_pos do
        skip_padding cur end_pos;
        if cur.position < end_pos then begin
          let entry = parse_field_entry cur end_pos in
          members := entry :: !members
        end
      done;
      FieldList { members = List.rev !members }
  | 0x1503 (* LF_ARRAY *) ->
      let element_type = read_type_index cur in
      let index_type = read_type_index cur in
      let size = parse_numeric_leaf cur in
      let name = read_cstring cur in
      Array { element_type; index_type; size; name }
  | 0x1504 (* LF_CLASS *) -> Class (parse_class_record cur end_pos)
  | 0x1505 (* LF_STRUCTURE *) -> Structure (parse_class_record cur end_pos)
  | 0x1519 (* LF_INTERFACE *) -> Interface (parse_class_record cur end_pos)
  | 0x1506 (* LF_UNION *) ->
      let field_count = read_u16 cur in
      let properties = parse_type_properties (read_u16 cur) in
      let field_list = read_type_index cur in
      let size = parse_numeric_leaf cur in
      let name = read_cstring cur in
      let unique_name =
        if properties.has_unique_name && cur.position < end_pos then
          Some (read_cstring cur)
        else Option.None
      in
      Union { field_count; properties; field_list; size; name; unique_name }
  | 0x1507 (* LF_ENUM *) ->
      let field_count = read_u16 cur in
      let properties = parse_type_properties (read_u16 cur) in
      let underlying_type = read_type_index cur in
      let field_list = read_type_index cur in
      let name = read_cstring cur in
      let unique_name =
        if properties.has_unique_name && cur.position < end_pos then
          Some (read_cstring cur)
        else Option.None
      in
      Enum
        {
          field_count;
          properties;
          underlying_type;
          field_list;
          name;
          unique_name;
        }
  | 0x1205 (* LF_BITFIELD *) ->
      let underlying_type = read_type_index cur in
      let length = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
      let position = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
      Bitfield { underlying_type; length; position }
  | 0x000a (* LF_VTSHAPE *) ->
      let count = read_u16 cur in
      let num_bytes = (count + 1) / 2 in
      let raw = Object.Buffer.Read.fixed_string cur num_bytes in
      let descriptors =
        Array.init count (fun i ->
            let byte_val = Char.code raw.[i / 2] in
            if i mod 2 = 0 then byte_val land 0x0F
            else (byte_val lsr 4) land 0x0F)
      in
      VTShape { descriptors }
  | 0x1206 (* LF_METHODLIST *) ->
      let entries = ref [] in
      while cur.position < end_pos do
        let attrs = read_u16 cur in
        let _padding = read_u16 cur in
        let method_type = read_type_index cur in
        let method_kind = (attrs lsr 2) land 0x07 in
        let vftable_offset =
          if method_kind = 0x04 || method_kind = 0x06 then
            Some (Unsigned.UInt32.to_int (read_u32 cur))
          else Option.None
        in
        entries := (attrs, method_type, vftable_offset) :: !entries
      done;
      MethodList { entries = List.rev !entries }
  (* IPI records *)
  | 0x1601 (* LF_FUNC_ID *) ->
      let scope_id = read_type_index cur in
      let func_type = read_type_index cur in
      let name = read_cstring cur in
      FuncId { scope_id; func_type; name }
  | 0x1602 (* LF_MFUNC_ID *) ->
      let parent_type = read_type_index cur in
      let func_type = read_type_index cur in
      let name = read_cstring cur in
      MFuncId { parent_type; func_type; name }
  | 0x1605 (* LF_STRING_ID *) ->
      let id = read_type_index cur in
      let str = read_cstring cur in
      StringId { id; str }
  | 0x1603 (* LF_BUILDINFO *) ->
      let count = read_u16 cur in
      let args = Array.init count (fun _ -> read_type_index cur) in
      BuildInfo { args }
  | 0x1606 (* LF_UDT_SRC_LINE *) ->
      let udt = read_type_index cur in
      let source = read_type_index cur in
      let line = read_u32 cur in
      UdtSrcLine { udt; source; line }
  | 0x1607 (* LF_UDT_MOD_SRC_LINE *) ->
      let udt = read_type_index cur in
      let source = read_type_index cur in
      let line = read_u32 cur in
      let module_ = read_u16 cur in
      UdtModSrcLine { udt; source; line; module_ }
  | 0x1604 (* LF_SUBSTR_LIST *) ->
      let count = read_u32 cur |> Unsigned.UInt32.to_int in
      let strings = Array.init count (fun _ -> read_type_index cur) in
      SubstrList { strings }
  | _ ->
      let remaining = end_pos - cur.position in
      let raw =
        if remaining > 0 then Object.Buffer.Read.fixed_string cur remaining
        else ""
      in
      (* Strip trailing padding bytes (0xf0..0xff) added by the writer. *)
      let strip_trailing_padding s =
        let len = ref (String.length s) in
        while !len > 0 && Char.code s.[!len - 1] >= 0xf0 do
          decr len
        done;
        String.sub s 0 !len
      in
      let data = strip_trailing_padding raw in
      Unknown { kind; data }

(** {2 Writing} *)

let write_cstring buf s =
  Buffer.add_string buf s;
  Buffer.add_char buf '\000'

(** Add padding bytes to align to 4 bytes *)
let write_padding buf =
  let pos = Buffer.length buf in
  let align = (4 - (pos mod 4)) mod 4 in
  for i = 1 to align do
    Buffer.add_char buf (Char.chr (0xf0 + i))
  done

let write_type_record (buf : Buffer.t) (record : type_record) : unit =
  (* Write into a temporary buffer to compute the length *)
  let rec_buf = Buffer.create 64 in
  (match record with
  | Modifier { modified_type; modifiers } ->
      write_u16_le rec_buf 0x1001;
      write_type_index rec_buf modified_type;
      write_u16_le rec_buf modifiers
  | Pointer { pointee_type; attrs } ->
      write_u16_le rec_buf 0x1002;
      write_type_index rec_buf pointee_type;
      write_u32_le rec_buf (Unsigned.UInt32.to_int attrs)
  | Procedure { return_type; calling_conv; options; param_count; arg_list } ->
      write_u16_le rec_buf 0x1008;
      write_type_index rec_buf return_type;
      Buffer.add_char rec_buf
        (Char.chr (Codeview_constants.int_of_calling_convention calling_conv));
      Buffer.add_char rec_buf (Char.chr (options land 0xFF));
      write_u16_le rec_buf param_count;
      write_type_index rec_buf arg_list
  | MFunction
      {
        return_type;
        class_type;
        this_type;
        calling_conv;
        options;
        param_count;
        arg_list;
        this_adjust;
      } ->
      write_u16_le rec_buf 0x1009;
      write_type_index rec_buf return_type;
      write_type_index rec_buf class_type;
      write_type_index rec_buf this_type;
      Buffer.add_char rec_buf
        (Char.chr (Codeview_constants.int_of_calling_convention calling_conv));
      Buffer.add_char rec_buf (Char.chr (options land 0xFF));
      write_u16_le rec_buf param_count;
      write_type_index rec_buf arg_list;
      write_i32_le rec_buf this_adjust
  | ArgList { args } ->
      write_u16_le rec_buf 0x1201;
      write_u32_le rec_buf (Array.length args);
      Array.iter (fun ti -> write_type_index rec_buf ti) args
  | FieldList { members } ->
      write_u16_le rec_buf 0x1203;
      (* Each field-list entry is aligned to 4 bytes from the absolute
         start of the record. Since [rec_buf] doesn't include the
         2-byte length prefix that will be prepended, we pad based on
         [Buffer.length rec_buf + 2]. Padding goes AFTER each entry so
         the NEXT entry starts aligned; the first entry (right after
         the 2-byte kind) is already at absolute offset 4. *)
      let pad_after_entry () =
        let absolute_pos = Buffer.length rec_buf + 2 in
        let align = (4 - (absolute_pos mod 4)) mod 4 in
        for i = 1 to align do
          Buffer.add_char rec_buf (Char.chr (0xf0 + i))
        done
      in
      List.iteri
        (fun i entry ->
          if i > 0 then pad_after_entry ();
          ignore pad_after_entry;
          match entry with
          | Member { attrs; field_type; offset; name } ->
              write_u16_le rec_buf 0x150d;
              write_u16_le rec_buf attrs;
              write_type_index rec_buf field_type;
              write_numeric_leaf rec_buf offset;
              write_cstring rec_buf name
          | Enumerate { attrs; value; name } ->
              write_u16_le rec_buf 0x1502;
              write_u16_le rec_buf attrs;
              write_numeric_leaf rec_buf value;
              write_cstring rec_buf name
          | OneMethod { attrs; method_type; vftable_offset; name } ->
              write_u16_le rec_buf 0x1511;
              write_u16_le rec_buf attrs;
              write_type_index rec_buf method_type;
              (match vftable_offset with
              | Some off -> write_u32_le rec_buf off
              | Option.None -> ());
              write_cstring rec_buf name
          | Method { count; method_list; name } ->
              write_u16_le rec_buf 0x150f;
              write_u16_le rec_buf count;
              write_type_index rec_buf method_list;
              write_cstring rec_buf name
          | BaseClass { attrs; base_type; offset } ->
              write_u16_le rec_buf 0x1400;
              write_u16_le rec_buf attrs;
              write_type_index rec_buf base_type;
              write_numeric_leaf rec_buf offset
          | VBaseClass
              { attrs; base_type; vbptr_type; vbptr_offset; vbtable_index } ->
              write_u16_le rec_buf 0x1401;
              write_u16_le rec_buf attrs;
              write_type_index rec_buf base_type;
              write_type_index rec_buf vbptr_type;
              write_numeric_leaf rec_buf vbptr_offset;
              write_numeric_leaf rec_buf vbtable_index
          | NestedType { attrs; nested_type; name } ->
              write_u16_le rec_buf 0x1510;
              write_u16_le rec_buf attrs;
              write_type_index rec_buf nested_type;
              write_cstring rec_buf name
          | VFuncTab { vftable_type } ->
              write_u16_le rec_buf 0x1409;
              write_u16_le rec_buf 0;
              (* padding *)
              write_type_index rec_buf vftable_type
          | StaticMember { attrs; field_type; name } ->
              write_u16_le rec_buf 0x150e;
              write_u16_le rec_buf attrs;
              write_type_index rec_buf field_type;
              write_cstring rec_buf name
          | Index { continuation } ->
              write_u16_le rec_buf 0x1404;
              write_u16_le rec_buf 0;
              write_type_index rec_buf continuation)
        members
  | Array { element_type; index_type; size; name } ->
      write_u16_le rec_buf 0x1503;
      write_type_index rec_buf element_type;
      write_type_index rec_buf index_type;
      write_numeric_leaf rec_buf size;
      write_cstring rec_buf name
  | Class cr -> (
      write_u16_le rec_buf 0x1504;
      write_u16_le rec_buf cr.field_count;
      write_u16_le rec_buf (int_of_type_properties cr.properties);
      write_type_index rec_buf cr.field_list;
      write_type_index rec_buf cr.derived_from;
      write_type_index rec_buf cr.vtable_shape;
      write_numeric_leaf rec_buf cr.size;
      write_cstring rec_buf cr.name;
      match cr.unique_name with
      | Some un -> write_cstring rec_buf un
      | Option.None -> ())
  | Structure cr -> (
      write_u16_le rec_buf 0x1505;
      write_u16_le rec_buf cr.field_count;
      write_u16_le rec_buf (int_of_type_properties cr.properties);
      write_type_index rec_buf cr.field_list;
      write_type_index rec_buf cr.derived_from;
      write_type_index rec_buf cr.vtable_shape;
      write_numeric_leaf rec_buf cr.size;
      write_cstring rec_buf cr.name;
      match cr.unique_name with
      | Some un -> write_cstring rec_buf un
      | Option.None -> ())
  | Interface cr -> (
      write_u16_le rec_buf 0x1519;
      write_u16_le rec_buf cr.field_count;
      write_u16_le rec_buf (int_of_type_properties cr.properties);
      write_type_index rec_buf cr.field_list;
      write_type_index rec_buf cr.derived_from;
      write_type_index rec_buf cr.vtable_shape;
      write_numeric_leaf rec_buf cr.size;
      write_cstring rec_buf cr.name;
      match cr.unique_name with
      | Some un -> write_cstring rec_buf un
      | Option.None -> ())
  | Union { field_count; properties; field_list; size; name; unique_name } -> (
      write_u16_le rec_buf 0x1506;
      write_u16_le rec_buf field_count;
      write_u16_le rec_buf (int_of_type_properties properties);
      write_type_index rec_buf field_list;
      write_numeric_leaf rec_buf size;
      write_cstring rec_buf name;
      match unique_name with
      | Some un -> write_cstring rec_buf un
      | Option.None -> ())
  | Enum
      {
        field_count;
        properties;
        underlying_type;
        field_list;
        name;
        unique_name;
      } -> (
      write_u16_le rec_buf 0x1507;
      write_u16_le rec_buf field_count;
      write_u16_le rec_buf (int_of_type_properties properties);
      write_type_index rec_buf underlying_type;
      write_type_index rec_buf field_list;
      write_cstring rec_buf name;
      match unique_name with
      | Some un -> write_cstring rec_buf un
      | Option.None -> ())
  | Bitfield { underlying_type; length; position } ->
      write_u16_le rec_buf 0x1205;
      write_type_index rec_buf underlying_type;
      Buffer.add_char rec_buf (Char.chr length);
      Buffer.add_char rec_buf (Char.chr position)
  | VTShape { descriptors } ->
      write_u16_le rec_buf 0x000a;
      write_u16_le rec_buf (Array.length descriptors);
      let count = Array.length descriptors in
      let num_bytes = (count + 1) / 2 in
      for i = 0 to num_bytes - 1 do
        let lo = if i * 2 < count then descriptors.(i * 2) land 0x0F else 0 in
        let hi =
          if (i * 2) + 1 < count then
            (descriptors.((i * 2) + 1) land 0x0F) lsl 4
          else 0
        in
        Buffer.add_char rec_buf (Char.chr (lo lor hi))
      done
  | MethodList { entries } ->
      write_u16_le rec_buf 0x1206;
      List.iter
        (fun (attrs, method_type, vftable_offset) ->
          write_u16_le rec_buf attrs;
          write_u16_le rec_buf 0;
          (* padding *)
          write_type_index rec_buf method_type;
          match vftable_offset with
          | Some off -> write_u32_le rec_buf off
          | Option.None -> ())
        entries
  | FuncId { scope_id; func_type; name } ->
      write_u16_le rec_buf 0x1601;
      write_type_index rec_buf scope_id;
      write_type_index rec_buf func_type;
      write_cstring rec_buf name
  | MFuncId { parent_type; func_type; name } ->
      write_u16_le rec_buf 0x1602;
      write_type_index rec_buf parent_type;
      write_type_index rec_buf func_type;
      write_cstring rec_buf name
  | StringId { id; str } ->
      write_u16_le rec_buf 0x1605;
      write_type_index rec_buf id;
      write_cstring rec_buf str
  | BuildInfo { args } ->
      write_u16_le rec_buf 0x1603;
      write_u16_le rec_buf (Array.length args);
      Array.iter (fun ti -> write_type_index rec_buf ti) args
  | UdtSrcLine { udt; source; line } ->
      write_u16_le rec_buf 0x1606;
      write_type_index rec_buf udt;
      write_type_index rec_buf source;
      write_u32_le rec_buf (Unsigned.UInt32.to_int line)
  | UdtModSrcLine { udt; source; line; module_ } ->
      write_u16_le rec_buf 0x1607;
      write_type_index rec_buf udt;
      write_type_index rec_buf source;
      write_u32_le rec_buf (Unsigned.UInt32.to_int line);
      write_u16_le rec_buf module_
  | SubstrList { strings } ->
      write_u16_le rec_buf 0x1604;
      write_u32_le rec_buf (Array.length strings);
      Array.iter (fun ti -> write_type_index rec_buf ti) strings
  | Unknown { kind; data } ->
      write_u16_le rec_buf kind;
      Buffer.add_string rec_buf data);
  (* Write length prefix + record content *)
  let content = Buffer.contents rec_buf in
  let content_len = String.length content in
  (* Pad to 4-byte alignment. Padding bytes 0xf1..0xf3 are appended to
     the content, and the length field records content+padding (not
     including the length field itself), so readers can find the next
     record at offset + 2 + length. *)
  let total_unaligned = 2 + content_len in
  let align = (4 - (total_unaligned mod 4)) mod 4 in
  let padded_len = content_len + align in
  write_u16_le buf padded_len;
  Buffer.add_string buf content;
  for i = 1 to align do
    Buffer.add_char buf (Char.chr (0xf0 + i))
  done
