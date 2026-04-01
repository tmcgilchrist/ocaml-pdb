(** CodeView symbol record definitions and parsing.

    References:
    - LLVM: llvm/include/llvm/DebugInfo/CodeView/SymbolRecord.h
    - LLVM: llvm/lib/DebugInfo/CodeView/SymbolRecordMapping.cpp *)

open Pdb_types
module Buffer = Stdlib.Buffer

type proc_record = {
  parent : u32;
  end_ : u32;
  next : u32;
  code_size : u32;
  debug_start : u32;
  debug_end : u32;
  type_index : u32;
  offset : u32;
  segment : int;
  flags : int;
  name : string;
}

type data_record = {
  type_index : u32;
  offset : u32;
  segment : int;
  name : string;
}

type symbol_record =
  | Compile3 of {
      flags : u32;
      machine : int;
      frontend_version : int * int * int * int;
      backend_version : int * int * int * int;
      version_string : string;
    }
  | ObjName of { signature : u32; name : string }
  | BuildInfo of { id : u32 }
  | GProc32 of proc_record
  | LProc32 of proc_record
  | GProc32Id of proc_record
  | LProc32Id of proc_record
  | End
  | GData32 of data_record
  | LData32 of data_record
  | GThread32 of data_record
  | LThread32 of data_record
  | Local of { type_index : u32; flags : int; name : string }
  | DefRangeFramePointerRel of {
      offset : int32;
      range_offset : u32;
      range_section : int;
      range_length : int;
    }
  | DefRangeRegisterRel of {
      base_register : int;
      offset : int32;
      range_offset : u32;
      range_section : int;
      range_length : int;
    }
  | DefRangeRegister of {
      register : int;
      may_have_no_name : int;
      range_offset : u32;
      range_section : int;
      range_length : int;
    }
  | DefRangeFramePointerRelFullScope of { offset : int32 }
  | Block32 of {
      parent : u32;
      end_ : u32;
      length : u32;
      offset : u32;
      segment : int;
      name : string;
    }
  | InlineSite of {
      parent : u32;
      end_ : u32;
      inlinee : u32;
      annotations : string;
    }
  | InlineSiteEnd
  | ProcIdEnd
  | Udt of { type_index : u32; name : string }
  | Constant of { type_index : u32; value : int64; name : string }
  | Pub32 of { flags : u32; offset : u32; segment : int; name : string }
  | FrameProc of {
      total_frame_bytes : u32;
      padding_frame_bytes : u32;
      offset_to_padding : u32;
      callee_saved_reg_bytes : u32;
      exception_handler_offset : u32;
      exception_handler_section : int;
      frame_proc_flags : u32;
    }
  | RegRel32 of {
      offset : int32;
      type_index : u32;
      register : int;
      name : string;
    }
  | BPRel32 of { offset : int32; type_index : u32; name : string }
  | Register of { type_index : u32; register : int; name : string }
  | Label32 of { offset : u32; segment : int; flags : int; name : string }
  | UNamespace of { name : string }
  | Unknown of { kind : int; data : string }

let read_u16 cur = Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int
let read_u32 cur = Object.Buffer.Read.u32 cur

let read_cstring (cur : Object.Buffer.cursor) : string =
  match Object.Buffer.Read.zero_string cur () with
  | Some s -> s
  | Option.None -> ""

let parse_proc_record (cur : Object.Buffer.cursor) : proc_record =
  let parent = read_u32 cur in
  let end_ = read_u32 cur in
  let next = read_u32 cur in
  let code_size = read_u32 cur in
  let debug_start = read_u32 cur in
  let debug_end = read_u32 cur in
  let type_index = read_u32 cur in
  let offset = read_u32 cur in
  let segment = read_u16 cur in
  let flags = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
  let name = read_cstring cur in
  {
    parent;
    end_;
    next;
    code_size;
    debug_start;
    debug_end;
    type_index;
    offset;
    segment;
    flags;
    name;
  }

let parse_data_record (cur : Object.Buffer.cursor) : data_record =
  let type_index = read_u32 cur in
  let offset = read_u32 cur in
  let segment = read_u16 cur in
  let name = read_cstring cur in
  { type_index; offset; segment; name }

let parse_local_range (cur : Object.Buffer.cursor) =
  let range_offset = read_u32 cur in
  let range_section = read_u16 cur in
  let range_length = read_u16 cur in
  (range_offset, range_section, range_length)

let parse_symbol_record (cur : Object.Buffer.cursor) (record_data_len : int) :
    symbol_record =
  let start_pos = cur.position in
  let end_pos = start_pos + record_data_len in
  let kind = read_u16 cur in
  match kind with
  | 0x113c (* S_COMPILE3 *) ->
      let flags = read_u32 cur in
      let machine = read_u16 cur in
      let fe_major = read_u16 cur in
      let fe_minor = read_u16 cur in
      let fe_build = read_u16 cur in
      let fe_qfe = read_u16 cur in
      let be_major = read_u16 cur in
      let be_minor = read_u16 cur in
      let be_build = read_u16 cur in
      let be_qfe = read_u16 cur in
      let version_string = read_cstring cur in
      Compile3
        {
          flags;
          machine;
          frontend_version = (fe_major, fe_minor, fe_build, fe_qfe);
          backend_version = (be_major, be_minor, be_build, be_qfe);
          version_string;
        }
  | 0x1101 (* S_OBJNAME *) ->
      let signature = read_u32 cur in
      let name = read_cstring cur in
      ObjName { signature; name }
  | 0x114c (* S_BUILDINFO *) ->
      let id = read_u32 cur in
      BuildInfo { id }
  | 0x1110 (* S_GPROC32 *) -> GProc32 (parse_proc_record cur)
  | 0x110f (* S_LPROC32 *) -> LProc32 (parse_proc_record cur)
  | 0x1147 (* S_GPROC32_ID *) -> GProc32Id (parse_proc_record cur)
  | 0x1146 (* S_LPROC32_ID *) -> LProc32Id (parse_proc_record cur)
  | 0x0006 (* S_END *) -> End
  | 0x114e (* S_INLINESITE_END *) -> InlineSiteEnd
  | 0x114f (* S_PROC_ID_END *) -> ProcIdEnd
  | 0x110d (* S_GDATA32 *) -> GData32 (parse_data_record cur)
  | 0x110c (* S_LDATA32 *) -> LData32 (parse_data_record cur)
  | 0x1113 (* S_GTHREAD32 *) -> GThread32 (parse_data_record cur)
  | 0x1112 (* S_LTHREAD32 *) -> LThread32 (parse_data_record cur)
  | 0x113e (* S_LOCAL *) ->
      let type_index = read_u32 cur in
      let flags = read_u16 cur in
      let name = read_cstring cur in
      Local { type_index; flags; name }
  | 0x1142 (* S_DEFRANGE_FRAMEPOINTER_REL *) ->
      let offset = Unsigned.UInt32.to_int32 (read_u32 cur) in
      let range_offset, range_section, range_length = parse_local_range cur in
      (* Skip any gap entries *)
      if cur.position < end_pos then Object.Buffer.seek cur end_pos;
      DefRangeFramePointerRel
        { offset; range_offset; range_section; range_length }
  | 0x1145 (* S_DEFRANGE_REGISTER_REL *) ->
      let base_register = read_u16 cur in
      let _flags = read_u16 cur in
      let offset = Unsigned.UInt32.to_int32 (read_u32 cur) in
      let range_offset, range_section, range_length = parse_local_range cur in
      if cur.position < end_pos then Object.Buffer.seek cur end_pos;
      DefRangeRegisterRel
        { base_register; offset; range_offset; range_section; range_length }
  | 0x1141 (* S_DEFRANGE_REGISTER *) ->
      let register = read_u16 cur in
      let may_have_no_name = read_u16 cur in
      let range_offset, range_section, range_length = parse_local_range cur in
      if cur.position < end_pos then Object.Buffer.seek cur end_pos;
      DefRangeRegister
        {
          register;
          may_have_no_name;
          range_offset;
          range_section;
          range_length;
        }
  | 0x1144 (* S_DEFRANGE_FRAMEPOINTER_REL_FULL_SCOPE *) ->
      let offset = Unsigned.UInt32.to_int32 (read_u32 cur) in
      DefRangeFramePointerRelFullScope { offset }
  | 0x1103 (* S_BLOCK32 *) ->
      let parent = read_u32 cur in
      let end_ = read_u32 cur in
      let length = read_u32 cur in
      let offset = read_u32 cur in
      let segment = read_u16 cur in
      let name = read_cstring cur in
      Block32 { parent; end_; length; offset; segment; name }
  | 0x114d (* S_INLINESITE *) ->
      let parent = read_u32 cur in
      let end_ = read_u32 cur in
      let inlinee = read_u32 cur in
      let remaining = end_pos - cur.position in
      let annotations =
        if remaining > 0 then Object.Buffer.Read.fixed_string cur remaining
        else ""
      in
      InlineSite { parent; end_; inlinee; annotations }
  | 0x1108 (* S_UDT *) ->
      let type_index = read_u32 cur in
      let name = read_cstring cur in
      Udt { type_index; name }
  | 0x1107 (* S_CONSTANT *) ->
      let type_index = read_u32 cur in
      let value = Codeview_types.parse_numeric_leaf cur in
      let name = read_cstring cur in
      Constant { type_index; value; name }
  | 0x110e (* S_PUB32 *) ->
      let flags = read_u32 cur in
      let offset = read_u32 cur in
      let segment = read_u16 cur in
      let name = read_cstring cur in
      Pub32 { flags; offset; segment; name }
  | 0x1012 (* S_FRAMEPROC *) ->
      let total_frame_bytes = read_u32 cur in
      let padding_frame_bytes = read_u32 cur in
      let offset_to_padding = read_u32 cur in
      let callee_saved_reg_bytes = read_u32 cur in
      let exception_handler_offset = read_u32 cur in
      let exception_handler_section = read_u16 cur in
      let frame_proc_flags = read_u32 cur in
      FrameProc
        {
          total_frame_bytes;
          padding_frame_bytes;
          offset_to_padding;
          callee_saved_reg_bytes;
          exception_handler_offset;
          exception_handler_section;
          frame_proc_flags;
        }
  | 0x1111 (* S_REGREL32 *) ->
      let offset = Unsigned.UInt32.to_int32 (read_u32 cur) in
      let type_index = read_u32 cur in
      let register = read_u16 cur in
      let name = read_cstring cur in
      RegRel32 { offset; type_index; register; name }
  | 0x110b (* S_BPREL32 *) ->
      let offset = Unsigned.UInt32.to_int32 (read_u32 cur) in
      let type_index = read_u32 cur in
      let name = read_cstring cur in
      BPRel32 { offset; type_index; name }
  | 0x1106 (* S_REGISTER *) ->
      let type_index = read_u32 cur in
      let register = read_u16 cur in
      let name = read_cstring cur in
      Register { type_index; register; name }
  | 0x1105 (* S_LABEL32 *) ->
      let offset = read_u32 cur in
      let segment = read_u16 cur in
      let flags = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
      let name = read_cstring cur in
      Label32 { offset; segment; flags; name }
  | 0x1124 (* S_UNAMESPACE *) ->
      let name = read_cstring cur in
      UNamespace { name }
  | _ ->
      let remaining = end_pos - cur.position in
      let data =
        if remaining > 0 then Object.Buffer.Read.fixed_string cur remaining
        else ""
      in
      Unknown { kind; data }

(** {2 Writing} *)

let write_u16_le buf v =
  Buffer.add_char buf (Char.chr (v land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xFF))

let write_u32_le buf v =
  Buffer.add_char buf (Char.chr (v land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 16) land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 24) land 0xFF))

let write_i32_le buf (v : int32) = write_u32_le buf (Int32.to_int v)

let write_cstring buf s =
  Buffer.add_string buf s;
  Buffer.add_char buf '\000'

let write_proc_record buf kind (p : proc_record) =
  write_u16_le buf kind;
  write_u32_le buf (Unsigned.UInt32.to_int p.parent);
  write_u32_le buf (Unsigned.UInt32.to_int p.end_);
  write_u32_le buf (Unsigned.UInt32.to_int p.next);
  write_u32_le buf (Unsigned.UInt32.to_int p.code_size);
  write_u32_le buf (Unsigned.UInt32.to_int p.debug_start);
  write_u32_le buf (Unsigned.UInt32.to_int p.debug_end);
  write_u32_le buf (Unsigned.UInt32.to_int p.type_index);
  write_u32_le buf (Unsigned.UInt32.to_int p.offset);
  write_u16_le buf p.segment;
  Buffer.add_char buf (Char.chr (p.flags land 0xFF));
  write_cstring buf p.name

let write_data_record buf kind (d : data_record) =
  write_u16_le buf kind;
  write_u32_le buf (Unsigned.UInt32.to_int d.type_index);
  write_u32_le buf (Unsigned.UInt32.to_int d.offset);
  write_u16_le buf d.segment;
  write_cstring buf d.name

let write_symbol_record (buf : Buffer.t) (record : symbol_record) : unit =
  let rec_buf = Buffer.create 64 in
  (match record with
  | Compile3
      {
        flags;
        machine;
        frontend_version = fe_maj, fe_min, fe_bld, fe_qfe;
        backend_version = be_maj, be_min, be_bld, be_qfe;
        version_string;
      } ->
      write_u16_le rec_buf 0x113c;
      write_u32_le rec_buf (Unsigned.UInt32.to_int flags);
      write_u16_le rec_buf machine;
      write_u16_le rec_buf fe_maj;
      write_u16_le rec_buf fe_min;
      write_u16_le rec_buf fe_bld;
      write_u16_le rec_buf fe_qfe;
      write_u16_le rec_buf be_maj;
      write_u16_le rec_buf be_min;
      write_u16_le rec_buf be_bld;
      write_u16_le rec_buf be_qfe;
      write_cstring rec_buf version_string
  | ObjName { signature; name } ->
      write_u16_le rec_buf 0x1101;
      write_u32_le rec_buf (Unsigned.UInt32.to_int signature);
      write_cstring rec_buf name
  | BuildInfo { id } ->
      write_u16_le rec_buf 0x114c;
      write_u32_le rec_buf (Unsigned.UInt32.to_int id)
  | GProc32 p -> write_proc_record rec_buf 0x1110 p
  | LProc32 p -> write_proc_record rec_buf 0x110f p
  | GProc32Id p -> write_proc_record rec_buf 0x1147 p
  | LProc32Id p -> write_proc_record rec_buf 0x1146 p
  | End -> write_u16_le rec_buf 0x0006
  | InlineSiteEnd -> write_u16_le rec_buf 0x114e
  | ProcIdEnd -> write_u16_le rec_buf 0x114f
  | GData32 d -> write_data_record rec_buf 0x110d d
  | LData32 d -> write_data_record rec_buf 0x110c d
  | GThread32 d -> write_data_record rec_buf 0x1113 d
  | LThread32 d -> write_data_record rec_buf 0x1112 d
  | Local { type_index; flags; name } ->
      write_u16_le rec_buf 0x113e;
      write_u32_le rec_buf (Unsigned.UInt32.to_int type_index);
      write_u16_le rec_buf flags;
      write_cstring rec_buf name
  | DefRangeFramePointerRel
      { offset; range_offset; range_section; range_length } ->
      write_u16_le rec_buf 0x1142;
      write_i32_le rec_buf offset;
      write_u32_le rec_buf (Unsigned.UInt32.to_int range_offset);
      write_u16_le rec_buf range_section;
      write_u16_le rec_buf range_length
  | DefRangeRegisterRel
      { base_register; offset; range_offset; range_section; range_length } ->
      write_u16_le rec_buf 0x1145;
      write_u16_le rec_buf base_register;
      write_u16_le rec_buf 0;
      (* flags *)
      write_i32_le rec_buf offset;
      write_u32_le rec_buf (Unsigned.UInt32.to_int range_offset);
      write_u16_le rec_buf range_section;
      write_u16_le rec_buf range_length
  | DefRangeRegister
      { register; may_have_no_name; range_offset; range_section; range_length }
    ->
      write_u16_le rec_buf 0x1141;
      write_u16_le rec_buf register;
      write_u16_le rec_buf may_have_no_name;
      write_u32_le rec_buf (Unsigned.UInt32.to_int range_offset);
      write_u16_le rec_buf range_section;
      write_u16_le rec_buf range_length
  | DefRangeFramePointerRelFullScope { offset } ->
      write_u16_le rec_buf 0x1144;
      write_i32_le rec_buf offset
  | Block32 { parent; end_; length; offset; segment; name } ->
      write_u16_le rec_buf 0x1103;
      write_u32_le rec_buf (Unsigned.UInt32.to_int parent);
      write_u32_le rec_buf (Unsigned.UInt32.to_int end_);
      write_u32_le rec_buf (Unsigned.UInt32.to_int length);
      write_u32_le rec_buf (Unsigned.UInt32.to_int offset);
      write_u16_le rec_buf segment;
      write_cstring rec_buf name
  | InlineSite { parent; end_; inlinee; annotations } ->
      write_u16_le rec_buf 0x114d;
      write_u32_le rec_buf (Unsigned.UInt32.to_int parent);
      write_u32_le rec_buf (Unsigned.UInt32.to_int end_);
      write_u32_le rec_buf (Unsigned.UInt32.to_int inlinee);
      Buffer.add_string rec_buf annotations
  | Udt { type_index; name } ->
      write_u16_le rec_buf 0x1108;
      write_u32_le rec_buf (Unsigned.UInt32.to_int type_index);
      write_cstring rec_buf name
  | Constant { type_index; value; name } ->
      write_u16_le rec_buf 0x1107;
      write_u32_le rec_buf (Unsigned.UInt32.to_int type_index);
      Codeview_types.write_numeric_leaf rec_buf value;
      write_cstring rec_buf name
  | Pub32 { flags; offset; segment; name } ->
      write_u16_le rec_buf 0x110e;
      write_u32_le rec_buf (Unsigned.UInt32.to_int flags);
      write_u32_le rec_buf (Unsigned.UInt32.to_int offset);
      write_u16_le rec_buf segment;
      write_cstring rec_buf name
  | FrameProc
      {
        total_frame_bytes;
        padding_frame_bytes;
        offset_to_padding;
        callee_saved_reg_bytes;
        exception_handler_offset;
        exception_handler_section;
        frame_proc_flags;
      } ->
      write_u16_le rec_buf 0x1012;
      write_u32_le rec_buf (Unsigned.UInt32.to_int total_frame_bytes);
      write_u32_le rec_buf (Unsigned.UInt32.to_int padding_frame_bytes);
      write_u32_le rec_buf (Unsigned.UInt32.to_int offset_to_padding);
      write_u32_le rec_buf (Unsigned.UInt32.to_int callee_saved_reg_bytes);
      write_u32_le rec_buf (Unsigned.UInt32.to_int exception_handler_offset);
      write_u16_le rec_buf exception_handler_section;
      write_u32_le rec_buf (Unsigned.UInt32.to_int frame_proc_flags)
  | RegRel32 { offset; type_index; register; name } ->
      write_u16_le rec_buf 0x1111;
      write_i32_le rec_buf offset;
      write_u32_le rec_buf (Unsigned.UInt32.to_int type_index);
      write_u16_le rec_buf register;
      write_cstring rec_buf name
  | BPRel32 { offset; type_index; name } ->
      write_u16_le rec_buf 0x110b;
      write_i32_le rec_buf offset;
      write_u32_le rec_buf (Unsigned.UInt32.to_int type_index);
      write_cstring rec_buf name
  | Register { type_index; register; name } ->
      write_u16_le rec_buf 0x1106;
      write_u32_le rec_buf (Unsigned.UInt32.to_int type_index);
      write_u16_le rec_buf register;
      write_cstring rec_buf name
  | Label32 { offset; segment; flags; name } ->
      write_u16_le rec_buf 0x1105;
      write_u32_le rec_buf (Unsigned.UInt32.to_int offset);
      write_u16_le rec_buf segment;
      Buffer.add_char rec_buf (Char.chr (flags land 0xFF));
      write_cstring rec_buf name
  | UNamespace { name } ->
      write_u16_le rec_buf 0x1124;
      write_cstring rec_buf name
  | Unknown { kind; data } ->
      write_u16_le rec_buf kind;
      Buffer.add_string rec_buf data);
  let content = Buffer.contents rec_buf in
  let len = String.length content in
  write_u16_le buf len;
  Buffer.add_string buf content;
  (* Pad to 4-byte alignment *)
  let total = 2 + len in
  let align = (4 - (total mod 4)) mod 4 in
  for _ = 1 to align do
    Buffer.add_char buf '\000'
  done

let parse_symbol_stream (cur : Object.Buffer.cursor) (total_bytes : int) :
    symbol_record Seq.t =
  let end_pos = cur.position + total_bytes in
  let rec next () =
    if cur.position >= end_pos then Seq.Nil
    else
      let rec_len = read_u16 cur in
      if rec_len = 0 then Seq.Nil
      else begin
        let record = parse_symbol_record cur rec_len in
        let record_end_unaligned = cur.position in
        let aligned = (record_end_unaligned + 3) land lnot 3 in
        if aligned > record_end_unaligned && aligned <= end_pos then
          Object.Buffer.seek cur aligned;
        Seq.Cons (record, next)
      end
  in
  next
