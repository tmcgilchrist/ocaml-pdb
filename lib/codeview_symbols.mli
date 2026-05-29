(** CodeView symbol record definitions and parsing.

    Symbol records appear in module symbol streams (referenced from the DBI
    stream) and in the global/public symbol streams. Each record has a 2-byte
    length prefix, a 2-byte symbol kind, then payload. *)

open Pdb_types

type proc_record = {
  parent : u32;
  end_ : u32;
  next : u32;
  code_size : u32;
  debug_start : u32;
  debug_end : u32;
  type_index : Type_index.t;
  offset : u32;
  segment : int;
  flags : int;
  name : string;
}

type data_record = {
  type_index : Type_index.t;
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
  | BuildInfo of { id : Type_index.t }
  | GProc32 of proc_record
  | LProc32 of proc_record
  | GProc32Id of proc_record
  | LProc32Id of proc_record
  | End
  | GData32 of data_record
  | LData32 of data_record
  | GThread32 of data_record
  | LThread32 of data_record
  | Local of { type_index : Type_index.t; flags : int; name : string }
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
      inlinee : Type_index.t;
      annotations : string;
    }
  | InlineSiteEnd
  | ProcIdEnd
  | Udt of { type_index : Type_index.t; name : string }
  | Constant of { type_index : Type_index.t; value : int64; name : string }
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
      type_index : Type_index.t;
      register : int;
      name : string;
    }
  | BPRel32 of { offset : int32; type_index : Type_index.t; name : string }
  | Register of { type_index : Type_index.t; register : int; name : string }
  | Label32 of { offset : u32; segment : int; flags : int; name : string }
  | UNamespace of { name : string }
  | EnvBlock of { fields : string list }
  | Unknown of { kind : int; data : string }

val parse_symbol_record : Object.Buffer.cursor -> int -> symbol_record
(** [parse_symbol_record cur record_data_len] parses a single symbol
    record. The cursor should be positioned at the symbol kind u16.
    [record_data_len] is the byte count following the length prefix
    (so it includes the 2-byte symbol kind).
    Raises [Object.Buffer.Invalid_format] if the cursor has fewer than
    [record_data_len] bytes remaining or the record's declared length
    is smaller than the encoded fields require. *)

val write_symbol_record : Stdlib.Buffer.t -> symbol_record -> unit
(** [write_symbol_record buf record] serializes a symbol record
    including the length prefix and symbol kind. *)

val parse_symbol_stream : Object.Buffer.cursor -> int -> symbol_record Seq.t
(** [parse_symbol_stream cur total_bytes] lazily iterates symbol records.
    Raises [Object.Buffer.Invalid_format] (during iteration) on a malformed
    record. *)
