(** C13 debug subsections.

    References:
    - LLVM: llvm/include/llvm/DebugInfo/CodeView/DebugLinesSubsection.h
    - LLVM: llvm/include/llvm/DebugInfo/CodeView/DebugChecksumsSubsection.h
    - LLVM: llvm/lib/DebugInfo/CodeView/DebugLinesSubsection.cpp *)

open Pdb_types
module Buffer = Stdlib.Buffer

open Binary_writer

type line_entry = {
  offset : u32;
  line_start : int;
  delta_line_end : int;
  is_statement : bool;
}

type column_entry = { start_column : int; end_column : int }

type line_block = {
  file_index : u32;
  lines : line_entry array;
  columns : column_entry array option;
}

type lines_subsection = {
  contrib_offset : u32;
  contrib_segment : int;
  flags : int;
  contrib_size : u32;
  blocks : line_block array;
}

type checksum_kind = None | MD5 | SHA1 | SHA256

type file_checksum_entry = {
  file_name_offset : u32;
  checksum_kind : checksum_kind;
  checksum : string;
}

type inlinee_line = { inlinee : u32; file_id : u32; source_line : u32 }

type frame_data_entry = {
  rva_start : u32;
  code_size : u32;
  local_size : u32;
  params_size : u32;
  max_stack_size : u32;
  frame_func : u32;
  prolog_size : int;
  saved_regs_size : int;
  flags : u32;
}

type cross_module_export = { local : u32; global : u32 }

type cross_module_import = {
  module_name_offset : u32;
  references : u32 array;
}

type subsection =
  | Lines of lines_subsection
  | FileChecksums of file_checksum_entry array
  | StringTable of string array
  | InlineeLines of inlinee_line array
  | FrameData of frame_data_entry array
  | CrossModuleExports of cross_module_export array
  | CrossModuleImports of cross_module_import array
  | Unknown of { kind : int; data : string }


let checksum_kind_of_int = function
  | 0 -> None
  | 1 -> MD5
  | 2 -> SHA1
  | 3 -> SHA256
  | _ -> None

let int_of_checksum_kind = function
  | None -> 0
  | MD5 -> 1
  | SHA1 -> 2
  | SHA256 -> 3

let checksum_size = function None -> 0 | MD5 -> 16 | SHA1 -> 20 | SHA256 -> 32

let parse_line_entry cur =
  let offset = read_u32 cur in
  let flags = Unsigned.UInt32.to_int (read_u32 cur) in
  let line_start = flags land 0x00FFFFFF in
  let delta_line_end = (flags lsr 24) land 0x7F in
  let is_statement = flags land 0x80000000 <> 0 in
  { offset; line_start; delta_line_end; is_statement }

let parse_lines cur sub_end =
  (* LineFragmentHeader: u32 offset, u16 segment, u16 flags, u32 code_size *)
  let contrib_offset = read_u32 cur in
  let contrib_segment = read_u16 cur in
  let flags = read_u16 cur in
  let contrib_size = read_u32 cur in
  let has_columns = flags land 0x0001 <> 0 in
  (* Parse line blocks *)
  let blocks = ref [] in
  while cur.position < sub_end do
    let file_index = read_u32 cur in
    let num_lines = Unsigned.UInt32.to_int (read_u32 cur) in
    let _block_size = read_u32 cur in
    let lines = Array.init num_lines (fun _ -> parse_line_entry cur) in
    let columns =
      if has_columns then
        Some
          (Array.init num_lines (fun _ ->
               let start_column = read_u16 cur in
               let end_column = read_u16 cur in
               { start_column; end_column }))
      else Option.None
    in
    blocks := { file_index; lines; columns } :: !blocks
  done;
  {
    contrib_offset;
    contrib_segment;
    flags;
    contrib_size;
    blocks = Array.of_list (List.rev !blocks);
  }

let parse_file_checksums (cur : Object.Buffer.cursor) sub_end =
  let entries = ref [] in
  while cur.position < sub_end do
    let file_name_offset = read_u32 cur in
    let checksum_len = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
    let kind_raw = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
    let checksum_kind = checksum_kind_of_int kind_raw in
    let checksum =
      if checksum_len > 0 then Object.Buffer.Read.fixed_string cur checksum_len
      else ""
    in
    entries := { file_name_offset; checksum_kind; checksum } :: !entries;
    (* Align to 4 bytes *)
    let pos = cur.position in
    let aligned = (pos + 3) land lnot 3 in
    if aligned > pos && aligned <= sub_end then Object.Buffer.seek cur aligned
  done;
  Array.of_list (List.rev !entries)

let parse_string_table (cur : Object.Buffer.cursor) sub_end =
  let strings = ref [] in
  while cur.position < sub_end do
    match Object.Buffer.Read.zero_string cur () with
    | Some s -> if String.length s > 0 then strings := s :: !strings
    | Option.None -> Object.Buffer.seek cur sub_end
  done;
  Array.of_list (List.rev !strings)

let parse_inlinee_lines (cur : Object.Buffer.cursor) sub_end =
  (* First u32 is the signature/version *)
  let _signature = read_u32 cur in
  let entries = ref [] in
  while cur.position + 12 <= sub_end do
    let inlinee = read_u32 cur in
    let file_id = read_u32 cur in
    let source_line = read_u32 cur in
    entries := { inlinee; file_id; source_line } :: !entries
  done;
  if cur.position < sub_end then Object.Buffer.seek cur sub_end;
  Array.of_list (List.rev !entries)

let parse_frame_data (cur : Object.Buffer.cursor) sub_end =
  (* First u32 is a relocation pointer (RVA of the contribution) *)
  let _reloc_ptr = read_u32 cur in
  (* Each FrameData entry is 32 bytes *)
  let entries = ref [] in
  while cur.position + 32 <= sub_end do
    let rva_start = read_u32 cur in
    let code_size = read_u32 cur in
    let local_size = read_u32 cur in
    let params_size = read_u32 cur in
    let max_stack_size = read_u32 cur in
    let frame_func = read_u32 cur in
    let prolog_size = read_u16 cur in
    let saved_regs_size = read_u16 cur in
    let flags = read_u32 cur in
    entries :=
      { rva_start; code_size; local_size; params_size; max_stack_size;
        frame_func; prolog_size; saved_regs_size; flags }
      :: !entries
  done;
  if cur.position < sub_end then Object.Buffer.seek cur sub_end;
  Array.of_list (List.rev !entries)

(* CrossModuleExports: array of { u32 Local; u32 Global }, fills the subsection. *)
let parse_cross_module_exports (cur : Object.Buffer.cursor) sub_end =
  let entries = ref [] in
  while cur.position + 8 <= sub_end do
    let local = read_u32 cur in
    let global = read_u32 cur in
    entries := { local; global } :: !entries
  done;
  if cur.position < sub_end then Object.Buffer.seek cur sub_end;
  Array.of_list (List.rev !entries)

(* CrossModuleImports: sequence of { u32 ModuleNameOffset; u32 Count; u32[Count] ids }. *)
let parse_cross_module_imports (cur : Object.Buffer.cursor) sub_end =
  let entries = ref [] in
  while cur.position + 8 <= sub_end do
    let module_name_offset = read_u32 cur in
    let count = Unsigned.UInt32.to_int (read_u32 cur) in
    Object.Buffer.ensure cur (count * 4)
      (Printf.sprintf
         "CrossModuleImports: %d references at offset %d overrun subsection"
         count (cur.position - 8));
    let references = Array.init count (fun _ -> read_u32 cur) in
    entries := { module_name_offset; references } :: !entries
  done;
  if cur.position < sub_end then Object.Buffer.seek cur sub_end;
  Array.of_list (List.rev !entries)

let parse_subsections (cur : Object.Buffer.cursor) total_bytes =
  let end_pos = cur.position + total_bytes in
  let rec next () =
    if cur.position >= end_pos then Seq.Nil
    else begin
      Object.Buffer.ensure cur 8
        "C13 debug subsection: truncated kind/size header";
      let kind = Unsigned.UInt32.to_int (read_u32 cur) in
      let size = Unsigned.UInt32.to_int (read_u32 cur) in
      if cur.position + size > end_pos then
        Object.Buffer.invalid_format
          (Printf.sprintf
             "C13 debug subsection 0x%x: size %d at offset %d overruns stream end"
             kind size (cur.position - 8));
      let sub_end = cur.position + size in
      let sub =
        match kind with
        | 0xf2 -> Lines (parse_lines cur sub_end)
        | 0xf4 -> FileChecksums (parse_file_checksums cur sub_end)
        | 0xf3 -> StringTable (parse_string_table cur sub_end)
        | 0xf6 -> InlineeLines (parse_inlinee_lines cur sub_end)
        | 0xf5 -> FrameData (parse_frame_data cur sub_end)
        | 0xf7 -> CrossModuleImports (parse_cross_module_imports cur sub_end)
        | 0xf8 -> CrossModuleExports (parse_cross_module_exports cur sub_end)
        | _ ->
            let data =
              if size > 0 then Object.Buffer.Read.fixed_string cur size else ""
            in
            Unknown { kind; data }
      in
      (* Ensure cursor is at sub_end, aligned to 4 *)
      if cur.position < sub_end then Object.Buffer.seek cur sub_end;
      let aligned = (sub_end + 3) land lnot 3 in
      if aligned > sub_end && aligned <= end_pos then
        Object.Buffer.seek cur aligned;
      Seq.Cons (sub, next)
    end
  in
  next

(** {2 Writing} *)

let write_subsection buf sub =
  let content_buf = Buffer.create 128 in
  let kind =
    match sub with
    | Lines ls ->
        (* Determine if any block has column info *)
        let has_columns =
          Array.exists
            (fun (b : line_block) -> b.columns <> Option.None)
            ls.blocks
        in
        let flags =
          if has_columns then ls.flags lor 0x0001 else ls.flags land lnot 0x0001
        in
        (* LineFragmentHeader *)
        write_u32_le content_buf (Unsigned.UInt32.to_int ls.contrib_offset);
        write_u16_le content_buf ls.contrib_segment;
        write_u16_le content_buf flags;
        write_u32_le content_buf (Unsigned.UInt32.to_int ls.contrib_size);
        Array.iter
          (fun (block : line_block) ->
            let num_lines = Array.length block.lines in
            write_u32_le content_buf (Unsigned.UInt32.to_int block.file_index);
            write_u32_le content_buf num_lines;
            (* BlockSize = 12 (header) + num_lines * 8 (lines)
               + if columns: num_lines * 4 (columns) *)
            let col_size =
              if has_columns then num_lines * 4 else 0
            in
            write_u32_le content_buf (12 + (num_lines * 8) + col_size);
            Array.iter
              (fun (le : line_entry) ->
                write_u32_le content_buf (Unsigned.UInt32.to_int le.offset);
                let f =
                  le.line_start land 0x00FFFFFF
                  lor ((le.delta_line_end land 0x7F) lsl 24)
                  lor (if le.is_statement then 0x80000000 else 0)
                in
                write_u32_le content_buf f)
              block.lines;
            if has_columns then
              let cols =
                match block.columns with
                | Some c -> c
                | Option.None ->
                    (* Fill with zeros if this block has no columns but others do *)
                    Array.make num_lines { start_column = 0; end_column = 0 }
              in
              Array.iter
                (fun (c : column_entry) ->
                  write_u16_le content_buf c.start_column;
                  write_u16_le content_buf c.end_column)
                cols)
          ls.blocks;
        0xf2
    | FileChecksums entries ->
        Array.iter
          (fun (e : file_checksum_entry) ->
            write_u32_le content_buf (Unsigned.UInt32.to_int e.file_name_offset);
            let cs_len = String.length e.checksum in
            Buffer.add_char content_buf (Char.chr cs_len);
            Buffer.add_char content_buf
              (Char.chr (int_of_checksum_kind e.checksum_kind));
            Buffer.add_string content_buf e.checksum;
            write_padding_to_align content_buf 4)
          entries;
        0xf4
    | StringTable strings ->
        Array.iter
          (fun s ->
            Buffer.add_string content_buf s;
            Buffer.add_char content_buf '\000')
          strings;
        0xf3
    | InlineeLines entries ->
        write_u32_le content_buf 0;
        (* signature *)
        Array.iter
          (fun (e : inlinee_line) ->
            write_u32_le content_buf (Unsigned.UInt32.to_int e.inlinee);
            write_u32_le content_buf (Unsigned.UInt32.to_int e.file_id);
            write_u32_le content_buf (Unsigned.UInt32.to_int e.source_line))
          entries;
        0xf6
    | FrameData entries ->
        (* Relocation pointer (RVA of contribution start) *)
        write_u32_le content_buf 0;
        Array.iter
          (fun (e : frame_data_entry) ->
            write_u32_le content_buf (Unsigned.UInt32.to_int e.rva_start);
            write_u32_le content_buf (Unsigned.UInt32.to_int e.code_size);
            write_u32_le content_buf (Unsigned.UInt32.to_int e.local_size);
            write_u32_le content_buf (Unsigned.UInt32.to_int e.params_size);
            write_u32_le content_buf (Unsigned.UInt32.to_int e.max_stack_size);
            write_u32_le content_buf (Unsigned.UInt32.to_int e.frame_func);
            write_u16_le content_buf e.prolog_size;
            write_u16_le content_buf e.saved_regs_size;
            write_u32_le content_buf (Unsigned.UInt32.to_int e.flags))
          entries;
        0xf5
    | CrossModuleExports entries ->
        Array.iter
          (fun (e : cross_module_export) ->
            write_u32_le content_buf (Unsigned.UInt32.to_int e.local);
            write_u32_le content_buf (Unsigned.UInt32.to_int e.global))
          entries;
        0xf8
    | CrossModuleImports entries ->
        Array.iter
          (fun (e : cross_module_import) ->
            write_u32_le content_buf
              (Unsigned.UInt32.to_int e.module_name_offset);
            write_u32_le content_buf (Array.length e.references);
            Array.iter
              (fun r -> write_u32_le content_buf (Unsigned.UInt32.to_int r))
              e.references)
          entries;
        0xf7
    | Unknown { kind; data } ->
        Buffer.add_string content_buf data;
        kind
  in
  let content = Buffer.contents content_buf in
  write_u32_le buf kind;
  write_u32_le buf (String.length content);
  Buffer.add_string buf content;
  write_padding_to_align buf 4
