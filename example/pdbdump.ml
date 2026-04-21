(** pdbdump -- a tool for dumping PDB file contents.

    Usage: pdbdump [OPTIONS] FILE.pdb *)

let buffer_of_file path =
  let fd = Unix.openfile path [ Unix.O_RDONLY ] 0 in
  let len = Unix.lseek fd 0 Unix.SEEK_END in
  let buf =
    Bigarray.array1_of_genarray
      (Unix.map_file fd Bigarray.int8_unsigned Bigarray.c_layout false [| len |])
  in
  Unix.close fd;
  buf

let dump_summary msf =
  let sb = Pdb.Msf.superblock msf in
  Printf.printf "MSF Summary:\n";
  Printf.printf "  Block size:    %d\n" (Unsigned.UInt32.to_int sb.block_size);
  Printf.printf "  Block count:   %d\n" (Unsigned.UInt32.to_int sb.num_blocks);
  Printf.printf "  Stream count:  %d\n" (Pdb.Msf.stream_count msf);
  for i = 0 to Pdb.Msf.stream_count msf - 1 do
    match Pdb.Msf.get_stream msf i with
    | Some s ->
        Printf.printf "  Stream %2d:     %d bytes\n" i (Bigarray.Array1.dim s)
    | None -> Printf.printf "  Stream %2d:     (empty)\n" i
  done;
  Printf.printf "\n"

let dump_pdb_stream msf =
  match Pdb.Msf.get_stream msf 1 with
  | None -> Printf.printf "PDB Info Stream: not present\n\n"
  | Some stream ->
      let cur = Object.Buffer.cursor stream in
      let info = Pdb.Pdb_stream.read cur in
      Printf.printf "PDB Info Stream:\n";
      Printf.printf "  Version:    %s\n"
        (Pdb.Pdb_stream.string_of_pdb_version info.version);
      Printf.printf "  Signature:  0x%08X\n"
        (Unsigned.UInt32.to_int info.signature);
      Printf.printf "  Age:        %d\n" (Unsigned.UInt32.to_int info.age);
      Printf.printf "  GUID:       %s\n"
        (Pdb.Pdb_types.string_of_guid info.guid);
      if info.named_streams <> [] then begin
        Printf.printf "  Named Streams:\n";
        List.iter
          (fun (name, idx) -> Printf.printf "    %-30s -> Stream %d\n" name idx)
          info.named_streams
      end;
      if info.features <> [] then begin
        Printf.printf "  Features:\n";
        List.iter
          (fun f ->
            Printf.printf "    %s\n"
              (match f with
              | Pdb.Pdb_stream.ContainsIdStream -> "ContainsIdStream"
              | NoTypeMerging -> "NoTypeMerging"
              | MinimalDebugInfo -> "MinimalDebugInfo"))
          info.features
      end;
      Printf.printf "\n"

let dump_types msf stream_idx label =
  match Pdb.Msf.get_stream msf stream_idx with
  | None -> Printf.printf "%s: not present\n\n" label
  | Some stream ->
      let cur = Object.Buffer.cursor stream in
      let header = Pdb.Tpi.parse_header cur in
      let num = Pdb.Tpi.num_type_records header in
      Printf.printf "%s (%d records, index range 0x%X-0x%X):\n" label num
        (Unsigned.UInt32.to_int header.type_index_begin)
        (Unsigned.UInt32.to_int header.type_index_end);
      let records = Pdb.Tpi.parse_type_records cur header in
      let idx = ref (Unsigned.UInt32.to_int header.type_index_begin) in
      Seq.iter
        (fun r ->
          Printf.printf "  0x%04X: %s\n" !idx
            (match r with
            | Pdb.Codeview_types.Modifier _ -> "LF_MODIFIER"
            | Pointer _ -> "LF_POINTER"
            | Procedure _ -> "LF_PROCEDURE"
            | MFunction _ -> "LF_MFUNCTION"
            | ArgList { args } ->
                Printf.sprintf "LF_ARGLIST (%d args)" (Array.length args)
            | FieldList { members } ->
                Printf.sprintf "LF_FIELDLIST (%d members)" (List.length members)
            | Array { name; _ } -> Printf.sprintf "LF_ARRAY \"%s\"" name
            | Class { name; _ } -> Printf.sprintf "LF_CLASS \"%s\"" name
            | Structure { name; _ } -> Printf.sprintf "LF_STRUCTURE \"%s\"" name
            | Interface { name; _ } -> Printf.sprintf "LF_INTERFACE \"%s\"" name
            | Union { name; _ } -> Printf.sprintf "LF_UNION \"%s\"" name
            | Enum { name; _ } -> Printf.sprintf "LF_ENUM \"%s\"" name
            | Bitfield _ -> "LF_BITFIELD"
            | VTShape _ -> "LF_VTSHAPE"
            | MethodList _ -> "LF_METHODLIST"
            | FuncId { name; _ } -> Printf.sprintf "LF_FUNC_ID \"%s\"" name
            | MFuncId { name; _ } -> Printf.sprintf "LF_MFUNC_ID \"%s\"" name
            | StringId { str; _ } -> Printf.sprintf "LF_STRING_ID \"%s\"" str
            | BuildInfo { args } ->
                Printf.sprintf "LF_BUILDINFO (%d args)" (Array.length args)
            | UdtSrcLine { line; _ } ->
                Printf.sprintf "LF_UDT_SRC_LINE (line %d)"
                  (Unsigned.UInt32.to_int line)
            | UdtModSrcLine { line; _ } ->
                Printf.sprintf "LF_UDT_MOD_SRC_LINE (line %d)"
                  (Unsigned.UInt32.to_int line)
            | SubstrList _ -> "LF_SUBSTR_LIST"
            | Unknown { kind; _ } -> Printf.sprintf "Unknown(0x%04X)" kind);
          incr idx)
        records;
      Printf.printf "\n"

let dump_dbi msf =
  match Pdb.Msf.get_stream msf 3 with
  | None -> Printf.printf "DBI Stream: not present\n\n"
  | Some stream ->
      let cur = Object.Buffer.cursor stream in
      let dbi = Pdb.Dbi.parse cur in
      Printf.printf "DBI Stream:\n";
      Printf.printf "  Machine:       0x%04X\n" dbi.header.machine;
      Printf.printf "  Age:           %d\n"
        (Unsigned.UInt32.to_int dbi.header.age);
      Printf.printf "  Modules:       %d\n" (Array.length dbi.modules);
      Printf.printf "  Section Contribs: %d\n"
        (Array.length dbi.section_contributions);
      Printf.printf "\n";
      Array.iteri
        (fun i (m : Pdb.Dbi.module_info) ->
          Printf.printf "  Module %d:\n" i;
          Printf.printf "    Name:        %s\n" m.module_name;
          Printf.printf "    Obj:         %s\n" m.obj_file_name;
          Printf.printf "    Sym Stream:  %d\n" m.module_sym_stream;
          Printf.printf "    Sym Bytes:   %d\n" m.sym_byte_size;
          Printf.printf "    C13 Bytes:   %d\n" m.c13_byte_size)
        dbi.modules;
      Printf.printf "\n"

let dump_symbols msf =
  match Pdb.Msf.get_stream msf 3 with
  | None -> ()
  | Some stream ->
      let cur = Object.Buffer.cursor stream in
      let dbi = Pdb.Dbi.parse cur in
      Printf.printf "Module Symbols:\n";
      Array.iteri
        (fun i (m : Pdb.Dbi.module_info) ->
          let syms = Pdb.Dbi.module_symbols msf m in
          let sym_list = List.of_seq syms in
          if sym_list <> [] then begin
            Printf.printf "  Module %d (%s):\n" i m.module_name;
            List.iter
              (fun s ->
                Printf.printf "    %s\n"
                  (match s with
                  | Pdb.Codeview_symbols.Compile3 { version_string; _ } ->
                      Printf.sprintf "S_COMPILE3 \"%s\"" version_string
                  | ObjName { name; _ } ->
                      Printf.sprintf "S_OBJNAME \"%s\"" name
                  | BuildInfo { id } ->
                      Printf.sprintf "S_BUILDINFO id=0x%X"
                        (Unsigned.UInt32.to_int id)
                  | GProc32 p | GProc32Id p ->
                      Printf.sprintf "S_GPROC32 \"%s\" size=%d" p.name
                        (Unsigned.UInt32.to_int p.code_size)
                  | LProc32 p | LProc32Id p ->
                      Printf.sprintf "S_LPROC32 \"%s\" size=%d" p.name
                        (Unsigned.UInt32.to_int p.code_size)
                  | End -> "S_END"
                  | InlineSiteEnd -> "S_INLINESITE_END"
                  | ProcIdEnd -> "S_PROC_ID_END"
                  | GData32 d -> Printf.sprintf "S_GDATA32 \"%s\"" d.name
                  | LData32 d -> Printf.sprintf "S_LDATA32 \"%s\"" d.name
                  | GThread32 d -> Printf.sprintf "S_GTHREAD32 \"%s\"" d.name
                  | LThread32 d -> Printf.sprintf "S_LTHREAD32 \"%s\"" d.name
                  | Local { name; _ } -> Printf.sprintf "S_LOCAL \"%s\"" name
                  | DefRangeFramePointerRel { offset; _ } ->
                      Printf.sprintf "S_DEFRANGE_FRAMEPOINTER_REL offset=%ld"
                        offset
                  | DefRangeRegisterRel { base_register; offset; _ } ->
                      Printf.sprintf "S_DEFRANGE_REGISTER_REL reg=%d offset=%ld"
                        base_register offset
                  | DefRangeRegister { register; _ } ->
                      Printf.sprintf "S_DEFRANGE_REGISTER reg=%d" register
                  | DefRangeFramePointerRelFullScope { offset } ->
                      Printf.sprintf
                        "S_DEFRANGE_FRAMEPOINTER_REL_FULL_SCOPE offset=%ld"
                        offset
                  | Block32 { name; _ } ->
                      Printf.sprintf "S_BLOCK32 \"%s\"" name
                  | InlineSite { inlinee; _ } ->
                      Printf.sprintf "S_INLINESITE inlinee=0x%X"
                        (Unsigned.UInt32.to_int inlinee)
                  | Udt { name; _ } -> Printf.sprintf "S_UDT \"%s\"" name
                  | Constant { name; value; _ } ->
                      Printf.sprintf "S_CONSTANT \"%s\" = %Ld" name value
                  | Pub32 { name; _ } -> Printf.sprintf "S_PUB32 \"%s\"" name
                  | FrameProc { total_frame_bytes; _ } ->
                      Printf.sprintf "S_FRAMEPROC frame=%d"
                        (Unsigned.UInt32.to_int total_frame_bytes)
                  | RegRel32 { name; register; offset; _ } ->
                      Printf.sprintf "S_REGREL32 \"%s\" reg=%d offset=%ld" name
                        register offset
                  | BPRel32 { name; offset; _ } ->
                      Printf.sprintf "S_BPREL32 \"%s\" offset=%ld" name offset
                  | Register { name; register; _ } ->
                      Printf.sprintf "S_REGISTER \"%s\" reg=%d" name register
                  | Label32 { name; _ } ->
                      Printf.sprintf "S_LABEL32 \"%s\"" name
                  | UNamespace { name } ->
                      Printf.sprintf "S_UNAMESPACE \"%s\"" name
                  | EnvBlock { fields } ->
                      Printf.sprintf "S_ENVBLOCK (%d fields)" (List.length fields)
                  | Unknown { kind; _ } -> Printf.sprintf "Unknown(0x%04X)" kind))
              sym_list
          end)
        dbi.modules;
      Printf.printf "\n"

let run path summary pdb_stream types ids dbi symbols all =
  let buf = buffer_of_file path in
  let msf = Pdb.Msf.read buf in
  let show_all =
    all || not (summary || pdb_stream || types || ids || dbi || symbols)
  in
  if summary || show_all then dump_summary msf;
  if pdb_stream || show_all then dump_pdb_stream msf;
  if types || show_all then dump_types msf 2 "TPI Stream";
  if ids || show_all then dump_types msf 4 "IPI Stream";
  if dbi || show_all then dump_dbi msf;
  if symbols || show_all then dump_symbols msf;
  ()

open Cmdliner

let path_arg =
  let doc = "Path to PDB file." in
  Arg.(required & pos 0 (some string) Option.None & info [] ~docv:"FILE" ~doc)

let summary_flag =
  let doc = "Print MSF summary (stream count, sizes)." in
  Arg.(value & flag & info [ "summary" ] ~doc)

let pdb_stream_flag =
  let doc = "Dump PDB info stream (GUID, age, version)." in
  Arg.(value & flag & info [ "pdb-stream" ] ~doc)

let types_flag =
  let doc = "Dump TPI type records." in
  Arg.(value & flag & info [ "types" ] ~doc)

let ids_flag =
  let doc = "Dump IPI id records." in
  Arg.(value & flag & info [ "ids" ] ~doc)

let dbi_flag =
  let doc = "Dump DBI stream (modules, section contributions)." in
  Arg.(value & flag & info [ "dbi" ] ~doc)

let symbols_flag =
  let doc = "Dump symbol records from all modules." in
  Arg.(value & flag & info [ "symbols" ] ~doc)

let all_flag =
  let doc = "Dump everything." in
  Arg.(value & flag & info [ "all" ] ~doc)

let cmd =
  let doc = "Dump PDB file contents" in
  let info = Cmd.info "pdbdump" ~doc in
  let term =
    Term.(
      const run $ path_arg $ summary_flag $ pdb_stream_flag $ types_flag
      $ ids_flag $ dbi_flag $ symbols_flag $ all_flag)
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
