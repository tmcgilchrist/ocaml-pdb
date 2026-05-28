(** x86-64 Windows unwind information (.xdata).

    References:
    - LLVM: llvm/include/llvm/Support/Win64EH.h
    - LLVM: llvm/lib/MC/MCWin64EH.cpp
    - Microsoft: https://docs.microsoft.com/en-us/cpp/build/exception-handling-x64 *)

open Pdb_types

module Buffer = Stdlib.Buffer

open Binary_writer

type register =
  | RAX | RCX | RDX | RBX | RSP | RBP | RSI | RDI
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

let register_to_int = function
  | RAX -> 0 | RCX -> 1 | RDX -> 2 | RBX -> 3
  | RSP -> 4 | RBP -> 5 | RSI -> 6 | RDI -> 7
  | R8 -> 8 | R9 -> 9 | R10 -> 10 | R11 -> 11
  | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15

let int_to_register = function
  | 0 -> RAX | 1 -> RCX | 2 -> RDX | 3 -> RBX
  | 4 -> RSP | 5 -> RBP | 6 -> RSI | 7 -> RDI
  | 8 -> R8 | 9 -> R9 | 10 -> R10 | 11 -> R11
  | 12 -> R12 | 13 -> R13 | 14 -> R14 | 15 -> R15
  | n -> failwith (Printf.sprintf "invalid unwind register: %d" n)

type unwind_code =
  | PushNonVol of { code_offset : int; reg : register }
  | AllocSmall of { code_offset : int; size : int }
  | AllocLarge of { code_offset : int; size : int }
  | SetFPReg of { code_offset : int }
  | SaveNonVol of { code_offset : int; reg : register; offset : int }
  | SaveNonVolFar of { code_offset : int; reg : register; offset : int }
  | SaveXMM128 of { code_offset : int; reg : int; offset : int }
  | SaveXMM128Far of { code_offset : int; reg : int; offset : int }
  | PushMachFrame of { code_offset : int; error_code : bool }

type unwind_flags = {
  exception_handler : bool;
  termination_handler : bool;
  chain_info : bool;
}

type unwind_info = {
  version : int;
  flags : unwind_flags;
  size_of_prolog : int;
  frame_register : register option;
  frame_offset : int;
  unwind_codes : unwind_code list;
  exception_handler : u32 option;
}

let parse (cur : Object.Buffer.cursor) : unwind_info =
  let version_and_flags = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
  let version = version_and_flags land 0x07 in
  let flag_bits = (version_and_flags lsr 3) land 0x1F in
  let flags =
    {
      exception_handler = flag_bits land 0x01 <> 0;
      termination_handler = flag_bits land 0x02 <> 0;
      chain_info = flag_bits land 0x04 <> 0;
    }
  in
  let size_of_prolog = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
  let count_of_codes = Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int in
  let frame_reg_and_offset =
    Object.Buffer.Read.u8 cur |> Unsigned.UInt8.to_int
  in
  let frame_reg_num = frame_reg_and_offset land 0x0F in
  let frame_offset = (frame_reg_and_offset lsr 4) land 0x0F in
  let frame_register =
    if frame_reg_num = 0 then None
    else Some (int_to_register frame_reg_num)
  in
  let raw_slots =
    Array.init count_of_codes (fun _ ->
        Object.Buffer.Read.u16 cur |> Unsigned.UInt16.to_int)
  in
  if count_of_codes mod 2 <> 0 then ignore (Object.Buffer.Read.u16 cur);
  let codes = ref [] in
  let i = ref 0 in
  while !i < count_of_codes do
    let slot = raw_slots.(!i) in
    let code_offset = slot land 0xFF in
    let op_and_info = (slot lsr 8) land 0xFF in
    let op = op_and_info land 0x0F in
    let info = (op_and_info lsr 4) land 0x0F in
    (match op with
    | 0 ->
        codes :=
          PushNonVol { code_offset; reg = int_to_register info } :: !codes;
        incr i
    | 1 ->
        if info = 0 then begin
          let size = raw_slots.(!i + 1) * 8 in
          codes := AllocLarge { code_offset; size } :: !codes;
          i := !i + 2
        end
        else begin
          let lo = raw_slots.(!i + 1) in
          let hi = raw_slots.(!i + 2) in
          let size = lo lor (hi lsl 16) in
          codes := AllocLarge { code_offset; size } :: !codes;
          i := !i + 3
        end
    | 2 ->
        codes := AllocSmall { code_offset; size = (info * 8) + 8 } :: !codes;
        incr i
    | 3 ->
        codes := SetFPReg { code_offset } :: !codes;
        incr i
    | 4 ->
        let offset = raw_slots.(!i + 1) * 8 in
        codes :=
          SaveNonVol { code_offset; reg = int_to_register info; offset }
          :: !codes;
        i := !i + 2
    | 5 ->
        let lo = raw_slots.(!i + 1) in
        let hi = raw_slots.(!i + 2) in
        let offset = lo lor (hi lsl 16) in
        codes :=
          SaveNonVolFar { code_offset; reg = int_to_register info; offset }
          :: !codes;
        i := !i + 3
    | 8 ->
        let offset = raw_slots.(!i + 1) * 16 in
        codes := SaveXMM128 { code_offset; reg = info; offset } :: !codes;
        i := !i + 2
    | 9 ->
        let lo = raw_slots.(!i + 1) in
        let hi = raw_slots.(!i + 2) in
        let offset = lo lor (hi lsl 16) in
        codes := SaveXMM128Far { code_offset; reg = info; offset } :: !codes;
        i := !i + 3
    | 10 ->
        codes :=
          PushMachFrame { code_offset; error_code = info <> 0 } :: !codes;
        incr i
    | _ -> incr i)
  done;
  let exception_handler =
    if flags.exception_handler || flags.termination_handler then
      Some (Object.Buffer.Read.u32 cur)
    else None
  in
  {
    version;
    flags;
    size_of_prolog;
    frame_register;
    frame_offset;
    unwind_codes = List.rev !codes;
    exception_handler;
  }

let count_slots (code : unwind_code) : int =
  match code with
  | PushNonVol _ | AllocSmall _ | SetFPReg _ | PushMachFrame _ -> 1
  | SaveNonVol _ | SaveXMM128 _ -> 2
  | SaveNonVolFar _ | SaveXMM128Far _ -> 3
  | AllocLarge { size; _ } ->
      if size <= (512 * 1024) - 8 then 2 else 3

let write_code (buf : Buffer.t) (code : unwind_code) : unit =
  match code with
  | PushNonVol { code_offset; reg } ->
      let b0 = code_offset land 0xFF in
      let b1 = (register_to_int reg lsl 4) lor 0 in
      write_u16_le buf (b0 lor (b1 lsl 8))
  | AllocSmall { code_offset; size } ->
      let info = (size - 8) / 8 in
      write_u16_le buf ((code_offset land 0xFF) lor (((info lsl 4) lor 2) lsl 8))
  | AllocLarge { code_offset; size } ->
      if size <= (512 * 1024) - 8 then begin
        write_u16_le buf ((code_offset land 0xFF) lor (0x01 lsl 8));
        write_u16_le buf (size / 8)
      end
      else begin
        write_u16_le buf ((code_offset land 0xFF) lor (0x11 lsl 8));
        write_u16_le buf (size land 0xFFFF);
        write_u16_le buf ((size lsr 16) land 0xFFFF)
      end
  | SetFPReg { code_offset } ->
      write_u16_le buf ((code_offset land 0xFF) lor (0x03 lsl 8))
  | SaveNonVol { code_offset; reg; offset } ->
      let b1 = (register_to_int reg lsl 4) lor 4 in
      write_u16_le buf ((code_offset land 0xFF) lor (b1 lsl 8));
      write_u16_le buf (offset / 8)
  | SaveNonVolFar { code_offset; reg; offset } ->
      let b1 = (register_to_int reg lsl 4) lor 5 in
      write_u16_le buf ((code_offset land 0xFF) lor (b1 lsl 8));
      write_u16_le buf (offset land 0xFFFF);
      write_u16_le buf ((offset lsr 16) land 0xFFFF)
  | SaveXMM128 { code_offset; reg; offset } ->
      let b1 = (reg lsl 4) lor 8 in
      write_u16_le buf ((code_offset land 0xFF) lor (b1 lsl 8));
      write_u16_le buf (offset / 16)
  | SaveXMM128Far { code_offset; reg; offset } ->
      let b1 = (reg lsl 4) lor 9 in
      write_u16_le buf ((code_offset land 0xFF) lor (b1 lsl 8));
      write_u16_le buf (offset land 0xFFFF);
      write_u16_le buf ((offset lsr 16) land 0xFFFF)
  | PushMachFrame { code_offset; error_code } ->
      let b1 = ((if error_code then 1 else 0) lsl 4) lor 10 in
      write_u16_le buf ((code_offset land 0xFF) lor (b1 lsl 8))

let write (buf : Buffer.t) (info : unwind_info) : unit =
  let flag_bits =
    (if info.flags.exception_handler then 1 else 0)
    lor (if info.flags.termination_handler then 2 else 0)
    lor (if info.flags.chain_info then 4 else 0)
  in
  let version_and_flags = (info.version land 0x07) lor (flag_bits lsl 3) in
  let count_of_codes =
    List.fold_left (fun acc c -> acc + count_slots c) 0 info.unwind_codes
  in
  let frame_reg_num =
    match info.frame_register with
    | None -> 0
    | Some reg -> register_to_int reg
  in
  let frame_reg_and_offset =
    (frame_reg_num land 0x0F) lor ((info.frame_offset land 0x0F) lsl 4)
  in
  Buffer.add_char buf (Char.chr version_and_flags);
  Buffer.add_char buf (Char.chr (info.size_of_prolog land 0xFF));
  Buffer.add_char buf (Char.chr (count_of_codes land 0xFF));
  Buffer.add_char buf (Char.chr frame_reg_and_offset);
  List.iter (write_code buf) info.unwind_codes;
  if count_of_codes mod 2 <> 0 then write_u16_le buf 0;
  match info.exception_handler with
  | Some handler_rva ->
      write_u32_le buf (Unsigned.UInt32.to_int handler_rva)
  | None ->
      if count_of_codes < 2 then write_u32_le buf 0
