(** ARM64 (AArch64) Windows unwind information (.xdata).

    References:
    - LLVM: llvm/include/llvm/Support/Win64EH.h
    - LLVM: llvm/lib/MC/MCWin64EH.cpp (ARM64 emission)
    - Microsoft: https://docs.microsoft.com/en-us/cpp/build/arm64-exception-handling *)

open Pdb_types

module Buffer = Stdlib.Buffer

open Binary_writer

type unwind_code =
  | AllocSmall of { size : int }
  | AllocMedium of { size : int }
  | AllocLarge of { size : int }
  | SaveFPLR of { offset : int }
  | SaveFPLRX of { offset : int }
  | SaveR19R20X of { offset : int }
  | SaveRegP of { reg : int; offset : int }
  | SaveRegPX of { reg : int; offset : int }
  | SaveReg of { reg : int; offset : int }
  | SaveRegX of { reg : int; offset : int }
  | SaveLRPair of { reg : int; offset : int }
  | SaveFRegP of { reg : int; offset : int }
  | SaveFRegPX of { reg : int; offset : int }
  | SaveFReg of { reg : int; offset : int }
  | SaveFRegX of { reg : int; offset : int }
  | SetFP
  | AddFP of { offset : int }
  | Nop
  | End
  | SaveNext
  | PACSignLR
  | TrapFrame
  | MachineFrame
  | Context
  | ClearUnwoundToCall

type unwind_info = {
  function_length : int;
  has_exception_data : bool;
  codes : unwind_code list;
  exception_handler : u32 option;
}

let write_code (buf : Buffer.t) (code : unwind_code) : unit =
  match code with
  | AllocSmall { size } ->
      Buffer.add_char buf (Char.chr ((size lsr 4) land 0x1F))
  | AllocMedium { size } ->
      let hw = (size lsr 4) land 0x7FF in
      Buffer.add_char buf (Char.chr (0xC0 lor ((hw lsr 8) land 0x07)));
      Buffer.add_char buf (Char.chr (hw land 0xFF))
  | AllocLarge { size } ->
      let w = (size lsr 4) land 0xFFFFFF in
      Buffer.add_char buf (Char.chr 0xE0);
      Buffer.add_char buf (Char.chr ((w lsr 16) land 0xFF));
      Buffer.add_char buf (Char.chr ((w lsr 8) land 0xFF));
      Buffer.add_char buf (Char.chr (w land 0xFF))
  | SaveR19R20X { offset } ->
      Buffer.add_char buf (Char.chr (0x20 lor ((offset lsr 3) land 0x1F)))
  | SaveFPLR { offset } ->
      Buffer.add_char buf (Char.chr (0x40 lor ((offset lsr 3) land 0x3F)))
  | SaveFPLRX { offset } ->
      Buffer.add_char buf
        (Char.chr (0x80 lor (((offset lsr 3) - 1) land 0x3F)))
  | SaveRegP { reg; offset } ->
      Buffer.add_char buf (Char.chr (0xC8 lor ((reg land 0xC) lsr 2)));
      Buffer.add_char buf
        (Char.chr (((reg land 0x3) lsl 6) lor ((offset lsr 3) land 0x3F)))
  | SaveRegPX { reg; offset } ->
      Buffer.add_char buf (Char.chr (0xCC lor ((reg land 0xC) lsr 2)));
      Buffer.add_char buf
        (Char.chr
           (((reg land 0x3) lsl 6) lor (((offset lsr 3) - 1) land 0x3F)))
  | SaveReg { reg; offset } ->
      Buffer.add_char buf (Char.chr (0xD0 lor ((reg land 0xC) lsr 2)));
      Buffer.add_char buf
        (Char.chr (((reg land 0x3) lsl 6) lor ((offset lsr 3) land 0x3F)))
  | SaveRegX { reg; offset } ->
      Buffer.add_char buf (Char.chr (0xD4 lor ((reg land 0x8) lsr 3)));
      Buffer.add_char buf
        (Char.chr
           (((reg land 0x7) lsl 5) lor (((offset lsr 3) - 1) land 0x1F)))
  | SaveLRPair { reg; offset } ->
      Buffer.add_char buf (Char.chr (0xD6 lor ((reg land 0x7) lsr 2)));
      Buffer.add_char buf
        (Char.chr (((reg land 0x3) lsl 6) lor ((offset lsr 3) land 0x3F)))
  | SaveFRegP { reg; offset } ->
      Buffer.add_char buf (Char.chr (0xD8 lor ((reg land 0x4) lsr 2)));
      Buffer.add_char buf
        (Char.chr (((reg land 0x3) lsl 6) lor ((offset lsr 3) land 0x3F)))
  | SaveFRegPX { reg; offset } ->
      Buffer.add_char buf (Char.chr (0xDA lor ((reg land 0x4) lsr 2)));
      Buffer.add_char buf
        (Char.chr
           (((reg land 0x3) lsl 6) lor (((offset lsr 3) - 1) land 0x3F)))
  | SaveFReg { reg; offset } ->
      Buffer.add_char buf (Char.chr (0xDC lor ((reg land 0x4) lsr 2)));
      Buffer.add_char buf
        (Char.chr (((reg land 0x3) lsl 6) lor ((offset lsr 3) land 0x3F)))
  | SaveFRegX { reg; offset } ->
      Buffer.add_char buf (Char.chr 0xDE);
      Buffer.add_char buf
        (Char.chr
           (((reg land 0x7) lsl 5) lor (((offset lsr 3) - 1) land 0x1F)))
  | SetFP -> Buffer.add_char buf (Char.chr 0xE1)
  | AddFP { offset } ->
      Buffer.add_char buf (Char.chr 0xE2);
      Buffer.add_char buf (Char.chr ((offset lsr 3) land 0xFF))
  | Nop -> Buffer.add_char buf (Char.chr 0xE3)
  | End -> Buffer.add_char buf (Char.chr 0xE4)
  | SaveNext -> Buffer.add_char buf (Char.chr 0xE6)
  | TrapFrame -> Buffer.add_char buf (Char.chr 0xE8)
  | MachineFrame -> Buffer.add_char buf (Char.chr 0xE9)
  | Context -> Buffer.add_char buf (Char.chr 0xEA)
  | ClearUnwoundToCall -> Buffer.add_char buf (Char.chr 0xEC)
  | PACSignLR -> Buffer.add_char buf (Char.chr 0xFC)

let parse_code (bytes : string) (pos : int ref) : unwind_code =
  let b0 = Char.code bytes.[!pos] in
  incr pos;
  if b0 land 0xE0 = 0x00 then AllocSmall { size = (b0 land 0x1F) lsl 4 }
  else if b0 land 0xE0 = 0x20 then
    SaveR19R20X { offset = (b0 land 0x1F) lsl 3 }
  else if b0 land 0xC0 = 0x40 then SaveFPLR { offset = (b0 land 0x3F) lsl 3 }
  else if b0 land 0xC0 = 0x80 then
    SaveFPLRX { offset = ((b0 land 0x3F) + 1) lsl 3 }
  else if b0 land 0xF8 = 0xC0 then begin
    let b1 = Char.code bytes.[!pos] in
    incr pos;
    AllocMedium { size = (((b0 land 0x07) lsl 8) lor b1) lsl 4 }
  end
  else if b0 land 0xFC = 0xC8 then begin
    let b1 = Char.code bytes.[!pos] in
    incr pos;
    SaveRegP
      {
        reg = ((b0 land 0x3) lsl 2) lor ((b1 lsr 6) land 0x3);
        offset = (b1 land 0x3F) lsl 3;
      }
  end
  else if b0 land 0xFC = 0xCC then begin
    let b1 = Char.code bytes.[!pos] in
    incr pos;
    SaveRegPX
      {
        reg = ((b0 land 0x3) lsl 2) lor ((b1 lsr 6) land 0x3);
        offset = ((b1 land 0x3F) + 1) lsl 3;
      }
  end
  else if b0 land 0xFC = 0xD0 then begin
    let b1 = Char.code bytes.[!pos] in
    incr pos;
    SaveReg
      {
        reg = ((b0 land 0x3) lsl 2) lor ((b1 lsr 6) land 0x3);
        offset = (b1 land 0x3F) lsl 3;
      }
  end
  else if b0 land 0xFE = 0xD4 then begin
    let b1 = Char.code bytes.[!pos] in
    incr pos;
    SaveRegX
      {
        reg = ((b0 land 0x1) lsl 3) lor ((b1 lsr 5) land 0x7);
        offset = ((b1 land 0x1F) + 1) lsl 3;
      }
  end
  else if b0 land 0xFE = 0xD6 then begin
    let b1 = Char.code bytes.[!pos] in
    incr pos;
    SaveLRPair
      {
        reg = ((b0 land 0x1) lsl 2) lor ((b1 lsr 6) land 0x3);
        offset = (b1 land 0x3F) lsl 3;
      }
  end
  else if b0 land 0xFE = 0xD8 then begin
    (* SaveFRegP/PX encode a 3-bit reg: top bit in byte 0, low two bits in
       byte 1's high bits. Adjacent SaveRegP/PX (0xC8/0xCC) use a 4-bit
       reg by contrast -- don't be tempted to widen the mask here. *)
    let b1 = Char.code bytes.[!pos] in
    incr pos;
    SaveFRegP
      {
        reg = ((b0 land 0x1) lsl 2) lor ((b1 lsr 6) land 0x3);
        offset = (b1 land 0x3F) lsl 3;
      }
  end
  else if b0 land 0xFE = 0xDA then begin
    let b1 = Char.code bytes.[!pos] in
    incr pos;
    SaveFRegPX
      {
        reg = ((b0 land 0x1) lsl 2) lor ((b1 lsr 6) land 0x3);
        offset = ((b1 land 0x3F) + 1) lsl 3;
      }
  end
  else if b0 land 0xFE = 0xDC then begin
    let b1 = Char.code bytes.[!pos] in
    incr pos;
    SaveFReg
      {
        reg = ((b0 land 0x1) lsl 2) lor ((b1 lsr 6) land 0x3);
        offset = (b1 land 0x3F) lsl 3;
      }
  end
  else if b0 = 0xDE then begin
    let b1 = Char.code bytes.[!pos] in
    incr pos;
    SaveFRegX
      {
        reg = (b1 lsr 5) land 0x7;
        offset = ((b1 land 0x1F) + 1) lsl 3;
      }
  end
  else if b0 = 0xE0 then begin
    let b1 = Char.code bytes.[!pos] in
    let b2 = Char.code bytes.[!pos + 1] in
    let b3 = Char.code bytes.[!pos + 2] in
    pos := !pos + 3;
    AllocLarge { size = ((b1 lsl 16) lor (b2 lsl 8) lor b3) lsl 4 }
  end
  else if b0 = 0xE1 then SetFP
  else if b0 = 0xE2 then begin
    let b1 = Char.code bytes.[!pos] in
    incr pos;
    AddFP { offset = b1 lsl 3 }
  end
  else if b0 = 0xE3 then Nop
  else if b0 = 0xE4 then End
  else if b0 = 0xE6 then SaveNext
  else if b0 = 0xE8 then TrapFrame
  else if b0 = 0xE9 then MachineFrame
  else if b0 = 0xEA then Context
  else if b0 = 0xEC then ClearUnwoundToCall
  else if b0 = 0xFC then PACSignLR
  else Nop

let parse (cur : Object.Buffer.cursor) : unwind_info =
  (* The 4-byte [row1] is mandatory; everything else (extended-header
     [row2], epilog scopes, code words, exception handler) is governed
     by bit fields inside [row1]. Guard [row1] with [ensure] and wrap
     the variable tail in [try/with] so a truncated stream surfaces as
     Invalid_format rather than leaking a Bigarray bounds error. *)
  Object.Buffer.ensure cur 4 "ARM64 .pdata: truncated header";
  try
    let row1 = Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int in
    let function_length = (row1 land 0x3FFFF) * 4 in
    let x_flag = (row1 lsr 20) land 1 <> 0 in
    let e_flag = (row1 lsr 21) land 1 <> 0 in
    let epilog_count = (row1 lsr 22) land 0x1F in
    let code_words = (row1 lsr 27) land 0x1F in
    let _epilog_count, code_words =
      if epilog_count = 0 && code_words = 0 then begin
        let row2 = Object.Buffer.Read.u32 cur |> Unsigned.UInt32.to_int in
        let ext_epilog = (row2 lsr 8) land 0xFFFF in
        let ext_codes = row2 land 0xFF in
        (ext_epilog, ext_codes)
      end
      else (epilog_count, code_words)
    in
    if not e_flag then
      for _ = 1 to epilog_count do
        ignore (Object.Buffer.Read.u32 cur)
      done;
    let code_byte_count = code_words * 4 in
    let code_bytes =
      if code_byte_count > 0 then
        Object.Buffer.Read.fixed_string cur code_byte_count
      else ""
    in
    let codes = ref [] in
    let pos = ref 0 in
    while !pos < code_byte_count do
      let code = parse_code code_bytes pos in
      codes := code :: !codes;
      if code = End then pos := code_byte_count
    done;
    let exception_handler =
      if x_flag then Some (Object.Buffer.Read.u32 cur) else None
    in
    {
      function_length;
      has_exception_data = x_flag;
      codes = List.rev !codes;
      exception_handler;
    }
  with Invalid_argument _ ->
    Object.Buffer.invalid_format
      "ARM64 .pdata: truncated unwind codes or trailing exception handler"

let write (buf : Buffer.t) (info : unwind_info) : unit =
  let code_buf = Buffer.create 32 in
  List.iter (write_code code_buf) info.codes;
  let code_byte_len = Buffer.length code_buf in
  let code_words = (code_byte_len + 3) / 4 in
  let padded_len = code_words * 4 in
  let func_len_field = (info.function_length / 4) land 0x3FFFF in
  let x_bit = if info.has_exception_data then 1 else 0 in
  let e_bit = 1 in
  let row1 =
    func_len_field
    lor (x_bit lsl 20)
    lor (e_bit lsl 21)
    lor ((code_words land 0x1F) lsl 27)
  in
  write_u32_le buf row1;
  Buffer.add_string buf (Buffer.contents code_buf);
  for _ = 1 to padded_len - code_byte_len do
    Buffer.add_char buf '\000'
  done;
  match info.exception_handler with
  | Some rva -> write_u32_le buf (Unsigned.UInt32.to_int rva)
  | None -> ()
