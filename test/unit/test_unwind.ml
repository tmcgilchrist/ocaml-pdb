(** Tests for x86-64 and ARM64 Windows unwind info read/write. *)

module Buffer = Stdlib.Buffer

open Test_support

let u32 n = Unsigned.UInt32.of_int n

(** {2 x86-64 register encoding} *)

let test_x64_register_roundtrip () =
  let open Pdb.Unwind.X64 in
  let regs =
    [ RAX; RCX; RDX; RBX; RSP; RBP; RSI; RDI; R8; R9; R10; R11; R12; R13;
      R14; R15 ]
  in
  List.iteri
    (fun i reg ->
      Alcotest.(check int) (Printf.sprintf "reg %d" i) i
        (register_to_int reg);
      let rt = int_to_register i in
      Alcotest.(check int) (Printf.sprintf "roundtrip %d" i)
        (register_to_int reg) (register_to_int rt))
    regs

(** {2 x86-64 UNWIND_INFO} *)

let test_x64_minimal () =
  let open Pdb.Unwind.X64 in
  let info =
    {
      version = 1;
      flags =
        { exception_handler = false; termination_handler = false;
          chain_info = false };
      size_of_prolog = 8;
      frame_register = Some RBP;
      frame_offset = 0;
      unwind_codes =
        [
          AllocSmall { code_offset = 8; size = 64 };
          SetFPReg { code_offset = 4 };
          PushNonVol { code_offset = 1; reg = RBP };
        ];
      exception_handler = None;
    }
  in
  let buf = Buffer.create 32 in
  write buf info;
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  let parsed = parse (Object.Buffer.cursor obj_buf) in
  Alcotest.(check int) "version" 1 parsed.version;
  Alcotest.(check int) "prolog" 8 parsed.size_of_prolog;
  Alcotest.(check int) "3 codes" 3 (List.length parsed.unwind_codes);
  (match List.nth parsed.unwind_codes 0 with
  | AllocSmall { size; _ } -> Alcotest.(check int) "alloc" 64 size
  | _ -> Alcotest.fail "expected AllocSmall");
  (match List.nth parsed.unwind_codes 1 with
  | SetFPReg _ -> ()
  | _ -> Alcotest.fail "expected SetFPReg");
  match List.nth parsed.unwind_codes 2 with
  | PushNonVol { reg; _ } ->
      Alcotest.(check int) "rbp" 5 (register_to_int reg)
  | _ -> Alcotest.fail "expected PushNonVol"

let test_x64_ocaml_typical () =
  let open Pdb.Unwind.X64 in
  let info =
    {
      version = 1;
      flags =
        { exception_handler = false; termination_handler = false;
          chain_info = false };
      size_of_prolog = 20;
      frame_register = Some RBP;
      frame_offset = 0;
      unwind_codes =
        [
          AllocSmall { code_offset = 20; size = 48 };
          SetFPReg { code_offset = 16 };
          PushNonVol { code_offset = 12; reg = R15 };
          PushNonVol { code_offset = 10; reg = R14 };
          PushNonVol { code_offset = 8; reg = R13 };
          PushNonVol { code_offset = 6; reg = R12 };
          PushNonVol { code_offset = 4; reg = RBX };
          PushNonVol { code_offset = 1; reg = RBP };
        ];
      exception_handler = None;
    }
  in
  let buf = Buffer.create 32 in
  write buf info;
  let parsed = parse (Object.Buffer.cursor (buffer_of_string (Buffer.contents buf))) in
  Alcotest.(check int) "8 codes" 8 (List.length parsed.unwind_codes)

let test_x64_alloc_large () =
  let open Pdb.Unwind.X64 in
  let info =
    {
      version = 1;
      flags =
        { exception_handler = false; termination_handler = false;
          chain_info = false };
      size_of_prolog = 10;
      frame_register = None;
      frame_offset = 0;
      unwind_codes =
        [
          AllocLarge { code_offset = 10; size = 4096 };
          PushNonVol { code_offset = 1; reg = RBP };
        ];
      exception_handler = None;
    }
  in
  let buf = Buffer.create 32 in
  write buf info;
  let parsed = parse (Object.Buffer.cursor (buffer_of_string (Buffer.contents buf))) in
  match List.nth parsed.unwind_codes 0 with
  | AllocLarge { size; _ } -> Alcotest.(check int) "4096" 4096 size
  | _ -> Alcotest.fail "expected AllocLarge"

let test_x64_save_nonvol () =
  let open Pdb.Unwind.X64 in
  let info =
    {
      version = 1;
      flags =
        { exception_handler = false; termination_handler = false;
          chain_info = false };
      size_of_prolog = 15;
      frame_register = None;
      frame_offset = 0;
      unwind_codes =
        [
          SaveNonVol { code_offset = 15; reg = RBX; offset = 32 };
          AllocSmall { code_offset = 8; size = 64 };
          PushNonVol { code_offset = 1; reg = RBP };
        ];
      exception_handler = None;
    }
  in
  let buf = Buffer.create 32 in
  write buf info;
  let parsed = parse (Object.Buffer.cursor (buffer_of_string (Buffer.contents buf))) in
  match List.nth parsed.unwind_codes 0 with
  | SaveNonVol { reg; offset; _ } ->
      Alcotest.(check int) "rbx" 3 (register_to_int reg);
      Alcotest.(check int) "offset" 32 offset
  | _ -> Alcotest.fail "expected SaveNonVol"

let test_x64_exception_handler () =
  let open Pdb.Unwind.X64 in
  let info =
    {
      version = 1;
      flags =
        { exception_handler = true; termination_handler = false;
          chain_info = false };
      size_of_prolog = 5;
      frame_register = None;
      frame_offset = 0;
      unwind_codes = [ PushNonVol { code_offset = 1; reg = RBP } ];
      exception_handler = Some (u32 0x5000);
    }
  in
  let buf = Buffer.create 32 in
  write buf info;
  let parsed = parse (Object.Buffer.cursor (buffer_of_string (Buffer.contents buf))) in
  Alcotest.(check bool) "ehandler" true parsed.flags.exception_handler;
  match parsed.exception_handler with
  | Some rva -> Alcotest.(check int) "rva" 0x5000 (Unsigned.UInt32.to_int rva)
  | None -> Alcotest.fail "expected handler"

let test_x64_no_codes () =
  let open Pdb.Unwind.X64 in
  let info =
    {
      version = 1;
      flags =
        { exception_handler = false; termination_handler = false;
          chain_info = false };
      size_of_prolog = 0;
      frame_register = None;
      frame_offset = 0;
      unwind_codes = [];
      exception_handler = None;
    }
  in
  let buf = Buffer.create 16 in
  write buf info;
  let bytes = Buffer.contents buf in
  Alcotest.(check bool) "min 8 bytes" true (String.length bytes >= 8);
  let parsed = parse (Object.Buffer.cursor (buffer_of_string bytes)) in
  Alcotest.(check int) "no codes" 0 (List.length parsed.unwind_codes)

(** {2 ARM64 UNWIND_INFO} *)

let test_arm64_minimal () =
  let open Pdb.Unwind.Arm64 in
  let info =
    {
      function_length = 64;
      has_exception_data = false;
      codes =
        [ SaveFPLRX { offset = 16 }; SetFP; AllocSmall { size = 32 }; End ];
      exception_handler = None;
    }
  in
  let buf = Buffer.create 32 in
  write buf info;
  let parsed = parse (Object.Buffer.cursor (buffer_of_string (Buffer.contents buf))) in
  Alcotest.(check int) "func length" 64 parsed.function_length;
  Alcotest.(check int) "4 codes" 4 (List.length parsed.codes);
  (match List.nth parsed.codes 0 with
  | SaveFPLRX { offset } -> Alcotest.(check int) "fplrx" 16 offset
  | _ -> Alcotest.fail "expected SaveFPLRX");
  (match List.nth parsed.codes 1 with
  | SetFP -> ()
  | _ -> Alcotest.fail "expected SetFP");
  match List.nth parsed.codes 3 with
  | End -> ()
  | _ -> Alcotest.fail "expected End"

let test_arm64_callee_saves () =
  let open Pdb.Unwind.Arm64 in
  let info =
    {
      function_length = 200;
      has_exception_data = false;
      codes =
        [
          SaveFPLRX { offset = 48 };
          SaveRegP { reg = 0; offset = 16 };
          SaveRegP { reg = 2; offset = 32 };
          SetFP;
          End;
        ];
      exception_handler = None;
    }
  in
  let buf = Buffer.create 32 in
  write buf info;
  let parsed = parse (Object.Buffer.cursor (buffer_of_string (Buffer.contents buf))) in
  Alcotest.(check int) "5 codes" 5 (List.length parsed.codes);
  match List.nth parsed.codes 1 with
  | SaveRegP { reg; offset } ->
      Alcotest.(check int) "reg" 0 reg;
      Alcotest.(check int) "offset" 16 offset
  | _ -> Alcotest.fail "expected SaveRegP"

let test_arm64_alloc_medium () =
  let open Pdb.Unwind.Arm64 in
  let info =
    {
      function_length = 100;
      has_exception_data = false;
      codes = [ AllocMedium { size = 1024 }; End ];
      exception_handler = None;
    }
  in
  let buf = Buffer.create 32 in
  write buf info;
  let parsed = parse (Object.Buffer.cursor (buffer_of_string (Buffer.contents buf))) in
  match List.nth parsed.codes 0 with
  | AllocMedium { size } -> Alcotest.(check int) "1024" 1024 size
  | _ -> Alcotest.fail "expected AllocMedium"

let test_arm64_alloc_large () =
  let open Pdb.Unwind.Arm64 in
  let info =
    {
      function_length = 100;
      has_exception_data = false;
      codes = [ AllocLarge { size = 1048576 }; End ];
      exception_handler = None;
    }
  in
  let buf = Buffer.create 32 in
  write buf info;
  let parsed = parse (Object.Buffer.cursor (buffer_of_string (Buffer.contents buf))) in
  match List.nth parsed.codes 0 with
  | AllocLarge { size } -> Alcotest.(check int) "1M" 1048576 size
  | _ -> Alcotest.fail "expected AllocLarge"

let test_arm64_float_regs () =
  let open Pdb.Unwind.Arm64 in
  let info =
    {
      function_length = 80;
      has_exception_data = false;
      codes =
        [ SaveFRegP { reg = 0; offset = 16 };
          SaveFReg { reg = 2; offset = 32 };
          End ];
      exception_handler = None;
    }
  in
  let buf = Buffer.create 32 in
  write buf info;
  let parsed = parse (Object.Buffer.cursor (buffer_of_string (Buffer.contents buf))) in
  (match List.nth parsed.codes 0 with
  | SaveFRegP { reg; offset } ->
      Alcotest.(check int) "reg" 0 reg;
      Alcotest.(check int) "offset" 16 offset
  | _ -> Alcotest.fail "expected SaveFRegP");
  match List.nth parsed.codes 1 with
  | SaveFReg { reg; offset } ->
      Alcotest.(check int) "reg" 2 reg;
      Alcotest.(check int) "offset" 32 offset
  | _ -> Alcotest.fail "expected SaveFReg"

let test_arm64_pac_sign_lr () =
  let open Pdb.Unwind.Arm64 in
  let info =
    {
      function_length = 40;
      has_exception_data = false;
      codes = [ PACSignLR; SaveFPLRX { offset = 16 }; SetFP; End ];
      exception_handler = None;
    }
  in
  let buf = Buffer.create 32 in
  write buf info;
  let parsed = parse (Object.Buffer.cursor (buffer_of_string (Buffer.contents buf))) in
  match List.nth parsed.codes 0 with
  | PACSignLR -> ()
  | _ -> Alcotest.fail "expected PACSignLR"

let test_arm64_exception_handler () =
  let open Pdb.Unwind.Arm64 in
  let info =
    {
      function_length = 100;
      has_exception_data = true;
      codes = [ SaveFPLRX { offset = 16 }; End ];
      exception_handler = Some (u32 0x8000);
    }
  in
  let buf = Buffer.create 32 in
  write buf info;
  let parsed = parse (Object.Buffer.cursor (buffer_of_string (Buffer.contents buf))) in
  Alcotest.(check bool) "has ex data" true parsed.has_exception_data;
  match parsed.exception_handler with
  | Some rva -> Alcotest.(check int) "handler" 0x8000 (Unsigned.UInt32.to_int rva)
  | None -> Alcotest.fail "expected handler"

(** {2 Dispatch tests} *)

let test_dispatch_x64 () =
  let open Pdb.Unwind.X64 in
  let info =
    {
      version = 1;
      flags =
        { exception_handler = false; termination_handler = false;
          chain_info = false };
      size_of_prolog = 5;
      frame_register = Some RBP;
      frame_offset = 0;
      unwind_codes = [ PushNonVol { code_offset = 1; reg = RBP } ];
      exception_handler = None;
    }
  in
  let buf = Buffer.create 16 in
  Pdb.Unwind.write buf (X64_unwind info);
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  match Pdb.Unwind.parse X64 (Object.Buffer.cursor obj_buf) with
  | Pdb.Unwind.X64_unwind parsed ->
      Alcotest.(check int) "version" 1 parsed.version
  | _ -> Alcotest.fail "expected X64_unwind"

let test_dispatch_arm64 () =
  let open Pdb.Unwind.Arm64 in
  let info =
    {
      function_length = 40;
      has_exception_data = false;
      codes = [ SaveFPLRX { offset = 16 }; SetFP; End ];
      exception_handler = None;
    }
  in
  let buf = Buffer.create 16 in
  Pdb.Unwind.write buf (Arm64_unwind info);
  let bytes = Buffer.contents buf in
  let obj_buf = buffer_of_string bytes in
  match Pdb.Unwind.parse Arm64 (Object.Buffer.cursor obj_buf) with
  | Pdb.Unwind.Arm64_unwind parsed ->
      Alcotest.(check int) "func length" 40 parsed.function_length
  | _ -> Alcotest.fail "expected Arm64_unwind"

let () =
  Alcotest.run "Unwind"
    [
      ( "x64_register",
        [ Alcotest.test_case "roundtrip" `Quick test_x64_register_roundtrip ] );
      ( "x64_unwind_info",
        [
          Alcotest.test_case "minimal" `Quick test_x64_minimal;
          Alcotest.test_case "ocaml typical" `Quick test_x64_ocaml_typical;
          Alcotest.test_case "alloc large" `Quick test_x64_alloc_large;
          Alcotest.test_case "save nonvol" `Quick test_x64_save_nonvol;
          Alcotest.test_case "exception handler" `Quick
            test_x64_exception_handler;
          Alcotest.test_case "no codes" `Quick test_x64_no_codes;
        ] );
      ( "arm64_unwind_info",
        [
          Alcotest.test_case "minimal" `Quick test_arm64_minimal;
          Alcotest.test_case "callee saves" `Quick test_arm64_callee_saves;
          Alcotest.test_case "alloc medium" `Quick test_arm64_alloc_medium;
          Alcotest.test_case "alloc large" `Quick test_arm64_alloc_large;
          Alcotest.test_case "float regs" `Quick test_arm64_float_regs;
          Alcotest.test_case "pac sign lr" `Quick test_arm64_pac_sign_lr;
          Alcotest.test_case "exception handler" `Quick
            test_arm64_exception_handler;
        ] );
      ( "dispatch",
        [
          Alcotest.test_case "x64" `Quick test_dispatch_x64;
          Alcotest.test_case "arm64" `Quick test_dispatch_arm64;
        ] );
    ]
