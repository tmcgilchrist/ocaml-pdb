(* Windows structured exception handling / stack unwinding. *)

type arch = X64 | Arm64

type t =
  | X64_unwind of Unwind_x64.unwind_info
  | Arm64_unwind of Unwind_arm64.unwind_info

let parse arch cur =
  match arch with
  | X64 -> X64_unwind (Unwind_x64.parse cur)
  | Arm64 -> Arm64_unwind (Unwind_arm64.parse cur)

let write buf = function
  | X64_unwind info -> Unwind_x64.write buf info
  | Arm64_unwind info -> Unwind_arm64.write buf info

module X64 = Unwind_x64
module Arm64 = Unwind_arm64
