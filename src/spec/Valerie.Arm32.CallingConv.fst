module Valerie.Arm32.CallingConv

open Valerie.Defs
open FStar.UInt32
module U32 = FStar.UInt32
open Valerie.Arm32.Machine

/// AAPCS32 (ARM 32-bit Architecture Procedure Call Standard)
/// Callee-saved (non-volatile) GPRs: R4, R5, R6, R7, R8, R9, R10, R11.
/// Caller-saved (volatile) GPRs: R0, R1, R2, R3, R12 (IP), LR (R14).
let is_callee_saved_gpr (r: reg) : bool =
  match r with
  | R4 | R5 | R6 | R7 | R8 | R9 | R10 | R11 -> true
  | _ -> false

let is_volatile_gpr (r: reg) : bool =
  match r with
  | R0 | R1 | R2 | R3 | R12 | LR -> true
  | _ -> false

/// Stack pointer 8-byte alignment required by AAPCS32 at external interfaces
let sp_aligned (s: state) : prop =
  U32.v (s.regs SP) % 8 == 0

let aapcs32_compliant (s0 sN: state) : prop =
  (forall (r: reg). is_callee_saved_gpr r ==> sN.regs r == s0.regs r) /\
  sN.regs SP == s0.regs SP

let lemma_volatile_write_preserves_calling_conv (r_dst: reg{is_volatile_gpr r_dst}) (v: nat32) (s0: state)
  : Lemma (ensures (forall (r: reg). is_callee_saved_gpr r ==> (fun r' -> if r' = r_dst then v else s0.regs r') r == s0.regs r)) =
  ()
