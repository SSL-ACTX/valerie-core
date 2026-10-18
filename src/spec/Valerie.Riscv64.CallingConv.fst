module Valerie.Riscv64.CallingConv

open Valerie.Defs
open FStar.UInt64
module U64 = FStar.UInt64
open Valerie.Riscv64.Machine

/// RISC-V LP64D Calling Convention Specification
/// Callee-saved (non-volatile) GPRs: S0 (FP), S1, S2-S11.
/// Caller-saved (volatile) GPRs: RA, T0-T2, A0-A7, T3-T6.
let is_callee_saved_gpr (r: reg) : bool =
  match r with
  | S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 | S10 | S11 -> true
  | _ -> false

let is_volatile_gpr (r: reg) : bool =
  match r with
  | RA | T0 | T1 | T2 | A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7 | T3 | T4 | T5 | T6 -> true
  | _ -> false

let sp_aligned (s: state) : prop =
  U64.v (s.regs SP) % 16 == 0

let riscv_abi_compliant (s0 sN: state) : prop =
  (forall (r: reg). is_callee_saved_gpr r ==> sN.regs r == s0.regs r) /\
  sN.regs SP == s0.regs SP

let lemma_volatile_write_preserves_calling_conv (r_dst: reg{is_volatile_gpr r_dst}) (v: nat64) (s0: state)
  : Lemma (ensures (forall (r: reg). is_callee_saved_gpr r ==> (fun r' -> if r' = r_dst then v else s0.regs r') r == s0.regs r)) =
  ()
