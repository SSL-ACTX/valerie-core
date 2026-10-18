module Valerie.X64.CallingConv

open Valerie.Defs
open FStar.UInt64
module U64 = FStar.UInt64
open Valerie.X64.Machine

/// System V AMD64 ABI Calling Convention Specification
/// Callee-saved (non-volatile) GPRs: RBX, RBP, R12, R13, R14, R15.
/// Caller-saved (volatile) GPRs: RAX, RCX, RDX, RSI, RDI, R8, R9, R10, R11.
let is_callee_saved_gpr (r: reg) : bool =
  match r with
  | Rbx | Rbp | R12 | R13 | R14 | R15 -> true
  | _ -> false

let is_volatile_gpr (r: reg) : bool =
  match r with
  | Rax | Rcx | Rdx | Rsi | Rdi | R8 | R9 | R10 | R11 -> true
  | _ -> false

/// Stack pointer 16-byte alignment invariant for AMD64
let rsp_aligned (s: state) : prop =
  U64.v (s.regs Rsp) % 16 == 0

/// System V compliance predicate:
/// 1. Callee-saved GPRs preserved.
/// 2. RSP preserved.
let sysv_compliant (s0 sN: state) : prop =
  (forall (r: reg). is_callee_saved_gpr r ==> sN.regs r == s0.regs r) /\
  sN.regs Rsp == s0.regs Rsp

let lemma_volatile_write_preserves_calling_conv (r_dst: reg{is_volatile_gpr r_dst}) (v: nat64) (s0: state)
  : Lemma (ensures (forall (r: reg). is_callee_saved_gpr r ==> (fun r' -> if r' = r_dst then v else s0.regs r') r == s0.regs r)) =
  ()
