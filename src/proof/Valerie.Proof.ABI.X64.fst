module Valerie.Proof.ABI.X64

open Valerie.Defs
open FStar.UInt64
module U64 = FStar.UInt64
open Valerie.X64.Machine
open Valerie.X64.Semantics
open Valerie.X64.CallingConv

/// System V AMD64 Callee-Saved Register Invariant Proofs
/// Proves that executing instructions writing to volatile registers (RAX, RCX, RDX, RSI, RDI, R8-R11)
/// strictly preserves callee-saved registers (RBX, RBP, R12-R15) and stack pointer integrity.

let update_reg (r: reg) (v: nat64) (s: state) : state =
  { s with regs = (fun r' -> if r' = r then v else s.regs r') }

let lemma_abi_volatile_update (dst: reg{is_volatile_gpr dst}) (v: nat64) (s0: state)
  : Lemma (ensures (let s1 = update_reg dst v s0 in
                    sysv_compliant s0 s1)) =
  ()

let lemma_abi_transitive (s0 s1 s2: state)
  (h01: sysv_compliant s0 s1)
  (h12: sysv_compliant s1 s2)
  : Lemma (ensures sysv_compliant s0 s2) =
  ()
