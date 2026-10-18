module Valerie.Proof.ABI.Arm32

open Valerie.Defs
open FStar.UInt32
module U32 = FStar.UInt32
open Valerie.Arm32.Machine
open Valerie.Arm32.Semantics
open Valerie.Arm32.CallingConv

/// AAPCS32 Callee-Saved Register Invariant Proofs
/// Proves that executing instructions writing to volatile registers (R0-R3, R12, LR)
/// strictly preserves callee-saved registers (R4-R11) and stack pointer integrity.

let update_reg (r: reg) (v: nat32) (s: state) : state =
  { s with regs = (fun r' -> if r' = r then v else s.regs r') }

let lemma_abi_volatile_update (dst: reg{is_volatile_gpr dst}) (v: nat32) (s0: state)
  : Lemma (ensures (let s1 = update_reg dst v s0 in
                    aapcs32_compliant s0 s1)) =
  ()

let lemma_abi_transitive (s0 s1 s2: state)
  (h01: aapcs32_compliant s0 s1)
  (h12: aapcs32_compliant s1 s2)
  : Lemma (ensures aapcs32_compliant s0 s2) =
  ()
