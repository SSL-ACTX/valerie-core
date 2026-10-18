module Valerie.Proof.ABI

open Valerie.Defs
open FStar.UInt64
module U64 = FStar.UInt64
open Valerie.AArch64.Machine
open Valerie.AArch64.Semantics
open Valerie.AArch64.CallingConv

/// ABI Compliance and Callee-Saved Register Invariant Proofs
/// Formally proves that verified routines preserve AAPCS64 non-volatile registers (X19-X30)
/// and stack pointer alignment when called from C / foreign environments.

/// Theorem: Single volatile register update satisfies callee-saved preservation
let lemma_abi_volatile_update (dst: reg{is_volatile_gpr dst}) (v: nat64) (s0: state)
  (h_aligned: sp_aligned s0)
  : Lemma (ensures (let s1 = update_reg dst v s0 in
                    aapcs64_compliant s0 s1)) =
  let s1 = update_reg dst v s0 in
  assert (forall (r: reg). is_callee_saved_gpr r ==> s1.regs r == s0.regs r);
  assert (s1.regs SP == s0.regs SP)

/// Theorem: Sequential composition of two AAPCS64 compliant steps is AAPCS64 compliant
let lemma_abi_transitive (s0 s1 s2: state)
  (h01: aapcs64_compliant s0 s1)
  (h12: aapcs64_compliant s1 s2)
  : Lemma (ensures aapcs64_compliant s0 s2) =
  ()

/// Theorem: Stack frame prologue and epilogue preserves AAPCS64 compliance
let lemma_abi_stack_frame_preservation (s0: state)
  (h_aligned: sp_aligned s0)
  : Lemma (ensures (let s1 = push_frame s0 in
                    let s2 = pop_frame (s0.regs X29) (s0.regs X30) s1 in
                    aapcs64_compliant s0 s2)) =
  lemma_frame_roundtrip s0
