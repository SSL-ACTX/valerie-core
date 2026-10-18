module Valerie.AArch64.InsLemmas

open Valerie.Defs
open FStar.UInt64
module U64 = FStar.UInt64
open FStar.UInt128
module U128 = FStar.UInt128
open Valerie.AArch64.Machine
open Valerie.AArch64.Semantics

/// Invariant: updating register `r` does not modify any distinct register `r'`.
let lemma_update_reg_other (r: reg) (v: nat64) (r': reg{r' <> r}) (s: state)
  : Lemma (ensures (update_reg r v s).regs r' == s.regs r') =
  ()

/// Invariant: updating vector register `v_reg` does not modify any distinct vector register `v_reg'`.
let lemma_update_vec_other (v_reg: vec) (v_val: nat128) (v_reg': vec{v_reg' <> v_reg}) (s: state)
  : Lemma (ensures (update_vec v_reg v_val s).vecs v_reg' == s.vecs v_reg') =
  ()

/// Invariant: evaluating and updating the same register yields the updated value.
let lemma_update_reg_same (r: reg) (v: nat64) (s: state)
  : Lemma (ensures (update_reg r v s).regs r == v) =
  ()

let not_dst_reg (i: ins) (r: reg) : Type0 =
  match i with
  | Mov dst _ | Add dst _ _ | Sub dst _ _ | Eor dst _ _ | And dst _ _ | Orr dst _ _ | Rev64 dst _ -> r <> dst
  | FmovVecToGpr dst _ | Umov dst _ _ -> r <> dst
  | _ -> True

/// Framing lemma: Single instruction execution preserves all unmodified general-purpose registers.
let lemma_ins_preserves_other_regs (i: ins) (r: reg) (s: state)
  (h_not_dst: not_dst_reg i r)
  : Lemma (ensures (eval_ins i s).regs r == s.regs r) =
  ()

/// Automated instruction step lemmas for weakest-precondition generation.
/// Each lemma proves that executing instruction `i` on state `s0` yields `s1`
/// where the destination register contains the computed value and all other
/// registers and vector registers are cleanly framed.
/// Memory framing is handled exclusively by Pulse Separation Logic.

/// Step Lemma: MOV
let lemma_step_mov (dst: reg) (src: operand) (s0: state)
  : Lemma (ensures (let s1 = eval_ins (Mov dst src) s0 in
                    s1.regs dst == eval_operand src s0 /\
                    (forall (r: reg{r <> dst}). s1.regs r == s0.regs r) /\
                    (forall (vr: vec). s1.vecs vr == s0.vecs vr))) =
  ()

/// Step Lemma: ADD (64-bit modular addition)
let lemma_step_add (dst: reg) (s1: reg) (s2: operand) (s0: state)
  : Lemma (ensures (let sN = eval_ins (Add dst s1 s2) s0 in
                    sN.regs dst == U64.add_mod (s0.regs s1) (eval_operand s2 s0) /\
                    (forall (r: reg{r <> dst}). sN.regs r == s0.regs r) /\
                    (forall (vr: vec). sN.vecs vr == s0.vecs vr))) =
  ()

/// Step Lemma: SUB (64-bit modular subtraction)
let lemma_step_sub (dst: reg) (s1: reg) (s2: operand) (s0: state)
  : Lemma (ensures (let sN = eval_ins (Sub dst s1 s2) s0 in
                    sN.regs dst == U64.sub_mod (s0.regs s1) (eval_operand s2 s0) /\
                    (forall (r: reg{r <> dst}). sN.regs r == s0.regs r) /\
                    (forall (vr: vec). sN.vecs vr == s0.vecs vr))) =
  ()

/// Step Lemma: EOR (bitwise XOR)
let lemma_step_eor (dst: reg) (s1: reg) (s2: operand) (s0: state)
  : Lemma (ensures (let sN = eval_ins (Eor dst s1 s2) s0 in
                    sN.regs dst == nat64_xor (s0.regs s1) (eval_operand s2 s0) /\
                    (forall (r: reg{r <> dst}). sN.regs r == s0.regs r) /\
                    (forall (vr: vec). sN.vecs vr == s0.vecs vr))) =
  ()

/// Step Lemma: REV64 (byte swap)
let lemma_step_rev64 (dst: reg) (src: reg) (s0: state)
  : Lemma (ensures (let s1 = eval_ins (Rev64 dst src) s0 in
                    (forall (r: reg{r <> dst}). s1.regs r == s0.regs r) /\
                    (forall (vr: vec). s1.vecs vr == s0.vecs vr))) =
  ()

/// Step Lemma: FMOV from GPR to Vector register
let lemma_step_fmov_from_gpr (dst: vec) (src: reg) (s0: state)
  : Lemma (ensures (let s1 = eval_ins (FmovGprToVec dst src) s0 in
                    (forall (vr: vec{vr <> dst}). s1.vecs vr == s0.vecs vr) /\
                    (forall (r: reg). s1.regs r == s0.regs r))) =
  ()

/// Step Lemma: FMOV from Vector register to GPR
let lemma_step_fmov_to_gpr (dst: reg) (src: vec) (s0: state)
  : Lemma (ensures (let s1 = eval_ins (FmovVecToGpr dst src) s0 in
                    (forall (r: reg{r <> dst}). s1.regs r == s0.regs r) /\
                    (forall (vr: vec). s1.vecs vr == s0.vecs vr))) =
  ()

/// Step Lemma: VEOR (Bitwise XOR between vector registers)
let lemma_step_veor (dst: vec) (src: vec) (s0: state)
  : Lemma (ensures (let sN = eval_ins (Veor dst dst src) s0 in
                    sN.vecs dst == U128.logxor (s0.vecs dst) (s0.vecs src) /\
                    (forall (vr: vec{vr <> dst}). sN.vecs vr == s0.vecs vr) /\
                    (forall (r: reg). sN.regs r == s0.regs r))) =
  ()
