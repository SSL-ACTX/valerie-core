module Valerie.Riscv64.InsLemmas

open Valerie.Defs
open FStar.UInt64
module U64 = FStar.UInt64
open Valerie.Riscv64.Machine
open Valerie.Riscv64.Semantics

/// Automated instruction step lemmas for RISC-V 64 architecture (RV64I + Zbb/Zk).
/// Proves that writing to Zero is a no-op and general destination registers update correctly.

/// Step Lemma: Add
let lemma_step_add (dst: reg) (s1: reg) (s2: operand) (s0: state)
  : Lemma (ensures (let sN = eval_ins (Add dst s1 s2) s0 in
                    (if dst = Zero then sN.regs == s0.regs
                     else sN.regs dst == U64.add_mod (eval_operand (OReg s1) s0) (eval_operand s2 s0) /\
                          (forall (r: reg{r <> dst}). sN.regs r == s0.regs r)))) =
  ()

/// Step Lemma: Sub
let lemma_step_sub (dst: reg) (s1: reg) (s2: operand) (s0: state)
  : Lemma (ensures (let sN = eval_ins (Sub dst s1 s2) s0 in
                    (if dst = Zero then sN.regs == s0.regs
                     else sN.regs dst == U64.sub_mod (eval_operand (OReg s1) s0) (eval_operand s2 s0) /\
                          (forall (r: reg{r <> dst}). sN.regs r == s0.regs r)))) =
  ()

/// Step Lemma: Xor
let lemma_step_xor (dst: reg) (s1: reg) (s2: operand) (s0: state)
  : Lemma (ensures (let sN = eval_ins (Xor dst s1 s2) s0 in
                    (if dst = Zero then sN.regs == s0.regs
                     else sN.regs dst == nat64_xor (eval_operand (OReg s1) s0) (eval_operand s2 s0) /\
                          (forall (r: reg{r <> dst}). sN.regs r == s0.regs r)))) =
  ()

/// Step Lemma: Rev8 (Byte-reversal in Zbb / Zk)
let lemma_step_rev8 (dst: reg) (src: reg) (s0: state)
  : Lemma (ensures (let sN = eval_ins (Rev8 dst src) s0 in
                    (if dst = Zero then sN.regs == s0.regs
                     else (forall (r: reg{r <> dst}). sN.regs r == s0.regs r)))) =
  ()
