module Valerie.X64.InsLemmas

open Valerie.Defs
open FStar.UInt64
module U64 = FStar.UInt64
open FStar.UInt128
module U128 = FStar.UInt128
open Valerie.X64.Machine
open Valerie.X64.Semantics

/// Automated instruction step lemmas for x86_64 architecture.
/// Each lemma proves that executing instruction `i` on state `s0` yields `s1`
/// where destination register is properly updated and all other registers
/// and vector registers are framed.

/// Step Lemma: Mov64
let lemma_step_mov64 (dst: reg) (src: operand) (s0: state)
  : Lemma (ensures (let s1 = eval_ins (Mov64 dst src) s0 in
                    s1.regs dst == eval_operand src s0 /\
                    (forall (r: reg{r <> dst}). s1.regs r == s0.regs r) /\
                    (forall (x: xmm). s1.xmms x == s0.xmms x))) =
  ()

/// Step Lemma: Add64 (modular addition)
let lemma_step_add64 (dst: reg) (src: operand) (s0: state)
  : Lemma (ensures (let s1 = eval_ins (Add64 dst src) s0 in
                    s1.regs dst == U64.add_mod (s0.regs dst) (eval_operand src s0) /\
                    (forall (r: reg{r <> dst}). s1.regs r == s0.regs r) /\
                    (forall (x: xmm). s1.xmms x == s0.xmms x))) =
  ()

/// Step Lemma: Sub64 (modular subtraction)
let lemma_step_sub64 (dst: reg) (src: operand) (s0: state)
  : Lemma (ensures (let s1 = eval_ins (Sub64 dst src) s0 in
                    s1.regs dst == U64.sub_mod (s0.regs dst) (eval_operand src s0) /\
                    (forall (r: reg{r <> dst}). s1.regs r == s0.regs r) /\
                    (forall (x: xmm). s1.xmms x == s0.xmms x))) =
  ()

/// Step Lemma: Xor64 (bitwise XOR)
let lemma_step_xor64 (dst: reg) (src: operand) (s0: state)
  : Lemma (ensures (let s1 = eval_ins (Xor64 dst src) s0 in
                    s1.regs dst == nat64_xor (s0.regs dst) (eval_operand src s0) /\
                    (forall (r: reg{r <> dst}). s1.regs r == s0.regs r) /\
                    (forall (x: xmm). s1.xmms x == s0.xmms x))) =
  ()

/// Step Lemma: Pxor (128-bit vector XOR)
let lemma_step_pxor (dst: xmm) (src: xmm) (s0: state)
  : Lemma (ensures (let s1 = eval_ins (Pxor dst src) s0 in
                    s1.xmms dst == nat128_xor (s0.xmms dst) (s0.xmms src) /\
                    (forall (x: xmm{x <> dst}). s1.xmms x == s0.xmms x) /\
                    (forall (r: reg). s1.regs r == s0.regs r))) =
  ()
