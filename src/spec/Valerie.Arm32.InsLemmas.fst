module Valerie.Arm32.InsLemmas

open Valerie.Defs
open FStar.UInt32
module U32 = FStar.UInt32
open Valerie.Arm32.Machine
open Valerie.Arm32.Semantics

/// Step lemma: Mov writes destination and frames all other registers
let lemma_step_mov (dst: reg) (src: operand) (s0: state)
  : Lemma (let s1 = eval_ins (Mov dst src) s0 in
           s1.ok == s0.ok /\
           s1.regs dst == eval_operand src s0 /\
           (forall (r: reg). r =!= dst ==> s1.regs r == s0.regs r)) =
  ()

/// Step lemma: Add writes destination and frames all other registers
let lemma_step_add (dst: reg) (s1_reg: reg) (s2_op: operand) (s0: state)
  : Lemma (let s1 = eval_ins (Add dst s1_reg s2_op) s0 in
           s1.ok == s0.ok /\
           s1.regs dst == U32.add_mod (s0.regs s1_reg) (eval_operand s2_op s0) /\
           (forall (r: reg). r =!= dst ==> s1.regs r == s0.regs r)) =
  ()

/// Step lemma: Sub writes destination and frames all other registers
let lemma_step_sub (dst: reg) (s1_reg: reg) (s2_op: operand) (s0: state)
  : Lemma (let s1 = eval_ins (Sub dst s1_reg s2_op) s0 in
           s1.ok == s0.ok /\
           s1.regs dst == U32.sub_mod (s0.regs s1_reg) (eval_operand s2_op s0) /\
           (forall (r: reg). r =!= dst ==> s1.regs r == s0.regs r)) =
  ()

/// Step lemma: Eor writes destination and frames all other registers
let lemma_step_eor (dst: reg) (s1_reg: reg) (s2_op: operand) (s0: state)
  : Lemma (let s1 = eval_ins (Eor dst s1_reg s2_op) s0 in
           s1.ok == s0.ok /\
           s1.regs dst == U32.logxor (s0.regs s1_reg) (eval_operand s2_op s0) /\
           (forall (r: reg). r =!= dst ==> s1.regs r == s0.regs r)) =
  ()

/// Step lemma: Rev writes destination and frames all other registers
let lemma_step_rev (dst: reg) (src: reg) (s0: state)
  : Lemma (let s1 = eval_ins (Rev dst src) s0 in
           s1.ok == s0.ok /\
           (forall (r: reg). r =!= dst ==> s1.regs r == s0.regs r)) =
  ()
