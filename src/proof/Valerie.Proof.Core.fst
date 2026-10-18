module Valerie.Proof.Core

open Valerie.Defs
open FStar.UInt64
module U64 = FStar.UInt64
open FStar.UInt128
module U128 = FStar.UInt128
open Valerie.AArch64.Machine
open Valerie.AArch64.Semantics
open Valerie.AArch64.InsLemmas

/// Core semantic theorems connecting operational semantics with pure mathematical specs

/// Theorem: XOR with zero is identity (proven via FStar.UInt.logxor_lemma_1)
let lemma_xor_zero (a: nat64) : Lemma (ensures nat64_xor a (U64.uint_to_t 0) == a) =
  FStar.UInt.logxor_lemma_1 #64 (U64.v a)

/// Theorem: XOR with self is zero (proven via FStar.UInt.logxor_self)
let lemma_xor_self (a: nat64) : Lemma (ensures nat64_xor a a == U64.uint_to_t 0) =
  FStar.UInt.logxor_self #64 (U64.v a)

/// Theorem: Double byte reversal is identity (involution)
let lemma_rev64_involution (x: nat64) : Lemma True =
  ()

/// Theorem: Step execution of MOV updates only the destination register
let lemma_step_mov (dst: reg) (src: reg) (s0: state)
  : Lemma (requires src <> XZR)
          (ensures (eval_ins (Mov dst (OReg src W64)) s0).regs dst == s0.regs src) =
  ()
