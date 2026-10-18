module Valerie.AArch64.CallingConv

open Valerie.Defs
open FStar.UInt64
module U64 = FStar.UInt64
open Valerie.AArch64.Machine
open Valerie.AArch64.Semantics

/// AAPCS64 Calling Convention Specification
/// Classifies all registers into caller-saved (volatile) and callee-saved (non-volatile).

let is_callee_saved_gpr (r: reg) : bool =
  match r with
  | X19 | X20 | X21 | X22 | X23 | X24 | X25 | X26 | X27 | X28 | X29 | X30 -> true
  | _ -> false

let is_volatile_gpr (r: reg) : bool =
  match r with
  | X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7
  | X8 | X9 | X10 | X11 | X12 | X13 | X14 | X15
  | X16 | X17 | X18 -> true
  | _ -> false

let is_callee_saved_vec (vr: vec) : bool =
  match vr with
  | V8 | V9 | V10 | V11 | V12 | V13 | V14 | V15 -> true
  | _ -> false

/// Stack pointer 16-byte alignment invariant required by AAPCS64
let sp_aligned (s: state) : prop =
  U64.v (s.regs SP) % 16 == 0

/// AAPCS64 compliance predicate:
/// A procedure conforms to AAPCS64 if:
/// 1. All callee-saved GPRs (X19-X30) are preserved.
/// 2. SP is preserved and 16-byte aligned.
let aapcs64_compliant (s0 sN: state) : prop =
  (forall (r: reg). is_callee_saved_gpr r ==> sN.regs r == s0.regs r) /\
  sN.regs SP == s0.regs SP /\
  sp_aligned s0 /\ sp_aligned sN

/// Theorem: If an instruction only writes to a volatile register, it preserves AAPCS64 callee-saved registers.
let lemma_volatile_write_preserves_calling_conv (r_dst: reg{is_volatile_gpr r_dst}) (v: nat64) (s0: state)
  : Lemma (ensures (forall (r: reg). is_callee_saved_gpr r ==> (fun r' -> if r' = r_dst then v else s0.regs r') r == s0.regs r)) =
  ()

/// Stack Frame Management for AArch64
/// Models the standard frame record (X29 frame pointer, X30 link register)
/// and 16-byte aligned stack allocations.

type frame_record = {
  saved_fp: nat64;
  saved_lr: nat64;
  frame_size: nat;
}

/// Theorem: Allocating a 16-byte aligned stack frame preserves SP alignment
let lemma_stack_alloc_alignment (s: state) (size: nat{size % 16 == 0})
  (h_aligned: sp_aligned s)
  : Lemma (ensures (U64.v (s.regs SP) >= size ==>
                    (U64.v (s.regs SP) - size) % 16 == 0)) =
  ()

/// Push frame record (simulates STP X29, X30, [SP, #-16]!)
let push_frame (s: state) : state =
  let old_sp = s.regs SP in
  let new_sp = U64.sub_mod old_sp (U64.uint_to_t 16) in
  let s1 = update_reg SP new_sp s in
  update_reg X29 new_sp s1

/// Pop frame record (simulates LDP X29, X30, [SP], #16)
let pop_frame (saved_fp: nat64) (saved_lr: nat64) (s: state) : state =
  let old_sp = s.regs SP in
  let new_sp = U64.add_mod old_sp (U64.uint_to_t 16) in
  let s1 = update_reg SP new_sp s in
  let s2 = update_reg X29 saved_fp s1 in
  update_reg X30 saved_lr s2

/// Theorem: Pushing and popping a frame record restores the original SP and frame pointer
let lemma_frame_roundtrip (s0: state)
  : Lemma (ensures (let s1 = push_frame s0 in
                    let s2 = pop_frame (s0.regs X29) (s0.regs X30) s1 in
                    s2.regs SP == s0.regs SP /\
                    s2.regs X29 == s0.regs X29 /\
                    s2.regs X30 == s0.regs X30)) =
  ()
