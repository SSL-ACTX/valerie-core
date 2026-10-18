module Valerie.Proof.ABI.Riscv64

open Valerie.Defs
open FStar.UInt64
module U64 = FStar.UInt64
open Valerie.Riscv64.Machine
open Valerie.Riscv64.Semantics
open Valerie.Riscv64.CallingConv

/// RISC-V LP64D Callee-Saved Register Invariant Proofs
/// Proves that executing instructions writing to volatile registers (RA, T0-T2, A0-A7, T3-T6)
/// strictly preserves callee-saved registers (S0-S11) and stack pointer integrity.

let update_reg (r: reg) (v: nat64) (s: state) : state =
  { s with regs = (fun r' -> if r' = r then (if r = Zero then U64.uint_to_t 0 else v) else s.regs r') }

let lemma_abi_volatile_update (dst: reg{is_volatile_gpr dst}) (v: nat64) (s0: state)
  : Lemma (ensures (let s1 = update_reg dst v s0 in
                    riscv_abi_compliant s0 s1)) =
  ()

let lemma_abi_transitive (s0 s1 s2: state)
  (h01: riscv_abi_compliant s0 s1)
  (h12: riscv_abi_compliant s1 s2)
  : Lemma (ensures riscv_abi_compliant s0 s2) =
  ()
