module Valerie.Riscv64.Machine

open Valerie.Defs

type reg =
  | Zero | RA | SP | GP | TP
  | T0 | T1 | T2
  | S0 | S1
  | A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7
  | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 | S10 | S11
  | T3 | T4 | T5 | T6

/// Pure physical machine CPU state (registers only)
/// Memory is exclusively modeled in Pulse separation logic
noeq type state = {
  ok: bool;
  regs: reg -> nat64;
}
