module Valerie.Arm32.Machine

open Valerie.Defs

type reg =
  | R0 | R1 | R2 | R3
  | R4 | R5 | R6 | R7
  | R8 | R9 | R10 | R11
  | R12 | SP | LR | PC

type sreg =
  | S0  | S1  | S2  | S3  | S4  | S5  | S6  | S7
  | S8  | S9  | S10 | S11 | S12 | S13 | S14 | S15
  | S16 | S17 | S18 | S19 | S20 | S21 | S22 | S23
  | S24 | S25 | S26 | S27 | S28 | S29 | S30 | S31

type dreg =
  | D0  | D1  | D2  | D3  | D4  | D5  | D6  | D7
  | D8  | D9  | D10 | D11 | D12 | D13 | D14 | D15
  | D16 | D17 | D18 | D19 | D20 | D21 | D22 | D23
  | D24 | D25 | D26 | D27 | D28 | D29 | D30 | D31

type qreg =
  | Q0  | Q1  | Q2  | Q3  | Q4  | Q5  | Q6  | Q7
  | Q8  | Q9  | Q10 | Q11 | Q12 | Q13 | Q14 | Q15

type cond_code =
  | CondEQ
  | CondNE
  | CondCS
  | CondCC
  | CondMI
  | CondPL
  | CondVS
  | CondVC
  | CondHI
  | CondLS
  | CondGE
  | CondLT
  | CondGT
  | CondLE
  | CondAL

type maddr =
  | MBase: base:reg -> maddr
  | MBaseOffset: base:reg -> offset:int -> maddr
  | MPreIndexed: base:reg -> offset:int -> maddr
  | MPostIndexed: base:reg -> offset:int -> maddr
  | MRegOffset: base:reg -> offset:reg -> maddr

type flags_t = {
  n: bool;
  z: bool;
  c: bool;
  v: bool;
}

/// Pure physical ARM32 machine CPU state (registers and condition flags only)
/// Memory is exclusively modeled in Pulse separation logic
noeq type state = {
  ok: bool;
  regs: reg -> nat32;
  sregs: sreg -> nat32;
  flags: flags_t;
}

let init_flags : flags_t = {
  n = false;
  z = false;
  c = false;
  v = false;
}
