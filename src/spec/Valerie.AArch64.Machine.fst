module Valerie.AArch64.Machine

open Valerie.Defs

type reg =
  | X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7
  | X8 | X9 | X10 | X11 | X12 | X13 | X14 | X15
  | X16 | X17 | X18 | X19 | X20 | X21 | X22 | X23
  | X24 | X25 | X26 | X27 | X28 | X29 | X30
  | SP | XZR

type vec =
  | V0 | V1 | V2 | V3 | V4 | V5 | V6 | V7
  | V8 | V9 | V10 | V11 | V12 | V13 | V14 | V15
  | V16 | V17 | V18 | V19 | V20 | V21 | V22 | V23
  | V24 | V25 | V26 | V27 | V28 | V29 | V30 | V31

type reg_width =
  | W32
  | W64

type fp_width =
  | FPS
  | FPD
  | FPQ

type vec_arrangement =
  | B16
  | H8
  | S4
  | D2
  | B8
  | H4
  | S2
  | D1
  | Q1

type vec_elem_size =
  | EB
  | EH
  | ES
  | ED

type shift_type =
  | LSL
  | LSR
  | ASR
  | ROR

type extend_type =
  | UXTB
  | UXTH
  | UXTW
  | UXTX
  | SXTB
  | SXTH
  | SXTW
  | SXTX

type cond_code =
  | CondEQ | CondNE | CondCS | CondCC
  | CondMI | CondPL | CondVS | CondVC
  | CondHI | CondLS | CondGE | CondLT
  | CondGT | CondLE | CondAL | CondNV

type maddr =
  | MReg: base:reg -> maddr
  | MOffset: base:reg -> offset:int -> maddr
  | MPreIndex: base:reg -> offset:int -> maddr
  | MPostIndex: base:reg -> offset:int -> maddr
  | MRegOffset: base:reg -> index:reg -> ext:extend_type -> shift:nat{shift <= 4} -> maddr

type flags_t = {
  n: bool;
  z: bool;
  c: bool;
  v: bool;
  dit: bool;
}

noeq type state = {
  ok: bool;
  regs: reg -> nat64;
  vecs: vec -> nat128;
  flags: flags_t;
}

let init_flags (dit_enabled: bool) : flags_t = {
  n = false;
  z = false;
  c = false;
  v = false;
  dit = dit_enabled;
}
