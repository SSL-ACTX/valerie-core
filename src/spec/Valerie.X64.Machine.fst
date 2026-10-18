module Valerie.X64.Machine

open Valerie.Defs

type reg =
  | Rax | Rbx | Rcx | Rdx
  | Rsi | Rdi | Rbp | Rsp
  | R8  | R9  | R10 | R11
  | R12 | R13 | R14 | R15

type reg_width =
  | R32
  | R64

type xmm =
  | Xmm0  | Xmm1  | Xmm2  | Xmm3
  | Xmm4  | Xmm5  | Xmm6  | Xmm7
  | Xmm8  | Xmm9  | Xmm10 | Xmm11
  | Xmm12 | Xmm13 | Xmm14 | Xmm15

type ymm =
  | Ymm0  | Ymm1  | Ymm2  | Ymm3
  | Ymm4  | Ymm5  | Ymm6  | Ymm7
  | Ymm8  | Ymm9  | Ymm10 | Ymm11
  | Ymm12 | Ymm13 | Ymm14 | Ymm15

type cond_code =
  | CondE
  | CondNE
  | CondB
  | CondBE
  | CondA
  | CondAE
  | CondL
  | CondLE
  | CondG
  | CondGE

type maddr =
  | MConst: addr:int -> maddr
  | MReg: r:reg -> offset:int -> maddr
  | MIndex: base:reg -> scale:nat{scale = 1 \/ scale = 2 \/ scale = 4 \/ scale = 8} -> index:reg -> offset:int -> maddr
  | MRipRel: offset:int -> maddr

type flags_t = {
  cf: bool;
  zf: bool;
  sf: bool;
  ov: bool;
}

noeq type state = {
  ok: bool;
  regs: reg -> nat64;
  xmms: xmm -> nat128;
  flags: flags_t;
}

let init_flags : flags_t = {
  cf = false;
  zf = false;
  sf = false;
  ov = false;
}
