open Prims
type reg =
  | Rax 
  | Rbx 
  | Rcx 
  | Rdx 
  | Rsi 
  | Rdi 
  | Rbp 
  | Rsp 
  | R8 
  | R9 
  | R10 
  | R11 
  | R12 
  | R13 
  | R14 
  | R15 
let uu___is_Rax (projectee : reg) : Prims.bool=
  match projectee with | Rax -> true | uu___ -> false
let uu___is_Rbx (projectee : reg) : Prims.bool=
  match projectee with | Rbx -> true | uu___ -> false
let uu___is_Rcx (projectee : reg) : Prims.bool=
  match projectee with | Rcx -> true | uu___ -> false
let uu___is_Rdx (projectee : reg) : Prims.bool=
  match projectee with | Rdx -> true | uu___ -> false
let uu___is_Rsi (projectee : reg) : Prims.bool=
  match projectee with | Rsi -> true | uu___ -> false
let uu___is_Rdi (projectee : reg) : Prims.bool=
  match projectee with | Rdi -> true | uu___ -> false
let uu___is_Rbp (projectee : reg) : Prims.bool=
  match projectee with | Rbp -> true | uu___ -> false
let uu___is_Rsp (projectee : reg) : Prims.bool=
  match projectee with | Rsp -> true | uu___ -> false
let uu___is_R8 (projectee : reg) : Prims.bool=
  match projectee with | R8 -> true | uu___ -> false
let uu___is_R9 (projectee : reg) : Prims.bool=
  match projectee with | R9 -> true | uu___ -> false
let uu___is_R10 (projectee : reg) : Prims.bool=
  match projectee with | R10 -> true | uu___ -> false
let uu___is_R11 (projectee : reg) : Prims.bool=
  match projectee with | R11 -> true | uu___ -> false
let uu___is_R12 (projectee : reg) : Prims.bool=
  match projectee with | R12 -> true | uu___ -> false
let uu___is_R13 (projectee : reg) : Prims.bool=
  match projectee with | R13 -> true | uu___ -> false
let uu___is_R14 (projectee : reg) : Prims.bool=
  match projectee with | R14 -> true | uu___ -> false
let uu___is_R15 (projectee : reg) : Prims.bool=
  match projectee with | R15 -> true | uu___ -> false
type reg_width =
  | R32 
  | R64 
let uu___is_R32 (projectee : reg_width) : Prims.bool=
  match projectee with | R32 -> true | uu___ -> false
let uu___is_R64 (projectee : reg_width) : Prims.bool=
  match projectee with | R64 -> true | uu___ -> false
type xmm =
  | Xmm0 
  | Xmm1 
  | Xmm2 
  | Xmm3 
  | Xmm4 
  | Xmm5 
  | Xmm6 
  | Xmm7 
  | Xmm8 
  | Xmm9 
  | Xmm10 
  | Xmm11 
  | Xmm12 
  | Xmm13 
  | Xmm14 
  | Xmm15 
let uu___is_Xmm0 (projectee : xmm) : Prims.bool=
  match projectee with | Xmm0 -> true | uu___ -> false
let uu___is_Xmm1 (projectee : xmm) : Prims.bool=
  match projectee with | Xmm1 -> true | uu___ -> false
let uu___is_Xmm2 (projectee : xmm) : Prims.bool=
  match projectee with | Xmm2 -> true | uu___ -> false
let uu___is_Xmm3 (projectee : xmm) : Prims.bool=
  match projectee with | Xmm3 -> true | uu___ -> false
let uu___is_Xmm4 (projectee : xmm) : Prims.bool=
  match projectee with | Xmm4 -> true | uu___ -> false
let uu___is_Xmm5 (projectee : xmm) : Prims.bool=
  match projectee with | Xmm5 -> true | uu___ -> false
let uu___is_Xmm6 (projectee : xmm) : Prims.bool=
  match projectee with | Xmm6 -> true | uu___ -> false
let uu___is_Xmm7 (projectee : xmm) : Prims.bool=
  match projectee with | Xmm7 -> true | uu___ -> false
let uu___is_Xmm8 (projectee : xmm) : Prims.bool=
  match projectee with | Xmm8 -> true | uu___ -> false
let uu___is_Xmm9 (projectee : xmm) : Prims.bool=
  match projectee with | Xmm9 -> true | uu___ -> false
let uu___is_Xmm10 (projectee : xmm) : Prims.bool=
  match projectee with | Xmm10 -> true | uu___ -> false
let uu___is_Xmm11 (projectee : xmm) : Prims.bool=
  match projectee with | Xmm11 -> true | uu___ -> false
let uu___is_Xmm12 (projectee : xmm) : Prims.bool=
  match projectee with | Xmm12 -> true | uu___ -> false
let uu___is_Xmm13 (projectee : xmm) : Prims.bool=
  match projectee with | Xmm13 -> true | uu___ -> false
let uu___is_Xmm14 (projectee : xmm) : Prims.bool=
  match projectee with | Xmm14 -> true | uu___ -> false
let uu___is_Xmm15 (projectee : xmm) : Prims.bool=
  match projectee with | Xmm15 -> true | uu___ -> false
type ymm =
  | Ymm0 
  | Ymm1 
  | Ymm2 
  | Ymm3 
  | Ymm4 
  | Ymm5 
  | Ymm6 
  | Ymm7 
  | Ymm8 
  | Ymm9 
  | Ymm10 
  | Ymm11 
  | Ymm12 
  | Ymm13 
  | Ymm14 
  | Ymm15 
let uu___is_Ymm0 (projectee : ymm) : Prims.bool=
  match projectee with | Ymm0 -> true | uu___ -> false
let uu___is_Ymm1 (projectee : ymm) : Prims.bool=
  match projectee with | Ymm1 -> true | uu___ -> false
let uu___is_Ymm2 (projectee : ymm) : Prims.bool=
  match projectee with | Ymm2 -> true | uu___ -> false
let uu___is_Ymm3 (projectee : ymm) : Prims.bool=
  match projectee with | Ymm3 -> true | uu___ -> false
let uu___is_Ymm4 (projectee : ymm) : Prims.bool=
  match projectee with | Ymm4 -> true | uu___ -> false
let uu___is_Ymm5 (projectee : ymm) : Prims.bool=
  match projectee with | Ymm5 -> true | uu___ -> false
let uu___is_Ymm6 (projectee : ymm) : Prims.bool=
  match projectee with | Ymm6 -> true | uu___ -> false
let uu___is_Ymm7 (projectee : ymm) : Prims.bool=
  match projectee with | Ymm7 -> true | uu___ -> false
let uu___is_Ymm8 (projectee : ymm) : Prims.bool=
  match projectee with | Ymm8 -> true | uu___ -> false
let uu___is_Ymm9 (projectee : ymm) : Prims.bool=
  match projectee with | Ymm9 -> true | uu___ -> false
let uu___is_Ymm10 (projectee : ymm) : Prims.bool=
  match projectee with | Ymm10 -> true | uu___ -> false
let uu___is_Ymm11 (projectee : ymm) : Prims.bool=
  match projectee with | Ymm11 -> true | uu___ -> false
let uu___is_Ymm12 (projectee : ymm) : Prims.bool=
  match projectee with | Ymm12 -> true | uu___ -> false
let uu___is_Ymm13 (projectee : ymm) : Prims.bool=
  match projectee with | Ymm13 -> true | uu___ -> false
let uu___is_Ymm14 (projectee : ymm) : Prims.bool=
  match projectee with | Ymm14 -> true | uu___ -> false
let uu___is_Ymm15 (projectee : ymm) : Prims.bool=
  match projectee with | Ymm15 -> true | uu___ -> false
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
let uu___is_CondE (projectee : cond_code) : Prims.bool=
  match projectee with | CondE -> true | uu___ -> false
let uu___is_CondNE (projectee : cond_code) : Prims.bool=
  match projectee with | CondNE -> true | uu___ -> false
let uu___is_CondB (projectee : cond_code) : Prims.bool=
  match projectee with | CondB -> true | uu___ -> false
let uu___is_CondBE (projectee : cond_code) : Prims.bool=
  match projectee with | CondBE -> true | uu___ -> false
let uu___is_CondA (projectee : cond_code) : Prims.bool=
  match projectee with | CondA -> true | uu___ -> false
let uu___is_CondAE (projectee : cond_code) : Prims.bool=
  match projectee with | CondAE -> true | uu___ -> false
let uu___is_CondL (projectee : cond_code) : Prims.bool=
  match projectee with | CondL -> true | uu___ -> false
let uu___is_CondLE (projectee : cond_code) : Prims.bool=
  match projectee with | CondLE -> true | uu___ -> false
let uu___is_CondG (projectee : cond_code) : Prims.bool=
  match projectee with | CondG -> true | uu___ -> false
let uu___is_CondGE (projectee : cond_code) : Prims.bool=
  match projectee with | CondGE -> true | uu___ -> false
type maddr =
  | MConst of Prims.int 
  | MReg of reg * Prims.int 
  | MIndex of reg * Prims.nat * reg * Prims.int 
  | MRipRel of Prims.int 
let uu___is_MConst (projectee : maddr) : Prims.bool=
  match projectee with | MConst addr -> true | uu___ -> false
let __proj__MConst__item__addr (projectee : maddr) : Prims.int=
  match projectee with | MConst addr -> addr
let uu___is_MReg (projectee : maddr) : Prims.bool=
  match projectee with | MReg (r, offset) -> true | uu___ -> false
let __proj__MReg__item__r (projectee : maddr) : reg=
  match projectee with | MReg (r, offset) -> r
let __proj__MReg__item__offset (projectee : maddr) : Prims.int=
  match projectee with | MReg (r, offset) -> offset
let uu___is_MIndex (projectee : maddr) : Prims.bool=
  match projectee with
  | MIndex (base, scale, index, offset) -> true
  | uu___ -> false
let __proj__MIndex__item__base (projectee : maddr) : reg=
  match projectee with | MIndex (base, scale, index, offset) -> base
let __proj__MIndex__item__scale (projectee : maddr) : Prims.nat=
  match projectee with | MIndex (base, scale, index, offset) -> scale
let __proj__MIndex__item__index (projectee : maddr) : reg=
  match projectee with | MIndex (base, scale, index, offset) -> index
let __proj__MIndex__item__offset (projectee : maddr) : Prims.int=
  match projectee with | MIndex (base, scale, index, offset) -> offset
let uu___is_MRipRel (projectee : maddr) : Prims.bool=
  match projectee with | MRipRel offset -> true | uu___ -> false
let __proj__MRipRel__item__offset (projectee : maddr) : Prims.int=
  match projectee with | MRipRel offset -> offset
type flags_t =
  {
  cf: Prims.bool ;
  zf: Prims.bool ;
  sf: Prims.bool ;
  ov: Prims.bool }
let __proj__Mkflags_t__item__cf (projectee : flags_t) : Prims.bool=
  match projectee with | { cf; zf; sf; ov;_} -> cf
let __proj__Mkflags_t__item__zf (projectee : flags_t) : Prims.bool=
  match projectee with | { cf; zf; sf; ov;_} -> zf
let __proj__Mkflags_t__item__sf (projectee : flags_t) : Prims.bool=
  match projectee with | { cf; zf; sf; ov;_} -> sf
let __proj__Mkflags_t__item__ov (projectee : flags_t) : Prims.bool=
  match projectee with | { cf; zf; sf; ov;_} -> ov
type state =
  {
  ok: Prims.bool ;
  regs: reg -> Valerie_Defs.nat64 ;
  xmms: xmm -> Valerie_Defs.nat128 ;
  flags: flags_t }
let __proj__Mkstate__item__ok (projectee : state) : Prims.bool=
  match projectee with | { ok; regs; xmms; flags;_} -> ok
let __proj__Mkstate__item__regs (projectee : state) :
  reg -> Valerie_Defs.nat64=
  match projectee with | { ok; regs; xmms; flags;_} -> regs
let __proj__Mkstate__item__xmms (projectee : state) :
  xmm -> Valerie_Defs.nat128=
  match projectee with | { ok; regs; xmms; flags;_} -> xmms
let __proj__Mkstate__item__flags (projectee : state) : flags_t=
  match projectee with | { ok; regs; xmms; flags;_} -> flags
let init_flags : flags_t= { cf = false; zf = false; sf = false; ov = false }
