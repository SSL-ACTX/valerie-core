open Prims
type operand =
  | OReg of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg_width 
  | OImm of Valerie_Defs.nat64 
  | OFpReg of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.fp_width
  
  | OVecReg of Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec_arrangement 
  | OVecElem of Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec_elem_size * Prims.nat 
  | OShifted of Valerie_AArch64_Machine.reg *
  Valerie_AArch64_Machine.reg_width * Valerie_AArch64_Machine.shift_type *
  Prims.nat 
  | OExtended of Valerie_AArch64_Machine.reg *
  Valerie_AArch64_Machine.reg_width * Valerie_AArch64_Machine.extend_type *
  Prims.nat 
  | OMem of Valerie_AArch64_Machine.maddr 
  | OCond of Valerie_AArch64_Machine.cond_code 
let uu___is_OReg (projectee : operand) : Prims.bool=
  match projectee with | OReg (r, w) -> true | uu___ -> false
let __proj__OReg__item__r (projectee : operand) :
  Valerie_AArch64_Machine.reg= match projectee with | OReg (r, w) -> r
let __proj__OReg__item__w (projectee : operand) :
  Valerie_AArch64_Machine.reg_width= match projectee with | OReg (r, w) -> w
let uu___is_OImm (projectee : operand) : Prims.bool=
  match projectee with | OImm imm -> true | uu___ -> false
let __proj__OImm__item__imm (projectee : operand) : Valerie_Defs.nat64=
  match projectee with | OImm imm -> imm
let uu___is_OFpReg (projectee : operand) : Prims.bool=
  match projectee with | OFpReg (v, w) -> true | uu___ -> false
let __proj__OFpReg__item__v (projectee : operand) :
  Valerie_AArch64_Machine.vec= match projectee with | OFpReg (v, w) -> v
let __proj__OFpReg__item__w (projectee : operand) :
  Valerie_AArch64_Machine.fp_width= match projectee with | OFpReg (v, w) -> w
let uu___is_OVecReg (projectee : operand) : Prims.bool=
  match projectee with | OVecReg (v, arr) -> true | uu___ -> false
let __proj__OVecReg__item__v (projectee : operand) :
  Valerie_AArch64_Machine.vec= match projectee with | OVecReg (v, arr) -> v
let __proj__OVecReg__item__arr (projectee : operand) :
  Valerie_AArch64_Machine.vec_arrangement=
  match projectee with | OVecReg (v, arr) -> arr
let uu___is_OVecElem (projectee : operand) : Prims.bool=
  match projectee with | OVecElem (v, sz, lane) -> true | uu___ -> false
let __proj__OVecElem__item__v (projectee : operand) :
  Valerie_AArch64_Machine.vec=
  match projectee with | OVecElem (v, sz, lane) -> v
let __proj__OVecElem__item__sz (projectee : operand) :
  Valerie_AArch64_Machine.vec_elem_size=
  match projectee with | OVecElem (v, sz, lane) -> sz
let __proj__OVecElem__item__lane (projectee : operand) : Prims.nat=
  match projectee with | OVecElem (v, sz, lane) -> lane
let uu___is_OShifted (projectee : operand) : Prims.bool=
  match projectee with | OShifted (r, w, st, amount) -> true | uu___ -> false
let __proj__OShifted__item__r (projectee : operand) :
  Valerie_AArch64_Machine.reg=
  match projectee with | OShifted (r, w, st, amount) -> r
let __proj__OShifted__item__w (projectee : operand) :
  Valerie_AArch64_Machine.reg_width=
  match projectee with | OShifted (r, w, st, amount) -> w
let __proj__OShifted__item__st (projectee : operand) :
  Valerie_AArch64_Machine.shift_type=
  match projectee with | OShifted (r, w, st, amount) -> st
let __proj__OShifted__item__amount (projectee : operand) : Prims.nat=
  match projectee with | OShifted (r, w, st, amount) -> amount
let uu___is_OExtended (projectee : operand) : Prims.bool=
  match projectee with
  | OExtended (r, w, ext, lsl_amount) -> true
  | uu___ -> false
let __proj__OExtended__item__r (projectee : operand) :
  Valerie_AArch64_Machine.reg=
  match projectee with | OExtended (r, w, ext, lsl_amount) -> r
let __proj__OExtended__item__w (projectee : operand) :
  Valerie_AArch64_Machine.reg_width=
  match projectee with | OExtended (r, w, ext, lsl_amount) -> w
let __proj__OExtended__item__ext (projectee : operand) :
  Valerie_AArch64_Machine.extend_type=
  match projectee with | OExtended (r, w, ext, lsl_amount) -> ext
let __proj__OExtended__item__lsl_amount (projectee : operand) : Prims.nat=
  match projectee with | OExtended (r, w, ext, lsl_amount) -> lsl_amount
let uu___is_OMem (projectee : operand) : Prims.bool=
  match projectee with | OMem m -> true | uu___ -> false
let __proj__OMem__item__m (projectee : operand) :
  Valerie_AArch64_Machine.maddr= match projectee with | OMem m -> m
let uu___is_OCond (projectee : operand) : Prims.bool=
  match projectee with | OCond c -> true | uu___ -> false
let __proj__OCond__item__c (projectee : operand) :
  Valerie_AArch64_Machine.cond_code= match projectee with | OCond c -> c
type ins =
  | Add of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  operand 
  | Adds of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  operand 
  | Sub of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  operand 
  | Subs of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  operand 
  | Adc of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  Valerie_AArch64_Machine.reg 
  | Adcs of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  Valerie_AArch64_Machine.reg 
  | Sbc of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  Valerie_AArch64_Machine.reg 
  | Sbcs of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  Valerie_AArch64_Machine.reg 
  | Cmp of Valerie_AArch64_Machine.reg * operand 
  | Cmn of Valerie_AArch64_Machine.reg * operand 
  | Tst of Valerie_AArch64_Machine.reg * operand 
  | Neg of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg 
  | Negs of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg 
  | Mvn of Valerie_AArch64_Machine.reg * operand 
  | Mul of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  Valerie_AArch64_Machine.reg 
  | Madd of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg 
  | Msub of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg 
  | Smulh of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  Valerie_AArch64_Machine.reg 
  | Umulh of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  Valerie_AArch64_Machine.reg 
  | Sdiv of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  Valerie_AArch64_Machine.reg 
  | Udiv of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  Valerie_AArch64_Machine.reg 
  | And of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  operand 
  | Ands of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  operand 
  | Orr of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  operand 
  | Orn of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  operand 
  | Eor of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  operand 
  | Eon of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  operand 
  | Bic of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  operand 
  | Bics of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  operand 
  | Add32 of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  operand 
  | Sub32 of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  operand 
  | Eor32 of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  operand 
  | And32 of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  operand 
  | Orr32 of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  operand 
  | Ror32 of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  operand 
  | Ldr32 of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.maddr 
  | Str32 of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.maddr 
  | Lsl of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  operand 
  | Lsr of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  operand 
  | Asr of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  operand 
  | Ror of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  operand 
  | Extr of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  Valerie_AArch64_Machine.reg * Prims.nat 
  | Clz of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg 
  | Rbit of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg 
  | Rev of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg 
  | Rev16 of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg 
  | Rev32 of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg 
  | Rev64 of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg 
  | Csel of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.cond_code 
  | Cset of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.cond_code 
  | Csetm of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.cond_code
  
  | Cinc of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  Valerie_AArch64_Machine.cond_code 
  | Cinv of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  Valerie_AArch64_Machine.cond_code 
  | Cneg of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  Valerie_AArch64_Machine.cond_code 
  | Csinc of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.cond_code 
  | Csinv of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.cond_code 
  | Csneg of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.cond_code 
  | Mov of Valerie_AArch64_Machine.reg * operand 
  | Movz of Valerie_AArch64_Machine.reg * Valerie_Defs.nat64 * Prims.nat 
  | Movn of Valerie_AArch64_Machine.reg * Valerie_Defs.nat64 * Prims.nat 
  | Movk of Valerie_AArch64_Machine.reg * Valerie_Defs.nat64 * Prims.nat 
  | Adrp of Valerie_AArch64_Machine.reg * Prims.string 
  | Ldr of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.maddr 
  | Ldrb of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.maddr 
  | Ldrh of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.maddr 
  | Ldrsb of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.maddr 
  | Ldrsh of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.maddr 
  | Ldrsw of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.maddr 
  | Str of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.maddr 
  | Strb of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.maddr 
  | Strh of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.maddr 
  | Ldp of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  Valerie_AArch64_Machine.maddr 
  | Stp of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.reg *
  Valerie_AArch64_Machine.maddr 
  | B of Prims.string 
  | BCond of Valerie_AArch64_Machine.cond_code * Prims.string 
  | Bl of Prims.string 
  | Blr of Valerie_AArch64_Machine.reg 
  | Ret 
  | Cbz of Valerie_AArch64_Machine.reg * Prims.string 
  | Cbnz of Valerie_AArch64_Machine.reg * Prims.string 
  | FmovGprToVec of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.reg
  
  | FmovVecToGpr of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.vec
  
  | Fadd of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec 
  | Fsub of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec 
  | Fmul of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec 
  | Fdiv of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec 
  | Dup of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.reg 
  | InsLane of Valerie_AArch64_Machine.vec * Prims.nat *
  Valerie_AArch64_Machine.reg 
  | Umov of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.vec *
  Prims.nat 
  | Smov of Valerie_AArch64_Machine.reg * Valerie_AArch64_Machine.vec *
  Prims.nat 
  | Vadd of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec_arrangement 
  | Vsub of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec_arrangement 
  | Vmul of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec_arrangement 
  | Vand of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec 
  | Vorr of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec 
  | Veor of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec 
  | Vext8 of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec * Prims.nat 
  | Vrev64 of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec_arrangement 
  | Vrev32 of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec_arrangement 
  | Vrev16 of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec_arrangement 
  | Pmull of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec 
  | Pmull2 of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec 
  | Aese of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec 
  | Aesd of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec 
  | Aesmc of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec 
  | Aesimc of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec 
  | Sha1c of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec 
  | Sha1p of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec 
  | Sha1m of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec 
  | Sha1h of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec 
  | Sha1su0 of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec 
  | Sha1su1 of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec 
  | Sha256h of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec 
  | Sha256h2 of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec 
  | Sha256su0 of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec 
  | Sha256su1 of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec 
  | Eor3 of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec 
  | Rax1 of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec 
  | Xar of Valerie_AArch64_Machine.vec * Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec * Prims.nat 
  | Ld1 of Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec_arrangement * Valerie_AArch64_Machine.maddr 
  | St1 of Valerie_AArch64_Machine.vec *
  Valerie_AArch64_Machine.vec_arrangement * Valerie_AArch64_Machine.maddr 
  | MsrDit of Prims.bool 
let uu___is_Add (projectee : ins) : Prims.bool=
  match projectee with | Add (dst, s1, s2) -> true | uu___ -> false
let __proj__Add__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Add (dst, s1, s2) -> dst
let __proj__Add__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Add (dst, s1, s2) -> s1
let __proj__Add__item__s2 (projectee : ins) : operand=
  match projectee with | Add (dst, s1, s2) -> s2
let uu___is_Adds (projectee : ins) : Prims.bool=
  match projectee with | Adds (dst, s1, s2) -> true | uu___ -> false
let __proj__Adds__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Adds (dst, s1, s2) -> dst
let __proj__Adds__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Adds (dst, s1, s2) -> s1
let __proj__Adds__item__s2 (projectee : ins) : operand=
  match projectee with | Adds (dst, s1, s2) -> s2
let uu___is_Sub (projectee : ins) : Prims.bool=
  match projectee with | Sub (dst, s1, s2) -> true | uu___ -> false
let __proj__Sub__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Sub (dst, s1, s2) -> dst
let __proj__Sub__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Sub (dst, s1, s2) -> s1
let __proj__Sub__item__s2 (projectee : ins) : operand=
  match projectee with | Sub (dst, s1, s2) -> s2
let uu___is_Subs (projectee : ins) : Prims.bool=
  match projectee with | Subs (dst, s1, s2) -> true | uu___ -> false
let __proj__Subs__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Subs (dst, s1, s2) -> dst
let __proj__Subs__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Subs (dst, s1, s2) -> s1
let __proj__Subs__item__s2 (projectee : ins) : operand=
  match projectee with | Subs (dst, s1, s2) -> s2
let uu___is_Adc (projectee : ins) : Prims.bool=
  match projectee with | Adc (dst, s1, s2) -> true | uu___ -> false
let __proj__Adc__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Adc (dst, s1, s2) -> dst
let __proj__Adc__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Adc (dst, s1, s2) -> s1
let __proj__Adc__item__s2 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Adc (dst, s1, s2) -> s2
let uu___is_Adcs (projectee : ins) : Prims.bool=
  match projectee with | Adcs (dst, s1, s2) -> true | uu___ -> false
let __proj__Adcs__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Adcs (dst, s1, s2) -> dst
let __proj__Adcs__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Adcs (dst, s1, s2) -> s1
let __proj__Adcs__item__s2 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Adcs (dst, s1, s2) -> s2
let uu___is_Sbc (projectee : ins) : Prims.bool=
  match projectee with | Sbc (dst, s1, s2) -> true | uu___ -> false
let __proj__Sbc__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Sbc (dst, s1, s2) -> dst
let __proj__Sbc__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Sbc (dst, s1, s2) -> s1
let __proj__Sbc__item__s2 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Sbc (dst, s1, s2) -> s2
let uu___is_Sbcs (projectee : ins) : Prims.bool=
  match projectee with | Sbcs (dst, s1, s2) -> true | uu___ -> false
let __proj__Sbcs__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Sbcs (dst, s1, s2) -> dst
let __proj__Sbcs__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Sbcs (dst, s1, s2) -> s1
let __proj__Sbcs__item__s2 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Sbcs (dst, s1, s2) -> s2
let uu___is_Cmp (projectee : ins) : Prims.bool=
  match projectee with | Cmp (s1, s2) -> true | uu___ -> false
let __proj__Cmp__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Cmp (s1, s2) -> s1
let __proj__Cmp__item__s2 (projectee : ins) : operand=
  match projectee with | Cmp (s1, s2) -> s2
let uu___is_Cmn (projectee : ins) : Prims.bool=
  match projectee with | Cmn (s1, s2) -> true | uu___ -> false
let __proj__Cmn__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Cmn (s1, s2) -> s1
let __proj__Cmn__item__s2 (projectee : ins) : operand=
  match projectee with | Cmn (s1, s2) -> s2
let uu___is_Tst (projectee : ins) : Prims.bool=
  match projectee with | Tst (s1, s2) -> true | uu___ -> false
let __proj__Tst__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Tst (s1, s2) -> s1
let __proj__Tst__item__s2 (projectee : ins) : operand=
  match projectee with | Tst (s1, s2) -> s2
let uu___is_Neg (projectee : ins) : Prims.bool=
  match projectee with | Neg (dst, src) -> true | uu___ -> false
let __proj__Neg__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Neg (dst, src) -> dst
let __proj__Neg__item__src (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Neg (dst, src) -> src
let uu___is_Negs (projectee : ins) : Prims.bool=
  match projectee with | Negs (dst, src) -> true | uu___ -> false
let __proj__Negs__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Negs (dst, src) -> dst
let __proj__Negs__item__src (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Negs (dst, src) -> src
let uu___is_Mvn (projectee : ins) : Prims.bool=
  match projectee with | Mvn (dst, src) -> true | uu___ -> false
let __proj__Mvn__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Mvn (dst, src) -> dst
let __proj__Mvn__item__src (projectee : ins) : operand=
  match projectee with | Mvn (dst, src) -> src
let uu___is_Mul (projectee : ins) : Prims.bool=
  match projectee with | Mul (dst, s1, s2) -> true | uu___ -> false
let __proj__Mul__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Mul (dst, s1, s2) -> dst
let __proj__Mul__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Mul (dst, s1, s2) -> s1
let __proj__Mul__item__s2 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Mul (dst, s1, s2) -> s2
let uu___is_Madd (projectee : ins) : Prims.bool=
  match projectee with | Madd (dst, s1, s2, s3) -> true | uu___ -> false
let __proj__Madd__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Madd (dst, s1, s2, s3) -> dst
let __proj__Madd__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Madd (dst, s1, s2, s3) -> s1
let __proj__Madd__item__s2 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Madd (dst, s1, s2, s3) -> s2
let __proj__Madd__item__s3 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Madd (dst, s1, s2, s3) -> s3
let uu___is_Msub (projectee : ins) : Prims.bool=
  match projectee with | Msub (dst, s1, s2, s3) -> true | uu___ -> false
let __proj__Msub__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Msub (dst, s1, s2, s3) -> dst
let __proj__Msub__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Msub (dst, s1, s2, s3) -> s1
let __proj__Msub__item__s2 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Msub (dst, s1, s2, s3) -> s2
let __proj__Msub__item__s3 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Msub (dst, s1, s2, s3) -> s3
let uu___is_Smulh (projectee : ins) : Prims.bool=
  match projectee with | Smulh (dst, s1, s2) -> true | uu___ -> false
let __proj__Smulh__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Smulh (dst, s1, s2) -> dst
let __proj__Smulh__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Smulh (dst, s1, s2) -> s1
let __proj__Smulh__item__s2 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Smulh (dst, s1, s2) -> s2
let uu___is_Umulh (projectee : ins) : Prims.bool=
  match projectee with | Umulh (dst, s1, s2) -> true | uu___ -> false
let __proj__Umulh__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Umulh (dst, s1, s2) -> dst
let __proj__Umulh__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Umulh (dst, s1, s2) -> s1
let __proj__Umulh__item__s2 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Umulh (dst, s1, s2) -> s2
let uu___is_Sdiv (projectee : ins) : Prims.bool=
  match projectee with | Sdiv (dst, s1, s2) -> true | uu___ -> false
let __proj__Sdiv__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Sdiv (dst, s1, s2) -> dst
let __proj__Sdiv__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Sdiv (dst, s1, s2) -> s1
let __proj__Sdiv__item__s2 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Sdiv (dst, s1, s2) -> s2
let uu___is_Udiv (projectee : ins) : Prims.bool=
  match projectee with | Udiv (dst, s1, s2) -> true | uu___ -> false
let __proj__Udiv__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Udiv (dst, s1, s2) -> dst
let __proj__Udiv__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Udiv (dst, s1, s2) -> s1
let __proj__Udiv__item__s2 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Udiv (dst, s1, s2) -> s2
let uu___is_And (projectee : ins) : Prims.bool=
  match projectee with | And (dst, s1, s2) -> true | uu___ -> false
let __proj__And__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | And (dst, s1, s2) -> dst
let __proj__And__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | And (dst, s1, s2) -> s1
let __proj__And__item__s2 (projectee : ins) : operand=
  match projectee with | And (dst, s1, s2) -> s2
let uu___is_Ands (projectee : ins) : Prims.bool=
  match projectee with | Ands (dst, s1, s2) -> true | uu___ -> false
let __proj__Ands__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Ands (dst, s1, s2) -> dst
let __proj__Ands__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Ands (dst, s1, s2) -> s1
let __proj__Ands__item__s2 (projectee : ins) : operand=
  match projectee with | Ands (dst, s1, s2) -> s2
let uu___is_Orr (projectee : ins) : Prims.bool=
  match projectee with | Orr (dst, s1, s2) -> true | uu___ -> false
let __proj__Orr__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Orr (dst, s1, s2) -> dst
let __proj__Orr__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Orr (dst, s1, s2) -> s1
let __proj__Orr__item__s2 (projectee : ins) : operand=
  match projectee with | Orr (dst, s1, s2) -> s2
let uu___is_Orn (projectee : ins) : Prims.bool=
  match projectee with | Orn (dst, s1, s2) -> true | uu___ -> false
let __proj__Orn__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Orn (dst, s1, s2) -> dst
let __proj__Orn__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Orn (dst, s1, s2) -> s1
let __proj__Orn__item__s2 (projectee : ins) : operand=
  match projectee with | Orn (dst, s1, s2) -> s2
let uu___is_Eor (projectee : ins) : Prims.bool=
  match projectee with | Eor (dst, s1, s2) -> true | uu___ -> false
let __proj__Eor__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Eor (dst, s1, s2) -> dst
let __proj__Eor__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Eor (dst, s1, s2) -> s1
let __proj__Eor__item__s2 (projectee : ins) : operand=
  match projectee with | Eor (dst, s1, s2) -> s2
let uu___is_Eon (projectee : ins) : Prims.bool=
  match projectee with | Eon (dst, s1, s2) -> true | uu___ -> false
let __proj__Eon__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Eon (dst, s1, s2) -> dst
let __proj__Eon__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Eon (dst, s1, s2) -> s1
let __proj__Eon__item__s2 (projectee : ins) : operand=
  match projectee with | Eon (dst, s1, s2) -> s2
let uu___is_Bic (projectee : ins) : Prims.bool=
  match projectee with | Bic (dst, s1, s2) -> true | uu___ -> false
let __proj__Bic__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Bic (dst, s1, s2) -> dst
let __proj__Bic__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Bic (dst, s1, s2) -> s1
let __proj__Bic__item__s2 (projectee : ins) : operand=
  match projectee with | Bic (dst, s1, s2) -> s2
let uu___is_Bics (projectee : ins) : Prims.bool=
  match projectee with | Bics (dst, s1, s2) -> true | uu___ -> false
let __proj__Bics__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Bics (dst, s1, s2) -> dst
let __proj__Bics__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Bics (dst, s1, s2) -> s1
let __proj__Bics__item__s2 (projectee : ins) : operand=
  match projectee with | Bics (dst, s1, s2) -> s2
let uu___is_Add32 (projectee : ins) : Prims.bool=
  match projectee with | Add32 (dst, s1, s2) -> true | uu___ -> false
let __proj__Add32__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Add32 (dst, s1, s2) -> dst
let __proj__Add32__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Add32 (dst, s1, s2) -> s1
let __proj__Add32__item__s2 (projectee : ins) : operand=
  match projectee with | Add32 (dst, s1, s2) -> s2
let uu___is_Sub32 (projectee : ins) : Prims.bool=
  match projectee with | Sub32 (dst, s1, s2) -> true | uu___ -> false
let __proj__Sub32__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Sub32 (dst, s1, s2) -> dst
let __proj__Sub32__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Sub32 (dst, s1, s2) -> s1
let __proj__Sub32__item__s2 (projectee : ins) : operand=
  match projectee with | Sub32 (dst, s1, s2) -> s2
let uu___is_Eor32 (projectee : ins) : Prims.bool=
  match projectee with | Eor32 (dst, s1, s2) -> true | uu___ -> false
let __proj__Eor32__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Eor32 (dst, s1, s2) -> dst
let __proj__Eor32__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Eor32 (dst, s1, s2) -> s1
let __proj__Eor32__item__s2 (projectee : ins) : operand=
  match projectee with | Eor32 (dst, s1, s2) -> s2
let uu___is_And32 (projectee : ins) : Prims.bool=
  match projectee with | And32 (dst, s1, s2) -> true | uu___ -> false
let __proj__And32__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | And32 (dst, s1, s2) -> dst
let __proj__And32__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | And32 (dst, s1, s2) -> s1
let __proj__And32__item__s2 (projectee : ins) : operand=
  match projectee with | And32 (dst, s1, s2) -> s2
let uu___is_Orr32 (projectee : ins) : Prims.bool=
  match projectee with | Orr32 (dst, s1, s2) -> true | uu___ -> false
let __proj__Orr32__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Orr32 (dst, s1, s2) -> dst
let __proj__Orr32__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Orr32 (dst, s1, s2) -> s1
let __proj__Orr32__item__s2 (projectee : ins) : operand=
  match projectee with | Orr32 (dst, s1, s2) -> s2
let uu___is_Ror32 (projectee : ins) : Prims.bool=
  match projectee with | Ror32 (dst, s1, s2) -> true | uu___ -> false
let __proj__Ror32__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Ror32 (dst, s1, s2) -> dst
let __proj__Ror32__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Ror32 (dst, s1, s2) -> s1
let __proj__Ror32__item__s2 (projectee : ins) : operand=
  match projectee with | Ror32 (dst, s1, s2) -> s2
let uu___is_Ldr32 (projectee : ins) : Prims.bool=
  match projectee with | Ldr32 (dst, addr) -> true | uu___ -> false
let __proj__Ldr32__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Ldr32 (dst, addr) -> dst
let __proj__Ldr32__item__addr (projectee : ins) :
  Valerie_AArch64_Machine.maddr=
  match projectee with | Ldr32 (dst, addr) -> addr
let uu___is_Str32 (projectee : ins) : Prims.bool=
  match projectee with | Str32 (src, addr) -> true | uu___ -> false
let __proj__Str32__item__src (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Str32 (src, addr) -> src
let __proj__Str32__item__addr (projectee : ins) :
  Valerie_AArch64_Machine.maddr=
  match projectee with | Str32 (src, addr) -> addr
let uu___is_Lsl (projectee : ins) : Prims.bool=
  match projectee with | Lsl (dst, s1, s2) -> true | uu___ -> false
let __proj__Lsl__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Lsl (dst, s1, s2) -> dst
let __proj__Lsl__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Lsl (dst, s1, s2) -> s1
let __proj__Lsl__item__s2 (projectee : ins) : operand=
  match projectee with | Lsl (dst, s1, s2) -> s2
let uu___is_Lsr (projectee : ins) : Prims.bool=
  match projectee with | Lsr (dst, s1, s2) -> true | uu___ -> false
let __proj__Lsr__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Lsr (dst, s1, s2) -> dst
let __proj__Lsr__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Lsr (dst, s1, s2) -> s1
let __proj__Lsr__item__s2 (projectee : ins) : operand=
  match projectee with | Lsr (dst, s1, s2) -> s2
let uu___is_Asr (projectee : ins) : Prims.bool=
  match projectee with | Asr (dst, s1, s2) -> true | uu___ -> false
let __proj__Asr__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Asr (dst, s1, s2) -> dst
let __proj__Asr__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Asr (dst, s1, s2) -> s1
let __proj__Asr__item__s2 (projectee : ins) : operand=
  match projectee with | Asr (dst, s1, s2) -> s2
let uu___is_Ror (projectee : ins) : Prims.bool=
  match projectee with | Ror (dst, s1, s2) -> true | uu___ -> false
let __proj__Ror__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Ror (dst, s1, s2) -> dst
let __proj__Ror__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Ror (dst, s1, s2) -> s1
let __proj__Ror__item__s2 (projectee : ins) : operand=
  match projectee with | Ror (dst, s1, s2) -> s2
let uu___is_Extr (projectee : ins) : Prims.bool=
  match projectee with | Extr (dst, s1, s2, lsb) -> true | uu___ -> false
let __proj__Extr__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Extr (dst, s1, s2, lsb) -> dst
let __proj__Extr__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Extr (dst, s1, s2, lsb) -> s1
let __proj__Extr__item__s2 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Extr (dst, s1, s2, lsb) -> s2
let __proj__Extr__item__lsb (projectee : ins) : Prims.nat=
  match projectee with | Extr (dst, s1, s2, lsb) -> lsb
let uu___is_Clz (projectee : ins) : Prims.bool=
  match projectee with | Clz (dst, src) -> true | uu___ -> false
let __proj__Clz__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Clz (dst, src) -> dst
let __proj__Clz__item__src (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Clz (dst, src) -> src
let uu___is_Rbit (projectee : ins) : Prims.bool=
  match projectee with | Rbit (dst, src) -> true | uu___ -> false
let __proj__Rbit__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Rbit (dst, src) -> dst
let __proj__Rbit__item__src (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Rbit (dst, src) -> src
let uu___is_Rev (projectee : ins) : Prims.bool=
  match projectee with | Rev (dst, src) -> true | uu___ -> false
let __proj__Rev__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Rev (dst, src) -> dst
let __proj__Rev__item__src (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Rev (dst, src) -> src
let uu___is_Rev16 (projectee : ins) : Prims.bool=
  match projectee with | Rev16 (dst, src) -> true | uu___ -> false
let __proj__Rev16__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Rev16 (dst, src) -> dst
let __proj__Rev16__item__src (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Rev16 (dst, src) -> src
let uu___is_Rev32 (projectee : ins) : Prims.bool=
  match projectee with | Rev32 (dst, src) -> true | uu___ -> false
let __proj__Rev32__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Rev32 (dst, src) -> dst
let __proj__Rev32__item__src (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Rev32 (dst, src) -> src
let uu___is_Rev64 (projectee : ins) : Prims.bool=
  match projectee with | Rev64 (dst, src) -> true | uu___ -> false
let __proj__Rev64__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Rev64 (dst, src) -> dst
let __proj__Rev64__item__src (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Rev64 (dst, src) -> src
let uu___is_Csel (projectee : ins) : Prims.bool=
  match projectee with | Csel (dst, s1, s2, cond) -> true | uu___ -> false
let __proj__Csel__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Csel (dst, s1, s2, cond) -> dst
let __proj__Csel__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Csel (dst, s1, s2, cond) -> s1
let __proj__Csel__item__s2 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Csel (dst, s1, s2, cond) -> s2
let __proj__Csel__item__cond (projectee : ins) :
  Valerie_AArch64_Machine.cond_code=
  match projectee with | Csel (dst, s1, s2, cond) -> cond
let uu___is_Cset (projectee : ins) : Prims.bool=
  match projectee with | Cset (dst, cond) -> true | uu___ -> false
let __proj__Cset__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Cset (dst, cond) -> dst
let __proj__Cset__item__cond (projectee : ins) :
  Valerie_AArch64_Machine.cond_code=
  match projectee with | Cset (dst, cond) -> cond
let uu___is_Csetm (projectee : ins) : Prims.bool=
  match projectee with | Csetm (dst, cond) -> true | uu___ -> false
let __proj__Csetm__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Csetm (dst, cond) -> dst
let __proj__Csetm__item__cond (projectee : ins) :
  Valerie_AArch64_Machine.cond_code=
  match projectee with | Csetm (dst, cond) -> cond
let uu___is_Cinc (projectee : ins) : Prims.bool=
  match projectee with | Cinc (dst, s1, cond) -> true | uu___ -> false
let __proj__Cinc__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Cinc (dst, s1, cond) -> dst
let __proj__Cinc__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Cinc (dst, s1, cond) -> s1
let __proj__Cinc__item__cond (projectee : ins) :
  Valerie_AArch64_Machine.cond_code=
  match projectee with | Cinc (dst, s1, cond) -> cond
let uu___is_Cinv (projectee : ins) : Prims.bool=
  match projectee with | Cinv (dst, s1, cond) -> true | uu___ -> false
let __proj__Cinv__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Cinv (dst, s1, cond) -> dst
let __proj__Cinv__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Cinv (dst, s1, cond) -> s1
let __proj__Cinv__item__cond (projectee : ins) :
  Valerie_AArch64_Machine.cond_code=
  match projectee with | Cinv (dst, s1, cond) -> cond
let uu___is_Cneg (projectee : ins) : Prims.bool=
  match projectee with | Cneg (dst, s1, cond) -> true | uu___ -> false
let __proj__Cneg__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Cneg (dst, s1, cond) -> dst
let __proj__Cneg__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Cneg (dst, s1, cond) -> s1
let __proj__Cneg__item__cond (projectee : ins) :
  Valerie_AArch64_Machine.cond_code=
  match projectee with | Cneg (dst, s1, cond) -> cond
let uu___is_Csinc (projectee : ins) : Prims.bool=
  match projectee with | Csinc (dst, s1, s2, cond) -> true | uu___ -> false
let __proj__Csinc__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Csinc (dst, s1, s2, cond) -> dst
let __proj__Csinc__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Csinc (dst, s1, s2, cond) -> s1
let __proj__Csinc__item__s2 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Csinc (dst, s1, s2, cond) -> s2
let __proj__Csinc__item__cond (projectee : ins) :
  Valerie_AArch64_Machine.cond_code=
  match projectee with | Csinc (dst, s1, s2, cond) -> cond
let uu___is_Csinv (projectee : ins) : Prims.bool=
  match projectee with | Csinv (dst, s1, s2, cond) -> true | uu___ -> false
let __proj__Csinv__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Csinv (dst, s1, s2, cond) -> dst
let __proj__Csinv__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Csinv (dst, s1, s2, cond) -> s1
let __proj__Csinv__item__s2 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Csinv (dst, s1, s2, cond) -> s2
let __proj__Csinv__item__cond (projectee : ins) :
  Valerie_AArch64_Machine.cond_code=
  match projectee with | Csinv (dst, s1, s2, cond) -> cond
let uu___is_Csneg (projectee : ins) : Prims.bool=
  match projectee with | Csneg (dst, s1, s2, cond) -> true | uu___ -> false
let __proj__Csneg__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Csneg (dst, s1, s2, cond) -> dst
let __proj__Csneg__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Csneg (dst, s1, s2, cond) -> s1
let __proj__Csneg__item__s2 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Csneg (dst, s1, s2, cond) -> s2
let __proj__Csneg__item__cond (projectee : ins) :
  Valerie_AArch64_Machine.cond_code=
  match projectee with | Csneg (dst, s1, s2, cond) -> cond
let uu___is_Mov (projectee : ins) : Prims.bool=
  match projectee with | Mov (dst, src) -> true | uu___ -> false
let __proj__Mov__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Mov (dst, src) -> dst
let __proj__Mov__item__src (projectee : ins) : operand=
  match projectee with | Mov (dst, src) -> src
let uu___is_Movz (projectee : ins) : Prims.bool=
  match projectee with | Movz (dst, imm, shift) -> true | uu___ -> false
let __proj__Movz__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Movz (dst, imm, shift) -> dst
let __proj__Movz__item__imm (projectee : ins) : Valerie_Defs.nat64=
  match projectee with | Movz (dst, imm, shift) -> imm
let __proj__Movz__item__shift (projectee : ins) : Prims.nat=
  match projectee with | Movz (dst, imm, shift) -> shift
let uu___is_Movn (projectee : ins) : Prims.bool=
  match projectee with | Movn (dst, imm, shift) -> true | uu___ -> false
let __proj__Movn__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Movn (dst, imm, shift) -> dst
let __proj__Movn__item__imm (projectee : ins) : Valerie_Defs.nat64=
  match projectee with | Movn (dst, imm, shift) -> imm
let __proj__Movn__item__shift (projectee : ins) : Prims.nat=
  match projectee with | Movn (dst, imm, shift) -> shift
let uu___is_Movk (projectee : ins) : Prims.bool=
  match projectee with | Movk (dst, imm, shift) -> true | uu___ -> false
let __proj__Movk__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Movk (dst, imm, shift) -> dst
let __proj__Movk__item__imm (projectee : ins) : Valerie_Defs.nat64=
  match projectee with | Movk (dst, imm, shift) -> imm
let __proj__Movk__item__shift (projectee : ins) : Prims.nat=
  match projectee with | Movk (dst, imm, shift) -> shift
let uu___is_Adrp (projectee : ins) : Prims.bool=
  match projectee with | Adrp (dst, label) -> true | uu___ -> false
let __proj__Adrp__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Adrp (dst, label) -> dst
let __proj__Adrp__item__label (projectee : ins) : Prims.string=
  match projectee with | Adrp (dst, label) -> label
let uu___is_Ldr (projectee : ins) : Prims.bool=
  match projectee with | Ldr (dst, addr) -> true | uu___ -> false
let __proj__Ldr__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Ldr (dst, addr) -> dst
let __proj__Ldr__item__addr (projectee : ins) :
  Valerie_AArch64_Machine.maddr=
  match projectee with | Ldr (dst, addr) -> addr
let uu___is_Ldrb (projectee : ins) : Prims.bool=
  match projectee with | Ldrb (dst, addr) -> true | uu___ -> false
let __proj__Ldrb__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Ldrb (dst, addr) -> dst
let __proj__Ldrb__item__addr (projectee : ins) :
  Valerie_AArch64_Machine.maddr=
  match projectee with | Ldrb (dst, addr) -> addr
let uu___is_Ldrh (projectee : ins) : Prims.bool=
  match projectee with | Ldrh (dst, addr) -> true | uu___ -> false
let __proj__Ldrh__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Ldrh (dst, addr) -> dst
let __proj__Ldrh__item__addr (projectee : ins) :
  Valerie_AArch64_Machine.maddr=
  match projectee with | Ldrh (dst, addr) -> addr
let uu___is_Ldrsb (projectee : ins) : Prims.bool=
  match projectee with | Ldrsb (dst, addr) -> true | uu___ -> false
let __proj__Ldrsb__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Ldrsb (dst, addr) -> dst
let __proj__Ldrsb__item__addr (projectee : ins) :
  Valerie_AArch64_Machine.maddr=
  match projectee with | Ldrsb (dst, addr) -> addr
let uu___is_Ldrsh (projectee : ins) : Prims.bool=
  match projectee with | Ldrsh (dst, addr) -> true | uu___ -> false
let __proj__Ldrsh__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Ldrsh (dst, addr) -> dst
let __proj__Ldrsh__item__addr (projectee : ins) :
  Valerie_AArch64_Machine.maddr=
  match projectee with | Ldrsh (dst, addr) -> addr
let uu___is_Ldrsw (projectee : ins) : Prims.bool=
  match projectee with | Ldrsw (dst, addr) -> true | uu___ -> false
let __proj__Ldrsw__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Ldrsw (dst, addr) -> dst
let __proj__Ldrsw__item__addr (projectee : ins) :
  Valerie_AArch64_Machine.maddr=
  match projectee with | Ldrsw (dst, addr) -> addr
let uu___is_Str (projectee : ins) : Prims.bool=
  match projectee with | Str (src, addr) -> true | uu___ -> false
let __proj__Str__item__src (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Str (src, addr) -> src
let __proj__Str__item__addr (projectee : ins) :
  Valerie_AArch64_Machine.maddr=
  match projectee with | Str (src, addr) -> addr
let uu___is_Strb (projectee : ins) : Prims.bool=
  match projectee with | Strb (src, addr) -> true | uu___ -> false
let __proj__Strb__item__src (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Strb (src, addr) -> src
let __proj__Strb__item__addr (projectee : ins) :
  Valerie_AArch64_Machine.maddr=
  match projectee with | Strb (src, addr) -> addr
let uu___is_Strh (projectee : ins) : Prims.bool=
  match projectee with | Strh (src, addr) -> true | uu___ -> false
let __proj__Strh__item__src (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Strh (src, addr) -> src
let __proj__Strh__item__addr (projectee : ins) :
  Valerie_AArch64_Machine.maddr=
  match projectee with | Strh (src, addr) -> addr
let uu___is_Ldp (projectee : ins) : Prims.bool=
  match projectee with | Ldp (d1, d2, addr) -> true | uu___ -> false
let __proj__Ldp__item__d1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Ldp (d1, d2, addr) -> d1
let __proj__Ldp__item__d2 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Ldp (d1, d2, addr) -> d2
let __proj__Ldp__item__addr (projectee : ins) :
  Valerie_AArch64_Machine.maddr=
  match projectee with | Ldp (d1, d2, addr) -> addr
let uu___is_Stp (projectee : ins) : Prims.bool=
  match projectee with | Stp (s1, s2, addr) -> true | uu___ -> false
let __proj__Stp__item__s1 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Stp (s1, s2, addr) -> s1
let __proj__Stp__item__s2 (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Stp (s1, s2, addr) -> s2
let __proj__Stp__item__addr (projectee : ins) :
  Valerie_AArch64_Machine.maddr=
  match projectee with | Stp (s1, s2, addr) -> addr
let uu___is_B (projectee : ins) : Prims.bool=
  match projectee with | B label -> true | uu___ -> false
let __proj__B__item__label (projectee : ins) : Prims.string=
  match projectee with | B label -> label
let uu___is_BCond (projectee : ins) : Prims.bool=
  match projectee with | BCond (cond, label) -> true | uu___ -> false
let __proj__BCond__item__cond (projectee : ins) :
  Valerie_AArch64_Machine.cond_code=
  match projectee with | BCond (cond, label) -> cond
let __proj__BCond__item__label (projectee : ins) : Prims.string=
  match projectee with | BCond (cond, label) -> label
let uu___is_Bl (projectee : ins) : Prims.bool=
  match projectee with | Bl label -> true | uu___ -> false
let __proj__Bl__item__label (projectee : ins) : Prims.string=
  match projectee with | Bl label -> label
let uu___is_Blr (projectee : ins) : Prims.bool=
  match projectee with | Blr r -> true | uu___ -> false
let __proj__Blr__item__r (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Blr r -> r
let uu___is_Ret (projectee : ins) : Prims.bool=
  match projectee with | Ret -> true | uu___ -> false
let uu___is_Cbz (projectee : ins) : Prims.bool=
  match projectee with | Cbz (r, label) -> true | uu___ -> false
let __proj__Cbz__item__r (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Cbz (r, label) -> r
let __proj__Cbz__item__label (projectee : ins) : Prims.string=
  match projectee with | Cbz (r, label) -> label
let uu___is_Cbnz (projectee : ins) : Prims.bool=
  match projectee with | Cbnz (r, label) -> true | uu___ -> false
let __proj__Cbnz__item__r (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Cbnz (r, label) -> r
let __proj__Cbnz__item__label (projectee : ins) : Prims.string=
  match projectee with | Cbnz (r, label) -> label
let uu___is_FmovGprToVec (projectee : ins) : Prims.bool=
  match projectee with | FmovGprToVec (dst, src) -> true | uu___ -> false
let __proj__FmovGprToVec__item__dst (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | FmovGprToVec (dst, src) -> dst
let __proj__FmovGprToVec__item__src (projectee : ins) :
  Valerie_AArch64_Machine.reg=
  match projectee with | FmovGprToVec (dst, src) -> src
let uu___is_FmovVecToGpr (projectee : ins) : Prims.bool=
  match projectee with | FmovVecToGpr (dst, src) -> true | uu___ -> false
let __proj__FmovVecToGpr__item__dst (projectee : ins) :
  Valerie_AArch64_Machine.reg=
  match projectee with | FmovVecToGpr (dst, src) -> dst
let __proj__FmovVecToGpr__item__src (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | FmovVecToGpr (dst, src) -> src
let uu___is_Fadd (projectee : ins) : Prims.bool=
  match projectee with | Fadd (dst, s1, s2) -> true | uu___ -> false
let __proj__Fadd__item__dst (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Fadd (dst, s1, s2) -> dst
let __proj__Fadd__item__s1 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Fadd (dst, s1, s2) -> s1
let __proj__Fadd__item__s2 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Fadd (dst, s1, s2) -> s2
let uu___is_Fsub (projectee : ins) : Prims.bool=
  match projectee with | Fsub (dst, s1, s2) -> true | uu___ -> false
let __proj__Fsub__item__dst (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Fsub (dst, s1, s2) -> dst
let __proj__Fsub__item__s1 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Fsub (dst, s1, s2) -> s1
let __proj__Fsub__item__s2 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Fsub (dst, s1, s2) -> s2
let uu___is_Fmul (projectee : ins) : Prims.bool=
  match projectee with | Fmul (dst, s1, s2) -> true | uu___ -> false
let __proj__Fmul__item__dst (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Fmul (dst, s1, s2) -> dst
let __proj__Fmul__item__s1 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Fmul (dst, s1, s2) -> s1
let __proj__Fmul__item__s2 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Fmul (dst, s1, s2) -> s2
let uu___is_Fdiv (projectee : ins) : Prims.bool=
  match projectee with | Fdiv (dst, s1, s2) -> true | uu___ -> false
let __proj__Fdiv__item__dst (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Fdiv (dst, s1, s2) -> dst
let __proj__Fdiv__item__s1 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Fdiv (dst, s1, s2) -> s1
let __proj__Fdiv__item__s2 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Fdiv (dst, s1, s2) -> s2
let uu___is_Dup (projectee : ins) : Prims.bool=
  match projectee with | Dup (dst, src) -> true | uu___ -> false
let __proj__Dup__item__dst (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Dup (dst, src) -> dst
let __proj__Dup__item__src (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Dup (dst, src) -> src
let uu___is_InsLane (projectee : ins) : Prims.bool=
  match projectee with
  | InsLane (dst, dst_lane, src) -> true
  | uu___ -> false
let __proj__InsLane__item__dst (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | InsLane (dst, dst_lane, src) -> dst
let __proj__InsLane__item__dst_lane (projectee : ins) : Prims.nat=
  match projectee with | InsLane (dst, dst_lane, src) -> dst_lane
let __proj__InsLane__item__src (projectee : ins) :
  Valerie_AArch64_Machine.reg=
  match projectee with | InsLane (dst, dst_lane, src) -> src
let uu___is_Umov (projectee : ins) : Prims.bool=
  match projectee with | Umov (dst, src, src_lane) -> true | uu___ -> false
let __proj__Umov__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Umov (dst, src, src_lane) -> dst
let __proj__Umov__item__src (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Umov (dst, src, src_lane) -> src
let __proj__Umov__item__src_lane (projectee : ins) : Prims.nat=
  match projectee with | Umov (dst, src, src_lane) -> src_lane
let uu___is_Smov (projectee : ins) : Prims.bool=
  match projectee with | Smov (dst, src, src_lane) -> true | uu___ -> false
let __proj__Smov__item__dst (projectee : ins) : Valerie_AArch64_Machine.reg=
  match projectee with | Smov (dst, src, src_lane) -> dst
let __proj__Smov__item__src (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Smov (dst, src, src_lane) -> src
let __proj__Smov__item__src_lane (projectee : ins) : Prims.nat=
  match projectee with | Smov (dst, src, src_lane) -> src_lane
let uu___is_Vadd (projectee : ins) : Prims.bool=
  match projectee with | Vadd (dst, s1, s2, arr) -> true | uu___ -> false
let __proj__Vadd__item__dst (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Vadd (dst, s1, s2, arr) -> dst
let __proj__Vadd__item__s1 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Vadd (dst, s1, s2, arr) -> s1
let __proj__Vadd__item__s2 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Vadd (dst, s1, s2, arr) -> s2
let __proj__Vadd__item__arr (projectee : ins) :
  Valerie_AArch64_Machine.vec_arrangement=
  match projectee with | Vadd (dst, s1, s2, arr) -> arr
let uu___is_Vsub (projectee : ins) : Prims.bool=
  match projectee with | Vsub (dst, s1, s2, arr) -> true | uu___ -> false
let __proj__Vsub__item__dst (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Vsub (dst, s1, s2, arr) -> dst
let __proj__Vsub__item__s1 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Vsub (dst, s1, s2, arr) -> s1
let __proj__Vsub__item__s2 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Vsub (dst, s1, s2, arr) -> s2
let __proj__Vsub__item__arr (projectee : ins) :
  Valerie_AArch64_Machine.vec_arrangement=
  match projectee with | Vsub (dst, s1, s2, arr) -> arr
let uu___is_Vmul (projectee : ins) : Prims.bool=
  match projectee with | Vmul (dst, s1, s2, arr) -> true | uu___ -> false
let __proj__Vmul__item__dst (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Vmul (dst, s1, s2, arr) -> dst
let __proj__Vmul__item__s1 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Vmul (dst, s1, s2, arr) -> s1
let __proj__Vmul__item__s2 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Vmul (dst, s1, s2, arr) -> s2
let __proj__Vmul__item__arr (projectee : ins) :
  Valerie_AArch64_Machine.vec_arrangement=
  match projectee with | Vmul (dst, s1, s2, arr) -> arr
let uu___is_Vand (projectee : ins) : Prims.bool=
  match projectee with | Vand (dst, s1, s2) -> true | uu___ -> false
let __proj__Vand__item__dst (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Vand (dst, s1, s2) -> dst
let __proj__Vand__item__s1 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Vand (dst, s1, s2) -> s1
let __proj__Vand__item__s2 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Vand (dst, s1, s2) -> s2
let uu___is_Vorr (projectee : ins) : Prims.bool=
  match projectee with | Vorr (dst, s1, s2) -> true | uu___ -> false
let __proj__Vorr__item__dst (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Vorr (dst, s1, s2) -> dst
let __proj__Vorr__item__s1 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Vorr (dst, s1, s2) -> s1
let __proj__Vorr__item__s2 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Vorr (dst, s1, s2) -> s2
let uu___is_Veor (projectee : ins) : Prims.bool=
  match projectee with | Veor (dst, s1, s2) -> true | uu___ -> false
let __proj__Veor__item__dst (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Veor (dst, s1, s2) -> dst
let __proj__Veor__item__s1 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Veor (dst, s1, s2) -> s1
let __proj__Veor__item__s2 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Veor (dst, s1, s2) -> s2
let uu___is_Vext8 (projectee : ins) : Prims.bool=
  match projectee with | Vext8 (dst, s1, s2, index) -> true | uu___ -> false
let __proj__Vext8__item__dst (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Vext8 (dst, s1, s2, index) -> dst
let __proj__Vext8__item__s1 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Vext8 (dst, s1, s2, index) -> s1
let __proj__Vext8__item__s2 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Vext8 (dst, s1, s2, index) -> s2
let __proj__Vext8__item__index (projectee : ins) : Prims.nat=
  match projectee with | Vext8 (dst, s1, s2, index) -> index
let uu___is_Vrev64 (projectee : ins) : Prims.bool=
  match projectee with | Vrev64 (dst, src, arr) -> true | uu___ -> false
let __proj__Vrev64__item__dst (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | Vrev64 (dst, src, arr) -> dst
let __proj__Vrev64__item__src (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | Vrev64 (dst, src, arr) -> src
let __proj__Vrev64__item__arr (projectee : ins) :
  Valerie_AArch64_Machine.vec_arrangement=
  match projectee with | Vrev64 (dst, src, arr) -> arr
let uu___is_Vrev32 (projectee : ins) : Prims.bool=
  match projectee with | Vrev32 (dst, src, arr) -> true | uu___ -> false
let __proj__Vrev32__item__dst (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | Vrev32 (dst, src, arr) -> dst
let __proj__Vrev32__item__src (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | Vrev32 (dst, src, arr) -> src
let __proj__Vrev32__item__arr (projectee : ins) :
  Valerie_AArch64_Machine.vec_arrangement=
  match projectee with | Vrev32 (dst, src, arr) -> arr
let uu___is_Vrev16 (projectee : ins) : Prims.bool=
  match projectee with | Vrev16 (dst, src, arr) -> true | uu___ -> false
let __proj__Vrev16__item__dst (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | Vrev16 (dst, src, arr) -> dst
let __proj__Vrev16__item__src (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | Vrev16 (dst, src, arr) -> src
let __proj__Vrev16__item__arr (projectee : ins) :
  Valerie_AArch64_Machine.vec_arrangement=
  match projectee with | Vrev16 (dst, src, arr) -> arr
let uu___is_Pmull (projectee : ins) : Prims.bool=
  match projectee with | Pmull (dst, s1, s2) -> true | uu___ -> false
let __proj__Pmull__item__dst (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Pmull (dst, s1, s2) -> dst
let __proj__Pmull__item__s1 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Pmull (dst, s1, s2) -> s1
let __proj__Pmull__item__s2 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Pmull (dst, s1, s2) -> s2
let uu___is_Pmull2 (projectee : ins) : Prims.bool=
  match projectee with | Pmull2 (dst, s1, s2) -> true | uu___ -> false
let __proj__Pmull2__item__dst (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | Pmull2 (dst, s1, s2) -> dst
let __proj__Pmull2__item__s1 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Pmull2 (dst, s1, s2) -> s1
let __proj__Pmull2__item__s2 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Pmull2 (dst, s1, s2) -> s2
let uu___is_Aese (projectee : ins) : Prims.bool=
  match projectee with | Aese (dst, src) -> true | uu___ -> false
let __proj__Aese__item__dst (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Aese (dst, src) -> dst
let __proj__Aese__item__src (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Aese (dst, src) -> src
let uu___is_Aesd (projectee : ins) : Prims.bool=
  match projectee with | Aesd (dst, src) -> true | uu___ -> false
let __proj__Aesd__item__dst (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Aesd (dst, src) -> dst
let __proj__Aesd__item__src (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Aesd (dst, src) -> src
let uu___is_Aesmc (projectee : ins) : Prims.bool=
  match projectee with | Aesmc (dst, src) -> true | uu___ -> false
let __proj__Aesmc__item__dst (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Aesmc (dst, src) -> dst
let __proj__Aesmc__item__src (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Aesmc (dst, src) -> src
let uu___is_Aesimc (projectee : ins) : Prims.bool=
  match projectee with | Aesimc (dst, src) -> true | uu___ -> false
let __proj__Aesimc__item__dst (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | Aesimc (dst, src) -> dst
let __proj__Aesimc__item__src (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | Aesimc (dst, src) -> src
let uu___is_Sha1c (projectee : ins) : Prims.bool=
  match projectee with | Sha1c (dst, s1, s2) -> true | uu___ -> false
let __proj__Sha1c__item__dst (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Sha1c (dst, s1, s2) -> dst
let __proj__Sha1c__item__s1 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Sha1c (dst, s1, s2) -> s1
let __proj__Sha1c__item__s2 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Sha1c (dst, s1, s2) -> s2
let uu___is_Sha1p (projectee : ins) : Prims.bool=
  match projectee with | Sha1p (dst, s1, s2) -> true | uu___ -> false
let __proj__Sha1p__item__dst (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Sha1p (dst, s1, s2) -> dst
let __proj__Sha1p__item__s1 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Sha1p (dst, s1, s2) -> s1
let __proj__Sha1p__item__s2 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Sha1p (dst, s1, s2) -> s2
let uu___is_Sha1m (projectee : ins) : Prims.bool=
  match projectee with | Sha1m (dst, s1, s2) -> true | uu___ -> false
let __proj__Sha1m__item__dst (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Sha1m (dst, s1, s2) -> dst
let __proj__Sha1m__item__s1 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Sha1m (dst, s1, s2) -> s1
let __proj__Sha1m__item__s2 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Sha1m (dst, s1, s2) -> s2
let uu___is_Sha1h (projectee : ins) : Prims.bool=
  match projectee with | Sha1h (dst, src) -> true | uu___ -> false
let __proj__Sha1h__item__dst (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Sha1h (dst, src) -> dst
let __proj__Sha1h__item__src (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Sha1h (dst, src) -> src
let uu___is_Sha1su0 (projectee : ins) : Prims.bool=
  match projectee with | Sha1su0 (dst, s1, s2) -> true | uu___ -> false
let __proj__Sha1su0__item__dst (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | Sha1su0 (dst, s1, s2) -> dst
let __proj__Sha1su0__item__s1 (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | Sha1su0 (dst, s1, s2) -> s1
let __proj__Sha1su0__item__s2 (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | Sha1su0 (dst, s1, s2) -> s2
let uu___is_Sha1su1 (projectee : ins) : Prims.bool=
  match projectee with | Sha1su1 (dst, src) -> true | uu___ -> false
let __proj__Sha1su1__item__dst (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | Sha1su1 (dst, src) -> dst
let __proj__Sha1su1__item__src (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | Sha1su1 (dst, src) -> src
let uu___is_Sha256h (projectee : ins) : Prims.bool=
  match projectee with | Sha256h (dst, s1, s2) -> true | uu___ -> false
let __proj__Sha256h__item__dst (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | Sha256h (dst, s1, s2) -> dst
let __proj__Sha256h__item__s1 (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | Sha256h (dst, s1, s2) -> s1
let __proj__Sha256h__item__s2 (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | Sha256h (dst, s1, s2) -> s2
let uu___is_Sha256h2 (projectee : ins) : Prims.bool=
  match projectee with | Sha256h2 (dst, s1, s2) -> true | uu___ -> false
let __proj__Sha256h2__item__dst (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | Sha256h2 (dst, s1, s2) -> dst
let __proj__Sha256h2__item__s1 (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | Sha256h2 (dst, s1, s2) -> s1
let __proj__Sha256h2__item__s2 (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | Sha256h2 (dst, s1, s2) -> s2
let uu___is_Sha256su0 (projectee : ins) : Prims.bool=
  match projectee with | Sha256su0 (dst, src) -> true | uu___ -> false
let __proj__Sha256su0__item__dst (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | Sha256su0 (dst, src) -> dst
let __proj__Sha256su0__item__src (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | Sha256su0 (dst, src) -> src
let uu___is_Sha256su1 (projectee : ins) : Prims.bool=
  match projectee with | Sha256su1 (dst, s1, s2) -> true | uu___ -> false
let __proj__Sha256su1__item__dst (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | Sha256su1 (dst, s1, s2) -> dst
let __proj__Sha256su1__item__s1 (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | Sha256su1 (dst, s1, s2) -> s1
let __proj__Sha256su1__item__s2 (projectee : ins) :
  Valerie_AArch64_Machine.vec=
  match projectee with | Sha256su1 (dst, s1, s2) -> s2
let uu___is_Eor3 (projectee : ins) : Prims.bool=
  match projectee with | Eor3 (dst, s1, s2, s3) -> true | uu___ -> false
let __proj__Eor3__item__dst (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Eor3 (dst, s1, s2, s3) -> dst
let __proj__Eor3__item__s1 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Eor3 (dst, s1, s2, s3) -> s1
let __proj__Eor3__item__s2 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Eor3 (dst, s1, s2, s3) -> s2
let __proj__Eor3__item__s3 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Eor3 (dst, s1, s2, s3) -> s3
let uu___is_Rax1 (projectee : ins) : Prims.bool=
  match projectee with | Rax1 (dst, s1, s2) -> true | uu___ -> false
let __proj__Rax1__item__dst (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Rax1 (dst, s1, s2) -> dst
let __proj__Rax1__item__s1 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Rax1 (dst, s1, s2) -> s1
let __proj__Rax1__item__s2 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Rax1 (dst, s1, s2) -> s2
let uu___is_Xar (projectee : ins) : Prims.bool=
  match projectee with | Xar (dst, s1, s2, imm) -> true | uu___ -> false
let __proj__Xar__item__dst (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Xar (dst, s1, s2, imm) -> dst
let __proj__Xar__item__s1 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Xar (dst, s1, s2, imm) -> s1
let __proj__Xar__item__s2 (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Xar (dst, s1, s2, imm) -> s2
let __proj__Xar__item__imm (projectee : ins) : Prims.nat=
  match projectee with | Xar (dst, s1, s2, imm) -> imm
let uu___is_Ld1 (projectee : ins) : Prims.bool=
  match projectee with | Ld1 (v, arr, addr) -> true | uu___ -> false
let __proj__Ld1__item__v (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | Ld1 (v, arr, addr) -> v
let __proj__Ld1__item__arr (projectee : ins) :
  Valerie_AArch64_Machine.vec_arrangement=
  match projectee with | Ld1 (v, arr, addr) -> arr
let __proj__Ld1__item__addr (projectee : ins) :
  Valerie_AArch64_Machine.maddr=
  match projectee with | Ld1 (v, arr, addr) -> addr
let uu___is_St1 (projectee : ins) : Prims.bool=
  match projectee with | St1 (v, arr, addr) -> true | uu___ -> false
let __proj__St1__item__v (projectee : ins) : Valerie_AArch64_Machine.vec=
  match projectee with | St1 (v, arr, addr) -> v
let __proj__St1__item__arr (projectee : ins) :
  Valerie_AArch64_Machine.vec_arrangement=
  match projectee with | St1 (v, arr, addr) -> arr
let __proj__St1__item__addr (projectee : ins) :
  Valerie_AArch64_Machine.maddr=
  match projectee with | St1 (v, arr, addr) -> addr
let uu___is_MsrDit (projectee : ins) : Prims.bool=
  match projectee with | MsrDit enable -> true | uu___ -> false
let __proj__MsrDit__item__enable (projectee : ins) : Prims.bool=
  match projectee with | MsrDit enable -> enable
type ocmp =
  | OEq of operand * operand 
  | ONe of operand * operand 
  | OLt of operand * operand 
  | OGe of operand * operand 
let uu___is_OEq (projectee : ocmp) : Prims.bool=
  match projectee with | OEq (o1, o2) -> true | uu___ -> false
let __proj__OEq__item__o1 (projectee : ocmp) : operand=
  match projectee with | OEq (o1, o2) -> o1
let __proj__OEq__item__o2 (projectee : ocmp) : operand=
  match projectee with | OEq (o1, o2) -> o2
let uu___is_ONe (projectee : ocmp) : Prims.bool=
  match projectee with | ONe (o1, o2) -> true | uu___ -> false
let __proj__ONe__item__o1 (projectee : ocmp) : operand=
  match projectee with | ONe (o1, o2) -> o1
let __proj__ONe__item__o2 (projectee : ocmp) : operand=
  match projectee with | ONe (o1, o2) -> o2
let uu___is_OLt (projectee : ocmp) : Prims.bool=
  match projectee with | OLt (o1, o2) -> true | uu___ -> false
let __proj__OLt__item__o1 (projectee : ocmp) : operand=
  match projectee with | OLt (o1, o2) -> o1
let __proj__OLt__item__o2 (projectee : ocmp) : operand=
  match projectee with | OLt (o1, o2) -> o2
let uu___is_OGe (projectee : ocmp) : Prims.bool=
  match projectee with | OGe (o1, o2) -> true | uu___ -> false
let __proj__OGe__item__o1 (projectee : ocmp) : operand=
  match projectee with | OGe (o1, o2) -> o1
let __proj__OGe__item__o2 (projectee : ocmp) : operand=
  match projectee with | OGe (o1, o2) -> o2
type code =
  | Ins of ins 
  | Block of code Prims.list 
  | IfElse of ocmp * code * code 
  | While of ocmp * code 
let uu___is_Ins (projectee : code) : Prims.bool=
  match projectee with | Ins ins1 -> true | uu___ -> false
let __proj__Ins__item__ins (projectee : code) : ins=
  match projectee with | Ins ins1 -> ins1
let uu___is_Block (projectee : code) : Prims.bool=
  match projectee with | Block block -> true | uu___ -> false
let __proj__Block__item__block (projectee : code) : code Prims.list=
  match projectee with | Block block -> block
let uu___is_IfElse (projectee : code) : Prims.bool=
  match projectee with
  | IfElse (ifCond, ifTrue, ifFalse) -> true
  | uu___ -> false
let __proj__IfElse__item__ifCond (projectee : code) : ocmp=
  match projectee with | IfElse (ifCond, ifTrue, ifFalse) -> ifCond
let __proj__IfElse__item__ifTrue (projectee : code) : code=
  match projectee with | IfElse (ifCond, ifTrue, ifFalse) -> ifTrue
let __proj__IfElse__item__ifFalse (projectee : code) : code=
  match projectee with | IfElse (ifCond, ifTrue, ifFalse) -> ifFalse
let uu___is_While (projectee : code) : Prims.bool=
  match projectee with
  | While (whileCond, whileBody) -> true
  | uu___ -> false
let __proj__While__item__whileCond (projectee : code) : ocmp=
  match projectee with | While (whileCond, whileBody) -> whileCond
let __proj__While__item__whileBody (projectee : code) : code=
  match projectee with | While (whileCond, whileBody) -> whileBody
type codes = code Prims.list
let eval_reg (r : Valerie_AArch64_Machine.reg)
  (s : Valerie_AArch64_Machine.state) : Valerie_Defs.nat64=
  s.Valerie_AArch64_Machine.regs r
let eval_vec (v : Valerie_AArch64_Machine.vec)
  (s : Valerie_AArch64_Machine.state) : Valerie_Defs.nat128=
  s.Valerie_AArch64_Machine.vecs v
let eval_maddr (m : Valerie_AArch64_Machine.maddr)
  (s : Valerie_AArch64_Machine.state) : Prims.int=
  match m with
  | Valerie_AArch64_Machine.MReg base -> FStar_UInt64.v (eval_reg base s)
  | Valerie_AArch64_Machine.MOffset (base, offset) ->
      (FStar_UInt64.v (eval_reg base s)) + offset
  | Valerie_AArch64_Machine.MPreIndex (base, offset) ->
      (FStar_UInt64.v (eval_reg base s)) + offset
  | Valerie_AArch64_Machine.MPostIndex (base, uu___) ->
      FStar_UInt64.v (eval_reg base s)
  | Valerie_AArch64_Machine.MRegOffset (base, index, uu___, shift) ->
      (FStar_UInt64.v (eval_reg base s)) +
        ((FStar_UInt64.v (eval_reg index s)) *
           ((match shift with
             | uu___1 when uu___1 = Prims.int_zero -> Prims.int_one
             | uu___1 when uu___1 = Prims.int_one -> Prims.of_int 2
             | uu___1 when uu___1 = (Prims.of_int 2) -> Prims.of_int 4
             | uu___1 when uu___1 = (Prims.of_int 3) -> Prims.of_int 8
             | uu___1 -> Prims.int_one)))
let update_reg (r : Valerie_AArch64_Machine.reg) (v : Valerie_Defs.nat64)
  (s : Valerie_AArch64_Machine.state) : Valerie_AArch64_Machine.state=
  {
    Valerie_AArch64_Machine.ok = (s.Valerie_AArch64_Machine.ok);
    Valerie_AArch64_Machine.regs =
      (fun r' -> if r' = r then v else s.Valerie_AArch64_Machine.regs r');
    Valerie_AArch64_Machine.vecs = (s.Valerie_AArch64_Machine.vecs);
    Valerie_AArch64_Machine.flags = (s.Valerie_AArch64_Machine.flags)
  }
let update_vec (vr : Valerie_AArch64_Machine.vec) (v : Valerie_Defs.nat128)
  (s : Valerie_AArch64_Machine.state) : Valerie_AArch64_Machine.state=
  {
    Valerie_AArch64_Machine.ok = (s.Valerie_AArch64_Machine.ok);
    Valerie_AArch64_Machine.regs = (s.Valerie_AArch64_Machine.regs);
    Valerie_AArch64_Machine.vecs =
      (fun vr' -> if vr' = vr then v else s.Valerie_AArch64_Machine.vecs vr');
    Valerie_AArch64_Machine.flags = (s.Valerie_AArch64_Machine.flags)
  }
let set_ok (ok : Prims.bool) (s : Valerie_AArch64_Machine.state) :
  Valerie_AArch64_Machine.state=
  {
    Valerie_AArch64_Machine.ok = ok;
    Valerie_AArch64_Machine.regs = (s.Valerie_AArch64_Machine.regs);
    Valerie_AArch64_Machine.vecs = (s.Valerie_AArch64_Machine.vecs);
    Valerie_AArch64_Machine.flags = (s.Valerie_AArch64_Machine.flags)
  }
let eval_operand (o : operand) (s : Valerie_AArch64_Machine.state) :
  Valerie_Defs.nat64=
  match o with
  | OReg (Valerie_AArch64_Machine.XZR, uu___) -> Stdint.Uint64.zero
  | OReg (r, uu___) -> s.Valerie_AArch64_Machine.regs r
  | OImm imm -> imm
  | uu___ -> Stdint.Uint64.zero
let eval_ocmp (c : ocmp) (s : Valerie_AArch64_Machine.state) : Prims.bool=
  match c with
  | OEq (o1, o2) -> (eval_operand o1 s) = (eval_operand o2 s)
  | ONe (o1, o2) -> (eval_operand o1 s) <> (eval_operand o2 s)
  | OLt (o1, o2) -> FStar_UInt64.lt (eval_operand o1 s) (eval_operand o2 s)
  | OGe (o1, o2) -> FStar_UInt64.gte (eval_operand o1 s) (eval_operand o2 s)
let eval_ins (i : ins) (s : Valerie_AArch64_Machine.state) :
  Valerie_AArch64_Machine.state=
  match i with
  | Mov (dst, OReg (Valerie_AArch64_Machine.XZR, uu___)) ->
      {
        Valerie_AArch64_Machine.ok = (s.Valerie_AArch64_Machine.ok);
        Valerie_AArch64_Machine.regs =
          ((fun r ->
              if r = dst
              then Stdint.Uint64.zero
              else s.Valerie_AArch64_Machine.regs r));
        Valerie_AArch64_Machine.vecs = (s.Valerie_AArch64_Machine.vecs);
        Valerie_AArch64_Machine.flags = (s.Valerie_AArch64_Machine.flags)
      }
  | Mov (dst, src) ->
      let v = eval_operand src s in
      {
        Valerie_AArch64_Machine.ok = (s.Valerie_AArch64_Machine.ok);
        Valerie_AArch64_Machine.regs =
          ((fun r -> if r = dst then v else s.Valerie_AArch64_Machine.regs r));
        Valerie_AArch64_Machine.vecs = (s.Valerie_AArch64_Machine.vecs);
        Valerie_AArch64_Machine.flags = (s.Valerie_AArch64_Machine.flags)
      }
  | Add (dst, s1, s2) ->
      let v1 = s.Valerie_AArch64_Machine.regs s1 in
      let v2 = eval_operand s2 s in
      let res = FStar_UInt64.add_mod v1 v2 in
      {
        Valerie_AArch64_Machine.ok = (s.Valerie_AArch64_Machine.ok);
        Valerie_AArch64_Machine.regs =
          ((fun r ->
              if r = dst then res else s.Valerie_AArch64_Machine.regs r));
        Valerie_AArch64_Machine.vecs = (s.Valerie_AArch64_Machine.vecs);
        Valerie_AArch64_Machine.flags = (s.Valerie_AArch64_Machine.flags)
      }
  | Sub (dst, s1, s2) ->
      let v1 = s.Valerie_AArch64_Machine.regs s1 in
      let v2 = eval_operand s2 s in
      let res = FStar_UInt64.sub_mod v1 v2 in
      {
        Valerie_AArch64_Machine.ok = (s.Valerie_AArch64_Machine.ok);
        Valerie_AArch64_Machine.regs =
          ((fun r ->
              if r = dst then res else s.Valerie_AArch64_Machine.regs r));
        Valerie_AArch64_Machine.vecs = (s.Valerie_AArch64_Machine.vecs);
        Valerie_AArch64_Machine.flags = (s.Valerie_AArch64_Machine.flags)
      }
  | Eor (dst, s1, s2) ->
      let v1 = s.Valerie_AArch64_Machine.regs s1 in
      let v2 = eval_operand s2 s in
      let res = FStar_UInt64.logxor v1 v2 in
      {
        Valerie_AArch64_Machine.ok = (s.Valerie_AArch64_Machine.ok);
        Valerie_AArch64_Machine.regs =
          ((fun r ->
              if r = dst then res else s.Valerie_AArch64_Machine.regs r));
        Valerie_AArch64_Machine.vecs = (s.Valerie_AArch64_Machine.vecs);
        Valerie_AArch64_Machine.flags = (s.Valerie_AArch64_Machine.flags)
      }
  | And (dst, s1, s2) ->
      let v1 = s.Valerie_AArch64_Machine.regs s1 in
      let v2 = eval_operand s2 s in
      let res = FStar_UInt64.logand v1 v2 in
      {
        Valerie_AArch64_Machine.ok = (s.Valerie_AArch64_Machine.ok);
        Valerie_AArch64_Machine.regs =
          ((fun r ->
              if r = dst then res else s.Valerie_AArch64_Machine.regs r));
        Valerie_AArch64_Machine.vecs = (s.Valerie_AArch64_Machine.vecs);
        Valerie_AArch64_Machine.flags = (s.Valerie_AArch64_Machine.flags)
      }
  | Orr (dst, s1, s2) ->
      let v1 = s.Valerie_AArch64_Machine.regs s1 in
      let v2 = eval_operand s2 s in
      let res = FStar_UInt64.logor v1 v2 in
      {
        Valerie_AArch64_Machine.ok = (s.Valerie_AArch64_Machine.ok);
        Valerie_AArch64_Machine.regs =
          ((fun r ->
              if r = dst then res else s.Valerie_AArch64_Machine.regs r));
        Valerie_AArch64_Machine.vecs = (s.Valerie_AArch64_Machine.vecs);
        Valerie_AArch64_Machine.flags = (s.Valerie_AArch64_Machine.flags)
      }
  | Rev64 (dst, src) ->
      let v = s.Valerie_AArch64_Machine.regs src in
      {
        Valerie_AArch64_Machine.ok = (s.Valerie_AArch64_Machine.ok);
        Valerie_AArch64_Machine.regs =
          ((fun r -> if r = dst then v else s.Valerie_AArch64_Machine.regs r));
        Valerie_AArch64_Machine.vecs = (s.Valerie_AArch64_Machine.vecs);
        Valerie_AArch64_Machine.flags = (s.Valerie_AArch64_Machine.flags)
      }
  | FmovVecToGpr (dst, uu___) ->
      {
        Valerie_AArch64_Machine.ok = (s.Valerie_AArch64_Machine.ok);
        Valerie_AArch64_Machine.regs =
          ((fun r ->
              if r = dst
              then Stdint.Uint64.zero
              else s.Valerie_AArch64_Machine.regs r));
        Valerie_AArch64_Machine.vecs = (s.Valerie_AArch64_Machine.vecs);
        Valerie_AArch64_Machine.flags = (s.Valerie_AArch64_Machine.flags)
      }
  | Umov (dst, uu___, uu___1) ->
      {
        Valerie_AArch64_Machine.ok = (s.Valerie_AArch64_Machine.ok);
        Valerie_AArch64_Machine.regs =
          ((fun r ->
              if r = dst
              then Stdint.Uint64.zero
              else s.Valerie_AArch64_Machine.regs r));
        Valerie_AArch64_Machine.vecs = (s.Valerie_AArch64_Machine.vecs);
        Valerie_AArch64_Machine.flags = (s.Valerie_AArch64_Machine.flags)
      }
  | Veor (dst, s1, s2) ->
      let v1 = s.Valerie_AArch64_Machine.vecs s1 in
      let v2 = s.Valerie_AArch64_Machine.vecs s2 in
      let res = FStar_UInt128.logxor v1 v2 in
      {
        Valerie_AArch64_Machine.ok = (s.Valerie_AArch64_Machine.ok);
        Valerie_AArch64_Machine.regs = (s.Valerie_AArch64_Machine.regs);
        Valerie_AArch64_Machine.vecs =
          ((fun v ->
              if v = dst then res else s.Valerie_AArch64_Machine.vecs v));
        Valerie_AArch64_Machine.flags = (s.Valerie_AArch64_Machine.flags)
      }
  | MsrDit enable ->
      {
        Valerie_AArch64_Machine.ok = (s.Valerie_AArch64_Machine.ok);
        Valerie_AArch64_Machine.regs = (s.Valerie_AArch64_Machine.regs);
        Valerie_AArch64_Machine.vecs = (s.Valerie_AArch64_Machine.vecs);
        Valerie_AArch64_Machine.flags =
          (let uu___ = s.Valerie_AArch64_Machine.flags in
           {
             Valerie_AArch64_Machine.n = (uu___.Valerie_AArch64_Machine.n);
             Valerie_AArch64_Machine.z = (uu___.Valerie_AArch64_Machine.z);
             Valerie_AArch64_Machine.c = (uu___.Valerie_AArch64_Machine.c);
             Valerie_AArch64_Machine.v = (uu___.Valerie_AArch64_Machine.v);
             Valerie_AArch64_Machine.dit = enable
           })
      }
  | uu___ -> s
let rec eval_code (c : code) (fuel : Prims.nat)
  (s : Valerie_AArch64_Machine.state) :
  Valerie_AArch64_Machine.state FStar_Pervasives_Native.option=
  if fuel = Prims.int_zero
  then FStar_Pervasives_Native.None
  else
    (match c with
     | Ins i -> FStar_Pervasives_Native.Some (eval_ins i s)
     | Block [] -> FStar_Pervasives_Native.Some s
     | Block (c1::rest) ->
         (match eval_code c1 (fuel - Prims.int_one) s with
          | FStar_Pervasives_Native.Some s' ->
              eval_code (Block rest) (fuel - Prims.int_one) s'
          | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None)
     | IfElse (cond, ifTrue, ifFalse) ->
         if eval_ocmp cond s
         then eval_code ifTrue (fuel - Prims.int_one) s
         else eval_code ifFalse (fuel - Prims.int_one) s
     | While (cond, body) ->
         if eval_ocmp cond s
         then
           (match eval_code body (fuel - Prims.int_one) s with
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
            | FStar_Pervasives_Native.Some s' ->
                eval_code (While (cond, body)) (fuel - Prims.int_one) s')
         else FStar_Pervasives_Native.Some s)
let is_constant_time_ins (i : ins) (dit : Prims.bool) : Prims.bool=
  match i with
  | Mov (uu___, uu___1) -> true
  | Add (uu___, uu___1, uu___2) -> true
  | Sub (uu___, uu___1, uu___2) -> true
  | Eor (uu___, uu___1, uu___2) -> true
  | And (uu___, uu___1, uu___2) -> true
  | Rev64 (uu___, uu___1) -> true
  | FmovGprToVec (uu___, uu___1) -> true
  | FmovVecToGpr (uu___, uu___1) -> true
  | Umov (uu___, uu___1, uu___2) -> true
  | Pmull (uu___, uu___1, uu___2) -> true
  | Pmull2 (uu___, uu___1, uu___2) -> true
  | Veor (uu___, uu___1, uu___2) -> true
  | Vext8 (uu___, uu___1, uu___2, uu___3) -> true
  | uu___ -> dit
