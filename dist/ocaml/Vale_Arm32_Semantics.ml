open Prims
type operand =
  | OReg of Vale_Arm32_Machine.reg 
  | OSreg of Vale_Arm32_Machine.sreg 
  | ODreg of Vale_Arm32_Machine.dreg 
  | OQreg of Vale_Arm32_Machine.qreg 
  | OImm of Vale_Defs.nat32 
  | OMem of Vale_Arm32_Machine.maddr 
  | ORegList of Vale_Arm32_Machine.reg Prims.list 
let uu___is_OReg (projectee : operand) : Prims.bool=
  match projectee with | OReg r -> true | uu___ -> false
let __proj__OReg__item__r (projectee : operand) : Vale_Arm32_Machine.reg=
  match projectee with | OReg r -> r
let uu___is_OSreg (projectee : operand) : Prims.bool=
  match projectee with | OSreg s -> true | uu___ -> false
let __proj__OSreg__item__s (projectee : operand) : Vale_Arm32_Machine.sreg=
  match projectee with | OSreg s -> s
let uu___is_ODreg (projectee : operand) : Prims.bool=
  match projectee with | ODreg d -> true | uu___ -> false
let __proj__ODreg__item__d (projectee : operand) : Vale_Arm32_Machine.dreg=
  match projectee with | ODreg d -> d
let uu___is_OQreg (projectee : operand) : Prims.bool=
  match projectee with | OQreg q -> true | uu___ -> false
let __proj__OQreg__item__q (projectee : operand) : Vale_Arm32_Machine.qreg=
  match projectee with | OQreg q -> q
let uu___is_OImm (projectee : operand) : Prims.bool=
  match projectee with | OImm imm -> true | uu___ -> false
let __proj__OImm__item__imm (projectee : operand) : Vale_Defs.nat32=
  match projectee with | OImm imm -> imm
let uu___is_OMem (projectee : operand) : Prims.bool=
  match projectee with | OMem m -> true | uu___ -> false
let __proj__OMem__item__m (projectee : operand) : Vale_Arm32_Machine.maddr=
  match projectee with | OMem m -> m
let uu___is_ORegList (projectee : operand) : Prims.bool=
  match projectee with | ORegList regs -> true | uu___ -> false
let __proj__ORegList__item__regs (projectee : operand) :
  Vale_Arm32_Machine.reg Prims.list=
  match projectee with | ORegList regs -> regs
type ins =
  | Mov of Vale_Arm32_Machine.reg * operand 
  | Movs of Vale_Arm32_Machine.reg * operand 
  | Movw of Vale_Arm32_Machine.reg * Vale_Defs.nat32 
  | Movt of Vale_Arm32_Machine.reg * Vale_Defs.nat32 
  | Add of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * operand 
  | Adds of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * operand 
  | Adc of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * operand 
  | Adcs of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * operand 
  | Sub of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * operand 
  | Subs of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * operand 
  | Sbc of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * operand 
  | Sbcs of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * operand 
  | Rsb of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * operand 
  | Rsbs of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * operand 
  | Rsc of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * operand 
  | Rscs of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * operand 
  | Mul of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg *
  Vale_Arm32_Machine.reg 
  | Mla of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg *
  Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg 
  | Mls of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg *
  Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg 
  | Smull of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg *
  Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg 
  | Umull of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg *
  Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg 
  | Sdiv of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg *
  Vale_Arm32_Machine.reg 
  | Udiv of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg *
  Vale_Arm32_Machine.reg 
  | And of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * operand 
  | Ands of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * operand 
  | Orr of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * operand 
  | Orrs of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * operand 
  | Eor of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * operand 
  | Eors of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * operand 
  | Bic of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * operand 
  | Bics of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * operand 
  | Orn of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * operand 
  | Mvn of Vale_Arm32_Machine.reg * operand 
  | Mvns of Vale_Arm32_Machine.reg * operand 
  | Lsl of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * operand 
  | Lsr of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * operand 
  | Asr of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * operand 
  | Ror of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * operand 
  | Clz of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg 
  | Rbit of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg 
  | Rev of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg 
  | Rev16 of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg 
  | Revsh of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg 
  | Ubfx of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * Vale_Defs.nat32
  * Vale_Defs.nat32 
  | Sbfx of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg * Vale_Defs.nat32
  * Vale_Defs.nat32 
  | Cmp of Vale_Arm32_Machine.reg * operand 
  | Cmn of Vale_Arm32_Machine.reg * operand 
  | Tst of Vale_Arm32_Machine.reg * operand 
  | Teq of Vale_Arm32_Machine.reg * operand 
  | Sxtb of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg 
  | Sxth of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg 
  | Uxtb of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg 
  | Uxth of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg 
  | Ldr of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.maddr 
  | Ldrb of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.maddr 
  | Ldrh of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.maddr 
  | Ldrsb of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.maddr 
  | Ldrsh of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.maddr 
  | Ldrd of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg *
  Vale_Arm32_Machine.maddr 
  | Str of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.maddr 
  | Strb of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.maddr 
  | Strh of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.maddr 
  | Strd of Vale_Arm32_Machine.reg * Vale_Arm32_Machine.reg *
  Vale_Arm32_Machine.maddr 
  | Push of Vale_Arm32_Machine.reg Prims.list 
  | Pop of Vale_Arm32_Machine.reg Prims.list 
  | B of Prims.string 
  | Bc of Vale_Arm32_Machine.cond_code * Prims.string 
  | Bl of Prims.string 
  | Bx of Vale_Arm32_Machine.reg 
  | Blx of Vale_Arm32_Machine.reg 
  | Vmov of operand * operand 
  | VaddF32 of Vale_Arm32_Machine.sreg * Vale_Arm32_Machine.sreg *
  Vale_Arm32_Machine.sreg 
  | VaddF64 of Vale_Arm32_Machine.dreg * Vale_Arm32_Machine.dreg *
  Vale_Arm32_Machine.dreg 
  | VsubF32 of Vale_Arm32_Machine.sreg * Vale_Arm32_Machine.sreg *
  Vale_Arm32_Machine.sreg 
  | VsubF64 of Vale_Arm32_Machine.dreg * Vale_Arm32_Machine.dreg *
  Vale_Arm32_Machine.dreg 
  | VmulF32 of Vale_Arm32_Machine.sreg * Vale_Arm32_Machine.sreg *
  Vale_Arm32_Machine.sreg 
  | VmulF64 of Vale_Arm32_Machine.dreg * Vale_Arm32_Machine.dreg *
  Vale_Arm32_Machine.dreg 
  | Vldr of Vale_Arm32_Machine.sreg * Vale_Arm32_Machine.maddr 
  | Vstr of Vale_Arm32_Machine.sreg * Vale_Arm32_Machine.maddr 
  | Vpush of Vale_Arm32_Machine.sreg Prims.list 
  | Vpop of Vale_Arm32_Machine.sreg Prims.list 
let uu___is_Mov (projectee : ins) : Prims.bool=
  match projectee with | Mov (dst, src) -> true | uu___ -> false
let __proj__Mov__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Mov (dst, src) -> dst
let __proj__Mov__item__src (projectee : ins) : operand=
  match projectee with | Mov (dst, src) -> src
let uu___is_Movs (projectee : ins) : Prims.bool=
  match projectee with | Movs (dst, src) -> true | uu___ -> false
let __proj__Movs__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Movs (dst, src) -> dst
let __proj__Movs__item__src (projectee : ins) : operand=
  match projectee with | Movs (dst, src) -> src
let uu___is_Movw (projectee : ins) : Prims.bool=
  match projectee with | Movw (dst, imm) -> true | uu___ -> false
let __proj__Movw__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Movw (dst, imm) -> dst
let __proj__Movw__item__imm (projectee : ins) : Vale_Defs.nat32=
  match projectee with | Movw (dst, imm) -> imm
let uu___is_Movt (projectee : ins) : Prims.bool=
  match projectee with | Movt (dst, imm) -> true | uu___ -> false
let __proj__Movt__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Movt (dst, imm) -> dst
let __proj__Movt__item__imm (projectee : ins) : Vale_Defs.nat32=
  match projectee with | Movt (dst, imm) -> imm
let uu___is_Add (projectee : ins) : Prims.bool=
  match projectee with | Add (dst, s1, s2) -> true | uu___ -> false
let __proj__Add__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Add (dst, s1, s2) -> dst
let __proj__Add__item__s1 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Add (dst, s1, s2) -> s1
let __proj__Add__item__s2 (projectee : ins) : operand=
  match projectee with | Add (dst, s1, s2) -> s2
let uu___is_Adds (projectee : ins) : Prims.bool=
  match projectee with | Adds (dst, s1, s2) -> true | uu___ -> false
let __proj__Adds__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Adds (dst, s1, s2) -> dst
let __proj__Adds__item__s1 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Adds (dst, s1, s2) -> s1
let __proj__Adds__item__s2 (projectee : ins) : operand=
  match projectee with | Adds (dst, s1, s2) -> s2
let uu___is_Adc (projectee : ins) : Prims.bool=
  match projectee with | Adc (dst, s1, s2) -> true | uu___ -> false
let __proj__Adc__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Adc (dst, s1, s2) -> dst
let __proj__Adc__item__s1 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Adc (dst, s1, s2) -> s1
let __proj__Adc__item__s2 (projectee : ins) : operand=
  match projectee with | Adc (dst, s1, s2) -> s2
let uu___is_Adcs (projectee : ins) : Prims.bool=
  match projectee with | Adcs (dst, s1, s2) -> true | uu___ -> false
let __proj__Adcs__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Adcs (dst, s1, s2) -> dst
let __proj__Adcs__item__s1 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Adcs (dst, s1, s2) -> s1
let __proj__Adcs__item__s2 (projectee : ins) : operand=
  match projectee with | Adcs (dst, s1, s2) -> s2
let uu___is_Sub (projectee : ins) : Prims.bool=
  match projectee with | Sub (dst, s1, s2) -> true | uu___ -> false
let __proj__Sub__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Sub (dst, s1, s2) -> dst
let __proj__Sub__item__s1 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Sub (dst, s1, s2) -> s1
let __proj__Sub__item__s2 (projectee : ins) : operand=
  match projectee with | Sub (dst, s1, s2) -> s2
let uu___is_Subs (projectee : ins) : Prims.bool=
  match projectee with | Subs (dst, s1, s2) -> true | uu___ -> false
let __proj__Subs__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Subs (dst, s1, s2) -> dst
let __proj__Subs__item__s1 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Subs (dst, s1, s2) -> s1
let __proj__Subs__item__s2 (projectee : ins) : operand=
  match projectee with | Subs (dst, s1, s2) -> s2
let uu___is_Sbc (projectee : ins) : Prims.bool=
  match projectee with | Sbc (dst, s1, s2) -> true | uu___ -> false
let __proj__Sbc__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Sbc (dst, s1, s2) -> dst
let __proj__Sbc__item__s1 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Sbc (dst, s1, s2) -> s1
let __proj__Sbc__item__s2 (projectee : ins) : operand=
  match projectee with | Sbc (dst, s1, s2) -> s2
let uu___is_Sbcs (projectee : ins) : Prims.bool=
  match projectee with | Sbcs (dst, s1, s2) -> true | uu___ -> false
let __proj__Sbcs__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Sbcs (dst, s1, s2) -> dst
let __proj__Sbcs__item__s1 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Sbcs (dst, s1, s2) -> s1
let __proj__Sbcs__item__s2 (projectee : ins) : operand=
  match projectee with | Sbcs (dst, s1, s2) -> s2
let uu___is_Rsb (projectee : ins) : Prims.bool=
  match projectee with | Rsb (dst, s1, s2) -> true | uu___ -> false
let __proj__Rsb__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Rsb (dst, s1, s2) -> dst
let __proj__Rsb__item__s1 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Rsb (dst, s1, s2) -> s1
let __proj__Rsb__item__s2 (projectee : ins) : operand=
  match projectee with | Rsb (dst, s1, s2) -> s2
let uu___is_Rsbs (projectee : ins) : Prims.bool=
  match projectee with | Rsbs (dst, s1, s2) -> true | uu___ -> false
let __proj__Rsbs__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Rsbs (dst, s1, s2) -> dst
let __proj__Rsbs__item__s1 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Rsbs (dst, s1, s2) -> s1
let __proj__Rsbs__item__s2 (projectee : ins) : operand=
  match projectee with | Rsbs (dst, s1, s2) -> s2
let uu___is_Rsc (projectee : ins) : Prims.bool=
  match projectee with | Rsc (dst, s1, s2) -> true | uu___ -> false
let __proj__Rsc__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Rsc (dst, s1, s2) -> dst
let __proj__Rsc__item__s1 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Rsc (dst, s1, s2) -> s1
let __proj__Rsc__item__s2 (projectee : ins) : operand=
  match projectee with | Rsc (dst, s1, s2) -> s2
let uu___is_Rscs (projectee : ins) : Prims.bool=
  match projectee with | Rscs (dst, s1, s2) -> true | uu___ -> false
let __proj__Rscs__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Rscs (dst, s1, s2) -> dst
let __proj__Rscs__item__s1 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Rscs (dst, s1, s2) -> s1
let __proj__Rscs__item__s2 (projectee : ins) : operand=
  match projectee with | Rscs (dst, s1, s2) -> s2
let uu___is_Mul (projectee : ins) : Prims.bool=
  match projectee with | Mul (dst, s1, s2) -> true | uu___ -> false
let __proj__Mul__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Mul (dst, s1, s2) -> dst
let __proj__Mul__item__s1 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Mul (dst, s1, s2) -> s1
let __proj__Mul__item__s2 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Mul (dst, s1, s2) -> s2
let uu___is_Mla (projectee : ins) : Prims.bool=
  match projectee with | Mla (dst, s1, s2, ra) -> true | uu___ -> false
let __proj__Mla__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Mla (dst, s1, s2, ra) -> dst
let __proj__Mla__item__s1 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Mla (dst, s1, s2, ra) -> s1
let __proj__Mla__item__s2 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Mla (dst, s1, s2, ra) -> s2
let __proj__Mla__item__ra (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Mla (dst, s1, s2, ra) -> ra
let uu___is_Mls (projectee : ins) : Prims.bool=
  match projectee with | Mls (dst, s1, s2, ra) -> true | uu___ -> false
let __proj__Mls__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Mls (dst, s1, s2, ra) -> dst
let __proj__Mls__item__s1 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Mls (dst, s1, s2, ra) -> s1
let __proj__Mls__item__s2 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Mls (dst, s1, s2, ra) -> s2
let __proj__Mls__item__ra (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Mls (dst, s1, s2, ra) -> ra
let uu___is_Smull (projectee : ins) : Prims.bool=
  match projectee with | Smull (rdlo, rdhi, rn, rm) -> true | uu___ -> false
let __proj__Smull__item__rdlo (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Smull (rdlo, rdhi, rn, rm) -> rdlo
let __proj__Smull__item__rdhi (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Smull (rdlo, rdhi, rn, rm) -> rdhi
let __proj__Smull__item__rn (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Smull (rdlo, rdhi, rn, rm) -> rn
let __proj__Smull__item__rm (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Smull (rdlo, rdhi, rn, rm) -> rm
let uu___is_Umull (projectee : ins) : Prims.bool=
  match projectee with | Umull (rdlo, rdhi, rn, rm) -> true | uu___ -> false
let __proj__Umull__item__rdlo (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Umull (rdlo, rdhi, rn, rm) -> rdlo
let __proj__Umull__item__rdhi (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Umull (rdlo, rdhi, rn, rm) -> rdhi
let __proj__Umull__item__rn (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Umull (rdlo, rdhi, rn, rm) -> rn
let __proj__Umull__item__rm (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Umull (rdlo, rdhi, rn, rm) -> rm
let uu___is_Sdiv (projectee : ins) : Prims.bool=
  match projectee with | Sdiv (dst, rn, rm) -> true | uu___ -> false
let __proj__Sdiv__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Sdiv (dst, rn, rm) -> dst
let __proj__Sdiv__item__rn (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Sdiv (dst, rn, rm) -> rn
let __proj__Sdiv__item__rm (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Sdiv (dst, rn, rm) -> rm
let uu___is_Udiv (projectee : ins) : Prims.bool=
  match projectee with | Udiv (dst, rn, rm) -> true | uu___ -> false
let __proj__Udiv__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Udiv (dst, rn, rm) -> dst
let __proj__Udiv__item__rn (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Udiv (dst, rn, rm) -> rn
let __proj__Udiv__item__rm (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Udiv (dst, rn, rm) -> rm
let uu___is_And (projectee : ins) : Prims.bool=
  match projectee with | And (dst, s1, s2) -> true | uu___ -> false
let __proj__And__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | And (dst, s1, s2) -> dst
let __proj__And__item__s1 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | And (dst, s1, s2) -> s1
let __proj__And__item__s2 (projectee : ins) : operand=
  match projectee with | And (dst, s1, s2) -> s2
let uu___is_Ands (projectee : ins) : Prims.bool=
  match projectee with | Ands (dst, s1, s2) -> true | uu___ -> false
let __proj__Ands__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Ands (dst, s1, s2) -> dst
let __proj__Ands__item__s1 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Ands (dst, s1, s2) -> s1
let __proj__Ands__item__s2 (projectee : ins) : operand=
  match projectee with | Ands (dst, s1, s2) -> s2
let uu___is_Orr (projectee : ins) : Prims.bool=
  match projectee with | Orr (dst, s1, s2) -> true | uu___ -> false
let __proj__Orr__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Orr (dst, s1, s2) -> dst
let __proj__Orr__item__s1 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Orr (dst, s1, s2) -> s1
let __proj__Orr__item__s2 (projectee : ins) : operand=
  match projectee with | Orr (dst, s1, s2) -> s2
let uu___is_Orrs (projectee : ins) : Prims.bool=
  match projectee with | Orrs (dst, s1, s2) -> true | uu___ -> false
let __proj__Orrs__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Orrs (dst, s1, s2) -> dst
let __proj__Orrs__item__s1 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Orrs (dst, s1, s2) -> s1
let __proj__Orrs__item__s2 (projectee : ins) : operand=
  match projectee with | Orrs (dst, s1, s2) -> s2
let uu___is_Eor (projectee : ins) : Prims.bool=
  match projectee with | Eor (dst, s1, s2) -> true | uu___ -> false
let __proj__Eor__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Eor (dst, s1, s2) -> dst
let __proj__Eor__item__s1 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Eor (dst, s1, s2) -> s1
let __proj__Eor__item__s2 (projectee : ins) : operand=
  match projectee with | Eor (dst, s1, s2) -> s2
let uu___is_Eors (projectee : ins) : Prims.bool=
  match projectee with | Eors (dst, s1, s2) -> true | uu___ -> false
let __proj__Eors__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Eors (dst, s1, s2) -> dst
let __proj__Eors__item__s1 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Eors (dst, s1, s2) -> s1
let __proj__Eors__item__s2 (projectee : ins) : operand=
  match projectee with | Eors (dst, s1, s2) -> s2
let uu___is_Bic (projectee : ins) : Prims.bool=
  match projectee with | Bic (dst, s1, s2) -> true | uu___ -> false
let __proj__Bic__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Bic (dst, s1, s2) -> dst
let __proj__Bic__item__s1 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Bic (dst, s1, s2) -> s1
let __proj__Bic__item__s2 (projectee : ins) : operand=
  match projectee with | Bic (dst, s1, s2) -> s2
let uu___is_Bics (projectee : ins) : Prims.bool=
  match projectee with | Bics (dst, s1, s2) -> true | uu___ -> false
let __proj__Bics__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Bics (dst, s1, s2) -> dst
let __proj__Bics__item__s1 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Bics (dst, s1, s2) -> s1
let __proj__Bics__item__s2 (projectee : ins) : operand=
  match projectee with | Bics (dst, s1, s2) -> s2
let uu___is_Orn (projectee : ins) : Prims.bool=
  match projectee with | Orn (dst, s1, s2) -> true | uu___ -> false
let __proj__Orn__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Orn (dst, s1, s2) -> dst
let __proj__Orn__item__s1 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Orn (dst, s1, s2) -> s1
let __proj__Orn__item__s2 (projectee : ins) : operand=
  match projectee with | Orn (dst, s1, s2) -> s2
let uu___is_Mvn (projectee : ins) : Prims.bool=
  match projectee with | Mvn (dst, src) -> true | uu___ -> false
let __proj__Mvn__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Mvn (dst, src) -> dst
let __proj__Mvn__item__src (projectee : ins) : operand=
  match projectee with | Mvn (dst, src) -> src
let uu___is_Mvns (projectee : ins) : Prims.bool=
  match projectee with | Mvns (dst, src) -> true | uu___ -> false
let __proj__Mvns__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Mvns (dst, src) -> dst
let __proj__Mvns__item__src (projectee : ins) : operand=
  match projectee with | Mvns (dst, src) -> src
let uu___is_Lsl (projectee : ins) : Prims.bool=
  match projectee with | Lsl (dst, rn, rm) -> true | uu___ -> false
let __proj__Lsl__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Lsl (dst, rn, rm) -> dst
let __proj__Lsl__item__rn (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Lsl (dst, rn, rm) -> rn
let __proj__Lsl__item__rm (projectee : ins) : operand=
  match projectee with | Lsl (dst, rn, rm) -> rm
let uu___is_Lsr (projectee : ins) : Prims.bool=
  match projectee with | Lsr (dst, rn, rm) -> true | uu___ -> false
let __proj__Lsr__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Lsr (dst, rn, rm) -> dst
let __proj__Lsr__item__rn (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Lsr (dst, rn, rm) -> rn
let __proj__Lsr__item__rm (projectee : ins) : operand=
  match projectee with | Lsr (dst, rn, rm) -> rm
let uu___is_Asr (projectee : ins) : Prims.bool=
  match projectee with | Asr (dst, rn, rm) -> true | uu___ -> false
let __proj__Asr__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Asr (dst, rn, rm) -> dst
let __proj__Asr__item__rn (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Asr (dst, rn, rm) -> rn
let __proj__Asr__item__rm (projectee : ins) : operand=
  match projectee with | Asr (dst, rn, rm) -> rm
let uu___is_Ror (projectee : ins) : Prims.bool=
  match projectee with | Ror (dst, rn, rm) -> true | uu___ -> false
let __proj__Ror__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Ror (dst, rn, rm) -> dst
let __proj__Ror__item__rn (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Ror (dst, rn, rm) -> rn
let __proj__Ror__item__rm (projectee : ins) : operand=
  match projectee with | Ror (dst, rn, rm) -> rm
let uu___is_Clz (projectee : ins) : Prims.bool=
  match projectee with | Clz (dst, src) -> true | uu___ -> false
let __proj__Clz__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Clz (dst, src) -> dst
let __proj__Clz__item__src (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Clz (dst, src) -> src
let uu___is_Rbit (projectee : ins) : Prims.bool=
  match projectee with | Rbit (dst, src) -> true | uu___ -> false
let __proj__Rbit__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Rbit (dst, src) -> dst
let __proj__Rbit__item__src (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Rbit (dst, src) -> src
let uu___is_Rev (projectee : ins) : Prims.bool=
  match projectee with | Rev (dst, src) -> true | uu___ -> false
let __proj__Rev__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Rev (dst, src) -> dst
let __proj__Rev__item__src (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Rev (dst, src) -> src
let uu___is_Rev16 (projectee : ins) : Prims.bool=
  match projectee with | Rev16 (dst, src) -> true | uu___ -> false
let __proj__Rev16__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Rev16 (dst, src) -> dst
let __proj__Rev16__item__src (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Rev16 (dst, src) -> src
let uu___is_Revsh (projectee : ins) : Prims.bool=
  match projectee with | Revsh (dst, src) -> true | uu___ -> false
let __proj__Revsh__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Revsh (dst, src) -> dst
let __proj__Revsh__item__src (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Revsh (dst, src) -> src
let uu___is_Ubfx (projectee : ins) : Prims.bool=
  match projectee with | Ubfx (dst, rn, lsb, width) -> true | uu___ -> false
let __proj__Ubfx__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Ubfx (dst, rn, lsb, width) -> dst
let __proj__Ubfx__item__rn (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Ubfx (dst, rn, lsb, width) -> rn
let __proj__Ubfx__item__lsb (projectee : ins) : Vale_Defs.nat32=
  match projectee with | Ubfx (dst, rn, lsb, width) -> lsb
let __proj__Ubfx__item__width (projectee : ins) : Vale_Defs.nat32=
  match projectee with | Ubfx (dst, rn, lsb, width) -> width
let uu___is_Sbfx (projectee : ins) : Prims.bool=
  match projectee with | Sbfx (dst, rn, lsb, width) -> true | uu___ -> false
let __proj__Sbfx__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Sbfx (dst, rn, lsb, width) -> dst
let __proj__Sbfx__item__rn (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Sbfx (dst, rn, lsb, width) -> rn
let __proj__Sbfx__item__lsb (projectee : ins) : Vale_Defs.nat32=
  match projectee with | Sbfx (dst, rn, lsb, width) -> lsb
let __proj__Sbfx__item__width (projectee : ins) : Vale_Defs.nat32=
  match projectee with | Sbfx (dst, rn, lsb, width) -> width
let uu___is_Cmp (projectee : ins) : Prims.bool=
  match projectee with | Cmp (rn, op) -> true | uu___ -> false
let __proj__Cmp__item__rn (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Cmp (rn, op) -> rn
let __proj__Cmp__item__op (projectee : ins) : operand=
  match projectee with | Cmp (rn, op) -> op
let uu___is_Cmn (projectee : ins) : Prims.bool=
  match projectee with | Cmn (rn, op) -> true | uu___ -> false
let __proj__Cmn__item__rn (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Cmn (rn, op) -> rn
let __proj__Cmn__item__op (projectee : ins) : operand=
  match projectee with | Cmn (rn, op) -> op
let uu___is_Tst (projectee : ins) : Prims.bool=
  match projectee with | Tst (rn, op) -> true | uu___ -> false
let __proj__Tst__item__rn (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Tst (rn, op) -> rn
let __proj__Tst__item__op (projectee : ins) : operand=
  match projectee with | Tst (rn, op) -> op
let uu___is_Teq (projectee : ins) : Prims.bool=
  match projectee with | Teq (rn, op) -> true | uu___ -> false
let __proj__Teq__item__rn (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Teq (rn, op) -> rn
let __proj__Teq__item__op (projectee : ins) : operand=
  match projectee with | Teq (rn, op) -> op
let uu___is_Sxtb (projectee : ins) : Prims.bool=
  match projectee with | Sxtb (dst, src) -> true | uu___ -> false
let __proj__Sxtb__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Sxtb (dst, src) -> dst
let __proj__Sxtb__item__src (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Sxtb (dst, src) -> src
let uu___is_Sxth (projectee : ins) : Prims.bool=
  match projectee with | Sxth (dst, src) -> true | uu___ -> false
let __proj__Sxth__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Sxth (dst, src) -> dst
let __proj__Sxth__item__src (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Sxth (dst, src) -> src
let uu___is_Uxtb (projectee : ins) : Prims.bool=
  match projectee with | Uxtb (dst, src) -> true | uu___ -> false
let __proj__Uxtb__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Uxtb (dst, src) -> dst
let __proj__Uxtb__item__src (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Uxtb (dst, src) -> src
let uu___is_Uxth (projectee : ins) : Prims.bool=
  match projectee with | Uxth (dst, src) -> true | uu___ -> false
let __proj__Uxth__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Uxth (dst, src) -> dst
let __proj__Uxth__item__src (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Uxth (dst, src) -> src
let uu___is_Ldr (projectee : ins) : Prims.bool=
  match projectee with | Ldr (dst, mem) -> true | uu___ -> false
let __proj__Ldr__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Ldr (dst, mem) -> dst
let __proj__Ldr__item__mem (projectee : ins) : Vale_Arm32_Machine.maddr=
  match projectee with | Ldr (dst, mem) -> mem
let uu___is_Ldrb (projectee : ins) : Prims.bool=
  match projectee with | Ldrb (dst, mem) -> true | uu___ -> false
let __proj__Ldrb__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Ldrb (dst, mem) -> dst
let __proj__Ldrb__item__mem (projectee : ins) : Vale_Arm32_Machine.maddr=
  match projectee with | Ldrb (dst, mem) -> mem
let uu___is_Ldrh (projectee : ins) : Prims.bool=
  match projectee with | Ldrh (dst, mem) -> true | uu___ -> false
let __proj__Ldrh__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Ldrh (dst, mem) -> dst
let __proj__Ldrh__item__mem (projectee : ins) : Vale_Arm32_Machine.maddr=
  match projectee with | Ldrh (dst, mem) -> mem
let uu___is_Ldrsb (projectee : ins) : Prims.bool=
  match projectee with | Ldrsb (dst, mem) -> true | uu___ -> false
let __proj__Ldrsb__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Ldrsb (dst, mem) -> dst
let __proj__Ldrsb__item__mem (projectee : ins) : Vale_Arm32_Machine.maddr=
  match projectee with | Ldrsb (dst, mem) -> mem
let uu___is_Ldrsh (projectee : ins) : Prims.bool=
  match projectee with | Ldrsh (dst, mem) -> true | uu___ -> false
let __proj__Ldrsh__item__dst (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Ldrsh (dst, mem) -> dst
let __proj__Ldrsh__item__mem (projectee : ins) : Vale_Arm32_Machine.maddr=
  match projectee with | Ldrsh (dst, mem) -> mem
let uu___is_Ldrd (projectee : ins) : Prims.bool=
  match projectee with | Ldrd (rt, rt2, mem) -> true | uu___ -> false
let __proj__Ldrd__item__rt (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Ldrd (rt, rt2, mem) -> rt
let __proj__Ldrd__item__rt2 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Ldrd (rt, rt2, mem) -> rt2
let __proj__Ldrd__item__mem (projectee : ins) : Vale_Arm32_Machine.maddr=
  match projectee with | Ldrd (rt, rt2, mem) -> mem
let uu___is_Str (projectee : ins) : Prims.bool=
  match projectee with | Str (src, mem) -> true | uu___ -> false
let __proj__Str__item__src (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Str (src, mem) -> src
let __proj__Str__item__mem (projectee : ins) : Vale_Arm32_Machine.maddr=
  match projectee with | Str (src, mem) -> mem
let uu___is_Strb (projectee : ins) : Prims.bool=
  match projectee with | Strb (src, mem) -> true | uu___ -> false
let __proj__Strb__item__src (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Strb (src, mem) -> src
let __proj__Strb__item__mem (projectee : ins) : Vale_Arm32_Machine.maddr=
  match projectee with | Strb (src, mem) -> mem
let uu___is_Strh (projectee : ins) : Prims.bool=
  match projectee with | Strh (src, mem) -> true | uu___ -> false
let __proj__Strh__item__src (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Strh (src, mem) -> src
let __proj__Strh__item__mem (projectee : ins) : Vale_Arm32_Machine.maddr=
  match projectee with | Strh (src, mem) -> mem
let uu___is_Strd (projectee : ins) : Prims.bool=
  match projectee with | Strd (rt, rt2, mem) -> true | uu___ -> false
let __proj__Strd__item__rt (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Strd (rt, rt2, mem) -> rt
let __proj__Strd__item__rt2 (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Strd (rt, rt2, mem) -> rt2
let __proj__Strd__item__mem (projectee : ins) : Vale_Arm32_Machine.maddr=
  match projectee with | Strd (rt, rt2, mem) -> mem
let uu___is_Push (projectee : ins) : Prims.bool=
  match projectee with | Push regs -> true | uu___ -> false
let __proj__Push__item__regs (projectee : ins) :
  Vale_Arm32_Machine.reg Prims.list= match projectee with | Push regs -> regs
let uu___is_Pop (projectee : ins) : Prims.bool=
  match projectee with | Pop regs -> true | uu___ -> false
let __proj__Pop__item__regs (projectee : ins) :
  Vale_Arm32_Machine.reg Prims.list= match projectee with | Pop regs -> regs
let uu___is_B (projectee : ins) : Prims.bool=
  match projectee with | B target -> true | uu___ -> false
let __proj__B__item__target (projectee : ins) : Prims.string=
  match projectee with | B target -> target
let uu___is_Bc (projectee : ins) : Prims.bool=
  match projectee with | Bc (cond, target) -> true | uu___ -> false
let __proj__Bc__item__cond (projectee : ins) : Vale_Arm32_Machine.cond_code=
  match projectee with | Bc (cond, target) -> cond
let __proj__Bc__item__target (projectee : ins) : Prims.string=
  match projectee with | Bc (cond, target) -> target
let uu___is_Bl (projectee : ins) : Prims.bool=
  match projectee with | Bl target -> true | uu___ -> false
let __proj__Bl__item__target (projectee : ins) : Prims.string=
  match projectee with | Bl target -> target
let uu___is_Bx (projectee : ins) : Prims.bool=
  match projectee with | Bx target -> true | uu___ -> false
let __proj__Bx__item__target (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Bx target -> target
let uu___is_Blx (projectee : ins) : Prims.bool=
  match projectee with | Blx target -> true | uu___ -> false
let __proj__Blx__item__target (projectee : ins) : Vale_Arm32_Machine.reg=
  match projectee with | Blx target -> target
let uu___is_Vmov (projectee : ins) : Prims.bool=
  match projectee with | Vmov (dst, src) -> true | uu___ -> false
let __proj__Vmov__item__dst (projectee : ins) : operand=
  match projectee with | Vmov (dst, src) -> dst
let __proj__Vmov__item__src (projectee : ins) : operand=
  match projectee with | Vmov (dst, src) -> src
let uu___is_VaddF32 (projectee : ins) : Prims.bool=
  match projectee with | VaddF32 (dst, s1, s2) -> true | uu___ -> false
let __proj__VaddF32__item__dst (projectee : ins) : Vale_Arm32_Machine.sreg=
  match projectee with | VaddF32 (dst, s1, s2) -> dst
let __proj__VaddF32__item__s1 (projectee : ins) : Vale_Arm32_Machine.sreg=
  match projectee with | VaddF32 (dst, s1, s2) -> s1
let __proj__VaddF32__item__s2 (projectee : ins) : Vale_Arm32_Machine.sreg=
  match projectee with | VaddF32 (dst, s1, s2) -> s2
let uu___is_VaddF64 (projectee : ins) : Prims.bool=
  match projectee with | VaddF64 (dst, s1, s2) -> true | uu___ -> false
let __proj__VaddF64__item__dst (projectee : ins) : Vale_Arm32_Machine.dreg=
  match projectee with | VaddF64 (dst, s1, s2) -> dst
let __proj__VaddF64__item__s1 (projectee : ins) : Vale_Arm32_Machine.dreg=
  match projectee with | VaddF64 (dst, s1, s2) -> s1
let __proj__VaddF64__item__s2 (projectee : ins) : Vale_Arm32_Machine.dreg=
  match projectee with | VaddF64 (dst, s1, s2) -> s2
let uu___is_VsubF32 (projectee : ins) : Prims.bool=
  match projectee with | VsubF32 (dst, s1, s2) -> true | uu___ -> false
let __proj__VsubF32__item__dst (projectee : ins) : Vale_Arm32_Machine.sreg=
  match projectee with | VsubF32 (dst, s1, s2) -> dst
let __proj__VsubF32__item__s1 (projectee : ins) : Vale_Arm32_Machine.sreg=
  match projectee with | VsubF32 (dst, s1, s2) -> s1
let __proj__VsubF32__item__s2 (projectee : ins) : Vale_Arm32_Machine.sreg=
  match projectee with | VsubF32 (dst, s1, s2) -> s2
let uu___is_VsubF64 (projectee : ins) : Prims.bool=
  match projectee with | VsubF64 (dst, s1, s2) -> true | uu___ -> false
let __proj__VsubF64__item__dst (projectee : ins) : Vale_Arm32_Machine.dreg=
  match projectee with | VsubF64 (dst, s1, s2) -> dst
let __proj__VsubF64__item__s1 (projectee : ins) : Vale_Arm32_Machine.dreg=
  match projectee with | VsubF64 (dst, s1, s2) -> s1
let __proj__VsubF64__item__s2 (projectee : ins) : Vale_Arm32_Machine.dreg=
  match projectee with | VsubF64 (dst, s1, s2) -> s2
let uu___is_VmulF32 (projectee : ins) : Prims.bool=
  match projectee with | VmulF32 (dst, s1, s2) -> true | uu___ -> false
let __proj__VmulF32__item__dst (projectee : ins) : Vale_Arm32_Machine.sreg=
  match projectee with | VmulF32 (dst, s1, s2) -> dst
let __proj__VmulF32__item__s1 (projectee : ins) : Vale_Arm32_Machine.sreg=
  match projectee with | VmulF32 (dst, s1, s2) -> s1
let __proj__VmulF32__item__s2 (projectee : ins) : Vale_Arm32_Machine.sreg=
  match projectee with | VmulF32 (dst, s1, s2) -> s2
let uu___is_VmulF64 (projectee : ins) : Prims.bool=
  match projectee with | VmulF64 (dst, s1, s2) -> true | uu___ -> false
let __proj__VmulF64__item__dst (projectee : ins) : Vale_Arm32_Machine.dreg=
  match projectee with | VmulF64 (dst, s1, s2) -> dst
let __proj__VmulF64__item__s1 (projectee : ins) : Vale_Arm32_Machine.dreg=
  match projectee with | VmulF64 (dst, s1, s2) -> s1
let __proj__VmulF64__item__s2 (projectee : ins) : Vale_Arm32_Machine.dreg=
  match projectee with | VmulF64 (dst, s1, s2) -> s2
let uu___is_Vldr (projectee : ins) : Prims.bool=
  match projectee with | Vldr (dst, mem) -> true | uu___ -> false
let __proj__Vldr__item__dst (projectee : ins) : Vale_Arm32_Machine.sreg=
  match projectee with | Vldr (dst, mem) -> dst
let __proj__Vldr__item__mem (projectee : ins) : Vale_Arm32_Machine.maddr=
  match projectee with | Vldr (dst, mem) -> mem
let uu___is_Vstr (projectee : ins) : Prims.bool=
  match projectee with | Vstr (src, mem) -> true | uu___ -> false
let __proj__Vstr__item__src (projectee : ins) : Vale_Arm32_Machine.sreg=
  match projectee with | Vstr (src, mem) -> src
let __proj__Vstr__item__mem (projectee : ins) : Vale_Arm32_Machine.maddr=
  match projectee with | Vstr (src, mem) -> mem
let uu___is_Vpush (projectee : ins) : Prims.bool=
  match projectee with | Vpush regs -> true | uu___ -> false
let __proj__Vpush__item__regs (projectee : ins) :
  Vale_Arm32_Machine.sreg Prims.list=
  match projectee with | Vpush regs -> regs
let uu___is_Vpop (projectee : ins) : Prims.bool=
  match projectee with | Vpop regs -> true | uu___ -> false
let __proj__Vpop__item__regs (projectee : ins) :
  Vale_Arm32_Machine.sreg Prims.list=
  match projectee with | Vpop regs -> regs
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
let safe_shift_amount (amount : Vale_Defs.nat32) : Vale_Defs.nat32=
  FStar_UInt32.uint_to_t ((mod) (FStar_UInt32.v amount) (Prims.of_int 32))
let safe_shl (v : Vale_Defs.nat32) (amt : Vale_Defs.nat32) : Vale_Defs.nat32=
  FStar_UInt32.shift_left v (safe_shift_amount amt)
let safe_shr (v : Vale_Defs.nat32) (amt : Vale_Defs.nat32) : Vale_Defs.nat32=
  FStar_UInt32.shift_right v (safe_shift_amount amt)
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
let eval_operand (o : operand) (s : Vale_Arm32_Machine.state) :
  Vale_Defs.nat32=
  match o with
  | OReg r -> s.Vale_Arm32_Machine.regs r
  | OSreg sr -> s.Vale_Arm32_Machine.sregs sr
  | OImm imm -> imm
  | uu___ -> Stdint.Uint32.zero
let eval_ins (i : ins) (s : Vale_Arm32_Machine.state) :
  Vale_Arm32_Machine.state=
  match i with
  | Mov (dst, src) ->
      let v = eval_operand src s in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then v else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Movs (dst, src) ->
      let v = eval_operand src s in
      let fl =
        let uu___ = s.Vale_Arm32_Machine.flags in
        {
          Vale_Arm32_Machine.n =
            ((FStar_UInt32.v v) >= (Prims.parse_int "2147483648"));
          Vale_Arm32_Machine.z = (v = Stdint.Uint32.zero);
          Vale_Arm32_Machine.c = (uu___.Vale_Arm32_Machine.c);
          Vale_Arm32_Machine.v = (uu___.Vale_Arm32_Machine.v)
        } in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then v else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = fl
      }
  | Movw (dst, imm) ->
      let v = FStar_UInt32.logand imm (Stdint.Uint32.of_int 0xffff) in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then v else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Movt (dst, imm) ->
      let cur = s.Vale_Arm32_Machine.regs dst in
      let v =
        FStar_UInt32.logor
          (FStar_UInt32.logand cur (Stdint.Uint32.of_int 0xffff))
          (FStar_UInt32.shift_left imm (Stdint.Uint32.of_int 16)) in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then v else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Add (dst, s1, s2) ->
      let v1 = s.Vale_Arm32_Machine.regs s1 in
      let v2 = eval_operand s2 s in
      let res = FStar_UInt32.add_mod v1 v2 in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Adds (dst, s1, s2) ->
      let v1 = s.Vale_Arm32_Machine.regs s1 in
      let v2 = eval_operand s2 s in
      let res = FStar_UInt32.add_mod v1 v2 in
      let fl =
        let uu___ = s.Vale_Arm32_Machine.flags in
        {
          Vale_Arm32_Machine.n =
            ((FStar_UInt32.v res) >= (Prims.parse_int "2147483648"));
          Vale_Arm32_Machine.z = (res = Stdint.Uint32.zero);
          Vale_Arm32_Machine.c = (uu___.Vale_Arm32_Machine.c);
          Vale_Arm32_Machine.v = (uu___.Vale_Arm32_Machine.v)
        } in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = fl
      }
  | Adc (dst, s1, s2) ->
      let v1 = s.Vale_Arm32_Machine.regs s1 in
      let v2 = eval_operand s2 s in
      let carry =
        if (s.Vale_Arm32_Machine.flags).Vale_Arm32_Machine.c
        then Stdint.Uint32.one
        else Stdint.Uint32.zero in
      let res = FStar_UInt32.add_mod (FStar_UInt32.add_mod v1 v2) carry in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Adcs (dst, s1, s2) ->
      let v1 = s.Vale_Arm32_Machine.regs s1 in
      let v2 = eval_operand s2 s in
      let carry =
        if (s.Vale_Arm32_Machine.flags).Vale_Arm32_Machine.c
        then Stdint.Uint32.one
        else Stdint.Uint32.zero in
      let res = FStar_UInt32.add_mod (FStar_UInt32.add_mod v1 v2) carry in
      let fl =
        let uu___ = s.Vale_Arm32_Machine.flags in
        {
          Vale_Arm32_Machine.n =
            ((FStar_UInt32.v res) >= (Prims.parse_int "2147483648"));
          Vale_Arm32_Machine.z = (res = Stdint.Uint32.zero);
          Vale_Arm32_Machine.c = (uu___.Vale_Arm32_Machine.c);
          Vale_Arm32_Machine.v = (uu___.Vale_Arm32_Machine.v)
        } in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = fl
      }
  | Sub (dst, s1, s2) ->
      let v1 = s.Vale_Arm32_Machine.regs s1 in
      let v2 = eval_operand s2 s in
      let res = FStar_UInt32.sub_mod v1 v2 in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Subs (dst, s1, s2) ->
      let v1 = s.Vale_Arm32_Machine.regs s1 in
      let v2 = eval_operand s2 s in
      let res = FStar_UInt32.sub_mod v1 v2 in
      let fl =
        let uu___ = s.Vale_Arm32_Machine.flags in
        {
          Vale_Arm32_Machine.n =
            ((FStar_UInt32.v res) >= (Prims.parse_int "2147483648"));
          Vale_Arm32_Machine.z = (res = Stdint.Uint32.zero);
          Vale_Arm32_Machine.c = (uu___.Vale_Arm32_Machine.c);
          Vale_Arm32_Machine.v = (uu___.Vale_Arm32_Machine.v)
        } in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = fl
      }
  | Sbc (dst, s1, s2) ->
      let v1 = s.Vale_Arm32_Machine.regs s1 in
      let v2 = eval_operand s2 s in
      let not_carry =
        if (s.Vale_Arm32_Machine.flags).Vale_Arm32_Machine.c
        then Stdint.Uint32.zero
        else Stdint.Uint32.one in
      let res = FStar_UInt32.sub_mod (FStar_UInt32.sub_mod v1 v2) not_carry in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Sbcs (dst, s1, s2) ->
      let v1 = s.Vale_Arm32_Machine.regs s1 in
      let v2 = eval_operand s2 s in
      let not_carry =
        if (s.Vale_Arm32_Machine.flags).Vale_Arm32_Machine.c
        then Stdint.Uint32.zero
        else Stdint.Uint32.one in
      let res = FStar_UInt32.sub_mod (FStar_UInt32.sub_mod v1 v2) not_carry in
      let fl =
        let uu___ = s.Vale_Arm32_Machine.flags in
        {
          Vale_Arm32_Machine.n =
            ((FStar_UInt32.v res) >= (Prims.parse_int "2147483648"));
          Vale_Arm32_Machine.z = (res = Stdint.Uint32.zero);
          Vale_Arm32_Machine.c = (uu___.Vale_Arm32_Machine.c);
          Vale_Arm32_Machine.v = (uu___.Vale_Arm32_Machine.v)
        } in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = fl
      }
  | Rsb (dst, s1, s2) ->
      let v1 = s.Vale_Arm32_Machine.regs s1 in
      let v2 = eval_operand s2 s in
      let res = FStar_UInt32.sub_mod v2 v1 in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Rsbs (dst, s1, s2) ->
      let v1 = s.Vale_Arm32_Machine.regs s1 in
      let v2 = eval_operand s2 s in
      let res = FStar_UInt32.sub_mod v2 v1 in
      let fl =
        let uu___ = s.Vale_Arm32_Machine.flags in
        {
          Vale_Arm32_Machine.n =
            ((FStar_UInt32.v res) >= (Prims.parse_int "2147483648"));
          Vale_Arm32_Machine.z = (res = Stdint.Uint32.zero);
          Vale_Arm32_Machine.c = (uu___.Vale_Arm32_Machine.c);
          Vale_Arm32_Machine.v = (uu___.Vale_Arm32_Machine.v)
        } in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = fl
      }
  | Rsc (dst, s1, s2) ->
      let v1 = s.Vale_Arm32_Machine.regs s1 in
      let v2 = eval_operand s2 s in
      let not_carry =
        if (s.Vale_Arm32_Machine.flags).Vale_Arm32_Machine.c
        then Stdint.Uint32.zero
        else Stdint.Uint32.one in
      let res = FStar_UInt32.sub_mod (FStar_UInt32.sub_mod v2 v1) not_carry in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Rscs (dst, s1, s2) ->
      let v1 = s.Vale_Arm32_Machine.regs s1 in
      let v2 = eval_operand s2 s in
      let not_carry =
        if (s.Vale_Arm32_Machine.flags).Vale_Arm32_Machine.c
        then Stdint.Uint32.zero
        else Stdint.Uint32.one in
      let res = FStar_UInt32.sub_mod (FStar_UInt32.sub_mod v2 v1) not_carry in
      let fl =
        let uu___ = s.Vale_Arm32_Machine.flags in
        {
          Vale_Arm32_Machine.n =
            ((FStar_UInt32.v res) >= (Prims.parse_int "2147483648"));
          Vale_Arm32_Machine.z = (res = Stdint.Uint32.zero);
          Vale_Arm32_Machine.c = (uu___.Vale_Arm32_Machine.c);
          Vale_Arm32_Machine.v = (uu___.Vale_Arm32_Machine.v)
        } in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = fl
      }
  | Mul (dst, s1, s2) ->
      let v1 = s.Vale_Arm32_Machine.regs s1 in
      let v2 = s.Vale_Arm32_Machine.regs s2 in
      let res = FStar_UInt32.mul_mod v1 v2 in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Mla (dst, s1, s2, ra) ->
      let v1 = s.Vale_Arm32_Machine.regs s1 in
      let v2 = s.Vale_Arm32_Machine.regs s2 in
      let a = s.Vale_Arm32_Machine.regs ra in
      let res = FStar_UInt32.add_mod (FStar_UInt32.mul_mod v1 v2) a in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Mls (dst, s1, s2, ra) ->
      let v1 = s.Vale_Arm32_Machine.regs s1 in
      let v2 = s.Vale_Arm32_Machine.regs s2 in
      let a = s.Vale_Arm32_Machine.regs ra in
      let res = FStar_UInt32.sub_mod a (FStar_UInt32.mul_mod v1 v2) in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Smull (rdlo, rdhi, rn, rm) ->
      let v1 = FStar_Int_Cast.uint32_to_uint64 (s.Vale_Arm32_Machine.regs rn) in
      let v2 = FStar_Int_Cast.uint32_to_uint64 (s.Vale_Arm32_Machine.regs rm) in
      let prod = FStar_UInt64.mul_mod v1 v2 in
      let r_lo =
        FStar_Int_Cast.uint64_to_uint32
          (FStar_UInt64.logand prod (Stdint.Uint64.of_string "0xffffffff")) in
      let r_hi =
        FStar_Int_Cast.uint64_to_uint32
          (FStar_UInt64.shift_right prod (Stdint.Uint32.of_int 32)) in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r ->
              if r = rdlo
              then r_lo
              else if r = rdhi then r_hi else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Umull (rdlo, rdhi, rn, rm) ->
      let v1 = FStar_Int_Cast.uint32_to_uint64 (s.Vale_Arm32_Machine.regs rn) in
      let v2 = FStar_Int_Cast.uint32_to_uint64 (s.Vale_Arm32_Machine.regs rm) in
      let prod = FStar_UInt64.mul_mod v1 v2 in
      let r_lo =
        FStar_Int_Cast.uint64_to_uint32
          (FStar_UInt64.logand prod (Stdint.Uint64.of_string "0xffffffff")) in
      let r_hi =
        FStar_Int_Cast.uint64_to_uint32
          (FStar_UInt64.shift_right prod (Stdint.Uint32.of_int 32)) in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r ->
              if r = rdlo
              then r_lo
              else if r = rdhi then r_hi else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Sdiv (dst, rn, rm) ->
      let d = s.Vale_Arm32_Machine.regs rm in
      let res =
        if d = Stdint.Uint32.zero
        then Stdint.Uint32.zero
        else FStar_UInt32.div (s.Vale_Arm32_Machine.regs rn) d in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Udiv (dst, rn, rm) ->
      let d = s.Vale_Arm32_Machine.regs rm in
      let res =
        if d = Stdint.Uint32.zero
        then Stdint.Uint32.zero
        else FStar_UInt32.div (s.Vale_Arm32_Machine.regs rn) d in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | And (dst, s1, s2) ->
      let v1 = s.Vale_Arm32_Machine.regs s1 in
      let v2 = eval_operand s2 s in
      let res = FStar_UInt32.logand v1 v2 in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Ands (dst, s1, s2) ->
      let v1 = s.Vale_Arm32_Machine.regs s1 in
      let v2 = eval_operand s2 s in
      let res = FStar_UInt32.logand v1 v2 in
      let fl =
        let uu___ = s.Vale_Arm32_Machine.flags in
        {
          Vale_Arm32_Machine.n =
            ((FStar_UInt32.v res) >= (Prims.parse_int "2147483648"));
          Vale_Arm32_Machine.z = (res = Stdint.Uint32.zero);
          Vale_Arm32_Machine.c = (uu___.Vale_Arm32_Machine.c);
          Vale_Arm32_Machine.v = (uu___.Vale_Arm32_Machine.v)
        } in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = fl
      }
  | Orr (dst, s1, s2) ->
      let v1 = s.Vale_Arm32_Machine.regs s1 in
      let v2 = eval_operand s2 s in
      let res = FStar_UInt32.logor v1 v2 in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Orrs (dst, s1, s2) ->
      let v1 = s.Vale_Arm32_Machine.regs s1 in
      let v2 = eval_operand s2 s in
      let res = FStar_UInt32.logor v1 v2 in
      let fl =
        let uu___ = s.Vale_Arm32_Machine.flags in
        {
          Vale_Arm32_Machine.n =
            ((FStar_UInt32.v res) >= (Prims.parse_int "2147483648"));
          Vale_Arm32_Machine.z = (res = Stdint.Uint32.zero);
          Vale_Arm32_Machine.c = (uu___.Vale_Arm32_Machine.c);
          Vale_Arm32_Machine.v = (uu___.Vale_Arm32_Machine.v)
        } in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = fl
      }
  | Eor (dst, s1, s2) ->
      let v1 = s.Vale_Arm32_Machine.regs s1 in
      let v2 = eval_operand s2 s in
      let res = FStar_UInt32.logxor v1 v2 in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Eors (dst, s1, s2) ->
      let v1 = s.Vale_Arm32_Machine.regs s1 in
      let v2 = eval_operand s2 s in
      let res = FStar_UInt32.logxor v1 v2 in
      let fl =
        let uu___ = s.Vale_Arm32_Machine.flags in
        {
          Vale_Arm32_Machine.n =
            ((FStar_UInt32.v res) >= (Prims.parse_int "2147483648"));
          Vale_Arm32_Machine.z = (res = Stdint.Uint32.zero);
          Vale_Arm32_Machine.c = (uu___.Vale_Arm32_Machine.c);
          Vale_Arm32_Machine.v = (uu___.Vale_Arm32_Machine.v)
        } in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = fl
      }
  | Bic (dst, s1, s2) ->
      let v1 = s.Vale_Arm32_Machine.regs s1 in
      let v2 = eval_operand s2 s in
      let res = FStar_UInt32.logand v1 (FStar_UInt32.lognot v2) in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Bics (dst, s1, s2) ->
      let v1 = s.Vale_Arm32_Machine.regs s1 in
      let v2 = eval_operand s2 s in
      let res = FStar_UInt32.logand v1 (FStar_UInt32.lognot v2) in
      let fl =
        let uu___ = s.Vale_Arm32_Machine.flags in
        {
          Vale_Arm32_Machine.n =
            ((FStar_UInt32.v res) >= (Prims.parse_int "2147483648"));
          Vale_Arm32_Machine.z = (res = Stdint.Uint32.zero);
          Vale_Arm32_Machine.c = (uu___.Vale_Arm32_Machine.c);
          Vale_Arm32_Machine.v = (uu___.Vale_Arm32_Machine.v)
        } in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = fl
      }
  | Orn (dst, s1, s2) ->
      let v1 = s.Vale_Arm32_Machine.regs s1 in
      let v2 = eval_operand s2 s in
      let res = FStar_UInt32.logor v1 (FStar_UInt32.lognot v2) in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Mvn (dst, src) ->
      let v = eval_operand src s in
      let res = FStar_UInt32.lognot v in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Mvns (dst, src) ->
      let v = eval_operand src s in
      let res = FStar_UInt32.lognot v in
      let fl =
        let uu___ = s.Vale_Arm32_Machine.flags in
        {
          Vale_Arm32_Machine.n =
            ((FStar_UInt32.v res) >= (Prims.parse_int "2147483648"));
          Vale_Arm32_Machine.z = (res = Stdint.Uint32.zero);
          Vale_Arm32_Machine.c = (uu___.Vale_Arm32_Machine.c);
          Vale_Arm32_Machine.v = (uu___.Vale_Arm32_Machine.v)
        } in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = fl
      }
  | Lsl (dst, rn, rm) ->
      let v = s.Vale_Arm32_Machine.regs rn in
      let res = safe_shl v (eval_operand rm s) in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Lsr (dst, rn, rm) ->
      let v = s.Vale_Arm32_Machine.regs rn in
      let res = safe_shr v (eval_operand rm s) in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Asr (dst, rn, rm) ->
      let v = s.Vale_Arm32_Machine.regs rn in
      let res = safe_shr v (eval_operand rm s) in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Ror (dst, rn, rm) ->
      let v = s.Vale_Arm32_Machine.regs rn in
      let shift =
        (mod) (FStar_UInt32.v (eval_operand rm s)) (Prims.of_int 32) in
      let res =
        if shift = Prims.int_zero
        then v
        else
          (let s1 = FStar_UInt32.uint_to_t shift in
           let s2 = FStar_UInt32.uint_to_t ((Prims.of_int 32) - shift) in
           FStar_UInt32.logor (FStar_UInt32.shift_right v s1)
             (FStar_UInt32.shift_left v s2)) in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Clz (dst, src) ->
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r ->
              if r = dst
              then Stdint.Uint32.zero
              else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Rbit (dst, src) ->
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r ->
              if r = dst
              then Stdint.Uint32.zero
              else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Rev (dst, src) ->
      let v = s.Vale_Arm32_Machine.regs src in
      let b0 =
        FStar_UInt32.shift_left
          (FStar_UInt32.logand v (Stdint.Uint32.of_int 0xff))
          (Stdint.Uint32.of_int 24) in
      let b1 =
        FStar_UInt32.shift_left
          (FStar_UInt32.logand
             (FStar_UInt32.shift_right v (Stdint.Uint32.of_int 8))
             (Stdint.Uint32.of_int 0xff)) (Stdint.Uint32.of_int 16) in
      let b2 =
        FStar_UInt32.shift_left
          (FStar_UInt32.logand
             (FStar_UInt32.shift_right v (Stdint.Uint32.of_int 16))
             (Stdint.Uint32.of_int 0xff)) (Stdint.Uint32.of_int 8) in
      let b3 =
        FStar_UInt32.logand
          (FStar_UInt32.shift_right v (Stdint.Uint32.of_int 24))
          (Stdint.Uint32.of_int 0xff) in
      let res =
        FStar_UInt32.logor (FStar_UInt32.logor b0 b1)
          (FStar_UInt32.logor b2 b3) in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Rev16 (dst, src) ->
      let v = s.Vale_Arm32_Machine.regs src in
      let b0 =
        FStar_UInt32.shift_left
          (FStar_UInt32.logand v (Stdint.Uint32.of_int 0xff))
          (Stdint.Uint32.of_int 8) in
      let b1 =
        FStar_UInt32.logand
          (FStar_UInt32.shift_right v (Stdint.Uint32.of_int 8))
          (Stdint.Uint32.of_int 0xff) in
      let b2 =
        FStar_UInt32.shift_left
          (FStar_UInt32.logand
             (FStar_UInt32.shift_right v (Stdint.Uint32.of_int 16))
             (Stdint.Uint32.of_int 0xff)) (Stdint.Uint32.of_int 8) in
      let b3 =
        FStar_UInt32.logand
          (FStar_UInt32.shift_right v (Stdint.Uint32.of_int 24))
          (Stdint.Uint32.of_int 0xff) in
      let res =
        FStar_UInt32.logor
          (FStar_UInt32.shift_left (FStar_UInt32.logor b2 b3)
             (Stdint.Uint32.of_int 16)) (FStar_UInt32.logor b0 b1) in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Revsh (dst, src) ->
      let v = s.Vale_Arm32_Machine.regs src in
      let b0 =
        FStar_UInt32.shift_left
          (FStar_UInt32.logand v (Stdint.Uint32.of_int 0xff))
          (Stdint.Uint32.of_int 8) in
      let b1 =
        FStar_UInt32.logand
          (FStar_UInt32.shift_right v (Stdint.Uint32.of_int 8))
          (Stdint.Uint32.of_int 0xff) in
      let res = FStar_UInt32.logor b0 b1 in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Ubfx (dst, rn, lsb, width) ->
      let v = s.Vale_Arm32_Machine.regs rn in
      let shifted = safe_shr v lsb in
      let w_val = FStar_UInt32.v width in
      let mask =
        if w_val >= (Prims.of_int 32)
        then Stdint.Uint32.of_string "0xffffffff"
        else
          (let w_shift = FStar_UInt32.uint_to_t w_val in
           FStar_UInt32.sub_mod
             (FStar_UInt32.shift_left Stdint.Uint32.one w_shift)
             Stdint.Uint32.one) in
      let res = FStar_UInt32.logand shifted mask in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Sbfx (dst, rn, lsb, width) ->
      let v = s.Vale_Arm32_Machine.regs rn in
      let shifted = safe_shr v lsb in
      let w_val = FStar_UInt32.v width in
      let mask =
        if w_val >= (Prims.of_int 32)
        then Stdint.Uint32.of_string "0xffffffff"
        else
          (let w_shift = FStar_UInt32.uint_to_t w_val in
           FStar_UInt32.sub_mod
             (FStar_UInt32.shift_left Stdint.Uint32.one w_shift)
             Stdint.Uint32.one) in
      let res = FStar_UInt32.logand shifted mask in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Cmp (rn, op) ->
      let v1 = s.Vale_Arm32_Machine.regs rn in
      let v2 = eval_operand op s in
      let diff = FStar_UInt32.sub_mod v1 v2 in
      let fl =
        {
          Vale_Arm32_Machine.n =
            ((FStar_UInt32.v diff) >= (Prims.parse_int "2147483648"));
          Vale_Arm32_Machine.z = (v1 = v2);
          Vale_Arm32_Machine.c = ((FStar_UInt32.v v1) >= (FStar_UInt32.v v2));
          Vale_Arm32_Machine.v = false
        } in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs = (s.Vale_Arm32_Machine.regs);
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = fl
      }
  | Cmn (rn, op) ->
      let v1 = s.Vale_Arm32_Machine.regs rn in
      let v2 = eval_operand op s in
      let sum = FStar_UInt32.add_mod v1 v2 in
      let fl =
        {
          Vale_Arm32_Machine.n =
            ((FStar_UInt32.v sum) >= (Prims.parse_int "2147483648"));
          Vale_Arm32_Machine.z = (sum = Stdint.Uint32.zero);
          Vale_Arm32_Machine.c = ((FStar_UInt32.v sum) < (FStar_UInt32.v v1));
          Vale_Arm32_Machine.v = false
        } in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs = (s.Vale_Arm32_Machine.regs);
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = fl
      }
  | Tst (rn, op) ->
      let v1 = s.Vale_Arm32_Machine.regs rn in
      let v2 = eval_operand op s in
      let res = FStar_UInt32.logand v1 v2 in
      let fl =
        let uu___ = s.Vale_Arm32_Machine.flags in
        {
          Vale_Arm32_Machine.n =
            ((FStar_UInt32.v res) >= (Prims.parse_int "2147483648"));
          Vale_Arm32_Machine.z = (res = Stdint.Uint32.zero);
          Vale_Arm32_Machine.c = (uu___.Vale_Arm32_Machine.c);
          Vale_Arm32_Machine.v = (uu___.Vale_Arm32_Machine.v)
        } in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs = (s.Vale_Arm32_Machine.regs);
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = fl
      }
  | Teq (rn, op) ->
      let v1 = s.Vale_Arm32_Machine.regs rn in
      let v2 = eval_operand op s in
      let res = FStar_UInt32.logxor v1 v2 in
      let fl =
        let uu___ = s.Vale_Arm32_Machine.flags in
        {
          Vale_Arm32_Machine.n =
            ((FStar_UInt32.v res) >= (Prims.parse_int "2147483648"));
          Vale_Arm32_Machine.z = (res = Stdint.Uint32.zero);
          Vale_Arm32_Machine.c = (uu___.Vale_Arm32_Machine.c);
          Vale_Arm32_Machine.v = (uu___.Vale_Arm32_Machine.v)
        } in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs = (s.Vale_Arm32_Machine.regs);
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = fl
      }
  | Sxtb (dst, src) ->
      let v =
        FStar_UInt32.logand (s.Vale_Arm32_Machine.regs src)
          (Stdint.Uint32.of_int 0xff) in
      let res =
        if (FStar_UInt32.v v) >= (Prims.of_int 128)
        then FStar_UInt32.logor v (Stdint.Uint32.of_string "0xffffff00")
        else v in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Sxth (dst, src) ->
      let v =
        FStar_UInt32.logand (s.Vale_Arm32_Machine.regs src)
          (Stdint.Uint32.of_int 0xffff) in
      let res =
        if (FStar_UInt32.v v) >= (Prims.of_int 32768)
        then FStar_UInt32.logor v (Stdint.Uint32.of_string "0xffff0000")
        else v in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Uxtb (dst, src) ->
      let res =
        FStar_UInt32.logand (s.Vale_Arm32_Machine.regs src)
          (Stdint.Uint32.of_int 0xff) in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Uxth (dst, src) ->
      let res =
        FStar_UInt32.logand (s.Vale_Arm32_Machine.regs src)
          (Stdint.Uint32.of_int 0xffff) in
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r -> if r = dst then res else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Ldr (dst, uu___) ->
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r ->
              if r = dst
              then Stdint.Uint32.zero
              else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Ldrb (dst, uu___) ->
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r ->
              if r = dst
              then Stdint.Uint32.zero
              else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Ldrh (dst, uu___) ->
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r ->
              if r = dst
              then Stdint.Uint32.zero
              else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Ldrsb (dst, uu___) ->
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r ->
              if r = dst
              then Stdint.Uint32.zero
              else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Ldrsh (dst, uu___) ->
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r ->
              if r = dst
              then Stdint.Uint32.zero
              else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Ldrd (rt, rt2, uu___) ->
      {
        Vale_Arm32_Machine.ok = (s.Vale_Arm32_Machine.ok);
        Vale_Arm32_Machine.regs =
          ((fun r ->
              if (r = rt) || (r = rt2)
              then Stdint.Uint32.zero
              else s.Vale_Arm32_Machine.regs r));
        Vale_Arm32_Machine.sregs = (s.Vale_Arm32_Machine.sregs);
        Vale_Arm32_Machine.flags = (s.Vale_Arm32_Machine.flags)
      }
  | Str (uu___, uu___1) -> s
  | Strb (uu___, uu___1) -> s
  | Strh (uu___, uu___1) -> s
  | Strd (uu___, uu___1, uu___2) -> s
  | Push uu___ -> s
  | Pop uu___ -> s
  | B uu___ -> s
  | Bc (uu___, uu___1) -> s
  | Bl uu___ -> s
  | Bx uu___ -> s
  | Blx uu___ -> s
  | Vmov (dst, src) -> s
  | VaddF32 (dst, s1, s2) -> s
  | VsubF32 (dst, s1, s2) -> s
  | VmulF32 (dst, s1, s2) -> s
  | VaddF64 (dst, s1, s2) -> s
  | VsubF64 (dst, s1, s2) -> s
  | VmulF64 (dst, s1, s2) -> s
  | Vldr (uu___, uu___1) -> s
  | Vstr (uu___, uu___1) -> s
  | Vpush uu___ -> s
  | Vpop uu___ -> s
let eval_ocmp (c : ocmp) (s : Vale_Arm32_Machine.state) : Prims.bool=
  match c with
  | OEq (o1, o2) ->
      (FStar_UInt32.v (eval_operand o1 s)) =
        (FStar_UInt32.v (eval_operand o2 s))
  | ONe (o1, o2) ->
      (FStar_UInt32.v (eval_operand o1 s)) <>
        (FStar_UInt32.v (eval_operand o2 s))
  | OLt (o1, o2) ->
      (FStar_UInt32.v (eval_operand o1 s)) <
        (FStar_UInt32.v (eval_operand o2 s))
  | OGe (o1, o2) ->
      (FStar_UInt32.v (eval_operand o1 s)) >=
        (FStar_UInt32.v (eval_operand o2 s))
let rec eval_code (c : code) (fuel : Prims.nat)
  (s : Vale_Arm32_Machine.state) :
  Vale_Arm32_Machine.state FStar_Pervasives_Native.option=
  if fuel = Prims.int_zero
  then FStar_Pervasives_Native.None
  else
    (match c with
     | Ins i -> FStar_Pervasives_Native.Some (eval_ins i s)
     | Block [] -> FStar_Pervasives_Native.Some s
     | Block (hd::tl) ->
         (match eval_code hd (fuel - Prims.int_one) s with
          | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
          | FStar_Pervasives_Native.Some s' ->
              eval_code (Block tl) (fuel - Prims.int_one) s')
     | IfElse (ifCond, ifTrue, ifFalse) ->
         if eval_ocmp ifCond s
         then eval_code ifTrue (fuel - Prims.int_one) s
         else eval_code ifFalse (fuel - Prims.int_one) s
     | While (whileCond, whileBody) ->
         if eval_ocmp whileCond s
         then
           (match eval_code whileBody (fuel - Prims.int_one) s with
            | FStar_Pervasives_Native.None -> FStar_Pervasives_Native.None
            | FStar_Pervasives_Native.Some s' ->
                eval_code (While (whileCond, whileBody))
                  (fuel - Prims.int_one) s')
         else FStar_Pervasives_Native.Some s)
