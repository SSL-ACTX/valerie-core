open Prims
type reg =
  | Zero 
  | RA 
  | SP 
  | GP 
  | TP 
  | T0 
  | T1 
  | T2 
  | S0 
  | S1 
  | A0 
  | A1 
  | A2 
  | A3 
  | A4 
  | A5 
  | A6 
  | A7 
  | S2 
  | S3 
  | S4 
  | S5 
  | S6 
  | S7 
  | S8 
  | S9 
  | S10 
  | S11 
  | T3 
  | T4 
  | T5 
  | T6 
let uu___is_Zero (projectee : reg) : Prims.bool=
  match projectee with | Zero -> true | uu___ -> false
let uu___is_RA (projectee : reg) : Prims.bool=
  match projectee with | RA -> true | uu___ -> false
let uu___is_SP (projectee : reg) : Prims.bool=
  match projectee with | SP -> true | uu___ -> false
let uu___is_GP (projectee : reg) : Prims.bool=
  match projectee with | GP -> true | uu___ -> false
let uu___is_TP (projectee : reg) : Prims.bool=
  match projectee with | TP -> true | uu___ -> false
let uu___is_T0 (projectee : reg) : Prims.bool=
  match projectee with | T0 -> true | uu___ -> false
let uu___is_T1 (projectee : reg) : Prims.bool=
  match projectee with | T1 -> true | uu___ -> false
let uu___is_T2 (projectee : reg) : Prims.bool=
  match projectee with | T2 -> true | uu___ -> false
let uu___is_S0 (projectee : reg) : Prims.bool=
  match projectee with | S0 -> true | uu___ -> false
let uu___is_S1 (projectee : reg) : Prims.bool=
  match projectee with | S1 -> true | uu___ -> false
let uu___is_A0 (projectee : reg) : Prims.bool=
  match projectee with | A0 -> true | uu___ -> false
let uu___is_A1 (projectee : reg) : Prims.bool=
  match projectee with | A1 -> true | uu___ -> false
let uu___is_A2 (projectee : reg) : Prims.bool=
  match projectee with | A2 -> true | uu___ -> false
let uu___is_A3 (projectee : reg) : Prims.bool=
  match projectee with | A3 -> true | uu___ -> false
let uu___is_A4 (projectee : reg) : Prims.bool=
  match projectee with | A4 -> true | uu___ -> false
let uu___is_A5 (projectee : reg) : Prims.bool=
  match projectee with | A5 -> true | uu___ -> false
let uu___is_A6 (projectee : reg) : Prims.bool=
  match projectee with | A6 -> true | uu___ -> false
let uu___is_A7 (projectee : reg) : Prims.bool=
  match projectee with | A7 -> true | uu___ -> false
let uu___is_S2 (projectee : reg) : Prims.bool=
  match projectee with | S2 -> true | uu___ -> false
let uu___is_S3 (projectee : reg) : Prims.bool=
  match projectee with | S3 -> true | uu___ -> false
let uu___is_S4 (projectee : reg) : Prims.bool=
  match projectee with | S4 -> true | uu___ -> false
let uu___is_S5 (projectee : reg) : Prims.bool=
  match projectee with | S5 -> true | uu___ -> false
let uu___is_S6 (projectee : reg) : Prims.bool=
  match projectee with | S6 -> true | uu___ -> false
let uu___is_S7 (projectee : reg) : Prims.bool=
  match projectee with | S7 -> true | uu___ -> false
let uu___is_S8 (projectee : reg) : Prims.bool=
  match projectee with | S8 -> true | uu___ -> false
let uu___is_S9 (projectee : reg) : Prims.bool=
  match projectee with | S9 -> true | uu___ -> false
let uu___is_S10 (projectee : reg) : Prims.bool=
  match projectee with | S10 -> true | uu___ -> false
let uu___is_S11 (projectee : reg) : Prims.bool=
  match projectee with | S11 -> true | uu___ -> false
let uu___is_T3 (projectee : reg) : Prims.bool=
  match projectee with | T3 -> true | uu___ -> false
let uu___is_T4 (projectee : reg) : Prims.bool=
  match projectee with | T4 -> true | uu___ -> false
let uu___is_T5 (projectee : reg) : Prims.bool=
  match projectee with | T5 -> true | uu___ -> false
let uu___is_T6 (projectee : reg) : Prims.bool=
  match projectee with | T6 -> true | uu___ -> false
type state = {
  ok: Prims.bool ;
  regs: reg -> Valerie_Defs.nat64 }
let __proj__Mkstate__item__ok (projectee : state) : Prims.bool=
  match projectee with | { ok; regs;_} -> ok
let __proj__Mkstate__item__regs (projectee : state) :
  reg -> Valerie_Defs.nat64= match projectee with | { ok; regs;_} -> regs
