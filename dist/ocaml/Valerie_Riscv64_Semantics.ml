open Prims
type operand =
  | OReg of Valerie_Riscv64_Machine.reg 
  | OImm of Valerie_Defs.nat64 
  | OMem of Valerie_Riscv64_Machine.reg * Prims.int 
  | OLabel of Prims.string 
let uu___is_OReg (projectee : operand) : Prims.bool=
  match projectee with | OReg r -> true | uu___ -> false
let __proj__OReg__item__r (projectee : operand) :
  Valerie_Riscv64_Machine.reg= match projectee with | OReg r -> r
let uu___is_OImm (projectee : operand) : Prims.bool=
  match projectee with | OImm imm -> true | uu___ -> false
let __proj__OImm__item__imm (projectee : operand) : Valerie_Defs.nat64=
  match projectee with | OImm imm -> imm
let uu___is_OMem (projectee : operand) : Prims.bool=
  match projectee with | OMem (base, offset) -> true | uu___ -> false
let __proj__OMem__item__base (projectee : operand) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | OMem (base, offset) -> base
let __proj__OMem__item__offset (projectee : operand) : Prims.int=
  match projectee with | OMem (base, offset) -> offset
let uu___is_OLabel (projectee : operand) : Prims.bool=
  match projectee with | OLabel label -> true | uu___ -> false
let __proj__OLabel__item__label (projectee : operand) : Prims.string=
  match projectee with | OLabel label -> label
type ins =
  | Add of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  operand 
  | Addw of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  operand 
  | Sub of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  operand 
  | Subw of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  operand 
  | And of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  operand 
  | Or of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg * operand
  
  | Xor of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  operand 
  | Sll of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  operand 
  | Sllw of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  operand 
  | Srl of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  operand 
  | Srlw of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  operand 
  | Sra of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  operand 
  | Sraw of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  operand 
  | Slt of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  operand 
  | Sltu of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  operand 
  | Ld of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Prims.int 
  | Lw of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Prims.int 
  | Lwu of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Prims.int 
  | Lh of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Prims.int 
  | Lhu of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Prims.int 
  | Lb of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Prims.int 
  | Lbu of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Prims.int 
  | Sd of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Prims.int 
  | Sw of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Prims.int 
  | Sh of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Prims.int 
  | Sb of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Prims.int 
  | Beq of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Prims.string 
  | Bne of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Prims.string 
  | Blt of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Prims.string 
  | Bge of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Prims.string 
  | Bltu of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Prims.string 
  | Bgeu of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Prims.string 
  | Jal of Valerie_Riscv64_Machine.reg * Prims.string 
  | Jalr of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Prims.int 
  | Ret 
  | Call of Prims.string 
  | Ecall 
  | Lui of Valerie_Riscv64_Machine.reg * Valerie_Defs.nat64 
  | Auipc of Valerie_Riscv64_Machine.reg * Valerie_Defs.nat64 
  | Li of Valerie_Riscv64_Machine.reg * Valerie_Defs.nat64 
  | Mv of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg 
  | Mul of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg 
  | Mulw of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg 
  | Mulh of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg 
  | Mulhu of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg 
  | Mulhsu of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg 
  | Div of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg 
  | Divw of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg 
  | Divu of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg 
  | Divuw of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg 
  | Rem of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg 
  | Remw of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg 
  | Remu of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg 
  | Remuw of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg 
  | Andn of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg 
  | Orn of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg 
  | Xnor of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg 
  | Clz of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg 
  | Ctz of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg 
  | Cpop of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg 
  | Max of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg 
  | Maxu of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg 
  | Min of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg 
  | Minu of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg 
  | Rol of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg 
  | Ror of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  operand 
  | Rev8 of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg 
  | Aes64es of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg 
  | Aes64esm of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg 
  | Aes64ds of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg 
  | Aes64dsm of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg 
  | Aes64ks1i of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Defs.nat64 
  | Aes64ks2 of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg 
  | Aes64im of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg 
  | Sha256sum0 of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg 
  | Sha256sum1 of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg 
  | Sha256sig0 of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg 
  | Sha256sig1 of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg 
  | Sha512sum0 of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg 
  | Sha512sum1 of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg 
  | Sha512sig0 of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg 
  | Sha512sig1 of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg 
  | Sm4ed of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg * Valerie_Defs.nat64 
  | Sm4ks of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg *
  Valerie_Riscv64_Machine.reg * Valerie_Defs.nat64 
  | Sm3p0 of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg 
  | Sm3p1 of Valerie_Riscv64_Machine.reg * Valerie_Riscv64_Machine.reg 
let uu___is_Add (projectee : ins) : Prims.bool=
  match projectee with | Add (dst, s1, s2) -> true | uu___ -> false
let __proj__Add__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Add (dst, s1, s2) -> dst
let __proj__Add__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Add (dst, s1, s2) -> s1
let __proj__Add__item__s2 (projectee : ins) : operand=
  match projectee with | Add (dst, s1, s2) -> s2
let uu___is_Addw (projectee : ins) : Prims.bool=
  match projectee with | Addw (dst, s1, s2) -> true | uu___ -> false
let __proj__Addw__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Addw (dst, s1, s2) -> dst
let __proj__Addw__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Addw (dst, s1, s2) -> s1
let __proj__Addw__item__s2 (projectee : ins) : operand=
  match projectee with | Addw (dst, s1, s2) -> s2
let uu___is_Sub (projectee : ins) : Prims.bool=
  match projectee with | Sub (dst, s1, s2) -> true | uu___ -> false
let __proj__Sub__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sub (dst, s1, s2) -> dst
let __proj__Sub__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sub (dst, s1, s2) -> s1
let __proj__Sub__item__s2 (projectee : ins) : operand=
  match projectee with | Sub (dst, s1, s2) -> s2
let uu___is_Subw (projectee : ins) : Prims.bool=
  match projectee with | Subw (dst, s1, s2) -> true | uu___ -> false
let __proj__Subw__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Subw (dst, s1, s2) -> dst
let __proj__Subw__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Subw (dst, s1, s2) -> s1
let __proj__Subw__item__s2 (projectee : ins) : operand=
  match projectee with | Subw (dst, s1, s2) -> s2
let uu___is_And (projectee : ins) : Prims.bool=
  match projectee with | And (dst, s1, s2) -> true | uu___ -> false
let __proj__And__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | And (dst, s1, s2) -> dst
let __proj__And__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | And (dst, s1, s2) -> s1
let __proj__And__item__s2 (projectee : ins) : operand=
  match projectee with | And (dst, s1, s2) -> s2
let uu___is_Or (projectee : ins) : Prims.bool=
  match projectee with | Or (dst, s1, s2) -> true | uu___ -> false
let __proj__Or__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Or (dst, s1, s2) -> dst
let __proj__Or__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Or (dst, s1, s2) -> s1
let __proj__Or__item__s2 (projectee : ins) : operand=
  match projectee with | Or (dst, s1, s2) -> s2
let uu___is_Xor (projectee : ins) : Prims.bool=
  match projectee with | Xor (dst, s1, s2) -> true | uu___ -> false
let __proj__Xor__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Xor (dst, s1, s2) -> dst
let __proj__Xor__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Xor (dst, s1, s2) -> s1
let __proj__Xor__item__s2 (projectee : ins) : operand=
  match projectee with | Xor (dst, s1, s2) -> s2
let uu___is_Sll (projectee : ins) : Prims.bool=
  match projectee with | Sll (dst, s1, s2) -> true | uu___ -> false
let __proj__Sll__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sll (dst, s1, s2) -> dst
let __proj__Sll__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sll (dst, s1, s2) -> s1
let __proj__Sll__item__s2 (projectee : ins) : operand=
  match projectee with | Sll (dst, s1, s2) -> s2
let uu___is_Sllw (projectee : ins) : Prims.bool=
  match projectee with | Sllw (dst, s1, s2) -> true | uu___ -> false
let __proj__Sllw__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sllw (dst, s1, s2) -> dst
let __proj__Sllw__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sllw (dst, s1, s2) -> s1
let __proj__Sllw__item__s2 (projectee : ins) : operand=
  match projectee with | Sllw (dst, s1, s2) -> s2
let uu___is_Srl (projectee : ins) : Prims.bool=
  match projectee with | Srl (dst, s1, s2) -> true | uu___ -> false
let __proj__Srl__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Srl (dst, s1, s2) -> dst
let __proj__Srl__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Srl (dst, s1, s2) -> s1
let __proj__Srl__item__s2 (projectee : ins) : operand=
  match projectee with | Srl (dst, s1, s2) -> s2
let uu___is_Srlw (projectee : ins) : Prims.bool=
  match projectee with | Srlw (dst, s1, s2) -> true | uu___ -> false
let __proj__Srlw__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Srlw (dst, s1, s2) -> dst
let __proj__Srlw__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Srlw (dst, s1, s2) -> s1
let __proj__Srlw__item__s2 (projectee : ins) : operand=
  match projectee with | Srlw (dst, s1, s2) -> s2
let uu___is_Sra (projectee : ins) : Prims.bool=
  match projectee with | Sra (dst, s1, s2) -> true | uu___ -> false
let __proj__Sra__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sra (dst, s1, s2) -> dst
let __proj__Sra__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sra (dst, s1, s2) -> s1
let __proj__Sra__item__s2 (projectee : ins) : operand=
  match projectee with | Sra (dst, s1, s2) -> s2
let uu___is_Sraw (projectee : ins) : Prims.bool=
  match projectee with | Sraw (dst, s1, s2) -> true | uu___ -> false
let __proj__Sraw__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sraw (dst, s1, s2) -> dst
let __proj__Sraw__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sraw (dst, s1, s2) -> s1
let __proj__Sraw__item__s2 (projectee : ins) : operand=
  match projectee with | Sraw (dst, s1, s2) -> s2
let uu___is_Slt (projectee : ins) : Prims.bool=
  match projectee with | Slt (dst, s1, s2) -> true | uu___ -> false
let __proj__Slt__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Slt (dst, s1, s2) -> dst
let __proj__Slt__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Slt (dst, s1, s2) -> s1
let __proj__Slt__item__s2 (projectee : ins) : operand=
  match projectee with | Slt (dst, s1, s2) -> s2
let uu___is_Sltu (projectee : ins) : Prims.bool=
  match projectee with | Sltu (dst, s1, s2) -> true | uu___ -> false
let __proj__Sltu__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sltu (dst, s1, s2) -> dst
let __proj__Sltu__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sltu (dst, s1, s2) -> s1
let __proj__Sltu__item__s2 (projectee : ins) : operand=
  match projectee with | Sltu (dst, s1, s2) -> s2
let uu___is_Ld (projectee : ins) : Prims.bool=
  match projectee with | Ld (dst, base, offset) -> true | uu___ -> false
let __proj__Ld__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Ld (dst, base, offset) -> dst
let __proj__Ld__item__base (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Ld (dst, base, offset) -> base
let __proj__Ld__item__offset (projectee : ins) : Prims.int=
  match projectee with | Ld (dst, base, offset) -> offset
let uu___is_Lw (projectee : ins) : Prims.bool=
  match projectee with | Lw (dst, base, offset) -> true | uu___ -> false
let __proj__Lw__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Lw (dst, base, offset) -> dst
let __proj__Lw__item__base (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Lw (dst, base, offset) -> base
let __proj__Lw__item__offset (projectee : ins) : Prims.int=
  match projectee with | Lw (dst, base, offset) -> offset
let uu___is_Lwu (projectee : ins) : Prims.bool=
  match projectee with | Lwu (dst, base, offset) -> true | uu___ -> false
let __proj__Lwu__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Lwu (dst, base, offset) -> dst
let __proj__Lwu__item__base (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Lwu (dst, base, offset) -> base
let __proj__Lwu__item__offset (projectee : ins) : Prims.int=
  match projectee with | Lwu (dst, base, offset) -> offset
let uu___is_Lh (projectee : ins) : Prims.bool=
  match projectee with | Lh (dst, base, offset) -> true | uu___ -> false
let __proj__Lh__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Lh (dst, base, offset) -> dst
let __proj__Lh__item__base (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Lh (dst, base, offset) -> base
let __proj__Lh__item__offset (projectee : ins) : Prims.int=
  match projectee with | Lh (dst, base, offset) -> offset
let uu___is_Lhu (projectee : ins) : Prims.bool=
  match projectee with | Lhu (dst, base, offset) -> true | uu___ -> false
let __proj__Lhu__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Lhu (dst, base, offset) -> dst
let __proj__Lhu__item__base (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Lhu (dst, base, offset) -> base
let __proj__Lhu__item__offset (projectee : ins) : Prims.int=
  match projectee with | Lhu (dst, base, offset) -> offset
let uu___is_Lb (projectee : ins) : Prims.bool=
  match projectee with | Lb (dst, base, offset) -> true | uu___ -> false
let __proj__Lb__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Lb (dst, base, offset) -> dst
let __proj__Lb__item__base (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Lb (dst, base, offset) -> base
let __proj__Lb__item__offset (projectee : ins) : Prims.int=
  match projectee with | Lb (dst, base, offset) -> offset
let uu___is_Lbu (projectee : ins) : Prims.bool=
  match projectee with | Lbu (dst, base, offset) -> true | uu___ -> false
let __proj__Lbu__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Lbu (dst, base, offset) -> dst
let __proj__Lbu__item__base (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Lbu (dst, base, offset) -> base
let __proj__Lbu__item__offset (projectee : ins) : Prims.int=
  match projectee with | Lbu (dst, base, offset) -> offset
let uu___is_Sd (projectee : ins) : Prims.bool=
  match projectee with | Sd (src, base, offset) -> true | uu___ -> false
let __proj__Sd__item__src (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sd (src, base, offset) -> src
let __proj__Sd__item__base (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sd (src, base, offset) -> base
let __proj__Sd__item__offset (projectee : ins) : Prims.int=
  match projectee with | Sd (src, base, offset) -> offset
let uu___is_Sw (projectee : ins) : Prims.bool=
  match projectee with | Sw (src, base, offset) -> true | uu___ -> false
let __proj__Sw__item__src (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sw (src, base, offset) -> src
let __proj__Sw__item__base (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sw (src, base, offset) -> base
let __proj__Sw__item__offset (projectee : ins) : Prims.int=
  match projectee with | Sw (src, base, offset) -> offset
let uu___is_Sh (projectee : ins) : Prims.bool=
  match projectee with | Sh (src, base, offset) -> true | uu___ -> false
let __proj__Sh__item__src (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sh (src, base, offset) -> src
let __proj__Sh__item__base (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sh (src, base, offset) -> base
let __proj__Sh__item__offset (projectee : ins) : Prims.int=
  match projectee with | Sh (src, base, offset) -> offset
let uu___is_Sb (projectee : ins) : Prims.bool=
  match projectee with | Sb (src, base, offset) -> true | uu___ -> false
let __proj__Sb__item__src (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sb (src, base, offset) -> src
let __proj__Sb__item__base (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sb (src, base, offset) -> base
let __proj__Sb__item__offset (projectee : ins) : Prims.int=
  match projectee with | Sb (src, base, offset) -> offset
let uu___is_Beq (projectee : ins) : Prims.bool=
  match projectee with | Beq (s1, s2, target) -> true | uu___ -> false
let __proj__Beq__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Beq (s1, s2, target) -> s1
let __proj__Beq__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Beq (s1, s2, target) -> s2
let __proj__Beq__item__target (projectee : ins) : Prims.string=
  match projectee with | Beq (s1, s2, target) -> target
let uu___is_Bne (projectee : ins) : Prims.bool=
  match projectee with | Bne (s1, s2, target) -> true | uu___ -> false
let __proj__Bne__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Bne (s1, s2, target) -> s1
let __proj__Bne__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Bne (s1, s2, target) -> s2
let __proj__Bne__item__target (projectee : ins) : Prims.string=
  match projectee with | Bne (s1, s2, target) -> target
let uu___is_Blt (projectee : ins) : Prims.bool=
  match projectee with | Blt (s1, s2, target) -> true | uu___ -> false
let __proj__Blt__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Blt (s1, s2, target) -> s1
let __proj__Blt__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Blt (s1, s2, target) -> s2
let __proj__Blt__item__target (projectee : ins) : Prims.string=
  match projectee with | Blt (s1, s2, target) -> target
let uu___is_Bge (projectee : ins) : Prims.bool=
  match projectee with | Bge (s1, s2, target) -> true | uu___ -> false
let __proj__Bge__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Bge (s1, s2, target) -> s1
let __proj__Bge__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Bge (s1, s2, target) -> s2
let __proj__Bge__item__target (projectee : ins) : Prims.string=
  match projectee with | Bge (s1, s2, target) -> target
let uu___is_Bltu (projectee : ins) : Prims.bool=
  match projectee with | Bltu (s1, s2, target) -> true | uu___ -> false
let __proj__Bltu__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Bltu (s1, s2, target) -> s1
let __proj__Bltu__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Bltu (s1, s2, target) -> s2
let __proj__Bltu__item__target (projectee : ins) : Prims.string=
  match projectee with | Bltu (s1, s2, target) -> target
let uu___is_Bgeu (projectee : ins) : Prims.bool=
  match projectee with | Bgeu (s1, s2, target) -> true | uu___ -> false
let __proj__Bgeu__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Bgeu (s1, s2, target) -> s1
let __proj__Bgeu__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Bgeu (s1, s2, target) -> s2
let __proj__Bgeu__item__target (projectee : ins) : Prims.string=
  match projectee with | Bgeu (s1, s2, target) -> target
let uu___is_Jal (projectee : ins) : Prims.bool=
  match projectee with | Jal (dst, target) -> true | uu___ -> false
let __proj__Jal__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Jal (dst, target) -> dst
let __proj__Jal__item__target (projectee : ins) : Prims.string=
  match projectee with | Jal (dst, target) -> target
let uu___is_Jalr (projectee : ins) : Prims.bool=
  match projectee with | Jalr (dst, base, offset) -> true | uu___ -> false
let __proj__Jalr__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Jalr (dst, base, offset) -> dst
let __proj__Jalr__item__base (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Jalr (dst, base, offset) -> base
let __proj__Jalr__item__offset (projectee : ins) : Prims.int=
  match projectee with | Jalr (dst, base, offset) -> offset
let uu___is_Ret (projectee : ins) : Prims.bool=
  match projectee with | Ret -> true | uu___ -> false
let uu___is_Call (projectee : ins) : Prims.bool=
  match projectee with | Call target -> true | uu___ -> false
let __proj__Call__item__target (projectee : ins) : Prims.string=
  match projectee with | Call target -> target
let uu___is_Ecall (projectee : ins) : Prims.bool=
  match projectee with | Ecall -> true | uu___ -> false
let uu___is_Lui (projectee : ins) : Prims.bool=
  match projectee with | Lui (dst, imm) -> true | uu___ -> false
let __proj__Lui__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Lui (dst, imm) -> dst
let __proj__Lui__item__imm (projectee : ins) : Valerie_Defs.nat64=
  match projectee with | Lui (dst, imm) -> imm
let uu___is_Auipc (projectee : ins) : Prims.bool=
  match projectee with | Auipc (dst, imm) -> true | uu___ -> false
let __proj__Auipc__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Auipc (dst, imm) -> dst
let __proj__Auipc__item__imm (projectee : ins) : Valerie_Defs.nat64=
  match projectee with | Auipc (dst, imm) -> imm
let uu___is_Li (projectee : ins) : Prims.bool=
  match projectee with | Li (dst, imm) -> true | uu___ -> false
let __proj__Li__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Li (dst, imm) -> dst
let __proj__Li__item__imm (projectee : ins) : Valerie_Defs.nat64=
  match projectee with | Li (dst, imm) -> imm
let uu___is_Mv (projectee : ins) : Prims.bool=
  match projectee with | Mv (dst, src) -> true | uu___ -> false
let __proj__Mv__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Mv (dst, src) -> dst
let __proj__Mv__item__src (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Mv (dst, src) -> src
let uu___is_Mul (projectee : ins) : Prims.bool=
  match projectee with | Mul (dst, s1, s2) -> true | uu___ -> false
let __proj__Mul__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Mul (dst, s1, s2) -> dst
let __proj__Mul__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Mul (dst, s1, s2) -> s1
let __proj__Mul__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Mul (dst, s1, s2) -> s2
let uu___is_Mulw (projectee : ins) : Prims.bool=
  match projectee with | Mulw (dst, s1, s2) -> true | uu___ -> false
let __proj__Mulw__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Mulw (dst, s1, s2) -> dst
let __proj__Mulw__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Mulw (dst, s1, s2) -> s1
let __proj__Mulw__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Mulw (dst, s1, s2) -> s2
let uu___is_Mulh (projectee : ins) : Prims.bool=
  match projectee with | Mulh (dst, s1, s2) -> true | uu___ -> false
let __proj__Mulh__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Mulh (dst, s1, s2) -> dst
let __proj__Mulh__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Mulh (dst, s1, s2) -> s1
let __proj__Mulh__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Mulh (dst, s1, s2) -> s2
let uu___is_Mulhu (projectee : ins) : Prims.bool=
  match projectee with | Mulhu (dst, s1, s2) -> true | uu___ -> false
let __proj__Mulhu__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Mulhu (dst, s1, s2) -> dst
let __proj__Mulhu__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Mulhu (dst, s1, s2) -> s1
let __proj__Mulhu__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Mulhu (dst, s1, s2) -> s2
let uu___is_Mulhsu (projectee : ins) : Prims.bool=
  match projectee with | Mulhsu (dst, s1, s2) -> true | uu___ -> false
let __proj__Mulhsu__item__dst (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Mulhsu (dst, s1, s2) -> dst
let __proj__Mulhsu__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Mulhsu (dst, s1, s2) -> s1
let __proj__Mulhsu__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Mulhsu (dst, s1, s2) -> s2
let uu___is_Div (projectee : ins) : Prims.bool=
  match projectee with | Div (dst, s1, s2) -> true | uu___ -> false
let __proj__Div__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Div (dst, s1, s2) -> dst
let __proj__Div__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Div (dst, s1, s2) -> s1
let __proj__Div__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Div (dst, s1, s2) -> s2
let uu___is_Divw (projectee : ins) : Prims.bool=
  match projectee with | Divw (dst, s1, s2) -> true | uu___ -> false
let __proj__Divw__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Divw (dst, s1, s2) -> dst
let __proj__Divw__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Divw (dst, s1, s2) -> s1
let __proj__Divw__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Divw (dst, s1, s2) -> s2
let uu___is_Divu (projectee : ins) : Prims.bool=
  match projectee with | Divu (dst, s1, s2) -> true | uu___ -> false
let __proj__Divu__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Divu (dst, s1, s2) -> dst
let __proj__Divu__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Divu (dst, s1, s2) -> s1
let __proj__Divu__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Divu (dst, s1, s2) -> s2
let uu___is_Divuw (projectee : ins) : Prims.bool=
  match projectee with | Divuw (dst, s1, s2) -> true | uu___ -> false
let __proj__Divuw__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Divuw (dst, s1, s2) -> dst
let __proj__Divuw__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Divuw (dst, s1, s2) -> s1
let __proj__Divuw__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Divuw (dst, s1, s2) -> s2
let uu___is_Rem (projectee : ins) : Prims.bool=
  match projectee with | Rem (dst, s1, s2) -> true | uu___ -> false
let __proj__Rem__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Rem (dst, s1, s2) -> dst
let __proj__Rem__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Rem (dst, s1, s2) -> s1
let __proj__Rem__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Rem (dst, s1, s2) -> s2
let uu___is_Remw (projectee : ins) : Prims.bool=
  match projectee with | Remw (dst, s1, s2) -> true | uu___ -> false
let __proj__Remw__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Remw (dst, s1, s2) -> dst
let __proj__Remw__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Remw (dst, s1, s2) -> s1
let __proj__Remw__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Remw (dst, s1, s2) -> s2
let uu___is_Remu (projectee : ins) : Prims.bool=
  match projectee with | Remu (dst, s1, s2) -> true | uu___ -> false
let __proj__Remu__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Remu (dst, s1, s2) -> dst
let __proj__Remu__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Remu (dst, s1, s2) -> s1
let __proj__Remu__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Remu (dst, s1, s2) -> s2
let uu___is_Remuw (projectee : ins) : Prims.bool=
  match projectee with | Remuw (dst, s1, s2) -> true | uu___ -> false
let __proj__Remuw__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Remuw (dst, s1, s2) -> dst
let __proj__Remuw__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Remuw (dst, s1, s2) -> s1
let __proj__Remuw__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Remuw (dst, s1, s2) -> s2
let uu___is_Andn (projectee : ins) : Prims.bool=
  match projectee with | Andn (dst, s1, s2) -> true | uu___ -> false
let __proj__Andn__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Andn (dst, s1, s2) -> dst
let __proj__Andn__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Andn (dst, s1, s2) -> s1
let __proj__Andn__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Andn (dst, s1, s2) -> s2
let uu___is_Orn (projectee : ins) : Prims.bool=
  match projectee with | Orn (dst, s1, s2) -> true | uu___ -> false
let __proj__Orn__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Orn (dst, s1, s2) -> dst
let __proj__Orn__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Orn (dst, s1, s2) -> s1
let __proj__Orn__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Orn (dst, s1, s2) -> s2
let uu___is_Xnor (projectee : ins) : Prims.bool=
  match projectee with | Xnor (dst, s1, s2) -> true | uu___ -> false
let __proj__Xnor__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Xnor (dst, s1, s2) -> dst
let __proj__Xnor__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Xnor (dst, s1, s2) -> s1
let __proj__Xnor__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Xnor (dst, s1, s2) -> s2
let uu___is_Clz (projectee : ins) : Prims.bool=
  match projectee with | Clz (dst, src) -> true | uu___ -> false
let __proj__Clz__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Clz (dst, src) -> dst
let __proj__Clz__item__src (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Clz (dst, src) -> src
let uu___is_Ctz (projectee : ins) : Prims.bool=
  match projectee with | Ctz (dst, src) -> true | uu___ -> false
let __proj__Ctz__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Ctz (dst, src) -> dst
let __proj__Ctz__item__src (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Ctz (dst, src) -> src
let uu___is_Cpop (projectee : ins) : Prims.bool=
  match projectee with | Cpop (dst, src) -> true | uu___ -> false
let __proj__Cpop__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Cpop (dst, src) -> dst
let __proj__Cpop__item__src (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Cpop (dst, src) -> src
let uu___is_Max (projectee : ins) : Prims.bool=
  match projectee with | Max (dst, s1, s2) -> true | uu___ -> false
let __proj__Max__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Max (dst, s1, s2) -> dst
let __proj__Max__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Max (dst, s1, s2) -> s1
let __proj__Max__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Max (dst, s1, s2) -> s2
let uu___is_Maxu (projectee : ins) : Prims.bool=
  match projectee with | Maxu (dst, s1, s2) -> true | uu___ -> false
let __proj__Maxu__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Maxu (dst, s1, s2) -> dst
let __proj__Maxu__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Maxu (dst, s1, s2) -> s1
let __proj__Maxu__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Maxu (dst, s1, s2) -> s2
let uu___is_Min (projectee : ins) : Prims.bool=
  match projectee with | Min (dst, s1, s2) -> true | uu___ -> false
let __proj__Min__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Min (dst, s1, s2) -> dst
let __proj__Min__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Min (dst, s1, s2) -> s1
let __proj__Min__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Min (dst, s1, s2) -> s2
let uu___is_Minu (projectee : ins) : Prims.bool=
  match projectee with | Minu (dst, s1, s2) -> true | uu___ -> false
let __proj__Minu__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Minu (dst, s1, s2) -> dst
let __proj__Minu__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Minu (dst, s1, s2) -> s1
let __proj__Minu__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Minu (dst, s1, s2) -> s2
let uu___is_Rol (projectee : ins) : Prims.bool=
  match projectee with | Rol (dst, s1, s2) -> true | uu___ -> false
let __proj__Rol__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Rol (dst, s1, s2) -> dst
let __proj__Rol__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Rol (dst, s1, s2) -> s1
let __proj__Rol__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Rol (dst, s1, s2) -> s2
let uu___is_Ror (projectee : ins) : Prims.bool=
  match projectee with | Ror (dst, s1, s2) -> true | uu___ -> false
let __proj__Ror__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Ror (dst, s1, s2) -> dst
let __proj__Ror__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Ror (dst, s1, s2) -> s1
let __proj__Ror__item__s2 (projectee : ins) : operand=
  match projectee with | Ror (dst, s1, s2) -> s2
let uu___is_Rev8 (projectee : ins) : Prims.bool=
  match projectee with | Rev8 (dst, src) -> true | uu___ -> false
let __proj__Rev8__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Rev8 (dst, src) -> dst
let __proj__Rev8__item__src (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Rev8 (dst, src) -> src
let uu___is_Aes64es (projectee : ins) : Prims.bool=
  match projectee with | Aes64es (dst, s1, s2) -> true | uu___ -> false
let __proj__Aes64es__item__dst (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Aes64es (dst, s1, s2) -> dst
let __proj__Aes64es__item__s1 (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Aes64es (dst, s1, s2) -> s1
let __proj__Aes64es__item__s2 (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Aes64es (dst, s1, s2) -> s2
let uu___is_Aes64esm (projectee : ins) : Prims.bool=
  match projectee with | Aes64esm (dst, s1, s2) -> true | uu___ -> false
let __proj__Aes64esm__item__dst (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Aes64esm (dst, s1, s2) -> dst
let __proj__Aes64esm__item__s1 (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Aes64esm (dst, s1, s2) -> s1
let __proj__Aes64esm__item__s2 (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Aes64esm (dst, s1, s2) -> s2
let uu___is_Aes64ds (projectee : ins) : Prims.bool=
  match projectee with | Aes64ds (dst, s1, s2) -> true | uu___ -> false
let __proj__Aes64ds__item__dst (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Aes64ds (dst, s1, s2) -> dst
let __proj__Aes64ds__item__s1 (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Aes64ds (dst, s1, s2) -> s1
let __proj__Aes64ds__item__s2 (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Aes64ds (dst, s1, s2) -> s2
let uu___is_Aes64dsm (projectee : ins) : Prims.bool=
  match projectee with | Aes64dsm (dst, s1, s2) -> true | uu___ -> false
let __proj__Aes64dsm__item__dst (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Aes64dsm (dst, s1, s2) -> dst
let __proj__Aes64dsm__item__s1 (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Aes64dsm (dst, s1, s2) -> s1
let __proj__Aes64dsm__item__s2 (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Aes64dsm (dst, s1, s2) -> s2
let uu___is_Aes64ks1i (projectee : ins) : Prims.bool=
  match projectee with | Aes64ks1i (dst, src, rnum) -> true | uu___ -> false
let __proj__Aes64ks1i__item__dst (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Aes64ks1i (dst, src, rnum) -> dst
let __proj__Aes64ks1i__item__src (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Aes64ks1i (dst, src, rnum) -> src
let __proj__Aes64ks1i__item__rnum (projectee : ins) : Valerie_Defs.nat64=
  match projectee with | Aes64ks1i (dst, src, rnum) -> rnum
let uu___is_Aes64ks2 (projectee : ins) : Prims.bool=
  match projectee with | Aes64ks2 (dst, s1, s2) -> true | uu___ -> false
let __proj__Aes64ks2__item__dst (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Aes64ks2 (dst, s1, s2) -> dst
let __proj__Aes64ks2__item__s1 (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Aes64ks2 (dst, s1, s2) -> s1
let __proj__Aes64ks2__item__s2 (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Aes64ks2 (dst, s1, s2) -> s2
let uu___is_Aes64im (projectee : ins) : Prims.bool=
  match projectee with | Aes64im (dst, src) -> true | uu___ -> false
let __proj__Aes64im__item__dst (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Aes64im (dst, src) -> dst
let __proj__Aes64im__item__src (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Aes64im (dst, src) -> src
let uu___is_Sha256sum0 (projectee : ins) : Prims.bool=
  match projectee with | Sha256sum0 (dst, src) -> true | uu___ -> false
let __proj__Sha256sum0__item__dst (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Sha256sum0 (dst, src) -> dst
let __proj__Sha256sum0__item__src (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Sha256sum0 (dst, src) -> src
let uu___is_Sha256sum1 (projectee : ins) : Prims.bool=
  match projectee with | Sha256sum1 (dst, src) -> true | uu___ -> false
let __proj__Sha256sum1__item__dst (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Sha256sum1 (dst, src) -> dst
let __proj__Sha256sum1__item__src (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Sha256sum1 (dst, src) -> src
let uu___is_Sha256sig0 (projectee : ins) : Prims.bool=
  match projectee with | Sha256sig0 (dst, src) -> true | uu___ -> false
let __proj__Sha256sig0__item__dst (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Sha256sig0 (dst, src) -> dst
let __proj__Sha256sig0__item__src (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Sha256sig0 (dst, src) -> src
let uu___is_Sha256sig1 (projectee : ins) : Prims.bool=
  match projectee with | Sha256sig1 (dst, src) -> true | uu___ -> false
let __proj__Sha256sig1__item__dst (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Sha256sig1 (dst, src) -> dst
let __proj__Sha256sig1__item__src (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Sha256sig1 (dst, src) -> src
let uu___is_Sha512sum0 (projectee : ins) : Prims.bool=
  match projectee with | Sha512sum0 (dst, src) -> true | uu___ -> false
let __proj__Sha512sum0__item__dst (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Sha512sum0 (dst, src) -> dst
let __proj__Sha512sum0__item__src (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Sha512sum0 (dst, src) -> src
let uu___is_Sha512sum1 (projectee : ins) : Prims.bool=
  match projectee with | Sha512sum1 (dst, src) -> true | uu___ -> false
let __proj__Sha512sum1__item__dst (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Sha512sum1 (dst, src) -> dst
let __proj__Sha512sum1__item__src (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Sha512sum1 (dst, src) -> src
let uu___is_Sha512sig0 (projectee : ins) : Prims.bool=
  match projectee with | Sha512sig0 (dst, src) -> true | uu___ -> false
let __proj__Sha512sig0__item__dst (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Sha512sig0 (dst, src) -> dst
let __proj__Sha512sig0__item__src (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Sha512sig0 (dst, src) -> src
let uu___is_Sha512sig1 (projectee : ins) : Prims.bool=
  match projectee with | Sha512sig1 (dst, src) -> true | uu___ -> false
let __proj__Sha512sig1__item__dst (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Sha512sig1 (dst, src) -> dst
let __proj__Sha512sig1__item__src (projectee : ins) :
  Valerie_Riscv64_Machine.reg=
  match projectee with | Sha512sig1 (dst, src) -> src
let uu___is_Sm4ed (projectee : ins) : Prims.bool=
  match projectee with | Sm4ed (dst, s1, s2, bs) -> true | uu___ -> false
let __proj__Sm4ed__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sm4ed (dst, s1, s2, bs) -> dst
let __proj__Sm4ed__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sm4ed (dst, s1, s2, bs) -> s1
let __proj__Sm4ed__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sm4ed (dst, s1, s2, bs) -> s2
let __proj__Sm4ed__item__bs (projectee : ins) : Valerie_Defs.nat64=
  match projectee with | Sm4ed (dst, s1, s2, bs) -> bs
let uu___is_Sm4ks (projectee : ins) : Prims.bool=
  match projectee with | Sm4ks (dst, s1, s2, bs) -> true | uu___ -> false
let __proj__Sm4ks__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sm4ks (dst, s1, s2, bs) -> dst
let __proj__Sm4ks__item__s1 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sm4ks (dst, s1, s2, bs) -> s1
let __proj__Sm4ks__item__s2 (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sm4ks (dst, s1, s2, bs) -> s2
let __proj__Sm4ks__item__bs (projectee : ins) : Valerie_Defs.nat64=
  match projectee with | Sm4ks (dst, s1, s2, bs) -> bs
let uu___is_Sm3p0 (projectee : ins) : Prims.bool=
  match projectee with | Sm3p0 (dst, src) -> true | uu___ -> false
let __proj__Sm3p0__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sm3p0 (dst, src) -> dst
let __proj__Sm3p0__item__src (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sm3p0 (dst, src) -> src
let uu___is_Sm3p1 (projectee : ins) : Prims.bool=
  match projectee with | Sm3p1 (dst, src) -> true | uu___ -> false
let __proj__Sm3p1__item__dst (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sm3p1 (dst, src) -> dst
let __proj__Sm3p1__item__src (projectee : ins) : Valerie_Riscv64_Machine.reg=
  match projectee with | Sm3p1 (dst, src) -> src
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
let eval_operand (o : operand) (s : Valerie_Riscv64_Machine.state) :
  Valerie_Defs.nat64=
  match o with
  | OReg (Valerie_Riscv64_Machine.Zero) -> Stdint.Uint64.zero
  | OReg r -> s.Valerie_Riscv64_Machine.regs r
  | OImm imm -> imm
  | uu___ -> Stdint.Uint64.zero
let write_reg (dst : Valerie_Riscv64_Machine.reg) (v : Valerie_Defs.nat64)
  (s : Valerie_Riscv64_Machine.state) : Valerie_Riscv64_Machine.state=
  if dst = Valerie_Riscv64_Machine.Zero
  then s
  else
    {
      Valerie_Riscv64_Machine.ok = (s.Valerie_Riscv64_Machine.ok);
      Valerie_Riscv64_Machine.regs =
        ((fun r -> if r = dst then v else s.Valerie_Riscv64_Machine.regs r))
    }
let safe_shift64 (amount : Valerie_Defs.nat64) : Valerie_Defs.nat32=
  FStar_UInt32.uint_to_t ((mod) (FStar_UInt64.v amount) (Prims.of_int 64))
let eval_ins (i : ins) (s : Valerie_Riscv64_Machine.state) :
  Valerie_Riscv64_Machine.state=
  match i with
  | Add (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand s2 s in
      write_reg dst (FStar_UInt64.add_mod v1 v2) s
  | Addw (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand s2 s in
      let sum = FStar_UInt64.add_mod v1 v2 in
      let low =
        FStar_UInt64.logand sum (Stdint.Uint64.of_string "0xffffffff") in
      let ext =
        if (FStar_UInt64.v low) >= (Prims.parse_int "2147483648")
        then
          FStar_UInt64.logor low
            (Stdint.Uint64.of_string "0xffffffff00000000")
        else low in
      write_reg dst ext s
  | Sub (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand s2 s in
      write_reg dst (FStar_UInt64.sub_mod v1 v2) s
  | Subw (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand s2 s in
      let diff = FStar_UInt64.sub_mod v1 v2 in
      let low =
        FStar_UInt64.logand diff (Stdint.Uint64.of_string "0xffffffff") in
      let ext =
        if (FStar_UInt64.v low) >= (Prims.parse_int "2147483648")
        then
          FStar_UInt64.logor low
            (Stdint.Uint64.of_string "0xffffffff00000000")
        else low in
      write_reg dst ext s
  | Xor (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand s2 s in
      write_reg dst (Valerie_Defs.nat64_xor v1 v2) s
  | And (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand s2 s in
      write_reg dst (FStar_UInt64.logand v1 v2) s
  | Or (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand s2 s in
      write_reg dst (FStar_UInt64.logor v1 v2) s
  | Sll (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let amt = safe_shift64 (eval_operand s2 s) in
      write_reg dst (FStar_UInt64.shift_left v1 amt) s
  | Srl (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let amt = safe_shift64 (eval_operand s2 s) in
      write_reg dst (FStar_UInt64.shift_right v1 amt) s
  | Sra (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let amt = safe_shift64 (eval_operand s2 s) in
      write_reg dst (FStar_UInt64.shift_right v1 amt) s
  | Slt (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand s2 s in
      let res =
        if FStar_UInt64.lt v1 v2
        then Stdint.Uint64.one
        else Stdint.Uint64.zero in
      write_reg dst res s
  | Sltu (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand s2 s in
      let res =
        if FStar_UInt64.lt v1 v2
        then Stdint.Uint64.one
        else Stdint.Uint64.zero in
      write_reg dst res s
  | Lui (dst, imm) ->
      let shifted = FStar_UInt64.shift_left imm (Stdint.Uint32.of_int 12) in
      write_reg dst shifted s
  | Auipc (dst, imm) ->
      let shifted = FStar_UInt64.shift_left imm (Stdint.Uint32.of_int 12) in
      write_reg dst shifted s
  | Li (dst, imm) -> write_reg dst imm s
  | Mv (dst, src) -> write_reg dst (eval_operand (OReg src) s) s
  | Mul (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand (OReg s2) s in
      write_reg dst (FStar_UInt64.mul_mod v1 v2) s
  | Mulw (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand (OReg s2) s in
      let prod = FStar_UInt64.mul_mod v1 v2 in
      let low =
        FStar_UInt64.logand prod (Stdint.Uint64.of_string "0xffffffff") in
      let ext =
        if (FStar_UInt64.v low) >= (Prims.parse_int "2147483648")
        then
          FStar_UInt64.logor low
            (Stdint.Uint64.of_string "0xffffffff00000000")
        else low in
      write_reg dst ext s
  | Div (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand (OReg s2) s in
      let res =
        if v2 = Stdint.Uint64.zero
        then Stdint.Uint64.zero
        else FStar_UInt64.div v1 v2 in
      write_reg dst res s
  | Divu (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand (OReg s2) s in
      let res =
        if v2 = Stdint.Uint64.zero
        then Stdint.Uint64.zero
        else FStar_UInt64.div v1 v2 in
      write_reg dst res s
  | Rem (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand (OReg s2) s in
      let res =
        if v2 = Stdint.Uint64.zero
        then Stdint.Uint64.zero
        else FStar_UInt64.rem v1 v2 in
      write_reg dst res s
  | Remu (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand (OReg s2) s in
      let res =
        if v2 = Stdint.Uint64.zero
        then Stdint.Uint64.zero
        else FStar_UInt64.rem v1 v2 in
      write_reg dst res s
  | Andn (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand (OReg s2) s in
      write_reg dst (FStar_UInt64.logand v1 (FStar_UInt64.lognot v2)) s
  | Orn (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand (OReg s2) s in
      write_reg dst (FStar_UInt64.logor v1 (FStar_UInt64.lognot v2)) s
  | Xnor (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand (OReg s2) s in
      write_reg dst (FStar_UInt64.lognot (Valerie_Defs.nat64_xor v1 v2)) s
  | Clz (dst, src) -> write_reg dst Stdint.Uint64.zero s
  | Ctz (dst, src) -> write_reg dst Stdint.Uint64.zero s
  | Cpop (dst, src) -> write_reg dst Stdint.Uint64.zero s
  | Max (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand (OReg s2) s in
      write_reg dst (if FStar_UInt64.gte v1 v2 then v1 else v2) s
  | Maxu (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand (OReg s2) s in
      write_reg dst (if FStar_UInt64.gte v1 v2 then v1 else v2) s
  | Min (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand (OReg s2) s in
      write_reg dst (if FStar_UInt64.lt v1 v2 then v1 else v2) s
  | Minu (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand (OReg s2) s in
      write_reg dst (if FStar_UInt64.lt v1 v2 then v1 else v2) s
  | Rol (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let shift =
        (mod) (FStar_UInt64.v (eval_operand (OReg s2) s)) (Prims.of_int 64) in
      let res =
        if shift = Prims.int_zero
        then v1
        else
          (let s11 = FStar_UInt32.uint_to_t shift in
           let s21 = FStar_UInt32.uint_to_t ((Prims.of_int 64) - shift) in
           FStar_UInt64.logor (FStar_UInt64.shift_left v1 s11)
             (FStar_UInt64.shift_right v1 s21)) in
      write_reg dst res s
  | Ror (dst, s1, s2) ->
      let v1 = eval_operand (OReg s1) s in
      let shift =
        (mod) (FStar_UInt64.v (eval_operand s2 s)) (Prims.of_int 64) in
      let res =
        if shift = Prims.int_zero
        then v1
        else
          (let s11 = FStar_UInt32.uint_to_t shift in
           let s21 = FStar_UInt32.uint_to_t ((Prims.of_int 64) - shift) in
           FStar_UInt64.logor (FStar_UInt64.shift_right v1 s11)
             (FStar_UInt64.shift_left v1 s21)) in
      write_reg dst res s
  | Rev8 (dst, src) ->
      let v = eval_operand (OReg src) s in
      let b0 = FStar_UInt64.logand v (Stdint.Uint64.of_int 0xff) in
      let b1 =
        FStar_UInt64.logand
          (FStar_UInt64.shift_right v (Stdint.Uint32.of_int 8))
          (Stdint.Uint64.of_int 0xff) in
      let b2 =
        FStar_UInt64.logand
          (FStar_UInt64.shift_right v (Stdint.Uint32.of_int 16))
          (Stdint.Uint64.of_int 0xff) in
      let b3 =
        FStar_UInt64.logand
          (FStar_UInt64.shift_right v (Stdint.Uint32.of_int 24))
          (Stdint.Uint64.of_int 0xff) in
      let b4 =
        FStar_UInt64.logand
          (FStar_UInt64.shift_right v (Stdint.Uint32.of_int 32))
          (Stdint.Uint64.of_int 0xff) in
      let b5 =
        FStar_UInt64.logand
          (FStar_UInt64.shift_right v (Stdint.Uint32.of_int 40))
          (Stdint.Uint64.of_int 0xff) in
      let b6 =
        FStar_UInt64.logand
          (FStar_UInt64.shift_right v (Stdint.Uint32.of_int 48))
          (Stdint.Uint64.of_int 0xff) in
      let b7 =
        FStar_UInt64.logand
          (FStar_UInt64.shift_right v (Stdint.Uint32.of_int 56))
          (Stdint.Uint64.of_int 0xff) in
      let res =
        FStar_UInt64.logor
          (FStar_UInt64.shift_left b0 (Stdint.Uint32.of_int 56))
          (FStar_UInt64.logor
             (FStar_UInt64.shift_left b1 (Stdint.Uint32.of_int 48))
             (FStar_UInt64.logor
                (FStar_UInt64.shift_left b2 (Stdint.Uint32.of_int 40))
                (FStar_UInt64.logor
                   (FStar_UInt64.shift_left b3 (Stdint.Uint32.of_int 32))
                   (FStar_UInt64.logor
                      (FStar_UInt64.shift_left b4 (Stdint.Uint32.of_int 24))
                      (FStar_UInt64.logor
                         (FStar_UInt64.shift_left b5
                            (Stdint.Uint32.of_int 16))
                         (FStar_UInt64.logor
                            (FStar_UInt64.shift_left b6
                               (Stdint.Uint32.of_int 8)) b7)))))) in
      write_reg dst res s
  | Ld (dst, uu___, uu___1) -> write_reg dst Stdint.Uint64.zero s
  | Lw (dst, uu___, uu___1) -> write_reg dst Stdint.Uint64.zero s
  | Lwu (dst, uu___, uu___1) -> write_reg dst Stdint.Uint64.zero s
  | Lh (dst, uu___, uu___1) -> write_reg dst Stdint.Uint64.zero s
  | Lhu (dst, uu___, uu___1) -> write_reg dst Stdint.Uint64.zero s
  | Lb (dst, uu___, uu___1) -> write_reg dst Stdint.Uint64.zero s
  | Lbu (dst, uu___, uu___1) -> write_reg dst Stdint.Uint64.zero s
  | Sd (uu___, uu___1, uu___2) -> s
  | Sw (uu___, uu___1, uu___2) -> s
  | Sh (uu___, uu___1, uu___2) -> s
  | Sb (uu___, uu___1, uu___2) -> s
  | Beq (uu___, uu___1, uu___2) -> s
  | Bne (uu___, uu___1, uu___2) -> s
  | Blt (uu___, uu___1, uu___2) -> s
  | Bge (uu___, uu___1, uu___2) -> s
  | Bltu (uu___, uu___1, uu___2) -> s
  | Bgeu (uu___, uu___1, uu___2) -> s
  | Jal (dst, uu___) -> write_reg dst Stdint.Uint64.zero s
  | Jalr (dst, uu___, uu___1) -> write_reg dst Stdint.Uint64.zero s
  | Ret -> s
  | Call uu___ -> s
  | Ecall -> s
  | Aes64es (dst, uu___, uu___1) -> write_reg dst Stdint.Uint64.zero s
  | Aes64esm (dst, uu___, uu___1) -> write_reg dst Stdint.Uint64.zero s
  | Aes64ds (dst, uu___, uu___1) -> write_reg dst Stdint.Uint64.zero s
  | Aes64dsm (dst, uu___, uu___1) -> write_reg dst Stdint.Uint64.zero s
  | Aes64ks1i (dst, uu___, uu___1) -> write_reg dst Stdint.Uint64.zero s
  | Aes64ks2 (dst, uu___, uu___1) -> write_reg dst Stdint.Uint64.zero s
  | Aes64im (dst, uu___) -> write_reg dst Stdint.Uint64.zero s
  | Sha256sum0 (dst, uu___) -> write_reg dst Stdint.Uint64.zero s
  | Sha256sum1 (dst, uu___) -> write_reg dst Stdint.Uint64.zero s
  | Sha256sig0 (dst, uu___) -> write_reg dst Stdint.Uint64.zero s
  | Sha256sig1 (dst, uu___) -> write_reg dst Stdint.Uint64.zero s
  | Sha512sum0 (dst, uu___) -> write_reg dst Stdint.Uint64.zero s
  | Sha512sum1 (dst, uu___) -> write_reg dst Stdint.Uint64.zero s
  | Sha512sig0 (dst, uu___) -> write_reg dst Stdint.Uint64.zero s
  | Sha512sig1 (dst, uu___) -> write_reg dst Stdint.Uint64.zero s
  | Sm4ed (dst, uu___, uu___1, uu___2) -> write_reg dst Stdint.Uint64.zero s
  | Sm4ks (dst, uu___, uu___1, uu___2) -> write_reg dst Stdint.Uint64.zero s
  | Sm3p0 (dst, uu___) -> write_reg dst Stdint.Uint64.zero s
  | Sm3p1 (dst, uu___) -> write_reg dst Stdint.Uint64.zero s
  | uu___ -> s
let eval_ocmp (c : ocmp) (s : Valerie_Riscv64_Machine.state) : Prims.bool=
  match c with
  | OEq (o1, o2) -> (eval_operand o1 s) = (eval_operand o2 s)
  | ONe (o1, o2) -> (eval_operand o1 s) <> (eval_operand o2 s)
  | OLt (o1, o2) -> FStar_UInt64.lt (eval_operand o1 s) (eval_operand o2 s)
  | OGe (o1, o2) -> FStar_UInt64.gte (eval_operand o1 s) (eval_operand o2 s)
let rec eval_code (c : code) (fuel : Prims.nat)
  (s : Valerie_Riscv64_Machine.state) :
  Valerie_Riscv64_Machine.state FStar_Pervasives_Native.option=
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
