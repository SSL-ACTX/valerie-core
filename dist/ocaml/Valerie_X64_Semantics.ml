open Prims
type operand =
  | OReg of Valerie_X64_Machine.reg * Valerie_X64_Machine.reg_width 
  | OXmm of Valerie_X64_Machine.xmm 
  | OYmm of Valerie_X64_Machine.ymm 
  | OImm of Valerie_Defs.nat64 
  | OMem of Valerie_X64_Machine.maddr 
let uu___is_OReg (projectee : operand) : Prims.bool=
  match projectee with | OReg (r, w) -> true | uu___ -> false
let __proj__OReg__item__r (projectee : operand) : Valerie_X64_Machine.reg=
  match projectee with | OReg (r, w) -> r
let __proj__OReg__item__w (projectee : operand) :
  Valerie_X64_Machine.reg_width= match projectee with | OReg (r, w) -> w
let uu___is_OXmm (projectee : operand) : Prims.bool=
  match projectee with | OXmm x -> true | uu___ -> false
let __proj__OXmm__item__x (projectee : operand) : Valerie_X64_Machine.xmm=
  match projectee with | OXmm x -> x
let uu___is_OYmm (projectee : operand) : Prims.bool=
  match projectee with | OYmm y -> true | uu___ -> false
let __proj__OYmm__item__y (projectee : operand) : Valerie_X64_Machine.ymm=
  match projectee with | OYmm y -> y
let uu___is_OImm (projectee : operand) : Prims.bool=
  match projectee with | OImm imm -> true | uu___ -> false
let __proj__OImm__item__imm (projectee : operand) : Valerie_Defs.nat64=
  match projectee with | OImm imm -> imm
let uu___is_OMem (projectee : operand) : Prims.bool=
  match projectee with | OMem m -> true | uu___ -> false
let __proj__OMem__item__m (projectee : operand) : Valerie_X64_Machine.maddr=
  match projectee with | OMem m -> m
type ins =
  | Mov64 of Valerie_X64_Machine.reg * operand 
  | Mov32 of Valerie_X64_Machine.reg * operand 
  | Add64 of Valerie_X64_Machine.reg * operand 
  | Adc64 of Valerie_X64_Machine.reg * operand 
  | Adcx64 of Valerie_X64_Machine.reg * operand 
  | Adox64 of Valerie_X64_Machine.reg * operand 
  | Sub64 of Valerie_X64_Machine.reg * operand 
  | Sbb64 of Valerie_X64_Machine.reg * operand 
  | Xor64 of Valerie_X64_Machine.reg * operand 
  | And64 of Valerie_X64_Machine.reg * operand 
  | Or64 of Valerie_X64_Machine.reg * operand 
  | Not64 of Valerie_X64_Machine.reg 
  | Neg64 of Valerie_X64_Machine.reg 
  | Shl64 of Valerie_X64_Machine.reg * operand 
  | Shr64 of Valerie_X64_Machine.reg * operand 
  | Sar64 of Valerie_X64_Machine.reg * operand 
  | Rol64 of Valerie_X64_Machine.reg * operand 
  | Ror64 of Valerie_X64_Machine.reg * operand 
  | Bswap64 of Valerie_X64_Machine.reg 
  | Cmp64 of Valerie_X64_Machine.reg * operand 
  | Test64 of Valerie_X64_Machine.reg * operand 
  | Lea64 of Valerie_X64_Machine.reg * Valerie_X64_Machine.maddr 
  | Imul64 of Valerie_X64_Machine.reg * operand 
  | Mul64 of operand 
  | Mulx64 of Valerie_X64_Machine.reg * Valerie_X64_Machine.reg * operand 
  | Andn64 of Valerie_X64_Machine.reg * Valerie_X64_Machine.reg * operand 
  | Rorx64 of Valerie_X64_Machine.reg * operand * Valerie_Defs.nat8 
  | Sarx64 of Valerie_X64_Machine.reg * operand * Valerie_X64_Machine.reg 
  | Shlx64 of Valerie_X64_Machine.reg * operand * Valerie_X64_Machine.reg 
  | Shrx64 of Valerie_X64_Machine.reg * operand * Valerie_X64_Machine.reg 
  | Bsf64 of Valerie_X64_Machine.reg * operand 
  | Bsr64 of Valerie_X64_Machine.reg * operand 
  | Tzcnt64 of Valerie_X64_Machine.reg * operand 
  | Lzcnt64 of Valerie_X64_Machine.reg * operand 
  | Popcnt64 of Valerie_X64_Machine.reg * operand 
  | Cmov64 of Valerie_X64_Machine.cond_code * Valerie_X64_Machine.reg *
  operand 
  | Jmp of Prims.string 
  | Jcc of Valerie_X64_Machine.cond_code * Prims.string 
  | Call of Prims.string 
  | Ret 
  | Push64 of operand 
  | Pop64 of Valerie_X64_Machine.reg 
  | Aesenc of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Aesenclast of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Aesdec of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Aesdeclast of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Aesimc of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Aeskeygenassist of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm *
  Valerie_Defs.nat8 
  | Vaesenc of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm *
  Valerie_X64_Machine.xmm 
  | Vaesdec of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm *
  Valerie_X64_Machine.xmm 
  | Pclmulqdq of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm *
  Valerie_Defs.nat8 
  | Vpclmulqdq of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm *
  Valerie_X64_Machine.xmm * Valerie_Defs.nat8 
  | Sha1rnds4 of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm *
  Valerie_Defs.nat8 
  | Sha1nexte of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Sha1msg1 of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Sha1msg2 of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Sha256rnds2 of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Sha256msg1 of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Sha256msg2 of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Movdqu of Valerie_X64_Machine.xmm * operand 
  | Movdqa of Valerie_X64_Machine.xmm * operand 
  | Vmovdqu of Valerie_X64_Machine.xmm * operand 
  | Vmovdqa of Valerie_X64_Machine.xmm * operand 
  | Pxor of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Vpxor of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm *
  Valerie_X64_Machine.xmm 
  | Pand of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Por of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Paddd of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Psubd of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Paddq of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Psubq of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Paddb of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Psubb of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Paddw of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Psubw of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Pslld of Valerie_X64_Machine.xmm * Valerie_Defs.nat8 
  | Psrld of Valerie_X64_Machine.xmm * Valerie_Defs.nat8 
  | Psllq of Valerie_X64_Machine.xmm * Valerie_Defs.nat8 
  | Psrlq of Valerie_X64_Machine.xmm * Valerie_Defs.nat8 
  | Pshufd of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm *
  Valerie_Defs.nat8 
  | Pshufb of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Palignr of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm *
  Valerie_Defs.nat8 
  | Punpcklbw of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Punpckhbw of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Punpcklwd of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Punpckhwd of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Punpckldq of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Punpckhdq of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Punpcklqdq of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Punpckhqdq of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Pcmpeqd of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Pcmpeqb of Valerie_X64_Machine.xmm * Valerie_X64_Machine.xmm 
  | Pextrq of Valerie_X64_Machine.reg * Valerie_X64_Machine.xmm *
  Valerie_Defs.nat8 
  | Pextrd of Valerie_X64_Machine.reg * Valerie_X64_Machine.xmm *
  Valerie_Defs.nat8 
  | Pinsrd of Valerie_X64_Machine.xmm * Valerie_X64_Machine.reg *
  Valerie_Defs.nat8 
  | Pinsrq of Valerie_X64_Machine.xmm * Valerie_X64_Machine.reg *
  Valerie_Defs.nat8 
let uu___is_Mov64 (projectee : ins) : Prims.bool=
  match projectee with | Mov64 (dst, src) -> true | uu___ -> false
let __proj__Mov64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Mov64 (dst, src) -> dst
let __proj__Mov64__item__src (projectee : ins) : operand=
  match projectee with | Mov64 (dst, src) -> src
let uu___is_Mov32 (projectee : ins) : Prims.bool=
  match projectee with | Mov32 (dst, src) -> true | uu___ -> false
let __proj__Mov32__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Mov32 (dst, src) -> dst
let __proj__Mov32__item__src (projectee : ins) : operand=
  match projectee with | Mov32 (dst, src) -> src
let uu___is_Add64 (projectee : ins) : Prims.bool=
  match projectee with | Add64 (dst, src) -> true | uu___ -> false
let __proj__Add64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Add64 (dst, src) -> dst
let __proj__Add64__item__src (projectee : ins) : operand=
  match projectee with | Add64 (dst, src) -> src
let uu___is_Adc64 (projectee : ins) : Prims.bool=
  match projectee with | Adc64 (dst, src) -> true | uu___ -> false
let __proj__Adc64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Adc64 (dst, src) -> dst
let __proj__Adc64__item__src (projectee : ins) : operand=
  match projectee with | Adc64 (dst, src) -> src
let uu___is_Adcx64 (projectee : ins) : Prims.bool=
  match projectee with | Adcx64 (dst, src) -> true | uu___ -> false
let __proj__Adcx64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Adcx64 (dst, src) -> dst
let __proj__Adcx64__item__src (projectee : ins) : operand=
  match projectee with | Adcx64 (dst, src) -> src
let uu___is_Adox64 (projectee : ins) : Prims.bool=
  match projectee with | Adox64 (dst, src) -> true | uu___ -> false
let __proj__Adox64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Adox64 (dst, src) -> dst
let __proj__Adox64__item__src (projectee : ins) : operand=
  match projectee with | Adox64 (dst, src) -> src
let uu___is_Sub64 (projectee : ins) : Prims.bool=
  match projectee with | Sub64 (dst, src) -> true | uu___ -> false
let __proj__Sub64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Sub64 (dst, src) -> dst
let __proj__Sub64__item__src (projectee : ins) : operand=
  match projectee with | Sub64 (dst, src) -> src
let uu___is_Sbb64 (projectee : ins) : Prims.bool=
  match projectee with | Sbb64 (dst, src) -> true | uu___ -> false
let __proj__Sbb64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Sbb64 (dst, src) -> dst
let __proj__Sbb64__item__src (projectee : ins) : operand=
  match projectee with | Sbb64 (dst, src) -> src
let uu___is_Xor64 (projectee : ins) : Prims.bool=
  match projectee with | Xor64 (dst, src) -> true | uu___ -> false
let __proj__Xor64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Xor64 (dst, src) -> dst
let __proj__Xor64__item__src (projectee : ins) : operand=
  match projectee with | Xor64 (dst, src) -> src
let uu___is_And64 (projectee : ins) : Prims.bool=
  match projectee with | And64 (dst, src) -> true | uu___ -> false
let __proj__And64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | And64 (dst, src) -> dst
let __proj__And64__item__src (projectee : ins) : operand=
  match projectee with | And64 (dst, src) -> src
let uu___is_Or64 (projectee : ins) : Prims.bool=
  match projectee with | Or64 (dst, src) -> true | uu___ -> false
let __proj__Or64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Or64 (dst, src) -> dst
let __proj__Or64__item__src (projectee : ins) : operand=
  match projectee with | Or64 (dst, src) -> src
let uu___is_Not64 (projectee : ins) : Prims.bool=
  match projectee with | Not64 dst -> true | uu___ -> false
let __proj__Not64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Not64 dst -> dst
let uu___is_Neg64 (projectee : ins) : Prims.bool=
  match projectee with | Neg64 dst -> true | uu___ -> false
let __proj__Neg64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Neg64 dst -> dst
let uu___is_Shl64 (projectee : ins) : Prims.bool=
  match projectee with | Shl64 (dst, count) -> true | uu___ -> false
let __proj__Shl64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Shl64 (dst, count) -> dst
let __proj__Shl64__item__count (projectee : ins) : operand=
  match projectee with | Shl64 (dst, count) -> count
let uu___is_Shr64 (projectee : ins) : Prims.bool=
  match projectee with | Shr64 (dst, count) -> true | uu___ -> false
let __proj__Shr64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Shr64 (dst, count) -> dst
let __proj__Shr64__item__count (projectee : ins) : operand=
  match projectee with | Shr64 (dst, count) -> count
let uu___is_Sar64 (projectee : ins) : Prims.bool=
  match projectee with | Sar64 (dst, count) -> true | uu___ -> false
let __proj__Sar64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Sar64 (dst, count) -> dst
let __proj__Sar64__item__count (projectee : ins) : operand=
  match projectee with | Sar64 (dst, count) -> count
let uu___is_Rol64 (projectee : ins) : Prims.bool=
  match projectee with | Rol64 (dst, count) -> true | uu___ -> false
let __proj__Rol64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Rol64 (dst, count) -> dst
let __proj__Rol64__item__count (projectee : ins) : operand=
  match projectee with | Rol64 (dst, count) -> count
let uu___is_Ror64 (projectee : ins) : Prims.bool=
  match projectee with | Ror64 (dst, count) -> true | uu___ -> false
let __proj__Ror64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Ror64 (dst, count) -> dst
let __proj__Ror64__item__count (projectee : ins) : operand=
  match projectee with | Ror64 (dst, count) -> count
let uu___is_Bswap64 (projectee : ins) : Prims.bool=
  match projectee with | Bswap64 dst -> true | uu___ -> false
let __proj__Bswap64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Bswap64 dst -> dst
let uu___is_Cmp64 (projectee : ins) : Prims.bool=
  match projectee with | Cmp64 (s1, s2) -> true | uu___ -> false
let __proj__Cmp64__item__s1 (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Cmp64 (s1, s2) -> s1
let __proj__Cmp64__item__s2 (projectee : ins) : operand=
  match projectee with | Cmp64 (s1, s2) -> s2
let uu___is_Test64 (projectee : ins) : Prims.bool=
  match projectee with | Test64 (s1, s2) -> true | uu___ -> false
let __proj__Test64__item__s1 (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Test64 (s1, s2) -> s1
let __proj__Test64__item__s2 (projectee : ins) : operand=
  match projectee with | Test64 (s1, s2) -> s2
let uu___is_Lea64 (projectee : ins) : Prims.bool=
  match projectee with | Lea64 (dst, addr) -> true | uu___ -> false
let __proj__Lea64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Lea64 (dst, addr) -> dst
let __proj__Lea64__item__addr (projectee : ins) : Valerie_X64_Machine.maddr=
  match projectee with | Lea64 (dst, addr) -> addr
let uu___is_Imul64 (projectee : ins) : Prims.bool=
  match projectee with | Imul64 (dst, src) -> true | uu___ -> false
let __proj__Imul64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Imul64 (dst, src) -> dst
let __proj__Imul64__item__src (projectee : ins) : operand=
  match projectee with | Imul64 (dst, src) -> src
let uu___is_Mul64 (projectee : ins) : Prims.bool=
  match projectee with | Mul64 src -> true | uu___ -> false
let __proj__Mul64__item__src (projectee : ins) : operand=
  match projectee with | Mul64 src -> src
let uu___is_Mulx64 (projectee : ins) : Prims.bool=
  match projectee with | Mulx64 (hi, lo, src) -> true | uu___ -> false
let __proj__Mulx64__item__hi (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Mulx64 (hi, lo, src) -> hi
let __proj__Mulx64__item__lo (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Mulx64 (hi, lo, src) -> lo
let __proj__Mulx64__item__src (projectee : ins) : operand=
  match projectee with | Mulx64 (hi, lo, src) -> src
let uu___is_Andn64 (projectee : ins) : Prims.bool=
  match projectee with | Andn64 (dst, s1, s2) -> true | uu___ -> false
let __proj__Andn64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Andn64 (dst, s1, s2) -> dst
let __proj__Andn64__item__s1 (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Andn64 (dst, s1, s2) -> s1
let __proj__Andn64__item__s2 (projectee : ins) : operand=
  match projectee with | Andn64 (dst, s1, s2) -> s2
let uu___is_Rorx64 (projectee : ins) : Prims.bool=
  match projectee with | Rorx64 (dst, src, imm) -> true | uu___ -> false
let __proj__Rorx64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Rorx64 (dst, src, imm) -> dst
let __proj__Rorx64__item__src (projectee : ins) : operand=
  match projectee with | Rorx64 (dst, src, imm) -> src
let __proj__Rorx64__item__imm (projectee : ins) : Valerie_Defs.nat8=
  match projectee with | Rorx64 (dst, src, imm) -> imm
let uu___is_Sarx64 (projectee : ins) : Prims.bool=
  match projectee with | Sarx64 (dst, src, count) -> true | uu___ -> false
let __proj__Sarx64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Sarx64 (dst, src, count) -> dst
let __proj__Sarx64__item__src (projectee : ins) : operand=
  match projectee with | Sarx64 (dst, src, count) -> src
let __proj__Sarx64__item__count (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Sarx64 (dst, src, count) -> count
let uu___is_Shlx64 (projectee : ins) : Prims.bool=
  match projectee with | Shlx64 (dst, src, count) -> true | uu___ -> false
let __proj__Shlx64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Shlx64 (dst, src, count) -> dst
let __proj__Shlx64__item__src (projectee : ins) : operand=
  match projectee with | Shlx64 (dst, src, count) -> src
let __proj__Shlx64__item__count (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Shlx64 (dst, src, count) -> count
let uu___is_Shrx64 (projectee : ins) : Prims.bool=
  match projectee with | Shrx64 (dst, src, count) -> true | uu___ -> false
let __proj__Shrx64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Shrx64 (dst, src, count) -> dst
let __proj__Shrx64__item__src (projectee : ins) : operand=
  match projectee with | Shrx64 (dst, src, count) -> src
let __proj__Shrx64__item__count (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Shrx64 (dst, src, count) -> count
let uu___is_Bsf64 (projectee : ins) : Prims.bool=
  match projectee with | Bsf64 (dst, src) -> true | uu___ -> false
let __proj__Bsf64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Bsf64 (dst, src) -> dst
let __proj__Bsf64__item__src (projectee : ins) : operand=
  match projectee with | Bsf64 (dst, src) -> src
let uu___is_Bsr64 (projectee : ins) : Prims.bool=
  match projectee with | Bsr64 (dst, src) -> true | uu___ -> false
let __proj__Bsr64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Bsr64 (dst, src) -> dst
let __proj__Bsr64__item__src (projectee : ins) : operand=
  match projectee with | Bsr64 (dst, src) -> src
let uu___is_Tzcnt64 (projectee : ins) : Prims.bool=
  match projectee with | Tzcnt64 (dst, src) -> true | uu___ -> false
let __proj__Tzcnt64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Tzcnt64 (dst, src) -> dst
let __proj__Tzcnt64__item__src (projectee : ins) : operand=
  match projectee with | Tzcnt64 (dst, src) -> src
let uu___is_Lzcnt64 (projectee : ins) : Prims.bool=
  match projectee with | Lzcnt64 (dst, src) -> true | uu___ -> false
let __proj__Lzcnt64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Lzcnt64 (dst, src) -> dst
let __proj__Lzcnt64__item__src (projectee : ins) : operand=
  match projectee with | Lzcnt64 (dst, src) -> src
let uu___is_Popcnt64 (projectee : ins) : Prims.bool=
  match projectee with | Popcnt64 (dst, src) -> true | uu___ -> false
let __proj__Popcnt64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Popcnt64 (dst, src) -> dst
let __proj__Popcnt64__item__src (projectee : ins) : operand=
  match projectee with | Popcnt64 (dst, src) -> src
let uu___is_Cmov64 (projectee : ins) : Prims.bool=
  match projectee with | Cmov64 (cond, dst, src) -> true | uu___ -> false
let __proj__Cmov64__item__cond (projectee : ins) :
  Valerie_X64_Machine.cond_code=
  match projectee with | Cmov64 (cond, dst, src) -> cond
let __proj__Cmov64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Cmov64 (cond, dst, src) -> dst
let __proj__Cmov64__item__src (projectee : ins) : operand=
  match projectee with | Cmov64 (cond, dst, src) -> src
let uu___is_Jmp (projectee : ins) : Prims.bool=
  match projectee with | Jmp label -> true | uu___ -> false
let __proj__Jmp__item__label (projectee : ins) : Prims.string=
  match projectee with | Jmp label -> label
let uu___is_Jcc (projectee : ins) : Prims.bool=
  match projectee with | Jcc (cond, label) -> true | uu___ -> false
let __proj__Jcc__item__cond (projectee : ins) :
  Valerie_X64_Machine.cond_code=
  match projectee with | Jcc (cond, label) -> cond
let __proj__Jcc__item__label (projectee : ins) : Prims.string=
  match projectee with | Jcc (cond, label) -> label
let uu___is_Call (projectee : ins) : Prims.bool=
  match projectee with | Call label -> true | uu___ -> false
let __proj__Call__item__label (projectee : ins) : Prims.string=
  match projectee with | Call label -> label
let uu___is_Ret (projectee : ins) : Prims.bool=
  match projectee with | Ret -> true | uu___ -> false
let uu___is_Push64 (projectee : ins) : Prims.bool=
  match projectee with | Push64 src -> true | uu___ -> false
let __proj__Push64__item__src (projectee : ins) : operand=
  match projectee with | Push64 src -> src
let uu___is_Pop64 (projectee : ins) : Prims.bool=
  match projectee with | Pop64 dst -> true | uu___ -> false
let __proj__Pop64__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Pop64 dst -> dst
let uu___is_Aesenc (projectee : ins) : Prims.bool=
  match projectee with | Aesenc (dst, src) -> true | uu___ -> false
let __proj__Aesenc__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Aesenc (dst, src) -> dst
let __proj__Aesenc__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Aesenc (dst, src) -> src
let uu___is_Aesenclast (projectee : ins) : Prims.bool=
  match projectee with | Aesenclast (dst, src) -> true | uu___ -> false
let __proj__Aesenclast__item__dst (projectee : ins) :
  Valerie_X64_Machine.xmm=
  match projectee with | Aesenclast (dst, src) -> dst
let __proj__Aesenclast__item__src (projectee : ins) :
  Valerie_X64_Machine.xmm=
  match projectee with | Aesenclast (dst, src) -> src
let uu___is_Aesdec (projectee : ins) : Prims.bool=
  match projectee with | Aesdec (dst, src) -> true | uu___ -> false
let __proj__Aesdec__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Aesdec (dst, src) -> dst
let __proj__Aesdec__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Aesdec (dst, src) -> src
let uu___is_Aesdeclast (projectee : ins) : Prims.bool=
  match projectee with | Aesdeclast (dst, src) -> true | uu___ -> false
let __proj__Aesdeclast__item__dst (projectee : ins) :
  Valerie_X64_Machine.xmm=
  match projectee with | Aesdeclast (dst, src) -> dst
let __proj__Aesdeclast__item__src (projectee : ins) :
  Valerie_X64_Machine.xmm=
  match projectee with | Aesdeclast (dst, src) -> src
let uu___is_Aesimc (projectee : ins) : Prims.bool=
  match projectee with | Aesimc (dst, src) -> true | uu___ -> false
let __proj__Aesimc__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Aesimc (dst, src) -> dst
let __proj__Aesimc__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Aesimc (dst, src) -> src
let uu___is_Aeskeygenassist (projectee : ins) : Prims.bool=
  match projectee with
  | Aeskeygenassist (dst, src, rcon) -> true
  | uu___ -> false
let __proj__Aeskeygenassist__item__dst (projectee : ins) :
  Valerie_X64_Machine.xmm=
  match projectee with | Aeskeygenassist (dst, src, rcon) -> dst
let __proj__Aeskeygenassist__item__src (projectee : ins) :
  Valerie_X64_Machine.xmm=
  match projectee with | Aeskeygenassist (dst, src, rcon) -> src
let __proj__Aeskeygenassist__item__rcon (projectee : ins) :
  Valerie_Defs.nat8=
  match projectee with | Aeskeygenassist (dst, src, rcon) -> rcon
let uu___is_Vaesenc (projectee : ins) : Prims.bool=
  match projectee with | Vaesenc (dst, s1, s2) -> true | uu___ -> false
let __proj__Vaesenc__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Vaesenc (dst, s1, s2) -> dst
let __proj__Vaesenc__item__s1 (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Vaesenc (dst, s1, s2) -> s1
let __proj__Vaesenc__item__s2 (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Vaesenc (dst, s1, s2) -> s2
let uu___is_Vaesdec (projectee : ins) : Prims.bool=
  match projectee with | Vaesdec (dst, s1, s2) -> true | uu___ -> false
let __proj__Vaesdec__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Vaesdec (dst, s1, s2) -> dst
let __proj__Vaesdec__item__s1 (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Vaesdec (dst, s1, s2) -> s1
let __proj__Vaesdec__item__s2 (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Vaesdec (dst, s1, s2) -> s2
let uu___is_Pclmulqdq (projectee : ins) : Prims.bool=
  match projectee with | Pclmulqdq (dst, src, imm) -> true | uu___ -> false
let __proj__Pclmulqdq__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Pclmulqdq (dst, src, imm) -> dst
let __proj__Pclmulqdq__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Pclmulqdq (dst, src, imm) -> src
let __proj__Pclmulqdq__item__imm (projectee : ins) : Valerie_Defs.nat8=
  match projectee with | Pclmulqdq (dst, src, imm) -> imm
let uu___is_Vpclmulqdq (projectee : ins) : Prims.bool=
  match projectee with
  | Vpclmulqdq (dst, s1, s2, imm) -> true
  | uu___ -> false
let __proj__Vpclmulqdq__item__dst (projectee : ins) :
  Valerie_X64_Machine.xmm=
  match projectee with | Vpclmulqdq (dst, s1, s2, imm) -> dst
let __proj__Vpclmulqdq__item__s1 (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Vpclmulqdq (dst, s1, s2, imm) -> s1
let __proj__Vpclmulqdq__item__s2 (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Vpclmulqdq (dst, s1, s2, imm) -> s2
let __proj__Vpclmulqdq__item__imm (projectee : ins) : Valerie_Defs.nat8=
  match projectee with | Vpclmulqdq (dst, s1, s2, imm) -> imm
let uu___is_Sha1rnds4 (projectee : ins) : Prims.bool=
  match projectee with | Sha1rnds4 (dst, src, imm) -> true | uu___ -> false
let __proj__Sha1rnds4__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Sha1rnds4 (dst, src, imm) -> dst
let __proj__Sha1rnds4__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Sha1rnds4 (dst, src, imm) -> src
let __proj__Sha1rnds4__item__imm (projectee : ins) : Valerie_Defs.nat8=
  match projectee with | Sha1rnds4 (dst, src, imm) -> imm
let uu___is_Sha1nexte (projectee : ins) : Prims.bool=
  match projectee with | Sha1nexte (dst, src) -> true | uu___ -> false
let __proj__Sha1nexte__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Sha1nexte (dst, src) -> dst
let __proj__Sha1nexte__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Sha1nexte (dst, src) -> src
let uu___is_Sha1msg1 (projectee : ins) : Prims.bool=
  match projectee with | Sha1msg1 (dst, src) -> true | uu___ -> false
let __proj__Sha1msg1__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Sha1msg1 (dst, src) -> dst
let __proj__Sha1msg1__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Sha1msg1 (dst, src) -> src
let uu___is_Sha1msg2 (projectee : ins) : Prims.bool=
  match projectee with | Sha1msg2 (dst, src) -> true | uu___ -> false
let __proj__Sha1msg2__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Sha1msg2 (dst, src) -> dst
let __proj__Sha1msg2__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Sha1msg2 (dst, src) -> src
let uu___is_Sha256rnds2 (projectee : ins) : Prims.bool=
  match projectee with | Sha256rnds2 (dst, src) -> true | uu___ -> false
let __proj__Sha256rnds2__item__dst (projectee : ins) :
  Valerie_X64_Machine.xmm=
  match projectee with | Sha256rnds2 (dst, src) -> dst
let __proj__Sha256rnds2__item__src (projectee : ins) :
  Valerie_X64_Machine.xmm=
  match projectee with | Sha256rnds2 (dst, src) -> src
let uu___is_Sha256msg1 (projectee : ins) : Prims.bool=
  match projectee with | Sha256msg1 (dst, src) -> true | uu___ -> false
let __proj__Sha256msg1__item__dst (projectee : ins) :
  Valerie_X64_Machine.xmm=
  match projectee with | Sha256msg1 (dst, src) -> dst
let __proj__Sha256msg1__item__src (projectee : ins) :
  Valerie_X64_Machine.xmm=
  match projectee with | Sha256msg1 (dst, src) -> src
let uu___is_Sha256msg2 (projectee : ins) : Prims.bool=
  match projectee with | Sha256msg2 (dst, src) -> true | uu___ -> false
let __proj__Sha256msg2__item__dst (projectee : ins) :
  Valerie_X64_Machine.xmm=
  match projectee with | Sha256msg2 (dst, src) -> dst
let __proj__Sha256msg2__item__src (projectee : ins) :
  Valerie_X64_Machine.xmm=
  match projectee with | Sha256msg2 (dst, src) -> src
let uu___is_Movdqu (projectee : ins) : Prims.bool=
  match projectee with | Movdqu (dst, src) -> true | uu___ -> false
let __proj__Movdqu__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Movdqu (dst, src) -> dst
let __proj__Movdqu__item__src (projectee : ins) : operand=
  match projectee with | Movdqu (dst, src) -> src
let uu___is_Movdqa (projectee : ins) : Prims.bool=
  match projectee with | Movdqa (dst, src) -> true | uu___ -> false
let __proj__Movdqa__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Movdqa (dst, src) -> dst
let __proj__Movdqa__item__src (projectee : ins) : operand=
  match projectee with | Movdqa (dst, src) -> src
let uu___is_Vmovdqu (projectee : ins) : Prims.bool=
  match projectee with | Vmovdqu (dst, src) -> true | uu___ -> false
let __proj__Vmovdqu__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Vmovdqu (dst, src) -> dst
let __proj__Vmovdqu__item__src (projectee : ins) : operand=
  match projectee with | Vmovdqu (dst, src) -> src
let uu___is_Vmovdqa (projectee : ins) : Prims.bool=
  match projectee with | Vmovdqa (dst, src) -> true | uu___ -> false
let __proj__Vmovdqa__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Vmovdqa (dst, src) -> dst
let __proj__Vmovdqa__item__src (projectee : ins) : operand=
  match projectee with | Vmovdqa (dst, src) -> src
let uu___is_Pxor (projectee : ins) : Prims.bool=
  match projectee with | Pxor (dst, src) -> true | uu___ -> false
let __proj__Pxor__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Pxor (dst, src) -> dst
let __proj__Pxor__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Pxor (dst, src) -> src
let uu___is_Vpxor (projectee : ins) : Prims.bool=
  match projectee with | Vpxor (dst, s1, s2) -> true | uu___ -> false
let __proj__Vpxor__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Vpxor (dst, s1, s2) -> dst
let __proj__Vpxor__item__s1 (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Vpxor (dst, s1, s2) -> s1
let __proj__Vpxor__item__s2 (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Vpxor (dst, s1, s2) -> s2
let uu___is_Pand (projectee : ins) : Prims.bool=
  match projectee with | Pand (dst, src) -> true | uu___ -> false
let __proj__Pand__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Pand (dst, src) -> dst
let __proj__Pand__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Pand (dst, src) -> src
let uu___is_Por (projectee : ins) : Prims.bool=
  match projectee with | Por (dst, src) -> true | uu___ -> false
let __proj__Por__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Por (dst, src) -> dst
let __proj__Por__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Por (dst, src) -> src
let uu___is_Paddd (projectee : ins) : Prims.bool=
  match projectee with | Paddd (dst, src) -> true | uu___ -> false
let __proj__Paddd__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Paddd (dst, src) -> dst
let __proj__Paddd__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Paddd (dst, src) -> src
let uu___is_Psubd (projectee : ins) : Prims.bool=
  match projectee with | Psubd (dst, src) -> true | uu___ -> false
let __proj__Psubd__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Psubd (dst, src) -> dst
let __proj__Psubd__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Psubd (dst, src) -> src
let uu___is_Paddq (projectee : ins) : Prims.bool=
  match projectee with | Paddq (dst, src) -> true | uu___ -> false
let __proj__Paddq__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Paddq (dst, src) -> dst
let __proj__Paddq__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Paddq (dst, src) -> src
let uu___is_Psubq (projectee : ins) : Prims.bool=
  match projectee with | Psubq (dst, src) -> true | uu___ -> false
let __proj__Psubq__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Psubq (dst, src) -> dst
let __proj__Psubq__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Psubq (dst, src) -> src
let uu___is_Paddb (projectee : ins) : Prims.bool=
  match projectee with | Paddb (dst, src) -> true | uu___ -> false
let __proj__Paddb__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Paddb (dst, src) -> dst
let __proj__Paddb__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Paddb (dst, src) -> src
let uu___is_Psubb (projectee : ins) : Prims.bool=
  match projectee with | Psubb (dst, src) -> true | uu___ -> false
let __proj__Psubb__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Psubb (dst, src) -> dst
let __proj__Psubb__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Psubb (dst, src) -> src
let uu___is_Paddw (projectee : ins) : Prims.bool=
  match projectee with | Paddw (dst, src) -> true | uu___ -> false
let __proj__Paddw__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Paddw (dst, src) -> dst
let __proj__Paddw__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Paddw (dst, src) -> src
let uu___is_Psubw (projectee : ins) : Prims.bool=
  match projectee with | Psubw (dst, src) -> true | uu___ -> false
let __proj__Psubw__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Psubw (dst, src) -> dst
let __proj__Psubw__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Psubw (dst, src) -> src
let uu___is_Pslld (projectee : ins) : Prims.bool=
  match projectee with | Pslld (dst, count) -> true | uu___ -> false
let __proj__Pslld__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Pslld (dst, count) -> dst
let __proj__Pslld__item__count (projectee : ins) : Valerie_Defs.nat8=
  match projectee with | Pslld (dst, count) -> count
let uu___is_Psrld (projectee : ins) : Prims.bool=
  match projectee with | Psrld (dst, count) -> true | uu___ -> false
let __proj__Psrld__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Psrld (dst, count) -> dst
let __proj__Psrld__item__count (projectee : ins) : Valerie_Defs.nat8=
  match projectee with | Psrld (dst, count) -> count
let uu___is_Psllq (projectee : ins) : Prims.bool=
  match projectee with | Psllq (dst, count) -> true | uu___ -> false
let __proj__Psllq__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Psllq (dst, count) -> dst
let __proj__Psllq__item__count (projectee : ins) : Valerie_Defs.nat8=
  match projectee with | Psllq (dst, count) -> count
let uu___is_Psrlq (projectee : ins) : Prims.bool=
  match projectee with | Psrlq (dst, count) -> true | uu___ -> false
let __proj__Psrlq__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Psrlq (dst, count) -> dst
let __proj__Psrlq__item__count (projectee : ins) : Valerie_Defs.nat8=
  match projectee with | Psrlq (dst, count) -> count
let uu___is_Pshufd (projectee : ins) : Prims.bool=
  match projectee with | Pshufd (dst, src, imm) -> true | uu___ -> false
let __proj__Pshufd__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Pshufd (dst, src, imm) -> dst
let __proj__Pshufd__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Pshufd (dst, src, imm) -> src
let __proj__Pshufd__item__imm (projectee : ins) : Valerie_Defs.nat8=
  match projectee with | Pshufd (dst, src, imm) -> imm
let uu___is_Pshufb (projectee : ins) : Prims.bool=
  match projectee with | Pshufb (dst, src) -> true | uu___ -> false
let __proj__Pshufb__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Pshufb (dst, src) -> dst
let __proj__Pshufb__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Pshufb (dst, src) -> src
let uu___is_Palignr (projectee : ins) : Prims.bool=
  match projectee with | Palignr (dst, src, imm) -> true | uu___ -> false
let __proj__Palignr__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Palignr (dst, src, imm) -> dst
let __proj__Palignr__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Palignr (dst, src, imm) -> src
let __proj__Palignr__item__imm (projectee : ins) : Valerie_Defs.nat8=
  match projectee with | Palignr (dst, src, imm) -> imm
let uu___is_Punpcklbw (projectee : ins) : Prims.bool=
  match projectee with | Punpcklbw (dst, src) -> true | uu___ -> false
let __proj__Punpcklbw__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Punpcklbw (dst, src) -> dst
let __proj__Punpcklbw__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Punpcklbw (dst, src) -> src
let uu___is_Punpckhbw (projectee : ins) : Prims.bool=
  match projectee with | Punpckhbw (dst, src) -> true | uu___ -> false
let __proj__Punpckhbw__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Punpckhbw (dst, src) -> dst
let __proj__Punpckhbw__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Punpckhbw (dst, src) -> src
let uu___is_Punpcklwd (projectee : ins) : Prims.bool=
  match projectee with | Punpcklwd (dst, src) -> true | uu___ -> false
let __proj__Punpcklwd__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Punpcklwd (dst, src) -> dst
let __proj__Punpcklwd__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Punpcklwd (dst, src) -> src
let uu___is_Punpckhwd (projectee : ins) : Prims.bool=
  match projectee with | Punpckhwd (dst, src) -> true | uu___ -> false
let __proj__Punpckhwd__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Punpckhwd (dst, src) -> dst
let __proj__Punpckhwd__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Punpckhwd (dst, src) -> src
let uu___is_Punpckldq (projectee : ins) : Prims.bool=
  match projectee with | Punpckldq (dst, src) -> true | uu___ -> false
let __proj__Punpckldq__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Punpckldq (dst, src) -> dst
let __proj__Punpckldq__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Punpckldq (dst, src) -> src
let uu___is_Punpckhdq (projectee : ins) : Prims.bool=
  match projectee with | Punpckhdq (dst, src) -> true | uu___ -> false
let __proj__Punpckhdq__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Punpckhdq (dst, src) -> dst
let __proj__Punpckhdq__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Punpckhdq (dst, src) -> src
let uu___is_Punpcklqdq (projectee : ins) : Prims.bool=
  match projectee with | Punpcklqdq (dst, src) -> true | uu___ -> false
let __proj__Punpcklqdq__item__dst (projectee : ins) :
  Valerie_X64_Machine.xmm=
  match projectee with | Punpcklqdq (dst, src) -> dst
let __proj__Punpcklqdq__item__src (projectee : ins) :
  Valerie_X64_Machine.xmm=
  match projectee with | Punpcklqdq (dst, src) -> src
let uu___is_Punpckhqdq (projectee : ins) : Prims.bool=
  match projectee with | Punpckhqdq (dst, src) -> true | uu___ -> false
let __proj__Punpckhqdq__item__dst (projectee : ins) :
  Valerie_X64_Machine.xmm=
  match projectee with | Punpckhqdq (dst, src) -> dst
let __proj__Punpckhqdq__item__src (projectee : ins) :
  Valerie_X64_Machine.xmm=
  match projectee with | Punpckhqdq (dst, src) -> src
let uu___is_Pcmpeqd (projectee : ins) : Prims.bool=
  match projectee with | Pcmpeqd (dst, src) -> true | uu___ -> false
let __proj__Pcmpeqd__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Pcmpeqd (dst, src) -> dst
let __proj__Pcmpeqd__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Pcmpeqd (dst, src) -> src
let uu___is_Pcmpeqb (projectee : ins) : Prims.bool=
  match projectee with | Pcmpeqb (dst, src) -> true | uu___ -> false
let __proj__Pcmpeqb__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Pcmpeqb (dst, src) -> dst
let __proj__Pcmpeqb__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Pcmpeqb (dst, src) -> src
let uu___is_Pextrq (projectee : ins) : Prims.bool=
  match projectee with | Pextrq (dst, src, imm) -> true | uu___ -> false
let __proj__Pextrq__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Pextrq (dst, src, imm) -> dst
let __proj__Pextrq__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Pextrq (dst, src, imm) -> src
let __proj__Pextrq__item__imm (projectee : ins) : Valerie_Defs.nat8=
  match projectee with | Pextrq (dst, src, imm) -> imm
let uu___is_Pextrd (projectee : ins) : Prims.bool=
  match projectee with | Pextrd (dst, src, imm) -> true | uu___ -> false
let __proj__Pextrd__item__dst (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Pextrd (dst, src, imm) -> dst
let __proj__Pextrd__item__src (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Pextrd (dst, src, imm) -> src
let __proj__Pextrd__item__imm (projectee : ins) : Valerie_Defs.nat8=
  match projectee with | Pextrd (dst, src, imm) -> imm
let uu___is_Pinsrd (projectee : ins) : Prims.bool=
  match projectee with | Pinsrd (dst, src, imm) -> true | uu___ -> false
let __proj__Pinsrd__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Pinsrd (dst, src, imm) -> dst
let __proj__Pinsrd__item__src (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Pinsrd (dst, src, imm) -> src
let __proj__Pinsrd__item__imm (projectee : ins) : Valerie_Defs.nat8=
  match projectee with | Pinsrd (dst, src, imm) -> imm
let uu___is_Pinsrq (projectee : ins) : Prims.bool=
  match projectee with | Pinsrq (dst, src, imm) -> true | uu___ -> false
let __proj__Pinsrq__item__dst (projectee : ins) : Valerie_X64_Machine.xmm=
  match projectee with | Pinsrq (dst, src, imm) -> dst
let __proj__Pinsrq__item__src (projectee : ins) : Valerie_X64_Machine.reg=
  match projectee with | Pinsrq (dst, src, imm) -> src
let __proj__Pinsrq__item__imm (projectee : ins) : Valerie_Defs.nat8=
  match projectee with | Pinsrq (dst, src, imm) -> imm
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
let eval_operand (o : operand) (s : Valerie_X64_Machine.state) :
  Valerie_Defs.nat64=
  match o with
  | OReg (r, uu___) -> s.Valerie_X64_Machine.regs r
  | OImm imm -> imm
  | uu___ -> Stdint.Uint64.zero
let eval_ins (i : ins) (s : Valerie_X64_Machine.state) :
  Valerie_X64_Machine.state=
  match i with
  | Mov64 (dst, src) ->
      let v = eval_operand src s in
      {
        Valerie_X64_Machine.ok = (s.Valerie_X64_Machine.ok);
        Valerie_X64_Machine.regs =
          ((fun r -> if r = dst then v else s.Valerie_X64_Machine.regs r));
        Valerie_X64_Machine.xmms = (s.Valerie_X64_Machine.xmms);
        Valerie_X64_Machine.flags = (s.Valerie_X64_Machine.flags)
      }
  | Mov32 (dst, src) ->
      let v = eval_operand src s in
      {
        Valerie_X64_Machine.ok = (s.Valerie_X64_Machine.ok);
        Valerie_X64_Machine.regs =
          ((fun r -> if r = dst then v else s.Valerie_X64_Machine.regs r));
        Valerie_X64_Machine.xmms = (s.Valerie_X64_Machine.xmms);
        Valerie_X64_Machine.flags = (s.Valerie_X64_Machine.flags)
      }
  | Add64 (dst, src) ->
      let v1 = s.Valerie_X64_Machine.regs dst in
      let v2 = eval_operand src s in
      let res = FStar_UInt64.add_mod v1 v2 in
      {
        Valerie_X64_Machine.ok = (s.Valerie_X64_Machine.ok);
        Valerie_X64_Machine.regs =
          ((fun r -> if r = dst then res else s.Valerie_X64_Machine.regs r));
        Valerie_X64_Machine.xmms = (s.Valerie_X64_Machine.xmms);
        Valerie_X64_Machine.flags = (s.Valerie_X64_Machine.flags)
      }
  | Sub64 (dst, src) ->
      let v1 = s.Valerie_X64_Machine.regs dst in
      let v2 = eval_operand src s in
      let res = FStar_UInt64.sub_mod v1 v2 in
      {
        Valerie_X64_Machine.ok = (s.Valerie_X64_Machine.ok);
        Valerie_X64_Machine.regs =
          ((fun r -> if r = dst then res else s.Valerie_X64_Machine.regs r));
        Valerie_X64_Machine.xmms = (s.Valerie_X64_Machine.xmms);
        Valerie_X64_Machine.flags = (s.Valerie_X64_Machine.flags)
      }
  | Xor64 (dst, src) ->
      let v1 = s.Valerie_X64_Machine.regs dst in
      let v2 = eval_operand src s in
      let res = Valerie_Defs.nat64_xor v1 v2 in
      {
        Valerie_X64_Machine.ok = (s.Valerie_X64_Machine.ok);
        Valerie_X64_Machine.regs =
          ((fun r -> if r = dst then res else s.Valerie_X64_Machine.regs r));
        Valerie_X64_Machine.xmms = (s.Valerie_X64_Machine.xmms);
        Valerie_X64_Machine.flags = (s.Valerie_X64_Machine.flags)
      }
  | And64 (dst, src) ->
      let v1 = s.Valerie_X64_Machine.regs dst in
      let v2 = eval_operand src s in
      let res = Valerie_Defs.nat64_and v1 v2 in
      {
        Valerie_X64_Machine.ok = (s.Valerie_X64_Machine.ok);
        Valerie_X64_Machine.regs =
          ((fun r -> if r = dst then res else s.Valerie_X64_Machine.regs r));
        Valerie_X64_Machine.xmms = (s.Valerie_X64_Machine.xmms);
        Valerie_X64_Machine.flags = (s.Valerie_X64_Machine.flags)
      }
  | Or64 (dst, src) ->
      let v1 = s.Valerie_X64_Machine.regs dst in
      let v2 = eval_operand src s in
      let res = Valerie_Defs.nat64_or v1 v2 in
      {
        Valerie_X64_Machine.ok = (s.Valerie_X64_Machine.ok);
        Valerie_X64_Machine.regs =
          ((fun r -> if r = dst then res else s.Valerie_X64_Machine.regs r));
        Valerie_X64_Machine.xmms = (s.Valerie_X64_Machine.xmms);
        Valerie_X64_Machine.flags = (s.Valerie_X64_Machine.flags)
      }
  | Pxor (dst, src) ->
      let v1 = s.Valerie_X64_Machine.xmms dst in
      let v2 = s.Valerie_X64_Machine.xmms src in
      let res = Valerie_Defs.nat128_xor v1 v2 in
      {
        Valerie_X64_Machine.ok = (s.Valerie_X64_Machine.ok);
        Valerie_X64_Machine.regs = (s.Valerie_X64_Machine.regs);
        Valerie_X64_Machine.xmms =
          ((fun x -> if x = dst then res else s.Valerie_X64_Machine.xmms x));
        Valerie_X64_Machine.flags = (s.Valerie_X64_Machine.flags)
      }
  | uu___ -> s
let eval_ocmp (c : ocmp) (s : Valerie_X64_Machine.state) : Prims.bool=
  match c with
  | OEq (o1, o2) -> (eval_operand o1 s) = (eval_operand o2 s)
  | ONe (o1, o2) -> (eval_operand o1 s) <> (eval_operand o2 s)
  | OLt (o1, o2) -> FStar_UInt64.lt (eval_operand o1 s) (eval_operand o2 s)
  | OGe (o1, o2) -> FStar_UInt64.gte (eval_operand o1 s) (eval_operand o2 s)
let rec eval_code (c : code) (fuel : Prims.nat)
  (s : Valerie_X64_Machine.state) :
  Valerie_X64_Machine.state FStar_Pervasives_Native.option=
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
