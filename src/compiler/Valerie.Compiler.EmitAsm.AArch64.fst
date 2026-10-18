module Valerie.Compiler.EmitAsm.AArch64

open FStar.String
open Valerie.Defs
open FStar.UInt64
open Valerie.AArch64.Machine
open Valerie.AArch64.Semantics
open Valerie.Compiler.AST
open Valerie.Compiler.Lower.AArch64

let print_reg (r: reg) : string =
  match r with
  | X0  -> "x0"
  | X1  -> "x1"
  | X2  -> "x2"
  | X3  -> "x3"
  | X4  -> "x4"
  | X5  -> "x5"
  | X6  -> "x6"
  | X7  -> "x7"
  | X8  -> "x8"
  | X9  -> "x9"
  | X10 -> "x10"
  | X11 -> "x11"
  | X12 -> "x12"
  | X13 -> "x13"
  | X14 -> "x14"
  | X15 -> "x15"
  | X16 -> "x16"
  | X17 -> "x17"
  | X18 -> "x18"
  | X19 -> "x19"
  | X20 -> "x20"
  | X21 -> "x21"
  | X22 -> "x22"
  | X23 -> "x23"
  | X24 -> "x24"
  | X25 -> "x25"
  | X26 -> "x26"
  | X27 -> "x27"
  | X28 -> "x28"
  | X29 -> "x29"
  | X30 -> "x30"
  | SP  -> "sp"
  | XZR -> "xzr"

let print_wreg (r: reg) : string =
  match r with
  | X0  -> "w0"
  | X1  -> "w1"
  | X2  -> "w2"
  | X3  -> "w3"
  | X4  -> "w4"
  | X5  -> "w5"
  | X6  -> "w6"
  | X7  -> "w7"
  | X8  -> "w8"
  | X9  -> "w9"
  | X10 -> "w10"
  | X11 -> "w11"
  | X12 -> "w12"
  | X13 -> "w13"
  | X14 -> "w14"
  | X15 -> "w15"
  | X16 -> "w16"
  | X17 -> "w17"
  | X18 -> "w18"
  | X19 -> "w19"
  | X20 -> "w20"
  | X21 -> "w21"
  | X22 -> "w22"
  | X23 -> "w23"
  | X24 -> "w24"
  | X25 -> "w25"
  | X26 -> "w26"
  | X27 -> "w27"
  | X28 -> "w28"
  | X29 -> "w29"
  | X30 -> "w30"
  | SP  -> "wsp"
  | XZR -> "wzr"

let print_vec (v: vec) : string =
  match v with
  | V0  -> "v0"
  | V1  -> "v1"
  | V2  -> "v2"
  | V3  -> "v3"
  | V4  -> "v4"
  | V5  -> "v5"
  | V6  -> "v6"
  | V7  -> "v7"
  | V8  -> "v8"
  | V9  -> "v9"
  | V10 -> "v10"
  | V11 -> "v11"
  | V12 -> "v12"
  | V13 -> "v13"
  | V14 -> "v14"
  | V15 -> "v15"
  | V16 -> "v16"
  | V17 -> "v17"
  | V18 -> "v18"
  | V19 -> "v19"
  | V20 -> "v20"
  | V21 -> "v21"
  | V22 -> "v22"
  | V23 -> "v23"
  | V24 -> "v24"
  | V25 -> "v25"
  | V26 -> "v26"
  | V27 -> "v27"
  | V28 -> "v28"
  | V29 -> "v29"
  | V30 -> "v30"
  | V31 -> "v31"

let print_arrangement (arr: vec_arrangement) : string =
  match arr with
  | B16 -> ".16b"
  | H8  -> ".8h"
  | S4  -> ".4s"
  | D2  -> ".2d"
  | B8  -> ".8b"
  | H4  -> ".4h"
  | S2  -> ".2s"
  | D1  -> ".1d"
  | Q1  -> ".1q"

let print_cond (c: cond_code) : string =
  match c with
  | CondEQ -> "eq"
  | CondNE -> "ne"
  | CondCS -> "cs"
  | CondCC -> "cc"
  | CondMI -> "mi"
  | CondPL -> "pl"
  | CondVS -> "vs"
  | CondVC -> "vc"
  | CondHI -> "hi"
  | CondLS -> "ls"
  | CondGE -> "ge"
  | CondLT -> "lt"
  | CondGT -> "gt"
  | CondLE -> "le"
  | CondAL -> "al"
  | CondNV -> "nv"

let print_maddr (m: maddr) : string =
  match m with
  | MReg base ->
      concat "" ["["; print_reg base; "]"]
  | MOffset base offset ->
      concat "" ["["; print_reg base; ", #"; string_of_int offset; "]"]
  | MPreIndex base offset ->
      concat "" ["["; print_reg base; ", #"; string_of_int offset; "]!"]
  | MPostIndex base offset ->
      concat "" ["["; print_reg base; "], #"; string_of_int offset]
  | MRegOffset base index _ shift ->
      if shift = 0 then concat "" ["["; print_reg base; ", "; print_reg index; "]"]
      else concat "" ["["; print_reg base; ", "; print_reg index; ", lsl #"; string_of_int shift; "]"]

let print_fpreg (v: vec) (w: fp_width) : string =
  let prefix = match w with FPS -> "s" | FPD -> "d" | FPQ -> "q" in
  let num_str =
    match v with
    | V0 -> "0" | V1 -> "1" | V2 -> "2" | V3 -> "3"
    | V4 -> "4" | V5 -> "5" | V6 -> "6" | V7 -> "7"
    | V8 -> "8" | V9 -> "9" | V10 -> "10" | V11 -> "11"
    | V12 -> "12" | V13 -> "13" | V14 -> "14" | V15 -> "15"
    | V16 -> "16" | V17 -> "17" | V18 -> "18" | V19 -> "19"
    | V20 -> "20" | V21 -> "21" | V22 -> "22" | V23 -> "23"
    | V24 -> "24" | V25 -> "25" | V26 -> "26" | V27 -> "27"
    | V28 -> "28" | V29 -> "29" | V30 -> "30" | V31 -> "31"
  in
  concat "" [prefix; num_str]

let print_operand (op: Valerie.AArch64.Semantics.operand) : string =
  match op with
  | Valerie.AArch64.Semantics.OReg r W64 -> print_reg r
  | Valerie.AArch64.Semantics.OReg r W32 -> print_wreg r
  | Valerie.AArch64.Semantics.OImm imm -> concat "" ["#"; FStar.UInt64.to_string imm]
  | Valerie.AArch64.Semantics.OFpReg v w -> print_fpreg v w
  | Valerie.AArch64.Semantics.OVecReg v arr -> concat "" [print_vec v; print_arrangement arr]
  | Valerie.AArch64.Semantics.OVecElem v sz lane ->
      let s = match sz with EB -> "b" | EH -> "h" | ES -> "s" | ED -> "d" in
      concat "" [print_vec v; "."; s; "["; string_of_int lane; "]"]
  | Valerie.AArch64.Semantics.OShifted r W64 LSL amt -> concat "" [print_reg r; ", lsl #"; string_of_int amt]
  | Valerie.AArch64.Semantics.OShifted r W64 LSR amt -> concat "" [print_reg r; ", lsr #"; string_of_int amt]
  | Valerie.AArch64.Semantics.OShifted r W64 ASR amt -> concat "" [print_reg r; ", asr #"; string_of_int amt]
  | Valerie.AArch64.Semantics.OShifted r W64 ROR amt -> concat "" [print_reg r; ", ror #"; string_of_int amt]
  | Valerie.AArch64.Semantics.OShifted r W32 LSL amt -> concat "" [print_wreg r; ", lsl #"; string_of_int amt]
  | Valerie.AArch64.Semantics.OShifted r W32 LSR amt -> concat "" [print_wreg r; ", lsr #"; string_of_int amt]
  | Valerie.AArch64.Semantics.OShifted r W32 ASR amt -> concat "" [print_wreg r; ", asr #"; string_of_int amt]
  | Valerie.AArch64.Semantics.OShifted r W32 ROR amt -> concat "" [print_wreg r; ", ror #"; string_of_int amt]
  | Valerie.AArch64.Semantics.OExtended r _ _ amt ->
      if amt = 0 then print_reg r
      else concat "" [print_reg r; ", uxtx #"; string_of_int amt]
  | Valerie.AArch64.Semantics.OMem m -> print_maddr m
  | Valerie.AArch64.Semantics.OCond c -> print_cond c

let print_ins (i: ins) : string =
  match i with
  | Mov dst src -> concat "" ["    mov "; print_reg dst; ", "; print_operand src; "\n"]
  | Movz dst imm shift -> concat "" ["    movz "; print_reg dst; ", #"; FStar.UInt64.to_string imm; ", lsl #"; string_of_int shift; "\n"]
  | Movn dst imm shift -> concat "" ["    movn "; print_reg dst; ", #"; FStar.UInt64.to_string imm; ", lsl #"; string_of_int shift; "\n"]
  | Movk dst imm shift -> concat "" ["    movk "; print_reg dst; ", #"; FStar.UInt64.to_string imm; ", lsl #"; string_of_int shift; "\n"]
  | Adrp dst lbl -> concat "" ["    adrp "; print_reg dst; ", "; lbl; "\n"]

  | Add dst s1 s2 -> concat "" ["    add "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Adds dst s1 s2 -> concat "" ["    adds "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Sub dst s1 s2 -> concat "" ["    sub "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Subs dst s1 s2 -> concat "" ["    subs "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Adc dst s1 s2 -> concat "" ["    adc "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Adcs dst s1 s2 -> concat "" ["    adcs "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Sbc dst s1 s2 -> concat "" ["    sbc "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Sbcs dst s1 s2 -> concat "" ["    sbcs "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Cmp s1 s2 -> concat "" ["    cmp "; print_reg s1; ", "; print_operand s2; "\n"]
  | Cmn s1 s2 -> concat "" ["    cmn "; print_reg s1; ", "; print_operand s2; "\n"]
  | Tst s1 s2 -> concat "" ["    tst "; print_reg s1; ", "; print_operand s2; "\n"]
  | Neg dst src -> concat "" ["    neg "; print_reg dst; ", "; print_reg src; "\n"]
  | Negs dst src -> concat "" ["    negs "; print_reg dst; ", "; print_reg src; "\n"]
  | Mvn dst src -> concat "" ["    mvn "; print_reg dst; ", "; print_operand src; "\n"]

  | Mul dst s1 s2 -> concat "" ["    mul "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Madd dst s1 s2 s3 -> concat "" ["    madd "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; ", "; print_reg s3; "\n"]
  | Msub dst s1 s2 s3 -> concat "" ["    msub "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; ", "; print_reg s3; "\n"]
  | Smulh dst s1 s2 -> concat "" ["    smulh "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Umulh dst s1 s2 -> concat "" ["    umulh "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Sdiv dst s1 s2 -> concat "" ["    sdiv "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Udiv dst s1 s2 -> concat "" ["    udiv "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]

  | And dst s1 s2 -> concat "" ["    and "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Ands dst s1 s2 -> concat "" ["    ands "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Orr dst s1 s2 -> concat "" ["    orr "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Orn dst s1 s2 -> concat "" ["    orn "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Eor dst s1 s2 -> concat "" ["    eor "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Eon dst s1 s2 -> concat "" ["    eon "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Bic dst s1 s2 -> concat "" ["    bic "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Bics dst s1 s2 -> concat "" ["    bics "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]

  | Lsl dst s1 s2 -> concat "" ["    lsl "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Lsr dst s1 s2 -> concat "" ["    lsr "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Asr dst s1 s2 -> concat "" ["    asr "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Ror dst s1 s2 -> concat "" ["    ror "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Extr dst s1 s2 lsb -> concat "" ["    extr "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; ", #"; string_of_int lsb; "\n"]
  | Clz dst src -> concat "" ["    clz "; print_reg dst; ", "; print_reg src; "\n"]
  | Rbit dst src -> concat "" ["    rbit "; print_reg dst; ", "; print_reg src; "\n"]
  | Rev dst src -> concat "" ["    rev "; print_reg dst; ", "; print_reg src; "\n"]
  | Rev16 dst src -> concat "" ["    rev16 "; print_reg dst; ", "; print_reg src; "\n"]
  | Rev32 dst src -> concat "" ["    rev32 "; print_reg dst; ", "; print_reg src; "\n"]
  | Rev64 dst src -> concat "" ["    rev "; print_reg dst; ", "; print_reg src; "\n"]

  | Csel dst s1 s2 cond -> concat "" ["    csel "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; ", "; print_cond cond; "\n"]
  | Cset dst cond -> concat "" ["    cset "; print_reg dst; ", "; print_cond cond; "\n"]
  | Csetm dst cond -> concat "" ["    csetm "; print_reg dst; ", "; print_cond cond; "\n"]
  | Cinc dst s1 cond -> concat "" ["    cinc "; print_reg dst; ", "; print_reg s1; ", "; print_cond cond; "\n"]
  | Cinv dst s1 cond -> concat "" ["    cinv "; print_reg dst; ", "; print_reg s1; ", "; print_cond cond; "\n"]
  | Cneg dst s1 cond -> concat "" ["    cneg "; print_reg dst; ", "; print_reg s1; ", "; print_cond cond; "\n"]
  | Csinc dst s1 s2 cond -> concat "" ["    csinc "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; ", "; print_cond cond; "\n"]
  | Csinv dst s1 s2 cond -> concat "" ["    csinv "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; ", "; print_cond cond; "\n"]
  | Csneg dst s1 s2 cond -> concat "" ["    csneg "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; ", "; print_cond cond; "\n"]

  | Ldr dst addr -> concat "" ["    ldr "; print_reg dst; ", "; print_maddr addr; "\n"]
  | Ldr32 dst addr -> concat "" ["    ldr "; print_wreg dst; ", "; print_maddr addr; "\n"]
  | Ldrb dst addr -> concat "" ["    ldrb "; print_wreg dst; ", "; print_maddr addr; "\n"]
  | Ldrh dst addr -> concat "" ["    ldrh "; print_wreg dst; ", "; print_maddr addr; "\n"]
  | Ldrsb dst addr -> concat "" ["    ldrsb "; print_reg dst; ", "; print_maddr addr; "\n"]
  | Ldrsh dst addr -> concat "" ["    ldrsh "; print_reg dst; ", "; print_maddr addr; "\n"]
  | Ldrsw dst addr -> concat "" ["    ldrsw "; print_reg dst; ", "; print_maddr addr; "\n"]
  | Str src addr -> concat "" ["    str "; print_reg src; ", "; print_maddr addr; "\n"]
  | Str32 src addr -> concat "" ["    str "; print_wreg src; ", "; print_maddr addr; "\n"]
  | Strb src addr -> concat "" ["    strb "; print_wreg src; ", "; print_maddr addr; "\n"]
  | Strh src addr -> concat "" ["    strh "; print_wreg src; ", "; print_maddr addr; "\n"]
  | Add32 dst s1 s2 -> concat "" ["    add "; print_wreg dst; ", "; print_wreg s1; ", "; print_operand s2; "\n"]
  | Sub32 dst s1 s2 -> concat "" ["    sub "; print_wreg dst; ", "; print_wreg s1; ", "; print_operand s2; "\n"]
  | Eor32 dst s1 s2 -> concat "" ["    eor "; print_wreg dst; ", "; print_wreg s1; ", "; print_operand s2; "\n"]
  | And32 dst s1 s2 -> concat "" ["    and "; print_wreg dst; ", "; print_wreg s1; ", "; print_operand s2; "\n"]
  | Orr32 dst s1 s2 -> concat "" ["    orr "; print_wreg dst; ", "; print_wreg s1; ", "; print_operand s2; "\n"]
  | Ror32 dst s1 s2 -> concat "" ["    ror "; print_wreg dst; ", "; print_wreg s1; ", "; print_operand s2; "\n"]
  | Ldp d1 d2 addr -> concat "" ["    ldp "; print_reg d1; ", "; print_reg d2; ", "; print_maddr addr; "\n"]
  | Stp s1 s2 addr -> concat "" ["    stp "; print_reg s1; ", "; print_reg s2; ", "; print_maddr addr; "\n"]

  | B lbl -> concat "" ["    b "; lbl; "\n"]
  | BCond cond lbl -> concat "" ["    b."; print_cond cond; " "; lbl; "\n"]
  | Bl lbl -> concat "" ["    bl "; lbl; "\n"]
  | Blr r -> concat "" ["    blr "; print_reg r; "\n"]
  | Ret -> "    ret\n"
  | Cbz r lbl -> concat "" ["    cbz "; print_reg r; ", "; lbl; "\n"]
  | Cbnz r lbl -> concat "" ["    cbnz "; print_reg r; ", "; lbl; "\n"]

  | FmovGprToVec dst src -> concat "" ["    fmov "; print_fpreg dst FPD; ", "; print_reg src; "\n"]
  | FmovVecToGpr dst src -> concat "" ["    fmov "; print_reg dst; ", "; print_fpreg src FPD; "\n"]
  | Fadd dst s1 s2 -> concat "" ["    fadd "; print_vec dst; ".2d, "; print_vec s1; ".2d, "; print_vec s2; ".2d\n"]
  | Fsub dst s1 s2 -> concat "" ["    fsub "; print_vec dst; ".2d, "; print_vec s1; ".2d, "; print_vec s2; ".2d\n"]
  | Fmul dst s1 s2 -> concat "" ["    fmul "; print_vec dst; ".2d, "; print_vec s1; ".2d, "; print_vec s2; ".2d\n"]
  | Fdiv dst s1 s2 -> concat "" ["    fdiv "; print_vec dst; ".2d, "; print_vec s1; ".2d, "; print_vec s2; ".2d\n"]

  | Dup dst src -> concat "" ["    dup "; print_vec dst; ".16b, "; print_wreg src; "\n"]
  | InsLane dst dst_lane src -> concat "" ["    ins "; print_vec dst; ".d["; string_of_int dst_lane; "], "; print_reg src; "\n"]
  | Umov dst src src_lane -> concat "" ["    umov "; print_reg dst; ", "; print_vec src; ".d["; string_of_int src_lane; "]\n"]
  | Smov dst src src_lane -> concat "" ["    smov "; print_reg dst; ", "; print_vec src; ".d["; string_of_int src_lane; "]\n"]
  | Vadd dst s1 s2 arr -> concat "" ["    add "; print_vec dst; print_arrangement arr; ", "; print_vec s1; print_arrangement arr; ", "; print_vec s2; print_arrangement arr; "\n"]
  | Vsub dst s1 s2 arr -> concat "" ["    sub "; print_vec dst; print_arrangement arr; ", "; print_vec s1; print_arrangement arr; ", "; print_vec s2; print_arrangement arr; "\n"]
  | Vmul dst s1 s2 arr -> concat "" ["    mul "; print_vec dst; print_arrangement arr; ", "; print_vec s1; print_arrangement arr; ", "; print_vec s2; print_arrangement arr; "\n"]
  | Vand dst s1 s2 -> concat "" ["    and "; print_vec dst; ".16b, "; print_vec s1; ".16b, "; print_vec s2; ".16b\n"]
  | Vorr dst s1 s2 -> concat "" ["    orr "; print_vec dst; ".16b, "; print_vec s1; ".16b, "; print_vec s2; ".16b\n"]
  | Veor dst s1 s2 -> concat "" ["    eor "; print_vec dst; ".16b, "; print_vec s1; ".16b, "; print_vec s2; ".16b\n"]
  | Vext8 dst s1 s2 idx -> concat "" ["    ext "; print_vec dst; ".16b, "; print_vec s1; ".16b, "; print_vec s2; ".16b, #"; string_of_int idx; "\n"]
  | Vrev64 dst src arr -> concat "" ["    rev64 "; print_vec dst; print_arrangement arr; ", "; print_vec src; print_arrangement arr; "\n"]
  | Vrev32 dst src arr -> concat "" ["    rev32 "; print_vec dst; print_arrangement arr; ", "; print_vec src; print_arrangement arr; "\n"]
  | Vrev16 dst src arr -> concat "" ["    rev16 "; print_vec dst; print_arrangement arr; ", "; print_vec src; print_arrangement arr; "\n"]

  | Pmull dst s1 s2 -> concat "" ["    pmull "; print_vec dst; ".1q, "; print_vec s1; ".1d, "; print_vec s2; ".1d\n"]
  | Pmull2 dst s1 s2 -> concat "" ["    pmull2 "; print_vec dst; ".1q, "; print_vec s1; ".2d, "; print_vec s2; ".2d\n"]
  | Aese dst src -> concat "" ["    aese "; print_vec dst; ".16b, "; print_vec src; ".16b\n"]
  | Aesd dst src -> concat "" ["    aesd "; print_vec dst; ".16b, "; print_vec src; ".16b\n"]
  | Aesmc dst src -> concat "" ["    aesmc "; print_vec dst; ".16b, "; print_vec src; ".16b\n"]
  | Aesimc dst src -> concat "" ["    aesimc "; print_vec dst; ".16b, "; print_vec src; ".16b\n"]
  | Sha1c dst s1 s2 -> concat "" ["    sha1c "; print_vec dst; ".4s, "; print_vec s1; ".4s, "; print_vec s2; ".4s\n"]
  | Sha1p dst s1 s2 -> concat "" ["    sha1p "; print_vec dst; ".4s, "; print_vec s1; ".4s, "; print_vec s2; ".4s\n"]
  | Sha1m dst s1 s2 -> concat "" ["    sha1m "; print_vec dst; ".4s, "; print_vec s1; ".4s, "; print_vec s2; ".4s\n"]
  | Sha1h dst src -> concat "" ["    sha1h "; print_vec dst; ".4s, "; print_vec src; ".4s\n"]
  | Sha1su0 dst s1 s2 -> concat "" ["    sha1su0 "; print_vec dst; ".4s, "; print_vec s1; ".4s, "; print_vec s2; ".4s\n"]
  | Sha1su1 dst src -> concat "" ["    sha1su1 "; print_vec dst; ".4s, "; print_vec src; ".4s\n"]
  | Sha256h dst s1 s2 -> concat "" ["    sha256h "; print_vec dst; ".4s, "; print_vec s1; ".4s, "; print_vec s2; ".4s\n"]
  | Sha256h2 dst s1 s2 -> concat "" ["    sha256h2 "; print_vec dst; ".4s, "; print_vec s1; ".4s, "; print_vec s2; ".4s\n"]
  | Sha256su0 dst src -> concat "" ["    sha256su0 "; print_vec dst; ".4s, "; print_vec src; ".4s\n"]
  | Sha256su1 dst s1 s2 -> concat "" ["    sha256su1 "; print_vec dst; ".4s, "; print_vec s1; ".4s, "; print_vec s2; ".4s\n"]
  | Eor3 dst s1 s2 s3 -> concat "" ["    eor3 "; print_vec dst; ".16b, "; print_vec s1; ".16b, "; print_vec s2; ".16b, "; print_vec s3; ".16b\n"]
  | Rax1 dst s1 s2 -> concat "" ["    rax1 "; print_vec dst; ".2d, "; print_vec s1; ".2d, "; print_vec s2; ".2d\n"]
  | Xar dst s1 s2 imm -> concat "" ["    xar "; print_vec dst; ".2d, "; print_vec s1; ".2d, "; print_vec s2; ".2d, #"; string_of_int imm; "\n"]

  | Ld1 v arr addr -> concat "" ["    ld1 {"; print_vec v; print_arrangement arr; "}, "; print_maddr addr; "\n"]
  | St1 v arr addr -> concat "" ["    st1 {"; print_vec v; print_arrangement arr; "}, "; print_maddr addr; "\n"]

  | MsrDit enable ->
      if enable then "    msr dit, #1\n"
      else "    msr dit, #0\n"

let rec print_code (c: code) : Tot string (decreases c) =
  match c with
  | Ins i -> print_ins i
  | Block cs -> print_codes cs
  | IfElse _ c1 c2 ->
      concat "" [print_code c1; print_code c2]
  | While _ body ->
      print_code body

and print_codes (cs: list code) : Tot string (decreases cs) =
  match cs with
  | [] -> ""
  | c :: rest -> concat "" [print_code c; print_codes rest]

let emit_header (name: string) : string =
  concat "" [
    "    .global "; name; "\n";
    "    .type "; name; ", %function\n";
    "    .p2align 2\n";
    name; ":\n";
    "    .cfi_startproc\n"
  ]

let emit_footer (name: string) : string =
  concat "" [
    "    ret\n";
    "    .cfi_endproc\n";
    "    .size "; name; ", .-"; name; "\n\n"
  ]

let emit_proc_asm (p: proc_decl) : string =
  if p.proc_is_ins then ""
  else
    match Valerie.Compiler.Lower.AArch64.lower_stmts p.proc_body with
    | Some c ->
        concat "" [
          emit_header p.proc_name;
          print_code c;
          emit_footer p.proc_name
        ]
    | None ->
        concat "" ["// Failed to lower procedure: "; p.proc_name; "\n"]

let rec emit_procs_asm (ps: list proc_decl) : Tot string (decreases ps) =
  match ps with
  | [] -> ""
  | p :: rest ->
      concat "" [emit_proc_asm p; emit_procs_asm rest]

let emit_module_asm (m: module_decl) (has_crypto: bool) (has_dit: bool) : string =
  let arch_dir =
    if has_crypto then "    .arch armv8-a+crypto\n"
    else if has_dit then "    .arch armv8.4-a\n"
    else "    .arch armv8-a\n"
  in
  let text_sec = concat "" [
    "    .text\n";
    arch_dir;
    "    .p2align 2\n\n"
  ] in
  concat "" [
    "// Emitted by Vale verified assembly compiler (AArch64)\n";
    text_sec;
    emit_procs_asm m.mod_procs;
    "    .section .note.GNU-stack,\"\",@progbits\n"
  ]
