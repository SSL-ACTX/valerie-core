open Prims
let print_reg (r : Vale_AArch64_Machine.reg) : Prims.string=
  match r with
  | Vale_AArch64_Machine.X0 -> "x0"
  | Vale_AArch64_Machine.X1 -> "x1"
  | Vale_AArch64_Machine.X2 -> "x2"
  | Vale_AArch64_Machine.X3 -> "x3"
  | Vale_AArch64_Machine.X4 -> "x4"
  | Vale_AArch64_Machine.X5 -> "x5"
  | Vale_AArch64_Machine.X6 -> "x6"
  | Vale_AArch64_Machine.X7 -> "x7"
  | Vale_AArch64_Machine.X8 -> "x8"
  | Vale_AArch64_Machine.X9 -> "x9"
  | Vale_AArch64_Machine.X10 -> "x10"
  | Vale_AArch64_Machine.X11 -> "x11"
  | Vale_AArch64_Machine.X12 -> "x12"
  | Vale_AArch64_Machine.X13 -> "x13"
  | Vale_AArch64_Machine.X14 -> "x14"
  | Vale_AArch64_Machine.X15 -> "x15"
  | Vale_AArch64_Machine.X16 -> "x16"
  | Vale_AArch64_Machine.X17 -> "x17"
  | Vale_AArch64_Machine.X18 -> "x18"
  | Vale_AArch64_Machine.X19 -> "x19"
  | Vale_AArch64_Machine.X20 -> "x20"
  | Vale_AArch64_Machine.X21 -> "x21"
  | Vale_AArch64_Machine.X22 -> "x22"
  | Vale_AArch64_Machine.X23 -> "x23"
  | Vale_AArch64_Machine.X24 -> "x24"
  | Vale_AArch64_Machine.X25 -> "x25"
  | Vale_AArch64_Machine.X26 -> "x26"
  | Vale_AArch64_Machine.X27 -> "x27"
  | Vale_AArch64_Machine.X28 -> "x28"
  | Vale_AArch64_Machine.X29 -> "x29"
  | Vale_AArch64_Machine.X30 -> "x30"
  | Vale_AArch64_Machine.SP -> "sp"
  | Vale_AArch64_Machine.XZR -> "xzr"
let print_wreg (r : Vale_AArch64_Machine.reg) : Prims.string=
  match r with
  | Vale_AArch64_Machine.X0 -> "w0"
  | Vale_AArch64_Machine.X1 -> "w1"
  | Vale_AArch64_Machine.X2 -> "w2"
  | Vale_AArch64_Machine.X3 -> "w3"
  | Vale_AArch64_Machine.X4 -> "w4"
  | Vale_AArch64_Machine.X5 -> "w5"
  | Vale_AArch64_Machine.X6 -> "w6"
  | Vale_AArch64_Machine.X7 -> "w7"
  | Vale_AArch64_Machine.X8 -> "w8"
  | Vale_AArch64_Machine.X9 -> "w9"
  | Vale_AArch64_Machine.X10 -> "w10"
  | Vale_AArch64_Machine.X11 -> "w11"
  | Vale_AArch64_Machine.X12 -> "w12"
  | Vale_AArch64_Machine.X13 -> "w13"
  | Vale_AArch64_Machine.X14 -> "w14"
  | Vale_AArch64_Machine.X15 -> "w15"
  | Vale_AArch64_Machine.X16 -> "w16"
  | Vale_AArch64_Machine.X17 -> "w17"
  | Vale_AArch64_Machine.X18 -> "w18"
  | Vale_AArch64_Machine.X19 -> "w19"
  | Vale_AArch64_Machine.X20 -> "w20"
  | Vale_AArch64_Machine.X21 -> "w21"
  | Vale_AArch64_Machine.X22 -> "w22"
  | Vale_AArch64_Machine.X23 -> "w23"
  | Vale_AArch64_Machine.X24 -> "w24"
  | Vale_AArch64_Machine.X25 -> "w25"
  | Vale_AArch64_Machine.X26 -> "w26"
  | Vale_AArch64_Machine.X27 -> "w27"
  | Vale_AArch64_Machine.X28 -> "w28"
  | Vale_AArch64_Machine.X29 -> "w29"
  | Vale_AArch64_Machine.X30 -> "w30"
  | Vale_AArch64_Machine.SP -> "wsp"
  | Vale_AArch64_Machine.XZR -> "wzr"
let print_vec (v : Vale_AArch64_Machine.vec) : Prims.string=
  match v with
  | Vale_AArch64_Machine.V0 -> "v0"
  | Vale_AArch64_Machine.V1 -> "v1"
  | Vale_AArch64_Machine.V2 -> "v2"
  | Vale_AArch64_Machine.V3 -> "v3"
  | Vale_AArch64_Machine.V4 -> "v4"
  | Vale_AArch64_Machine.V5 -> "v5"
  | Vale_AArch64_Machine.V6 -> "v6"
  | Vale_AArch64_Machine.V7 -> "v7"
  | Vale_AArch64_Machine.V8 -> "v8"
  | Vale_AArch64_Machine.V9 -> "v9"
  | Vale_AArch64_Machine.V10 -> "v10"
  | Vale_AArch64_Machine.V11 -> "v11"
  | Vale_AArch64_Machine.V12 -> "v12"
  | Vale_AArch64_Machine.V13 -> "v13"
  | Vale_AArch64_Machine.V14 -> "v14"
  | Vale_AArch64_Machine.V15 -> "v15"
  | Vale_AArch64_Machine.V16 -> "v16"
  | Vale_AArch64_Machine.V17 -> "v17"
  | Vale_AArch64_Machine.V18 -> "v18"
  | Vale_AArch64_Machine.V19 -> "v19"
  | Vale_AArch64_Machine.V20 -> "v20"
  | Vale_AArch64_Machine.V21 -> "v21"
  | Vale_AArch64_Machine.V22 -> "v22"
  | Vale_AArch64_Machine.V23 -> "v23"
  | Vale_AArch64_Machine.V24 -> "v24"
  | Vale_AArch64_Machine.V25 -> "v25"
  | Vale_AArch64_Machine.V26 -> "v26"
  | Vale_AArch64_Machine.V27 -> "v27"
  | Vale_AArch64_Machine.V28 -> "v28"
  | Vale_AArch64_Machine.V29 -> "v29"
  | Vale_AArch64_Machine.V30 -> "v30"
  | Vale_AArch64_Machine.V31 -> "v31"
let print_arrangement (arr : Vale_AArch64_Machine.vec_arrangement) :
  Prims.string=
  match arr with
  | Vale_AArch64_Machine.B16 -> ".16b"
  | Vale_AArch64_Machine.H8 -> ".8h"
  | Vale_AArch64_Machine.S4 -> ".4s"
  | Vale_AArch64_Machine.D2 -> ".2d"
  | Vale_AArch64_Machine.B8 -> ".8b"
  | Vale_AArch64_Machine.H4 -> ".4h"
  | Vale_AArch64_Machine.S2 -> ".2s"
  | Vale_AArch64_Machine.D1 -> ".1d"
  | Vale_AArch64_Machine.Q1 -> ".1q"
let print_cond (c : Vale_AArch64_Machine.cond_code) : Prims.string=
  match c with
  | Vale_AArch64_Machine.CondEQ -> "eq"
  | Vale_AArch64_Machine.CondNE -> "ne"
  | Vale_AArch64_Machine.CondCS -> "cs"
  | Vale_AArch64_Machine.CondCC -> "cc"
  | Vale_AArch64_Machine.CondMI -> "mi"
  | Vale_AArch64_Machine.CondPL -> "pl"
  | Vale_AArch64_Machine.CondVS -> "vs"
  | Vale_AArch64_Machine.CondVC -> "vc"
  | Vale_AArch64_Machine.CondHI -> "hi"
  | Vale_AArch64_Machine.CondLS -> "ls"
  | Vale_AArch64_Machine.CondGE -> "ge"
  | Vale_AArch64_Machine.CondLT -> "lt"
  | Vale_AArch64_Machine.CondGT -> "gt"
  | Vale_AArch64_Machine.CondLE -> "le"
  | Vale_AArch64_Machine.CondAL -> "al"
  | Vale_AArch64_Machine.CondNV -> "nv"
let print_maddr (m : Vale_AArch64_Machine.maddr) : Prims.string=
  match m with
  | Vale_AArch64_Machine.MReg base ->
      FStar_String.concat "" ["["; print_reg base; "]"]
  | Vale_AArch64_Machine.MOffset (base, offset) ->
      FStar_String.concat ""
        ["["; print_reg base; ", #"; Prims.string_of_int offset; "]"]
  | Vale_AArch64_Machine.MPreIndex (base, offset) ->
      FStar_String.concat ""
        ["["; print_reg base; ", #"; Prims.string_of_int offset; "]!"]
  | Vale_AArch64_Machine.MPostIndex (base, offset) ->
      FStar_String.concat ""
        ["["; print_reg base; "], #"; Prims.string_of_int offset]
  | Vale_AArch64_Machine.MRegOffset (base, index, uu___, shift) ->
      if shift = Prims.int_zero
      then
        FStar_String.concat ""
          ["["; print_reg base; ", "; print_reg index; "]"]
      else
        FStar_String.concat ""
          ["[";
          print_reg base;
          ", ";
          print_reg index;
          ", lsl #";
          Prims.string_of_int shift;
          "]"]
let print_fpreg (v : Vale_AArch64_Machine.vec)
  (w : Vale_AArch64_Machine.fp_width) : Prims.string=
  let prefix =
    match w with
    | Vale_AArch64_Machine.FPS -> "s"
    | Vale_AArch64_Machine.FPD -> "d"
    | Vale_AArch64_Machine.FPQ -> "q" in
  let num_str =
    match v with
    | Vale_AArch64_Machine.V0 -> "0"
    | Vale_AArch64_Machine.V1 -> "1"
    | Vale_AArch64_Machine.V2 -> "2"
    | Vale_AArch64_Machine.V3 -> "3"
    | Vale_AArch64_Machine.V4 -> "4"
    | Vale_AArch64_Machine.V5 -> "5"
    | Vale_AArch64_Machine.V6 -> "6"
    | Vale_AArch64_Machine.V7 -> "7"
    | Vale_AArch64_Machine.V8 -> "8"
    | Vale_AArch64_Machine.V9 -> "9"
    | Vale_AArch64_Machine.V10 -> "10"
    | Vale_AArch64_Machine.V11 -> "11"
    | Vale_AArch64_Machine.V12 -> "12"
    | Vale_AArch64_Machine.V13 -> "13"
    | Vale_AArch64_Machine.V14 -> "14"
    | Vale_AArch64_Machine.V15 -> "15"
    | Vale_AArch64_Machine.V16 -> "16"
    | Vale_AArch64_Machine.V17 -> "17"
    | Vale_AArch64_Machine.V18 -> "18"
    | Vale_AArch64_Machine.V19 -> "19"
    | Vale_AArch64_Machine.V20 -> "20"
    | Vale_AArch64_Machine.V21 -> "21"
    | Vale_AArch64_Machine.V22 -> "22"
    | Vale_AArch64_Machine.V23 -> "23"
    | Vale_AArch64_Machine.V24 -> "24"
    | Vale_AArch64_Machine.V25 -> "25"
    | Vale_AArch64_Machine.V26 -> "26"
    | Vale_AArch64_Machine.V27 -> "27"
    | Vale_AArch64_Machine.V28 -> "28"
    | Vale_AArch64_Machine.V29 -> "29"
    | Vale_AArch64_Machine.V30 -> "30"
    | Vale_AArch64_Machine.V31 -> "31" in
  FStar_String.concat "" [prefix; num_str]
let print_operand (op : Vale_AArch64_Semantics.operand) : Prims.string=
  match op with
  | Vale_AArch64_Semantics.OReg (r, Vale_AArch64_Machine.W64) -> print_reg r
  | Vale_AArch64_Semantics.OReg (r, Vale_AArch64_Machine.W32) -> print_wreg r
  | Vale_AArch64_Semantics.OImm imm ->
      FStar_String.concat "" ["#"; FStar_UInt64.to_string imm]
  | Vale_AArch64_Semantics.OFpReg (v, w) -> print_fpreg v w
  | Vale_AArch64_Semantics.OVecReg (v, arr) ->
      FStar_String.concat "" [print_vec v; print_arrangement arr]
  | Vale_AArch64_Semantics.OVecElem (v, sz, lane) ->
      let s =
        match sz with
        | Vale_AArch64_Machine.EB -> "b"
        | Vale_AArch64_Machine.EH -> "h"
        | Vale_AArch64_Machine.ES -> "s"
        | Vale_AArch64_Machine.ED -> "d" in
      FStar_String.concat ""
        [print_vec v; "."; s; "["; Prims.string_of_int lane; "]"]
  | Vale_AArch64_Semantics.OShifted
      (r, Vale_AArch64_Machine.W64, Vale_AArch64_Machine.LSL, amt) ->
      FStar_String.concat ""
        [print_reg r; ", lsl #"; Prims.string_of_int amt]
  | Vale_AArch64_Semantics.OShifted
      (r, Vale_AArch64_Machine.W64, Vale_AArch64_Machine.LSR, amt) ->
      FStar_String.concat ""
        [print_reg r; ", lsr #"; Prims.string_of_int amt]
  | Vale_AArch64_Semantics.OShifted
      (r, Vale_AArch64_Machine.W64, Vale_AArch64_Machine.ASR, amt) ->
      FStar_String.concat ""
        [print_reg r; ", asr #"; Prims.string_of_int amt]
  | Vale_AArch64_Semantics.OShifted
      (r, Vale_AArch64_Machine.W64, Vale_AArch64_Machine.ROR, amt) ->
      FStar_String.concat ""
        [print_reg r; ", ror #"; Prims.string_of_int amt]
  | Vale_AArch64_Semantics.OShifted
      (r, Vale_AArch64_Machine.W32, Vale_AArch64_Machine.LSL, amt) ->
      FStar_String.concat ""
        [print_wreg r; ", lsl #"; Prims.string_of_int amt]
  | Vale_AArch64_Semantics.OShifted
      (r, Vale_AArch64_Machine.W32, Vale_AArch64_Machine.LSR, amt) ->
      FStar_String.concat ""
        [print_wreg r; ", lsr #"; Prims.string_of_int amt]
  | Vale_AArch64_Semantics.OShifted
      (r, Vale_AArch64_Machine.W32, Vale_AArch64_Machine.ASR, amt) ->
      FStar_String.concat ""
        [print_wreg r; ", asr #"; Prims.string_of_int amt]
  | Vale_AArch64_Semantics.OShifted
      (r, Vale_AArch64_Machine.W32, Vale_AArch64_Machine.ROR, amt) ->
      FStar_String.concat ""
        [print_wreg r; ", ror #"; Prims.string_of_int amt]
  | Vale_AArch64_Semantics.OExtended (r, uu___, uu___1, amt) ->
      if amt = Prims.int_zero
      then print_reg r
      else
        FStar_String.concat ""
          [print_reg r; ", uxtx #"; Prims.string_of_int amt]
  | Vale_AArch64_Semantics.OMem m -> print_maddr m
  | Vale_AArch64_Semantics.OCond c -> print_cond c
let print_ins (i : Vale_AArch64_Semantics.ins) : Prims.string=
  match i with
  | Vale_AArch64_Semantics.Mov (dst, src) ->
      FStar_String.concat ""
        ["    mov "; print_reg dst; ", "; print_operand src; "\n"]
  | Vale_AArch64_Semantics.Movz (dst, imm, shift) ->
      FStar_String.concat ""
        ["    movz ";
        print_reg dst;
        ", #";
        FStar_UInt64.to_string imm;
        ", lsl #";
        Prims.string_of_int shift;
        "\n"]
  | Vale_AArch64_Semantics.Movn (dst, imm, shift) ->
      FStar_String.concat ""
        ["    movn ";
        print_reg dst;
        ", #";
        FStar_UInt64.to_string imm;
        ", lsl #";
        Prims.string_of_int shift;
        "\n"]
  | Vale_AArch64_Semantics.Movk (dst, imm, shift) ->
      FStar_String.concat ""
        ["    movk ";
        print_reg dst;
        ", #";
        FStar_UInt64.to_string imm;
        ", lsl #";
        Prims.string_of_int shift;
        "\n"]
  | Vale_AArch64_Semantics.Adrp (dst, lbl) ->
      FStar_String.concat "" ["    adrp "; print_reg dst; ", "; lbl; "\n"]
  | Vale_AArch64_Semantics.Add (dst, s1, s2) ->
      FStar_String.concat ""
        ["    add ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_AArch64_Semantics.Adds (dst, s1, s2) ->
      FStar_String.concat ""
        ["    adds ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_AArch64_Semantics.Sub (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sub ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_AArch64_Semantics.Subs (dst, s1, s2) ->
      FStar_String.concat ""
        ["    subs ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_AArch64_Semantics.Adc (dst, s1, s2) ->
      FStar_String.concat ""
        ["    adc ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_AArch64_Semantics.Adcs (dst, s1, s2) ->
      FStar_String.concat ""
        ["    adcs ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_AArch64_Semantics.Sbc (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sbc ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_AArch64_Semantics.Sbcs (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sbcs ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_AArch64_Semantics.Cmp (s1, s2) ->
      FStar_String.concat ""
        ["    cmp "; print_reg s1; ", "; print_operand s2; "\n"]
  | Vale_AArch64_Semantics.Cmn (s1, s2) ->
      FStar_String.concat ""
        ["    cmn "; print_reg s1; ", "; print_operand s2; "\n"]
  | Vale_AArch64_Semantics.Tst (s1, s2) ->
      FStar_String.concat ""
        ["    tst "; print_reg s1; ", "; print_operand s2; "\n"]
  | Vale_AArch64_Semantics.Neg (dst, src) ->
      FStar_String.concat ""
        ["    neg "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_AArch64_Semantics.Negs (dst, src) ->
      FStar_String.concat ""
        ["    negs "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_AArch64_Semantics.Mvn (dst, src) ->
      FStar_String.concat ""
        ["    mvn "; print_reg dst; ", "; print_operand src; "\n"]
  | Vale_AArch64_Semantics.Mul (dst, s1, s2) ->
      FStar_String.concat ""
        ["    mul ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_AArch64_Semantics.Madd (dst, s1, s2, s3) ->
      FStar_String.concat ""
        ["    madd ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        ", ";
        print_reg s3;
        "\n"]
  | Vale_AArch64_Semantics.Msub (dst, s1, s2, s3) ->
      FStar_String.concat ""
        ["    msub ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        ", ";
        print_reg s3;
        "\n"]
  | Vale_AArch64_Semantics.Smulh (dst, s1, s2) ->
      FStar_String.concat ""
        ["    smulh ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_AArch64_Semantics.Umulh (dst, s1, s2) ->
      FStar_String.concat ""
        ["    umulh ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_AArch64_Semantics.Sdiv (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sdiv ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_AArch64_Semantics.Udiv (dst, s1, s2) ->
      FStar_String.concat ""
        ["    udiv ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_AArch64_Semantics.And (dst, s1, s2) ->
      FStar_String.concat ""
        ["    and ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_AArch64_Semantics.Ands (dst, s1, s2) ->
      FStar_String.concat ""
        ["    ands ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_AArch64_Semantics.Orr (dst, s1, s2) ->
      FStar_String.concat ""
        ["    orr ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_AArch64_Semantics.Orn (dst, s1, s2) ->
      FStar_String.concat ""
        ["    orn ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_AArch64_Semantics.Eor (dst, s1, s2) ->
      FStar_String.concat ""
        ["    eor ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_AArch64_Semantics.Eon (dst, s1, s2) ->
      FStar_String.concat ""
        ["    eon ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_AArch64_Semantics.Bic (dst, s1, s2) ->
      FStar_String.concat ""
        ["    bic ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_AArch64_Semantics.Bics (dst, s1, s2) ->
      FStar_String.concat ""
        ["    bics ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_AArch64_Semantics.Lsl (dst, s1, s2) ->
      FStar_String.concat ""
        ["    lsl ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_AArch64_Semantics.Lsr (dst, s1, s2) ->
      FStar_String.concat ""
        ["    lsr ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_AArch64_Semantics.Asr (dst, s1, s2) ->
      FStar_String.concat ""
        ["    asr ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_AArch64_Semantics.Ror (dst, s1, s2) ->
      FStar_String.concat ""
        ["    ror ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_AArch64_Semantics.Extr (dst, s1, s2, lsb) ->
      FStar_String.concat ""
        ["    extr ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        ", #";
        Prims.string_of_int lsb;
        "\n"]
  | Vale_AArch64_Semantics.Clz (dst, src) ->
      FStar_String.concat ""
        ["    clz "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_AArch64_Semantics.Rbit (dst, src) ->
      FStar_String.concat ""
        ["    rbit "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_AArch64_Semantics.Rev (dst, src) ->
      FStar_String.concat ""
        ["    rev "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_AArch64_Semantics.Rev16 (dst, src) ->
      FStar_String.concat ""
        ["    rev16 "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_AArch64_Semantics.Rev32 (dst, src) ->
      FStar_String.concat ""
        ["    rev32 "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_AArch64_Semantics.Rev64 (dst, src) ->
      FStar_String.concat ""
        ["    rev "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_AArch64_Semantics.Csel (dst, s1, s2, cond) ->
      FStar_String.concat ""
        ["    csel ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        ", ";
        print_cond cond;
        "\n"]
  | Vale_AArch64_Semantics.Cset (dst, cond) ->
      FStar_String.concat ""
        ["    cset "; print_reg dst; ", "; print_cond cond; "\n"]
  | Vale_AArch64_Semantics.Csetm (dst, cond) ->
      FStar_String.concat ""
        ["    csetm "; print_reg dst; ", "; print_cond cond; "\n"]
  | Vale_AArch64_Semantics.Cinc (dst, s1, cond) ->
      FStar_String.concat ""
        ["    cinc ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_cond cond;
        "\n"]
  | Vale_AArch64_Semantics.Cinv (dst, s1, cond) ->
      FStar_String.concat ""
        ["    cinv ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_cond cond;
        "\n"]
  | Vale_AArch64_Semantics.Cneg (dst, s1, cond) ->
      FStar_String.concat ""
        ["    cneg ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_cond cond;
        "\n"]
  | Vale_AArch64_Semantics.Csinc (dst, s1, s2, cond) ->
      FStar_String.concat ""
        ["    csinc ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        ", ";
        print_cond cond;
        "\n"]
  | Vale_AArch64_Semantics.Csinv (dst, s1, s2, cond) ->
      FStar_String.concat ""
        ["    csinv ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        ", ";
        print_cond cond;
        "\n"]
  | Vale_AArch64_Semantics.Csneg (dst, s1, s2, cond) ->
      FStar_String.concat ""
        ["    csneg ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        ", ";
        print_cond cond;
        "\n"]
  | Vale_AArch64_Semantics.Ldr (dst, addr) ->
      FStar_String.concat ""
        ["    ldr "; print_reg dst; ", "; print_maddr addr; "\n"]
  | Vale_AArch64_Semantics.Ldr32 (dst, addr) ->
      FStar_String.concat ""
        ["    ldr "; print_wreg dst; ", "; print_maddr addr; "\n"]
  | Vale_AArch64_Semantics.Ldrb (dst, addr) ->
      FStar_String.concat ""
        ["    ldrb "; print_wreg dst; ", "; print_maddr addr; "\n"]
  | Vale_AArch64_Semantics.Ldrh (dst, addr) ->
      FStar_String.concat ""
        ["    ldrh "; print_wreg dst; ", "; print_maddr addr; "\n"]
  | Vale_AArch64_Semantics.Ldrsb (dst, addr) ->
      FStar_String.concat ""
        ["    ldrsb "; print_reg dst; ", "; print_maddr addr; "\n"]
  | Vale_AArch64_Semantics.Ldrsh (dst, addr) ->
      FStar_String.concat ""
        ["    ldrsh "; print_reg dst; ", "; print_maddr addr; "\n"]
  | Vale_AArch64_Semantics.Ldrsw (dst, addr) ->
      FStar_String.concat ""
        ["    ldrsw "; print_reg dst; ", "; print_maddr addr; "\n"]
  | Vale_AArch64_Semantics.Str (src, addr) ->
      FStar_String.concat ""
        ["    str "; print_reg src; ", "; print_maddr addr; "\n"]
  | Vale_AArch64_Semantics.Str32 (src, addr) ->
      FStar_String.concat ""
        ["    str "; print_wreg src; ", "; print_maddr addr; "\n"]
  | Vale_AArch64_Semantics.Strb (src, addr) ->
      FStar_String.concat ""
        ["    strb "; print_wreg src; ", "; print_maddr addr; "\n"]
  | Vale_AArch64_Semantics.Strh (src, addr) ->
      FStar_String.concat ""
        ["    strh "; print_wreg src; ", "; print_maddr addr; "\n"]
  | Vale_AArch64_Semantics.Add32 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    add ";
        print_wreg dst;
        ", ";
        print_wreg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_AArch64_Semantics.Sub32 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sub ";
        print_wreg dst;
        ", ";
        print_wreg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_AArch64_Semantics.Eor32 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    eor ";
        print_wreg dst;
        ", ";
        print_wreg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_AArch64_Semantics.And32 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    and ";
        print_wreg dst;
        ", ";
        print_wreg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_AArch64_Semantics.Orr32 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    orr ";
        print_wreg dst;
        ", ";
        print_wreg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_AArch64_Semantics.Ror32 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    ror ";
        print_wreg dst;
        ", ";
        print_wreg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_AArch64_Semantics.Ldp (d1, d2, addr) ->
      FStar_String.concat ""
        ["    ldp ";
        print_reg d1;
        ", ";
        print_reg d2;
        ", ";
        print_maddr addr;
        "\n"]
  | Vale_AArch64_Semantics.Stp (s1, s2, addr) ->
      FStar_String.concat ""
        ["    stp ";
        print_reg s1;
        ", ";
        print_reg s2;
        ", ";
        print_maddr addr;
        "\n"]
  | Vale_AArch64_Semantics.B lbl ->
      FStar_String.concat "" ["    b "; lbl; "\n"]
  | Vale_AArch64_Semantics.BCond (cond, lbl) ->
      FStar_String.concat "" ["    b."; print_cond cond; " "; lbl; "\n"]
  | Vale_AArch64_Semantics.Bl lbl ->
      FStar_String.concat "" ["    bl "; lbl; "\n"]
  | Vale_AArch64_Semantics.Blr r ->
      FStar_String.concat "" ["    blr "; print_reg r; "\n"]
  | Vale_AArch64_Semantics.Ret -> "    ret\n"
  | Vale_AArch64_Semantics.Cbz (r, lbl) ->
      FStar_String.concat "" ["    cbz "; print_reg r; ", "; lbl; "\n"]
  | Vale_AArch64_Semantics.Cbnz (r, lbl) ->
      FStar_String.concat "" ["    cbnz "; print_reg r; ", "; lbl; "\n"]
  | Vale_AArch64_Semantics.FmovGprToVec (dst, src) ->
      FStar_String.concat ""
        ["    fmov ";
        print_fpreg dst Vale_AArch64_Machine.FPD;
        ", ";
        print_reg src;
        "\n"]
  | Vale_AArch64_Semantics.FmovVecToGpr (dst, src) ->
      FStar_String.concat ""
        ["    fmov ";
        print_reg dst;
        ", ";
        print_fpreg src Vale_AArch64_Machine.FPD;
        "\n"]
  | Vale_AArch64_Semantics.Fadd (dst, s1, s2) ->
      FStar_String.concat ""
        ["    fadd ";
        print_vec dst;
        ".2d, ";
        print_vec s1;
        ".2d, ";
        print_vec s2;
        ".2d\n"]
  | Vale_AArch64_Semantics.Fsub (dst, s1, s2) ->
      FStar_String.concat ""
        ["    fsub ";
        print_vec dst;
        ".2d, ";
        print_vec s1;
        ".2d, ";
        print_vec s2;
        ".2d\n"]
  | Vale_AArch64_Semantics.Fmul (dst, s1, s2) ->
      FStar_String.concat ""
        ["    fmul ";
        print_vec dst;
        ".2d, ";
        print_vec s1;
        ".2d, ";
        print_vec s2;
        ".2d\n"]
  | Vale_AArch64_Semantics.Fdiv (dst, s1, s2) ->
      FStar_String.concat ""
        ["    fdiv ";
        print_vec dst;
        ".2d, ";
        print_vec s1;
        ".2d, ";
        print_vec s2;
        ".2d\n"]
  | Vale_AArch64_Semantics.Dup (dst, src) ->
      FStar_String.concat ""
        ["    dup "; print_vec dst; ".16b, "; print_wreg src; "\n"]
  | Vale_AArch64_Semantics.InsLane (dst, dst_lane, src) ->
      FStar_String.concat ""
        ["    ins ";
        print_vec dst;
        ".d[";
        Prims.string_of_int dst_lane;
        "], ";
        print_reg src;
        "\n"]
  | Vale_AArch64_Semantics.Umov (dst, src, src_lane) ->
      FStar_String.concat ""
        ["    umov ";
        print_reg dst;
        ", ";
        print_vec src;
        ".d[";
        Prims.string_of_int src_lane;
        "]\n"]
  | Vale_AArch64_Semantics.Smov (dst, src, src_lane) ->
      FStar_String.concat ""
        ["    smov ";
        print_reg dst;
        ", ";
        print_vec src;
        ".d[";
        Prims.string_of_int src_lane;
        "]\n"]
  | Vale_AArch64_Semantics.Vadd (dst, s1, s2, arr) ->
      FStar_String.concat ""
        ["    add ";
        print_vec dst;
        print_arrangement arr;
        ", ";
        print_vec s1;
        print_arrangement arr;
        ", ";
        print_vec s2;
        print_arrangement arr;
        "\n"]
  | Vale_AArch64_Semantics.Vsub (dst, s1, s2, arr) ->
      FStar_String.concat ""
        ["    sub ";
        print_vec dst;
        print_arrangement arr;
        ", ";
        print_vec s1;
        print_arrangement arr;
        ", ";
        print_vec s2;
        print_arrangement arr;
        "\n"]
  | Vale_AArch64_Semantics.Vmul (dst, s1, s2, arr) ->
      FStar_String.concat ""
        ["    mul ";
        print_vec dst;
        print_arrangement arr;
        ", ";
        print_vec s1;
        print_arrangement arr;
        ", ";
        print_vec s2;
        print_arrangement arr;
        "\n"]
  | Vale_AArch64_Semantics.Vand (dst, s1, s2) ->
      FStar_String.concat ""
        ["    and ";
        print_vec dst;
        ".16b, ";
        print_vec s1;
        ".16b, ";
        print_vec s2;
        ".16b\n"]
  | Vale_AArch64_Semantics.Vorr (dst, s1, s2) ->
      FStar_String.concat ""
        ["    orr ";
        print_vec dst;
        ".16b, ";
        print_vec s1;
        ".16b, ";
        print_vec s2;
        ".16b\n"]
  | Vale_AArch64_Semantics.Veor (dst, s1, s2) ->
      FStar_String.concat ""
        ["    eor ";
        print_vec dst;
        ".16b, ";
        print_vec s1;
        ".16b, ";
        print_vec s2;
        ".16b\n"]
  | Vale_AArch64_Semantics.Vext8 (dst, s1, s2, idx) ->
      FStar_String.concat ""
        ["    ext ";
        print_vec dst;
        ".16b, ";
        print_vec s1;
        ".16b, ";
        print_vec s2;
        ".16b, #";
        Prims.string_of_int idx;
        "\n"]
  | Vale_AArch64_Semantics.Vrev64 (dst, src, arr) ->
      FStar_String.concat ""
        ["    rev64 ";
        print_vec dst;
        print_arrangement arr;
        ", ";
        print_vec src;
        print_arrangement arr;
        "\n"]
  | Vale_AArch64_Semantics.Vrev32 (dst, src, arr) ->
      FStar_String.concat ""
        ["    rev32 ";
        print_vec dst;
        print_arrangement arr;
        ", ";
        print_vec src;
        print_arrangement arr;
        "\n"]
  | Vale_AArch64_Semantics.Vrev16 (dst, src, arr) ->
      FStar_String.concat ""
        ["    rev16 ";
        print_vec dst;
        print_arrangement arr;
        ", ";
        print_vec src;
        print_arrangement arr;
        "\n"]
  | Vale_AArch64_Semantics.Pmull (dst, s1, s2) ->
      FStar_String.concat ""
        ["    pmull ";
        print_vec dst;
        ".1q, ";
        print_vec s1;
        ".1d, ";
        print_vec s2;
        ".1d\n"]
  | Vale_AArch64_Semantics.Pmull2 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    pmull2 ";
        print_vec dst;
        ".1q, ";
        print_vec s1;
        ".2d, ";
        print_vec s2;
        ".2d\n"]
  | Vale_AArch64_Semantics.Aese (dst, src) ->
      FStar_String.concat ""
        ["    aese "; print_vec dst; ".16b, "; print_vec src; ".16b\n"]
  | Vale_AArch64_Semantics.Aesd (dst, src) ->
      FStar_String.concat ""
        ["    aesd "; print_vec dst; ".16b, "; print_vec src; ".16b\n"]
  | Vale_AArch64_Semantics.Aesmc (dst, src) ->
      FStar_String.concat ""
        ["    aesmc "; print_vec dst; ".16b, "; print_vec src; ".16b\n"]
  | Vale_AArch64_Semantics.Aesimc (dst, src) ->
      FStar_String.concat ""
        ["    aesimc "; print_vec dst; ".16b, "; print_vec src; ".16b\n"]
  | Vale_AArch64_Semantics.Sha1c (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sha1c ";
        print_vec dst;
        ".4s, ";
        print_vec s1;
        ".4s, ";
        print_vec s2;
        ".4s\n"]
  | Vale_AArch64_Semantics.Sha1p (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sha1p ";
        print_vec dst;
        ".4s, ";
        print_vec s1;
        ".4s, ";
        print_vec s2;
        ".4s\n"]
  | Vale_AArch64_Semantics.Sha1m (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sha1m ";
        print_vec dst;
        ".4s, ";
        print_vec s1;
        ".4s, ";
        print_vec s2;
        ".4s\n"]
  | Vale_AArch64_Semantics.Sha1h (dst, src) ->
      FStar_String.concat ""
        ["    sha1h "; print_vec dst; ".4s, "; print_vec src; ".4s\n"]
  | Vale_AArch64_Semantics.Sha1su0 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sha1su0 ";
        print_vec dst;
        ".4s, ";
        print_vec s1;
        ".4s, ";
        print_vec s2;
        ".4s\n"]
  | Vale_AArch64_Semantics.Sha1su1 (dst, src) ->
      FStar_String.concat ""
        ["    sha1su1 "; print_vec dst; ".4s, "; print_vec src; ".4s\n"]
  | Vale_AArch64_Semantics.Sha256h (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sha256h ";
        print_vec dst;
        ".4s, ";
        print_vec s1;
        ".4s, ";
        print_vec s2;
        ".4s\n"]
  | Vale_AArch64_Semantics.Sha256h2 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sha256h2 ";
        print_vec dst;
        ".4s, ";
        print_vec s1;
        ".4s, ";
        print_vec s2;
        ".4s\n"]
  | Vale_AArch64_Semantics.Sha256su0 (dst, src) ->
      FStar_String.concat ""
        ["    sha256su0 "; print_vec dst; ".4s, "; print_vec src; ".4s\n"]
  | Vale_AArch64_Semantics.Sha256su1 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sha256su1 ";
        print_vec dst;
        ".4s, ";
        print_vec s1;
        ".4s, ";
        print_vec s2;
        ".4s\n"]
  | Vale_AArch64_Semantics.Eor3 (dst, s1, s2, s3) ->
      FStar_String.concat ""
        ["    eor3 ";
        print_vec dst;
        ".16b, ";
        print_vec s1;
        ".16b, ";
        print_vec s2;
        ".16b, ";
        print_vec s3;
        ".16b\n"]
  | Vale_AArch64_Semantics.Rax1 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    rax1 ";
        print_vec dst;
        ".2d, ";
        print_vec s1;
        ".2d, ";
        print_vec s2;
        ".2d\n"]
  | Vale_AArch64_Semantics.Xar (dst, s1, s2, imm) ->
      FStar_String.concat ""
        ["    xar ";
        print_vec dst;
        ".2d, ";
        print_vec s1;
        ".2d, ";
        print_vec s2;
        ".2d, #";
        Prims.string_of_int imm;
        "\n"]
  | Vale_AArch64_Semantics.Ld1 (v, arr, addr) ->
      FStar_String.concat ""
        ["    ld1 {";
        print_vec v;
        print_arrangement arr;
        "}, ";
        print_maddr addr;
        "\n"]
  | Vale_AArch64_Semantics.St1 (v, arr, addr) ->
      FStar_String.concat ""
        ["    st1 {";
        print_vec v;
        print_arrangement arr;
        "}, ";
        print_maddr addr;
        "\n"]
  | Vale_AArch64_Semantics.MsrDit enable ->
      if enable then "    msr dit, #1\n" else "    msr dit, #0\n"
let rec print_code (c : Vale_AArch64_Semantics.code) : Prims.string=
  match c with
  | Vale_AArch64_Semantics.Ins i -> print_ins i
  | Vale_AArch64_Semantics.Block cs -> print_codes cs
  | Vale_AArch64_Semantics.IfElse (uu___, c1, c2) ->
      FStar_String.concat "" [print_code c1; print_code c2]
  | Vale_AArch64_Semantics.While (uu___, body) -> print_code body
and print_codes (cs : Vale_AArch64_Semantics.code Prims.list) : Prims.string=
  match cs with
  | [] -> ""
  | c::rest -> FStar_String.concat "" [print_code c; print_codes rest]
let emit_header (name : Prims.string) : Prims.string=
  FStar_String.concat ""
    ["    .global ";
    name;
    "\n";
    "    .type ";
    name;
    ", %function\n";
    "    .p2align 2\n";
    name;
    ":\n";
    "    .cfi_startproc\n"]
let emit_footer (name : Prims.string) : Prims.string=
  FStar_String.concat ""
    ["    ret\n";
    "    .cfi_endproc\n";
    "    .size ";
    name;
    ", .-";
    name;
    "\n\n"]
let emit_proc_asm (p : Vale_Compiler_AST.proc_decl) : Prims.string=
  if p.Vale_Compiler_AST.proc_is_ins
  then ""
  else
    (match Vale_Compiler_Lower_AArch64.lower_stmts
             p.Vale_Compiler_AST.proc_body
     with
     | FStar_Pervasives_Native.Some c ->
         FStar_String.concat ""
           [emit_header p.Vale_Compiler_AST.proc_name;
           print_code c;
           emit_footer p.Vale_Compiler_AST.proc_name]
     | FStar_Pervasives_Native.None ->
         FStar_String.concat ""
           ["// Failed to lower procedure: ";
           p.Vale_Compiler_AST.proc_name;
           "\n"])
let rec emit_procs_asm (ps : Vale_Compiler_AST.proc_decl Prims.list) :
  Prims.string=
  match ps with
  | [] -> ""
  | p::rest -> FStar_String.concat "" [emit_proc_asm p; emit_procs_asm rest]
let emit_module_asm (m : Vale_Compiler_AST.module_decl)
  (has_crypto : Prims.bool) (has_dit : Prims.bool) : Prims.string=
  let arch_dir =
    if has_crypto
    then "    .arch armv8-a+crypto\n"
    else if has_dit then "    .arch armv8.4-a\n" else "    .arch armv8-a\n" in
  let text_sec =
    FStar_String.concat "" ["    .text\n"; arch_dir; "    .p2align 2\n\n"] in
  FStar_String.concat ""
    ["// Emitted by Vale verified assembly compiler (AArch64)\n";
    text_sec;
    emit_procs_asm m.Vale_Compiler_AST.mod_procs;
    "    .section .note.GNU-stack,\"\",@progbits\n"]
