open Prims
let print_reg (r : Valerie_AArch64_Machine.reg) : Prims.string=
  match r with
  | Valerie_AArch64_Machine.X0 -> "x0"
  | Valerie_AArch64_Machine.X1 -> "x1"
  | Valerie_AArch64_Machine.X2 -> "x2"
  | Valerie_AArch64_Machine.X3 -> "x3"
  | Valerie_AArch64_Machine.X4 -> "x4"
  | Valerie_AArch64_Machine.X5 -> "x5"
  | Valerie_AArch64_Machine.X6 -> "x6"
  | Valerie_AArch64_Machine.X7 -> "x7"
  | Valerie_AArch64_Machine.X8 -> "x8"
  | Valerie_AArch64_Machine.X9 -> "x9"
  | Valerie_AArch64_Machine.X10 -> "x10"
  | Valerie_AArch64_Machine.X11 -> "x11"
  | Valerie_AArch64_Machine.X12 -> "x12"
  | Valerie_AArch64_Machine.X13 -> "x13"
  | Valerie_AArch64_Machine.X14 -> "x14"
  | Valerie_AArch64_Machine.X15 -> "x15"
  | Valerie_AArch64_Machine.X16 -> "x16"
  | Valerie_AArch64_Machine.X17 -> "x17"
  | Valerie_AArch64_Machine.X18 -> "x18"
  | Valerie_AArch64_Machine.X19 -> "x19"
  | Valerie_AArch64_Machine.X20 -> "x20"
  | Valerie_AArch64_Machine.X21 -> "x21"
  | Valerie_AArch64_Machine.X22 -> "x22"
  | Valerie_AArch64_Machine.X23 -> "x23"
  | Valerie_AArch64_Machine.X24 -> "x24"
  | Valerie_AArch64_Machine.X25 -> "x25"
  | Valerie_AArch64_Machine.X26 -> "x26"
  | Valerie_AArch64_Machine.X27 -> "x27"
  | Valerie_AArch64_Machine.X28 -> "x28"
  | Valerie_AArch64_Machine.X29 -> "x29"
  | Valerie_AArch64_Machine.X30 -> "x30"
  | Valerie_AArch64_Machine.SP -> "sp"
  | Valerie_AArch64_Machine.XZR -> "xzr"
let print_wreg (r : Valerie_AArch64_Machine.reg) : Prims.string=
  match r with
  | Valerie_AArch64_Machine.X0 -> "w0"
  | Valerie_AArch64_Machine.X1 -> "w1"
  | Valerie_AArch64_Machine.X2 -> "w2"
  | Valerie_AArch64_Machine.X3 -> "w3"
  | Valerie_AArch64_Machine.X4 -> "w4"
  | Valerie_AArch64_Machine.X5 -> "w5"
  | Valerie_AArch64_Machine.X6 -> "w6"
  | Valerie_AArch64_Machine.X7 -> "w7"
  | Valerie_AArch64_Machine.X8 -> "w8"
  | Valerie_AArch64_Machine.X9 -> "w9"
  | Valerie_AArch64_Machine.X10 -> "w10"
  | Valerie_AArch64_Machine.X11 -> "w11"
  | Valerie_AArch64_Machine.X12 -> "w12"
  | Valerie_AArch64_Machine.X13 -> "w13"
  | Valerie_AArch64_Machine.X14 -> "w14"
  | Valerie_AArch64_Machine.X15 -> "w15"
  | Valerie_AArch64_Machine.X16 -> "w16"
  | Valerie_AArch64_Machine.X17 -> "w17"
  | Valerie_AArch64_Machine.X18 -> "w18"
  | Valerie_AArch64_Machine.X19 -> "w19"
  | Valerie_AArch64_Machine.X20 -> "w20"
  | Valerie_AArch64_Machine.X21 -> "w21"
  | Valerie_AArch64_Machine.X22 -> "w22"
  | Valerie_AArch64_Machine.X23 -> "w23"
  | Valerie_AArch64_Machine.X24 -> "w24"
  | Valerie_AArch64_Machine.X25 -> "w25"
  | Valerie_AArch64_Machine.X26 -> "w26"
  | Valerie_AArch64_Machine.X27 -> "w27"
  | Valerie_AArch64_Machine.X28 -> "w28"
  | Valerie_AArch64_Machine.X29 -> "w29"
  | Valerie_AArch64_Machine.X30 -> "w30"
  | Valerie_AArch64_Machine.SP -> "wsp"
  | Valerie_AArch64_Machine.XZR -> "wzr"
let print_vec (v : Valerie_AArch64_Machine.vec) : Prims.string=
  match v with
  | Valerie_AArch64_Machine.V0 -> "v0"
  | Valerie_AArch64_Machine.V1 -> "v1"
  | Valerie_AArch64_Machine.V2 -> "v2"
  | Valerie_AArch64_Machine.V3 -> "v3"
  | Valerie_AArch64_Machine.V4 -> "v4"
  | Valerie_AArch64_Machine.V5 -> "v5"
  | Valerie_AArch64_Machine.V6 -> "v6"
  | Valerie_AArch64_Machine.V7 -> "v7"
  | Valerie_AArch64_Machine.V8 -> "v8"
  | Valerie_AArch64_Machine.V9 -> "v9"
  | Valerie_AArch64_Machine.V10 -> "v10"
  | Valerie_AArch64_Machine.V11 -> "v11"
  | Valerie_AArch64_Machine.V12 -> "v12"
  | Valerie_AArch64_Machine.V13 -> "v13"
  | Valerie_AArch64_Machine.V14 -> "v14"
  | Valerie_AArch64_Machine.V15 -> "v15"
  | Valerie_AArch64_Machine.V16 -> "v16"
  | Valerie_AArch64_Machine.V17 -> "v17"
  | Valerie_AArch64_Machine.V18 -> "v18"
  | Valerie_AArch64_Machine.V19 -> "v19"
  | Valerie_AArch64_Machine.V20 -> "v20"
  | Valerie_AArch64_Machine.V21 -> "v21"
  | Valerie_AArch64_Machine.V22 -> "v22"
  | Valerie_AArch64_Machine.V23 -> "v23"
  | Valerie_AArch64_Machine.V24 -> "v24"
  | Valerie_AArch64_Machine.V25 -> "v25"
  | Valerie_AArch64_Machine.V26 -> "v26"
  | Valerie_AArch64_Machine.V27 -> "v27"
  | Valerie_AArch64_Machine.V28 -> "v28"
  | Valerie_AArch64_Machine.V29 -> "v29"
  | Valerie_AArch64_Machine.V30 -> "v30"
  | Valerie_AArch64_Machine.V31 -> "v31"
let print_arrangement (arr : Valerie_AArch64_Machine.vec_arrangement) :
  Prims.string=
  match arr with
  | Valerie_AArch64_Machine.B16 -> ".16b"
  | Valerie_AArch64_Machine.H8 -> ".8h"
  | Valerie_AArch64_Machine.S4 -> ".4s"
  | Valerie_AArch64_Machine.D2 -> ".2d"
  | Valerie_AArch64_Machine.B8 -> ".8b"
  | Valerie_AArch64_Machine.H4 -> ".4h"
  | Valerie_AArch64_Machine.S2 -> ".2s"
  | Valerie_AArch64_Machine.D1 -> ".1d"
  | Valerie_AArch64_Machine.Q1 -> ".1q"
let print_cond (c : Valerie_AArch64_Machine.cond_code) : Prims.string=
  match c with
  | Valerie_AArch64_Machine.CondEQ -> "eq"
  | Valerie_AArch64_Machine.CondNE -> "ne"
  | Valerie_AArch64_Machine.CondCS -> "cs"
  | Valerie_AArch64_Machine.CondCC -> "cc"
  | Valerie_AArch64_Machine.CondMI -> "mi"
  | Valerie_AArch64_Machine.CondPL -> "pl"
  | Valerie_AArch64_Machine.CondVS -> "vs"
  | Valerie_AArch64_Machine.CondVC -> "vc"
  | Valerie_AArch64_Machine.CondHI -> "hi"
  | Valerie_AArch64_Machine.CondLS -> "ls"
  | Valerie_AArch64_Machine.CondGE -> "ge"
  | Valerie_AArch64_Machine.CondLT -> "lt"
  | Valerie_AArch64_Machine.CondGT -> "gt"
  | Valerie_AArch64_Machine.CondLE -> "le"
  | Valerie_AArch64_Machine.CondAL -> "al"
  | Valerie_AArch64_Machine.CondNV -> "nv"
let print_maddr (m : Valerie_AArch64_Machine.maddr) : Prims.string=
  match m with
  | Valerie_AArch64_Machine.MReg base ->
      FStar_String.concat "" ["["; print_reg base; "]"]
  | Valerie_AArch64_Machine.MOffset (base, offset) ->
      FStar_String.concat ""
        ["["; print_reg base; ", #"; Prims.string_of_int offset; "]"]
  | Valerie_AArch64_Machine.MPreIndex (base, offset) ->
      FStar_String.concat ""
        ["["; print_reg base; ", #"; Prims.string_of_int offset; "]!"]
  | Valerie_AArch64_Machine.MPostIndex (base, offset) ->
      FStar_String.concat ""
        ["["; print_reg base; "], #"; Prims.string_of_int offset]
  | Valerie_AArch64_Machine.MRegOffset (base, index, uu___, shift) ->
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
let print_fpreg (v : Valerie_AArch64_Machine.vec)
  (w : Valerie_AArch64_Machine.fp_width) : Prims.string=
  let prefix =
    match w with
    | Valerie_AArch64_Machine.FPS -> "s"
    | Valerie_AArch64_Machine.FPD -> "d"
    | Valerie_AArch64_Machine.FPQ -> "q" in
  let num_str =
    match v with
    | Valerie_AArch64_Machine.V0 -> "0"
    | Valerie_AArch64_Machine.V1 -> "1"
    | Valerie_AArch64_Machine.V2 -> "2"
    | Valerie_AArch64_Machine.V3 -> "3"
    | Valerie_AArch64_Machine.V4 -> "4"
    | Valerie_AArch64_Machine.V5 -> "5"
    | Valerie_AArch64_Machine.V6 -> "6"
    | Valerie_AArch64_Machine.V7 -> "7"
    | Valerie_AArch64_Machine.V8 -> "8"
    | Valerie_AArch64_Machine.V9 -> "9"
    | Valerie_AArch64_Machine.V10 -> "10"
    | Valerie_AArch64_Machine.V11 -> "11"
    | Valerie_AArch64_Machine.V12 -> "12"
    | Valerie_AArch64_Machine.V13 -> "13"
    | Valerie_AArch64_Machine.V14 -> "14"
    | Valerie_AArch64_Machine.V15 -> "15"
    | Valerie_AArch64_Machine.V16 -> "16"
    | Valerie_AArch64_Machine.V17 -> "17"
    | Valerie_AArch64_Machine.V18 -> "18"
    | Valerie_AArch64_Machine.V19 -> "19"
    | Valerie_AArch64_Machine.V20 -> "20"
    | Valerie_AArch64_Machine.V21 -> "21"
    | Valerie_AArch64_Machine.V22 -> "22"
    | Valerie_AArch64_Machine.V23 -> "23"
    | Valerie_AArch64_Machine.V24 -> "24"
    | Valerie_AArch64_Machine.V25 -> "25"
    | Valerie_AArch64_Machine.V26 -> "26"
    | Valerie_AArch64_Machine.V27 -> "27"
    | Valerie_AArch64_Machine.V28 -> "28"
    | Valerie_AArch64_Machine.V29 -> "29"
    | Valerie_AArch64_Machine.V30 -> "30"
    | Valerie_AArch64_Machine.V31 -> "31" in
  FStar_String.concat "" [prefix; num_str]
let print_operand (op : Valerie_AArch64_Semantics.operand) : Prims.string=
  match op with
  | Valerie_AArch64_Semantics.OReg (r, Valerie_AArch64_Machine.W64) ->
      print_reg r
  | Valerie_AArch64_Semantics.OReg (r, Valerie_AArch64_Machine.W32) ->
      print_wreg r
  | Valerie_AArch64_Semantics.OImm imm ->
      FStar_String.concat "" ["#"; FStar_UInt64.to_string imm]
  | Valerie_AArch64_Semantics.OFpReg (v, w) -> print_fpreg v w
  | Valerie_AArch64_Semantics.OVecReg (v, arr) ->
      FStar_String.concat "" [print_vec v; print_arrangement arr]
  | Valerie_AArch64_Semantics.OVecElem (v, sz, lane) ->
      let s =
        match sz with
        | Valerie_AArch64_Machine.EB -> "b"
        | Valerie_AArch64_Machine.EH -> "h"
        | Valerie_AArch64_Machine.ES -> "s"
        | Valerie_AArch64_Machine.ED -> "d" in
      FStar_String.concat ""
        [print_vec v; "."; s; "["; Prims.string_of_int lane; "]"]
  | Valerie_AArch64_Semantics.OShifted
      (r, Valerie_AArch64_Machine.W64, Valerie_AArch64_Machine.LSL, amt) ->
      FStar_String.concat ""
        [print_reg r; ", lsl #"; Prims.string_of_int amt]
  | Valerie_AArch64_Semantics.OShifted
      (r, Valerie_AArch64_Machine.W64, Valerie_AArch64_Machine.LSR, amt) ->
      FStar_String.concat ""
        [print_reg r; ", lsr #"; Prims.string_of_int amt]
  | Valerie_AArch64_Semantics.OShifted
      (r, Valerie_AArch64_Machine.W64, Valerie_AArch64_Machine.ASR, amt) ->
      FStar_String.concat ""
        [print_reg r; ", asr #"; Prims.string_of_int amt]
  | Valerie_AArch64_Semantics.OShifted
      (r, Valerie_AArch64_Machine.W64, Valerie_AArch64_Machine.ROR, amt) ->
      FStar_String.concat ""
        [print_reg r; ", ror #"; Prims.string_of_int amt]
  | Valerie_AArch64_Semantics.OShifted
      (r, Valerie_AArch64_Machine.W32, Valerie_AArch64_Machine.LSL, amt) ->
      FStar_String.concat ""
        [print_wreg r; ", lsl #"; Prims.string_of_int amt]
  | Valerie_AArch64_Semantics.OShifted
      (r, Valerie_AArch64_Machine.W32, Valerie_AArch64_Machine.LSR, amt) ->
      FStar_String.concat ""
        [print_wreg r; ", lsr #"; Prims.string_of_int amt]
  | Valerie_AArch64_Semantics.OShifted
      (r, Valerie_AArch64_Machine.W32, Valerie_AArch64_Machine.ASR, amt) ->
      FStar_String.concat ""
        [print_wreg r; ", asr #"; Prims.string_of_int amt]
  | Valerie_AArch64_Semantics.OShifted
      (r, Valerie_AArch64_Machine.W32, Valerie_AArch64_Machine.ROR, amt) ->
      FStar_String.concat ""
        [print_wreg r; ", ror #"; Prims.string_of_int amt]
  | Valerie_AArch64_Semantics.OExtended (r, uu___, uu___1, amt) ->
      if amt = Prims.int_zero
      then print_reg r
      else
        FStar_String.concat ""
          [print_reg r; ", uxtx #"; Prims.string_of_int amt]
  | Valerie_AArch64_Semantics.OMem m -> print_maddr m
  | Valerie_AArch64_Semantics.OCond c -> print_cond c
let print_ins (i : Valerie_AArch64_Semantics.ins) : Prims.string=
  match i with
  | Valerie_AArch64_Semantics.Mov (dst, src) ->
      FStar_String.concat ""
        ["    mov "; print_reg dst; ", "; print_operand src; "\n"]
  | Valerie_AArch64_Semantics.Movz (dst, imm, shift) ->
      FStar_String.concat ""
        ["    movz ";
        print_reg dst;
        ", #";
        FStar_UInt64.to_string imm;
        ", lsl #";
        Prims.string_of_int shift;
        "\n"]
  | Valerie_AArch64_Semantics.Movn (dst, imm, shift) ->
      FStar_String.concat ""
        ["    movn ";
        print_reg dst;
        ", #";
        FStar_UInt64.to_string imm;
        ", lsl #";
        Prims.string_of_int shift;
        "\n"]
  | Valerie_AArch64_Semantics.Movk (dst, imm, shift) ->
      FStar_String.concat ""
        ["    movk ";
        print_reg dst;
        ", #";
        FStar_UInt64.to_string imm;
        ", lsl #";
        Prims.string_of_int shift;
        "\n"]
  | Valerie_AArch64_Semantics.Adrp (dst, lbl) ->
      FStar_String.concat "" ["    adrp "; print_reg dst; ", "; lbl; "\n"]
  | Valerie_AArch64_Semantics.Add (dst, s1, s2) ->
      FStar_String.concat ""
        ["    add ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Valerie_AArch64_Semantics.Adds (dst, s1, s2) ->
      FStar_String.concat ""
        ["    adds ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Valerie_AArch64_Semantics.Sub (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sub ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Valerie_AArch64_Semantics.Subs (dst, s1, s2) ->
      FStar_String.concat ""
        ["    subs ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Valerie_AArch64_Semantics.Adc (dst, s1, s2) ->
      FStar_String.concat ""
        ["    adc ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Valerie_AArch64_Semantics.Adcs (dst, s1, s2) ->
      FStar_String.concat ""
        ["    adcs ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Valerie_AArch64_Semantics.Sbc (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sbc ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Valerie_AArch64_Semantics.Sbcs (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sbcs ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Valerie_AArch64_Semantics.Cmp (s1, s2) ->
      FStar_String.concat ""
        ["    cmp "; print_reg s1; ", "; print_operand s2; "\n"]
  | Valerie_AArch64_Semantics.Cmn (s1, s2) ->
      FStar_String.concat ""
        ["    cmn "; print_reg s1; ", "; print_operand s2; "\n"]
  | Valerie_AArch64_Semantics.Tst (s1, s2) ->
      FStar_String.concat ""
        ["    tst "; print_reg s1; ", "; print_operand s2; "\n"]
  | Valerie_AArch64_Semantics.Neg (dst, src) ->
      FStar_String.concat ""
        ["    neg "; print_reg dst; ", "; print_reg src; "\n"]
  | Valerie_AArch64_Semantics.Negs (dst, src) ->
      FStar_String.concat ""
        ["    negs "; print_reg dst; ", "; print_reg src; "\n"]
  | Valerie_AArch64_Semantics.Mvn (dst, src) ->
      FStar_String.concat ""
        ["    mvn "; print_reg dst; ", "; print_operand src; "\n"]
  | Valerie_AArch64_Semantics.Mul (dst, s1, s2) ->
      FStar_String.concat ""
        ["    mul ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Valerie_AArch64_Semantics.Madd (dst, s1, s2, s3) ->
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
  | Valerie_AArch64_Semantics.Msub (dst, s1, s2, s3) ->
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
  | Valerie_AArch64_Semantics.Smulh (dst, s1, s2) ->
      FStar_String.concat ""
        ["    smulh ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Valerie_AArch64_Semantics.Umulh (dst, s1, s2) ->
      FStar_String.concat ""
        ["    umulh ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Valerie_AArch64_Semantics.Sdiv (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sdiv ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Valerie_AArch64_Semantics.Udiv (dst, s1, s2) ->
      FStar_String.concat ""
        ["    udiv ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Valerie_AArch64_Semantics.And (dst, s1, s2) ->
      FStar_String.concat ""
        ["    and ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Valerie_AArch64_Semantics.Ands (dst, s1, s2) ->
      FStar_String.concat ""
        ["    ands ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Valerie_AArch64_Semantics.Orr (dst, s1, s2) ->
      FStar_String.concat ""
        ["    orr ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Valerie_AArch64_Semantics.Orn (dst, s1, s2) ->
      FStar_String.concat ""
        ["    orn ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Valerie_AArch64_Semantics.Eor (dst, s1, s2) ->
      FStar_String.concat ""
        ["    eor ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Valerie_AArch64_Semantics.Eon (dst, s1, s2) ->
      FStar_String.concat ""
        ["    eon ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Valerie_AArch64_Semantics.Bic (dst, s1, s2) ->
      FStar_String.concat ""
        ["    bic ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Valerie_AArch64_Semantics.Bics (dst, s1, s2) ->
      FStar_String.concat ""
        ["    bics ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Valerie_AArch64_Semantics.Lsl (dst, s1, s2) ->
      FStar_String.concat ""
        ["    lsl ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Valerie_AArch64_Semantics.Lsr (dst, s1, s2) ->
      FStar_String.concat ""
        ["    lsr ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Valerie_AArch64_Semantics.Asr (dst, s1, s2) ->
      FStar_String.concat ""
        ["    asr ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Valerie_AArch64_Semantics.Ror (dst, s1, s2) ->
      FStar_String.concat ""
        ["    ror ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Valerie_AArch64_Semantics.Extr (dst, s1, s2, lsb) ->
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
  | Valerie_AArch64_Semantics.Clz (dst, src) ->
      FStar_String.concat ""
        ["    clz "; print_reg dst; ", "; print_reg src; "\n"]
  | Valerie_AArch64_Semantics.Rbit (dst, src) ->
      FStar_String.concat ""
        ["    rbit "; print_reg dst; ", "; print_reg src; "\n"]
  | Valerie_AArch64_Semantics.Rev (dst, src) ->
      FStar_String.concat ""
        ["    rev "; print_reg dst; ", "; print_reg src; "\n"]
  | Valerie_AArch64_Semantics.Rev16 (dst, src) ->
      FStar_String.concat ""
        ["    rev16 "; print_reg dst; ", "; print_reg src; "\n"]
  | Valerie_AArch64_Semantics.Rev32 (dst, src) ->
      FStar_String.concat ""
        ["    rev32 "; print_reg dst; ", "; print_reg src; "\n"]
  | Valerie_AArch64_Semantics.Rev64 (dst, src) ->
      FStar_String.concat ""
        ["    rev "; print_reg dst; ", "; print_reg src; "\n"]
  | Valerie_AArch64_Semantics.Csel (dst, s1, s2, cond) ->
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
  | Valerie_AArch64_Semantics.Cset (dst, cond) ->
      FStar_String.concat ""
        ["    cset "; print_reg dst; ", "; print_cond cond; "\n"]
  | Valerie_AArch64_Semantics.Csetm (dst, cond) ->
      FStar_String.concat ""
        ["    csetm "; print_reg dst; ", "; print_cond cond; "\n"]
  | Valerie_AArch64_Semantics.Cinc (dst, s1, cond) ->
      FStar_String.concat ""
        ["    cinc ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_cond cond;
        "\n"]
  | Valerie_AArch64_Semantics.Cinv (dst, s1, cond) ->
      FStar_String.concat ""
        ["    cinv ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_cond cond;
        "\n"]
  | Valerie_AArch64_Semantics.Cneg (dst, s1, cond) ->
      FStar_String.concat ""
        ["    cneg ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_cond cond;
        "\n"]
  | Valerie_AArch64_Semantics.Csinc (dst, s1, s2, cond) ->
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
  | Valerie_AArch64_Semantics.Csinv (dst, s1, s2, cond) ->
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
  | Valerie_AArch64_Semantics.Csneg (dst, s1, s2, cond) ->
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
  | Valerie_AArch64_Semantics.Ldr (dst, addr) ->
      FStar_String.concat ""
        ["    ldr "; print_reg dst; ", "; print_maddr addr; "\n"]
  | Valerie_AArch64_Semantics.Ldr32 (dst, addr) ->
      FStar_String.concat ""
        ["    ldr "; print_wreg dst; ", "; print_maddr addr; "\n"]
  | Valerie_AArch64_Semantics.Ldrb (dst, addr) ->
      FStar_String.concat ""
        ["    ldrb "; print_wreg dst; ", "; print_maddr addr; "\n"]
  | Valerie_AArch64_Semantics.Ldrh (dst, addr) ->
      FStar_String.concat ""
        ["    ldrh "; print_wreg dst; ", "; print_maddr addr; "\n"]
  | Valerie_AArch64_Semantics.Ldrsb (dst, addr) ->
      FStar_String.concat ""
        ["    ldrsb "; print_reg dst; ", "; print_maddr addr; "\n"]
  | Valerie_AArch64_Semantics.Ldrsh (dst, addr) ->
      FStar_String.concat ""
        ["    ldrsh "; print_reg dst; ", "; print_maddr addr; "\n"]
  | Valerie_AArch64_Semantics.Ldrsw (dst, addr) ->
      FStar_String.concat ""
        ["    ldrsw "; print_reg dst; ", "; print_maddr addr; "\n"]
  | Valerie_AArch64_Semantics.Str (src, addr) ->
      FStar_String.concat ""
        ["    str "; print_reg src; ", "; print_maddr addr; "\n"]
  | Valerie_AArch64_Semantics.Str32 (src, addr) ->
      FStar_String.concat ""
        ["    str "; print_wreg src; ", "; print_maddr addr; "\n"]
  | Valerie_AArch64_Semantics.Strb (src, addr) ->
      FStar_String.concat ""
        ["    strb "; print_wreg src; ", "; print_maddr addr; "\n"]
  | Valerie_AArch64_Semantics.Strh (src, addr) ->
      FStar_String.concat ""
        ["    strh "; print_wreg src; ", "; print_maddr addr; "\n"]
  | Valerie_AArch64_Semantics.Add32 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    add ";
        print_wreg dst;
        ", ";
        print_wreg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Valerie_AArch64_Semantics.Sub32 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sub ";
        print_wreg dst;
        ", ";
        print_wreg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Valerie_AArch64_Semantics.Eor32 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    eor ";
        print_wreg dst;
        ", ";
        print_wreg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Valerie_AArch64_Semantics.And32 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    and ";
        print_wreg dst;
        ", ";
        print_wreg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Valerie_AArch64_Semantics.Orr32 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    orr ";
        print_wreg dst;
        ", ";
        print_wreg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Valerie_AArch64_Semantics.Ror32 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    ror ";
        print_wreg dst;
        ", ";
        print_wreg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Valerie_AArch64_Semantics.Ldp (d1, d2, addr) ->
      FStar_String.concat ""
        ["    ldp ";
        print_reg d1;
        ", ";
        print_reg d2;
        ", ";
        print_maddr addr;
        "\n"]
  | Valerie_AArch64_Semantics.Stp (s1, s2, addr) ->
      FStar_String.concat ""
        ["    stp ";
        print_reg s1;
        ", ";
        print_reg s2;
        ", ";
        print_maddr addr;
        "\n"]
  | Valerie_AArch64_Semantics.B lbl ->
      FStar_String.concat "" ["    b "; lbl; "\n"]
  | Valerie_AArch64_Semantics.BCond (cond, lbl) ->
      FStar_String.concat "" ["    b."; print_cond cond; " "; lbl; "\n"]
  | Valerie_AArch64_Semantics.Bl lbl ->
      FStar_String.concat "" ["    bl "; lbl; "\n"]
  | Valerie_AArch64_Semantics.Blr r ->
      FStar_String.concat "" ["    blr "; print_reg r; "\n"]
  | Valerie_AArch64_Semantics.Ret -> "    ret\n"
  | Valerie_AArch64_Semantics.Cbz (r, lbl) ->
      FStar_String.concat "" ["    cbz "; print_reg r; ", "; lbl; "\n"]
  | Valerie_AArch64_Semantics.Cbnz (r, lbl) ->
      FStar_String.concat "" ["    cbnz "; print_reg r; ", "; lbl; "\n"]
  | Valerie_AArch64_Semantics.FmovGprToVec (dst, src) ->
      FStar_String.concat ""
        ["    fmov ";
        print_fpreg dst Valerie_AArch64_Machine.FPD;
        ", ";
        print_reg src;
        "\n"]
  | Valerie_AArch64_Semantics.FmovVecToGpr (dst, src) ->
      FStar_String.concat ""
        ["    fmov ";
        print_reg dst;
        ", ";
        print_fpreg src Valerie_AArch64_Machine.FPD;
        "\n"]
  | Valerie_AArch64_Semantics.Fadd (dst, s1, s2) ->
      FStar_String.concat ""
        ["    fadd ";
        print_vec dst;
        ".2d, ";
        print_vec s1;
        ".2d, ";
        print_vec s2;
        ".2d\n"]
  | Valerie_AArch64_Semantics.Fsub (dst, s1, s2) ->
      FStar_String.concat ""
        ["    fsub ";
        print_vec dst;
        ".2d, ";
        print_vec s1;
        ".2d, ";
        print_vec s2;
        ".2d\n"]
  | Valerie_AArch64_Semantics.Fmul (dst, s1, s2) ->
      FStar_String.concat ""
        ["    fmul ";
        print_vec dst;
        ".2d, ";
        print_vec s1;
        ".2d, ";
        print_vec s2;
        ".2d\n"]
  | Valerie_AArch64_Semantics.Fdiv (dst, s1, s2) ->
      FStar_String.concat ""
        ["    fdiv ";
        print_vec dst;
        ".2d, ";
        print_vec s1;
        ".2d, ";
        print_vec s2;
        ".2d\n"]
  | Valerie_AArch64_Semantics.Dup (dst, src) ->
      FStar_String.concat ""
        ["    dup "; print_vec dst; ".16b, "; print_wreg src; "\n"]
  | Valerie_AArch64_Semantics.InsLane (dst, dst_lane, src) ->
      FStar_String.concat ""
        ["    ins ";
        print_vec dst;
        ".d[";
        Prims.string_of_int dst_lane;
        "], ";
        print_reg src;
        "\n"]
  | Valerie_AArch64_Semantics.Umov (dst, src, src_lane) ->
      FStar_String.concat ""
        ["    umov ";
        print_reg dst;
        ", ";
        print_vec src;
        ".d[";
        Prims.string_of_int src_lane;
        "]\n"]
  | Valerie_AArch64_Semantics.Smov (dst, src, src_lane) ->
      FStar_String.concat ""
        ["    smov ";
        print_reg dst;
        ", ";
        print_vec src;
        ".d[";
        Prims.string_of_int src_lane;
        "]\n"]
  | Valerie_AArch64_Semantics.Vadd (dst, s1, s2, arr) ->
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
  | Valerie_AArch64_Semantics.Vsub (dst, s1, s2, arr) ->
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
  | Valerie_AArch64_Semantics.Vmul (dst, s1, s2, arr) ->
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
  | Valerie_AArch64_Semantics.Vand (dst, s1, s2) ->
      FStar_String.concat ""
        ["    and ";
        print_vec dst;
        ".16b, ";
        print_vec s1;
        ".16b, ";
        print_vec s2;
        ".16b\n"]
  | Valerie_AArch64_Semantics.Vorr (dst, s1, s2) ->
      FStar_String.concat ""
        ["    orr ";
        print_vec dst;
        ".16b, ";
        print_vec s1;
        ".16b, ";
        print_vec s2;
        ".16b\n"]
  | Valerie_AArch64_Semantics.Veor (dst, s1, s2) ->
      FStar_String.concat ""
        ["    eor ";
        print_vec dst;
        ".16b, ";
        print_vec s1;
        ".16b, ";
        print_vec s2;
        ".16b\n"]
  | Valerie_AArch64_Semantics.Vext8 (dst, s1, s2, idx) ->
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
  | Valerie_AArch64_Semantics.Vrev64 (dst, src, arr) ->
      FStar_String.concat ""
        ["    rev64 ";
        print_vec dst;
        print_arrangement arr;
        ", ";
        print_vec src;
        print_arrangement arr;
        "\n"]
  | Valerie_AArch64_Semantics.Vrev32 (dst, src, arr) ->
      FStar_String.concat ""
        ["    rev32 ";
        print_vec dst;
        print_arrangement arr;
        ", ";
        print_vec src;
        print_arrangement arr;
        "\n"]
  | Valerie_AArch64_Semantics.Vrev16 (dst, src, arr) ->
      FStar_String.concat ""
        ["    rev16 ";
        print_vec dst;
        print_arrangement arr;
        ", ";
        print_vec src;
        print_arrangement arr;
        "\n"]
  | Valerie_AArch64_Semantics.Pmull (dst, s1, s2) ->
      FStar_String.concat ""
        ["    pmull ";
        print_vec dst;
        ".1q, ";
        print_vec s1;
        ".1d, ";
        print_vec s2;
        ".1d\n"]
  | Valerie_AArch64_Semantics.Pmull2 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    pmull2 ";
        print_vec dst;
        ".1q, ";
        print_vec s1;
        ".2d, ";
        print_vec s2;
        ".2d\n"]
  | Valerie_AArch64_Semantics.Aese (dst, src) ->
      FStar_String.concat ""
        ["    aese "; print_vec dst; ".16b, "; print_vec src; ".16b\n"]
  | Valerie_AArch64_Semantics.Aesd (dst, src) ->
      FStar_String.concat ""
        ["    aesd "; print_vec dst; ".16b, "; print_vec src; ".16b\n"]
  | Valerie_AArch64_Semantics.Aesmc (dst, src) ->
      FStar_String.concat ""
        ["    aesmc "; print_vec dst; ".16b, "; print_vec src; ".16b\n"]
  | Valerie_AArch64_Semantics.Aesimc (dst, src) ->
      FStar_String.concat ""
        ["    aesimc "; print_vec dst; ".16b, "; print_vec src; ".16b\n"]
  | Valerie_AArch64_Semantics.Sha1c (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sha1c ";
        print_vec dst;
        ".4s, ";
        print_vec s1;
        ".4s, ";
        print_vec s2;
        ".4s\n"]
  | Valerie_AArch64_Semantics.Sha1p (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sha1p ";
        print_vec dst;
        ".4s, ";
        print_vec s1;
        ".4s, ";
        print_vec s2;
        ".4s\n"]
  | Valerie_AArch64_Semantics.Sha1m (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sha1m ";
        print_vec dst;
        ".4s, ";
        print_vec s1;
        ".4s, ";
        print_vec s2;
        ".4s\n"]
  | Valerie_AArch64_Semantics.Sha1h (dst, src) ->
      FStar_String.concat ""
        ["    sha1h "; print_vec dst; ".4s, "; print_vec src; ".4s\n"]
  | Valerie_AArch64_Semantics.Sha1su0 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sha1su0 ";
        print_vec dst;
        ".4s, ";
        print_vec s1;
        ".4s, ";
        print_vec s2;
        ".4s\n"]
  | Valerie_AArch64_Semantics.Sha1su1 (dst, src) ->
      FStar_String.concat ""
        ["    sha1su1 "; print_vec dst; ".4s, "; print_vec src; ".4s\n"]
  | Valerie_AArch64_Semantics.Sha256h (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sha256h ";
        print_vec dst;
        ".4s, ";
        print_vec s1;
        ".4s, ";
        print_vec s2;
        ".4s\n"]
  | Valerie_AArch64_Semantics.Sha256h2 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sha256h2 ";
        print_vec dst;
        ".4s, ";
        print_vec s1;
        ".4s, ";
        print_vec s2;
        ".4s\n"]
  | Valerie_AArch64_Semantics.Sha256su0 (dst, src) ->
      FStar_String.concat ""
        ["    sha256su0 "; print_vec dst; ".4s, "; print_vec src; ".4s\n"]
  | Valerie_AArch64_Semantics.Sha256su1 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sha256su1 ";
        print_vec dst;
        ".4s, ";
        print_vec s1;
        ".4s, ";
        print_vec s2;
        ".4s\n"]
  | Valerie_AArch64_Semantics.Eor3 (dst, s1, s2, s3) ->
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
  | Valerie_AArch64_Semantics.Rax1 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    rax1 ";
        print_vec dst;
        ".2d, ";
        print_vec s1;
        ".2d, ";
        print_vec s2;
        ".2d\n"]
  | Valerie_AArch64_Semantics.Xar (dst, s1, s2, imm) ->
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
  | Valerie_AArch64_Semantics.Ld1 (v, arr, addr) ->
      FStar_String.concat ""
        ["    ld1 {";
        print_vec v;
        print_arrangement arr;
        "}, ";
        print_maddr addr;
        "\n"]
  | Valerie_AArch64_Semantics.St1 (v, arr, addr) ->
      FStar_String.concat ""
        ["    st1 {";
        print_vec v;
        print_arrangement arr;
        "}, ";
        print_maddr addr;
        "\n"]
  | Valerie_AArch64_Semantics.MsrDit enable ->
      if enable then "    msr dit, #1\n" else "    msr dit, #0\n"
let rec print_code (c : Valerie_AArch64_Semantics.code) : Prims.string=
  match c with
  | Valerie_AArch64_Semantics.Ins i -> print_ins i
  | Valerie_AArch64_Semantics.Block cs -> print_codes cs
  | Valerie_AArch64_Semantics.IfElse (uu___, c1, c2) ->
      FStar_String.concat "" [print_code c1; print_code c2]
  | Valerie_AArch64_Semantics.While (uu___, body) -> print_code body
and print_codes (cs : Valerie_AArch64_Semantics.code Prims.list) :
  Prims.string=
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
let emit_proc_asm (p : Valerie_Compiler_AST.proc_decl) : Prims.string=
  if p.Valerie_Compiler_AST.proc_is_ins
  then ""
  else
    (match Valerie_Compiler_Lower_AArch64.lower_stmts
             p.Valerie_Compiler_AST.proc_body
     with
     | FStar_Pervasives_Native.Some c ->
         FStar_String.concat ""
           [emit_header p.Valerie_Compiler_AST.proc_name;
           print_code c;
           emit_footer p.Valerie_Compiler_AST.proc_name]
     | FStar_Pervasives_Native.None ->
         FStar_String.concat ""
           ["// Failed to lower procedure: ";
           p.Valerie_Compiler_AST.proc_name;
           "\n"])
let rec emit_procs_asm (ps : Valerie_Compiler_AST.proc_decl Prims.list) :
  Prims.string=
  match ps with
  | [] -> ""
  | p::rest -> FStar_String.concat "" [emit_proc_asm p; emit_procs_asm rest]
let emit_module_asm (m : Valerie_Compiler_AST.module_decl)
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
    emit_procs_asm m.Valerie_Compiler_AST.mod_procs;
    "    .section .note.GNU-stack,\"\",@progbits\n"]
