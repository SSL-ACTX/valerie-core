open Prims
let print_reg (r : Vale_Arm32_Machine.reg) : Prims.string=
  match r with
  | Vale_Arm32_Machine.R0 -> "r0"
  | Vale_Arm32_Machine.R1 -> "r1"
  | Vale_Arm32_Machine.R2 -> "r2"
  | Vale_Arm32_Machine.R3 -> "r3"
  | Vale_Arm32_Machine.R4 -> "r4"
  | Vale_Arm32_Machine.R5 -> "r5"
  | Vale_Arm32_Machine.R6 -> "r6"
  | Vale_Arm32_Machine.R7 -> "r7"
  | Vale_Arm32_Machine.R8 -> "r8"
  | Vale_Arm32_Machine.R9 -> "r9"
  | Vale_Arm32_Machine.R10 -> "r10"
  | Vale_Arm32_Machine.R11 -> "r11"
  | Vale_Arm32_Machine.R12 -> "r12"
  | Vale_Arm32_Machine.SP -> "sp"
  | Vale_Arm32_Machine.LR -> "lr"
  | Vale_Arm32_Machine.PC -> "pc"
let print_sreg (s : Vale_Arm32_Machine.sreg) : Prims.string=
  match s with
  | Vale_Arm32_Machine.S0 -> "s0"
  | Vale_Arm32_Machine.S1 -> "s1"
  | Vale_Arm32_Machine.S2 -> "s2"
  | Vale_Arm32_Machine.S3 -> "s3"
  | Vale_Arm32_Machine.S4 -> "s4"
  | Vale_Arm32_Machine.S5 -> "s5"
  | Vale_Arm32_Machine.S6 -> "s6"
  | Vale_Arm32_Machine.S7 -> "s7"
  | Vale_Arm32_Machine.S8 -> "s8"
  | Vale_Arm32_Machine.S9 -> "s9"
  | Vale_Arm32_Machine.S10 -> "s10"
  | Vale_Arm32_Machine.S11 -> "s11"
  | Vale_Arm32_Machine.S12 -> "s12"
  | Vale_Arm32_Machine.S13 -> "s13"
  | Vale_Arm32_Machine.S14 -> "s14"
  | Vale_Arm32_Machine.S15 -> "s15"
  | Vale_Arm32_Machine.S16 -> "s16"
  | Vale_Arm32_Machine.S17 -> "s17"
  | Vale_Arm32_Machine.S18 -> "s18"
  | Vale_Arm32_Machine.S19 -> "s19"
  | Vale_Arm32_Machine.S20 -> "s20"
  | Vale_Arm32_Machine.S21 -> "s21"
  | Vale_Arm32_Machine.S22 -> "s22"
  | Vale_Arm32_Machine.S23 -> "s23"
  | Vale_Arm32_Machine.S24 -> "s24"
  | Vale_Arm32_Machine.S25 -> "s25"
  | Vale_Arm32_Machine.S26 -> "s26"
  | Vale_Arm32_Machine.S27 -> "s27"
  | Vale_Arm32_Machine.S28 -> "s28"
  | Vale_Arm32_Machine.S29 -> "s29"
  | Vale_Arm32_Machine.S30 -> "s30"
  | Vale_Arm32_Machine.S31 -> "s31"
let print_dreg (d : Vale_Arm32_Machine.dreg) : Prims.string=
  match d with
  | Vale_Arm32_Machine.D0 -> "d0"
  | Vale_Arm32_Machine.D1 -> "d1"
  | Vale_Arm32_Machine.D2 -> "d2"
  | Vale_Arm32_Machine.D3 -> "d3"
  | Vale_Arm32_Machine.D4 -> "d4"
  | Vale_Arm32_Machine.D5 -> "d5"
  | Vale_Arm32_Machine.D6 -> "d6"
  | Vale_Arm32_Machine.D7 -> "d7"
  | Vale_Arm32_Machine.D8 -> "d8"
  | Vale_Arm32_Machine.D9 -> "d9"
  | Vale_Arm32_Machine.D10 -> "d10"
  | Vale_Arm32_Machine.D11 -> "d11"
  | Vale_Arm32_Machine.D12 -> "d12"
  | Vale_Arm32_Machine.D13 -> "d13"
  | Vale_Arm32_Machine.D14 -> "d14"
  | Vale_Arm32_Machine.D15 -> "d15"
  | Vale_Arm32_Machine.D16 -> "d16"
  | Vale_Arm32_Machine.D17 -> "d17"
  | Vale_Arm32_Machine.D18 -> "d18"
  | Vale_Arm32_Machine.D19 -> "d19"
  | Vale_Arm32_Machine.D20 -> "d20"
  | Vale_Arm32_Machine.D21 -> "d21"
  | Vale_Arm32_Machine.D22 -> "d22"
  | Vale_Arm32_Machine.D23 -> "d23"
  | Vale_Arm32_Machine.D24 -> "d24"
  | Vale_Arm32_Machine.D25 -> "d25"
  | Vale_Arm32_Machine.D26 -> "d26"
  | Vale_Arm32_Machine.D27 -> "d27"
  | Vale_Arm32_Machine.D28 -> "d28"
  | Vale_Arm32_Machine.D29 -> "d29"
  | Vale_Arm32_Machine.D30 -> "d30"
  | Vale_Arm32_Machine.D31 -> "d31"
let print_qreg (q : Vale_Arm32_Machine.qreg) : Prims.string=
  match q with
  | Vale_Arm32_Machine.Q0 -> "q0"
  | Vale_Arm32_Machine.Q1 -> "q1"
  | Vale_Arm32_Machine.Q2 -> "q2"
  | Vale_Arm32_Machine.Q3 -> "q3"
  | Vale_Arm32_Machine.Q4 -> "q4"
  | Vale_Arm32_Machine.Q5 -> "q5"
  | Vale_Arm32_Machine.Q6 -> "q6"
  | Vale_Arm32_Machine.Q7 -> "q7"
  | Vale_Arm32_Machine.Q8 -> "q8"
  | Vale_Arm32_Machine.Q9 -> "q9"
  | Vale_Arm32_Machine.Q10 -> "q10"
  | Vale_Arm32_Machine.Q11 -> "q11"
  | Vale_Arm32_Machine.Q12 -> "q12"
  | Vale_Arm32_Machine.Q13 -> "q13"
  | Vale_Arm32_Machine.Q14 -> "q14"
  | Vale_Arm32_Machine.Q15 -> "q15"
let print_cond (c : Vale_Arm32_Machine.cond_code) : Prims.string=
  match c with
  | Vale_Arm32_Machine.CondEQ -> "eq"
  | Vale_Arm32_Machine.CondNE -> "ne"
  | Vale_Arm32_Machine.CondCS -> "cs"
  | Vale_Arm32_Machine.CondCC -> "cc"
  | Vale_Arm32_Machine.CondMI -> "mi"
  | Vale_Arm32_Machine.CondPL -> "pl"
  | Vale_Arm32_Machine.CondVS -> "vs"
  | Vale_Arm32_Machine.CondVC -> "vc"
  | Vale_Arm32_Machine.CondHI -> "hi"
  | Vale_Arm32_Machine.CondLS -> "ls"
  | Vale_Arm32_Machine.CondGE -> "ge"
  | Vale_Arm32_Machine.CondLT -> "lt"
  | Vale_Arm32_Machine.CondGT -> "gt"
  | Vale_Arm32_Machine.CondLE -> "le"
  | Vale_Arm32_Machine.CondAL -> ""
let print_maddr (m : Vale_Arm32_Machine.maddr) : Prims.string=
  match m with
  | Vale_Arm32_Machine.MBase r ->
      FStar_String.concat "" ["["; print_reg r; "]"]
  | Vale_Arm32_Machine.MBaseOffset (r, off) ->
      if off = Prims.int_zero
      then FStar_String.concat "" ["["; print_reg r; "]"]
      else
        FStar_String.concat ""
          ["["; print_reg r; ", #"; Prims.string_of_int off; "]"]
  | Vale_Arm32_Machine.MPreIndexed (r, off) ->
      FStar_String.concat ""
        ["["; print_reg r; ", #"; Prims.string_of_int off; "]!"]
  | Vale_Arm32_Machine.MPostIndexed (r, off) ->
      FStar_String.concat ""
        ["["; print_reg r; "], #"; Prims.string_of_int off]
  | Vale_Arm32_Machine.MRegOffset (base, off) ->
      FStar_String.concat "" ["["; print_reg base; ", "; print_reg off; "]"]
let rec print_reg_list (rs : Vale_Arm32_Machine.reg Prims.list) :
  Prims.string=
  match rs with
  | [] -> ""
  | r::[] -> print_reg r
  | r::rest ->
      FStar_String.concat "" [print_reg r; ", "; print_reg_list rest]
let rec print_sreg_list (ss : Vale_Arm32_Machine.sreg Prims.list) :
  Prims.string=
  match ss with
  | [] -> ""
  | s::[] -> print_sreg s
  | s::rest ->
      FStar_String.concat "" [print_sreg s; ", "; print_sreg_list rest]
let print_operand (op : Vale_Arm32_Semantics.operand) : Prims.string=
  match op with
  | Vale_Arm32_Semantics.OReg r -> print_reg r
  | Vale_Arm32_Semantics.OSreg s -> print_sreg s
  | Vale_Arm32_Semantics.ODreg d -> print_dreg d
  | Vale_Arm32_Semantics.OQreg q -> print_qreg q
  | Vale_Arm32_Semantics.OImm imm ->
      FStar_String.concat "" ["#"; FStar_UInt32.to_string imm]
  | Vale_Arm32_Semantics.OMem m -> print_maddr m
  | Vale_Arm32_Semantics.ORegList rs ->
      FStar_String.concat "" ["{"; print_reg_list rs; "}"]
let print_ins (i : Vale_Arm32_Semantics.ins) : Prims.string=
  match i with
  | Vale_Arm32_Semantics.Mov (dst, src) ->
      FStar_String.concat ""
        ["    mov "; print_reg dst; ", "; print_operand src; "\n"]
  | Vale_Arm32_Semantics.Movs (dst, src) ->
      FStar_String.concat ""
        ["    movs "; print_reg dst; ", "; print_operand src; "\n"]
  | Vale_Arm32_Semantics.Movw (dst, imm) ->
      FStar_String.concat ""
        ["    movw "; print_reg dst; ", #"; FStar_UInt32.to_string imm; "\n"]
  | Vale_Arm32_Semantics.Movt (dst, imm) ->
      FStar_String.concat ""
        ["    movt "; print_reg dst; ", #"; FStar_UInt32.to_string imm; "\n"]
  | Vale_Arm32_Semantics.Add (dst, s1, s2) ->
      FStar_String.concat ""
        ["    add ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Arm32_Semantics.Adds (dst, s1, s2) ->
      FStar_String.concat ""
        ["    adds ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Arm32_Semantics.Adc (dst, s1, s2) ->
      FStar_String.concat ""
        ["    adc ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Arm32_Semantics.Adcs (dst, s1, s2) ->
      FStar_String.concat ""
        ["    adcs ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Arm32_Semantics.Sub (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sub ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Arm32_Semantics.Subs (dst, s1, s2) ->
      FStar_String.concat ""
        ["    subs ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Arm32_Semantics.Sbc (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sbc ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Arm32_Semantics.Sbcs (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sbcs ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Arm32_Semantics.Rsb (dst, s1, s2) ->
      FStar_String.concat ""
        ["    rsb ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Arm32_Semantics.Rsbs (dst, s1, s2) ->
      FStar_String.concat ""
        ["    rsbs ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Arm32_Semantics.Rsc (dst, s1, s2) ->
      FStar_String.concat ""
        ["    rsc ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Arm32_Semantics.Rscs (dst, s1, s2) ->
      FStar_String.concat ""
        ["    rscs ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Arm32_Semantics.Mul (dst, s1, s2) ->
      FStar_String.concat ""
        ["    mul ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Arm32_Semantics.Mla (dst, s1, s2, ra) ->
      FStar_String.concat ""
        ["    mla ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        ", ";
        print_reg ra;
        "\n"]
  | Vale_Arm32_Semantics.Mls (dst, s1, s2, ra) ->
      FStar_String.concat ""
        ["    mls ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        ", ";
        print_reg ra;
        "\n"]
  | Vale_Arm32_Semantics.Smull (rdlo, rdhi, rn, rm) ->
      FStar_String.concat ""
        ["    smull ";
        print_reg rdlo;
        ", ";
        print_reg rdhi;
        ", ";
        print_reg rn;
        ", ";
        print_reg rm;
        "\n"]
  | Vale_Arm32_Semantics.Umull (rdlo, rdhi, rn, rm) ->
      FStar_String.concat ""
        ["    umull ";
        print_reg rdlo;
        ", ";
        print_reg rdhi;
        ", ";
        print_reg rn;
        ", ";
        print_reg rm;
        "\n"]
  | Vale_Arm32_Semantics.Sdiv (dst, rn, rm) ->
      FStar_String.concat ""
        ["    sdiv ";
        print_reg dst;
        ", ";
        print_reg rn;
        ", ";
        print_reg rm;
        "\n"]
  | Vale_Arm32_Semantics.Udiv (dst, rn, rm) ->
      FStar_String.concat ""
        ["    udiv ";
        print_reg dst;
        ", ";
        print_reg rn;
        ", ";
        print_reg rm;
        "\n"]
  | Vale_Arm32_Semantics.And (dst, s1, s2) ->
      FStar_String.concat ""
        ["    and ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Arm32_Semantics.Ands (dst, s1, s2) ->
      FStar_String.concat ""
        ["    ands ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Arm32_Semantics.Orr (dst, s1, s2) ->
      FStar_String.concat ""
        ["    orr ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Arm32_Semantics.Orrs (dst, s1, s2) ->
      FStar_String.concat ""
        ["    orrs ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Arm32_Semantics.Eor (dst, s1, s2) ->
      FStar_String.concat ""
        ["    eor ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Arm32_Semantics.Eors (dst, s1, s2) ->
      FStar_String.concat ""
        ["    eors ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Arm32_Semantics.Bic (dst, s1, s2) ->
      FStar_String.concat ""
        ["    bic ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Arm32_Semantics.Bics (dst, s1, s2) ->
      FStar_String.concat ""
        ["    bics ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Arm32_Semantics.Orn (dst, s1, s2) ->
      FStar_String.concat ""
        ["    orn ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Arm32_Semantics.Mvn (dst, src) ->
      FStar_String.concat ""
        ["    mvn "; print_reg dst; ", "; print_operand src; "\n"]
  | Vale_Arm32_Semantics.Mvns (dst, src) ->
      FStar_String.concat ""
        ["    mvns "; print_reg dst; ", "; print_operand src; "\n"]
  | Vale_Arm32_Semantics.Lsl (dst, rn, rm) ->
      FStar_String.concat ""
        ["    lsl ";
        print_reg dst;
        ", ";
        print_reg rn;
        ", ";
        print_operand rm;
        "\n"]
  | Vale_Arm32_Semantics.Lsr (dst, rn, rm) ->
      FStar_String.concat ""
        ["    lsr ";
        print_reg dst;
        ", ";
        print_reg rn;
        ", ";
        print_operand rm;
        "\n"]
  | Vale_Arm32_Semantics.Asr (dst, rn, rm) ->
      FStar_String.concat ""
        ["    asr ";
        print_reg dst;
        ", ";
        print_reg rn;
        ", ";
        print_operand rm;
        "\n"]
  | Vale_Arm32_Semantics.Ror (dst, rn, rm) ->
      FStar_String.concat ""
        ["    ror ";
        print_reg dst;
        ", ";
        print_reg rn;
        ", ";
        print_operand rm;
        "\n"]
  | Vale_Arm32_Semantics.Clz (dst, src) ->
      FStar_String.concat ""
        ["    clz "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_Arm32_Semantics.Rbit (dst, src) ->
      FStar_String.concat ""
        ["    rbit "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_Arm32_Semantics.Rev (dst, src) ->
      FStar_String.concat ""
        ["    rev "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_Arm32_Semantics.Rev16 (dst, src) ->
      FStar_String.concat ""
        ["    rev16 "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_Arm32_Semantics.Revsh (dst, src) ->
      FStar_String.concat ""
        ["    revsh "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_Arm32_Semantics.Ubfx (dst, rn, lsb, width) ->
      FStar_String.concat ""
        ["    ubfx ";
        print_reg dst;
        ", ";
        print_reg rn;
        ", #";
        FStar_UInt32.to_string lsb;
        ", #";
        FStar_UInt32.to_string width;
        "\n"]
  | Vale_Arm32_Semantics.Sbfx (dst, rn, lsb, width) ->
      FStar_String.concat ""
        ["    sbfx ";
        print_reg dst;
        ", ";
        print_reg rn;
        ", #";
        FStar_UInt32.to_string lsb;
        ", #";
        FStar_UInt32.to_string width;
        "\n"]
  | Vale_Arm32_Semantics.Cmp (rn, op) ->
      FStar_String.concat ""
        ["    cmp "; print_reg rn; ", "; print_operand op; "\n"]
  | Vale_Arm32_Semantics.Cmn (rn, op) ->
      FStar_String.concat ""
        ["    cmn "; print_reg rn; ", "; print_operand op; "\n"]
  | Vale_Arm32_Semantics.Tst (rn, op) ->
      FStar_String.concat ""
        ["    tst "; print_reg rn; ", "; print_operand op; "\n"]
  | Vale_Arm32_Semantics.Teq (rn, op) ->
      FStar_String.concat ""
        ["    teq "; print_reg rn; ", "; print_operand op; "\n"]
  | Vale_Arm32_Semantics.Sxtb (dst, src) ->
      FStar_String.concat ""
        ["    sxtb "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_Arm32_Semantics.Sxth (dst, src) ->
      FStar_String.concat ""
        ["    sxth "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_Arm32_Semantics.Uxtb (dst, src) ->
      FStar_String.concat ""
        ["    uxtb "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_Arm32_Semantics.Uxth (dst, src) ->
      FStar_String.concat ""
        ["    uxth "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_Arm32_Semantics.Ldr (dst, mem) ->
      FStar_String.concat ""
        ["    ldr "; print_reg dst; ", "; print_maddr mem; "\n"]
  | Vale_Arm32_Semantics.Ldrb (dst, mem) ->
      FStar_String.concat ""
        ["    ldrb "; print_reg dst; ", "; print_maddr mem; "\n"]
  | Vale_Arm32_Semantics.Ldrh (dst, mem) ->
      FStar_String.concat ""
        ["    ldrh "; print_reg dst; ", "; print_maddr mem; "\n"]
  | Vale_Arm32_Semantics.Ldrsb (dst, mem) ->
      FStar_String.concat ""
        ["    ldrsb "; print_reg dst; ", "; print_maddr mem; "\n"]
  | Vale_Arm32_Semantics.Ldrsh (dst, mem) ->
      FStar_String.concat ""
        ["    ldrsh "; print_reg dst; ", "; print_maddr mem; "\n"]
  | Vale_Arm32_Semantics.Ldrd (rt, rt2, mem) ->
      FStar_String.concat ""
        ["    ldrd ";
        print_reg rt;
        ", ";
        print_reg rt2;
        ", ";
        print_maddr mem;
        "\n"]
  | Vale_Arm32_Semantics.Str (src, mem) ->
      FStar_String.concat ""
        ["    str "; print_reg src; ", "; print_maddr mem; "\n"]
  | Vale_Arm32_Semantics.Strb (src, mem) ->
      FStar_String.concat ""
        ["    strb "; print_reg src; ", "; print_maddr mem; "\n"]
  | Vale_Arm32_Semantics.Strh (src, mem) ->
      FStar_String.concat ""
        ["    strh "; print_reg src; ", "; print_maddr mem; "\n"]
  | Vale_Arm32_Semantics.Strd (rt, rt2, mem) ->
      FStar_String.concat ""
        ["    strd ";
        print_reg rt;
        ", ";
        print_reg rt2;
        ", ";
        print_maddr mem;
        "\n"]
  | Vale_Arm32_Semantics.Push rs ->
      FStar_String.concat "" ["    push {"; print_reg_list rs; "}\n"]
  | Vale_Arm32_Semantics.Pop rs ->
      FStar_String.concat "" ["    pop {"; print_reg_list rs; "}\n"]
  | Vale_Arm32_Semantics.B target ->
      FStar_String.concat "" ["    b "; target; "\n"]
  | Vale_Arm32_Semantics.Bc (cond, target) ->
      FStar_String.concat "" ["    b"; print_cond cond; " "; target; "\n"]
  | Vale_Arm32_Semantics.Bl target ->
      FStar_String.concat "" ["    bl "; target; "\n"]
  | Vale_Arm32_Semantics.Bx target ->
      FStar_String.concat "" ["    bx "; print_reg target; "\n"]
  | Vale_Arm32_Semantics.Blx target ->
      FStar_String.concat "" ["    blx "; print_reg target; "\n"]
  | Vale_Arm32_Semantics.Vmov (dst, src) ->
      FStar_String.concat ""
        ["    vmov "; print_operand dst; ", "; print_operand src; "\n"]
  | Vale_Arm32_Semantics.VaddF32 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    vadd.f32 ";
        print_sreg dst;
        ", ";
        print_sreg s1;
        ", ";
        print_sreg s2;
        "\n"]
  | Vale_Arm32_Semantics.VaddF64 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    vadd.f64 ";
        print_dreg dst;
        ", ";
        print_dreg s1;
        ", ";
        print_dreg s2;
        "\n"]
  | Vale_Arm32_Semantics.VsubF32 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    vsub.f32 ";
        print_sreg dst;
        ", ";
        print_sreg s1;
        ", ";
        print_sreg s2;
        "\n"]
  | Vale_Arm32_Semantics.VsubF64 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    vsub.f64 ";
        print_dreg dst;
        ", ";
        print_dreg s1;
        ", ";
        print_dreg s2;
        "\n"]
  | Vale_Arm32_Semantics.VmulF32 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    vmul.f32 ";
        print_sreg dst;
        ", ";
        print_sreg s1;
        ", ";
        print_sreg s2;
        "\n"]
  | Vale_Arm32_Semantics.VmulF64 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    vmul.f64 ";
        print_dreg dst;
        ", ";
        print_dreg s1;
        ", ";
        print_dreg s2;
        "\n"]
  | Vale_Arm32_Semantics.Vldr (dst, mem) ->
      FStar_String.concat ""
        ["    vldr "; print_sreg dst; ", "; print_maddr mem; "\n"]
  | Vale_Arm32_Semantics.Vstr (src, mem) ->
      FStar_String.concat ""
        ["    vstr "; print_sreg src; ", "; print_maddr mem; "\n"]
  | Vale_Arm32_Semantics.Vpush sregs ->
      FStar_String.concat "" ["    vpush {"; print_sreg_list sregs; "}\n"]
  | Vale_Arm32_Semantics.Vpop sregs ->
      FStar_String.concat "" ["    vpop {"; print_sreg_list sregs; "}\n"]
let rec print_code (c : Vale_Arm32_Semantics.code) : Prims.string=
  match c with
  | Vale_Arm32_Semantics.Ins i -> print_ins i
  | Vale_Arm32_Semantics.Block cs -> print_codes cs
  | Vale_Arm32_Semantics.IfElse (uu___, ct, cf) ->
      FStar_String.concat "" [print_code ct; print_code cf]
  | Vale_Arm32_Semantics.While (uu___, body) -> print_code body
and print_codes (cs : Vale_Arm32_Semantics.code Prims.list) : Prims.string=
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
    name;
    ":\n"]
let emit_footer (name : Prims.string) : Prims.string=
  FStar_String.concat ""
    ["    bx lr\n"; "    .size "; name; ", .-"; name; "\n\n"]
let emit_proc_asm (p : Vale_Compiler_AST.proc_decl) : Prims.string=
  if p.Vale_Compiler_AST.proc_is_ins
  then ""
  else
    (match Vale_Compiler_Lower_Arm32.lower_stmts
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
  (comments : Prims.bool) : Prims.string=
  let preamble =
    "    .syntax unified\n    .arch armv7-a\n    .arch_extension idiv\n    .fpu vfpv3-d16\n    .arm\n    .text\n\n" in
  FStar_String.concat ""
    ["// Emitted by Vale verified assembly compiler (ARMv7-A)\n";
    preamble;
    emit_procs_asm m.Vale_Compiler_AST.mod_procs;
    "    .section .note.GNU-stack,\"\",%progbits\n"]
