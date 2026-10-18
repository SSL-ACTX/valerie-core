open Prims
let print_reg (r : Vale_Riscv64_Machine.reg) : Prims.string=
  match r with
  | Vale_Riscv64_Machine.Zero -> "zero"
  | Vale_Riscv64_Machine.RA -> "ra"
  | Vale_Riscv64_Machine.SP -> "sp"
  | Vale_Riscv64_Machine.GP -> "gp"
  | Vale_Riscv64_Machine.TP -> "tp"
  | Vale_Riscv64_Machine.T0 -> "t0"
  | Vale_Riscv64_Machine.T1 -> "t1"
  | Vale_Riscv64_Machine.T2 -> "t2"
  | Vale_Riscv64_Machine.S0 -> "s0"
  | Vale_Riscv64_Machine.S1 -> "s1"
  | Vale_Riscv64_Machine.A0 -> "a0"
  | Vale_Riscv64_Machine.A1 -> "a1"
  | Vale_Riscv64_Machine.A2 -> "a2"
  | Vale_Riscv64_Machine.A3 -> "a3"
  | Vale_Riscv64_Machine.A4 -> "a4"
  | Vale_Riscv64_Machine.A5 -> "a5"
  | Vale_Riscv64_Machine.A6 -> "a6"
  | Vale_Riscv64_Machine.A7 -> "a7"
  | Vale_Riscv64_Machine.S2 -> "s2"
  | Vale_Riscv64_Machine.S3 -> "s3"
  | Vale_Riscv64_Machine.S4 -> "s4"
  | Vale_Riscv64_Machine.S5 -> "s5"
  | Vale_Riscv64_Machine.S6 -> "s6"
  | Vale_Riscv64_Machine.S7 -> "s7"
  | Vale_Riscv64_Machine.S8 -> "s8"
  | Vale_Riscv64_Machine.S9 -> "s9"
  | Vale_Riscv64_Machine.S10 -> "s10"
  | Vale_Riscv64_Machine.S11 -> "s11"
  | Vale_Riscv64_Machine.T3 -> "t3"
  | Vale_Riscv64_Machine.T4 -> "t4"
  | Vale_Riscv64_Machine.T5 -> "t5"
  | Vale_Riscv64_Machine.T6 -> "t6"
let print_operand (op : Vale_Riscv64_Semantics.operand) : Prims.string=
  match op with
  | Vale_Riscv64_Semantics.OReg r -> print_reg r
  | Vale_Riscv64_Semantics.OImm imm -> FStar_UInt64.to_string imm
  | Vale_Riscv64_Semantics.OMem (base, off) ->
      FStar_String.concat ""
        [Prims.string_of_int off; "("; print_reg base; ")"]
  | Vale_Riscv64_Semantics.OLabel lbl -> lbl
let print_ins (i : Vale_Riscv64_Semantics.ins) : Prims.string=
  match i with
  | Vale_Riscv64_Semantics.Add (dst, s1, Vale_Riscv64_Semantics.OImm imm) ->
      FStar_String.concat ""
        ["    addi ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        FStar_UInt64.to_string imm;
        "\n"]
  | Vale_Riscv64_Semantics.Add (dst, s1, s2) ->
      FStar_String.concat ""
        ["    add ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Riscv64_Semantics.Addw (dst, s1, Vale_Riscv64_Semantics.OImm imm) ->
      FStar_String.concat ""
        ["    addiw ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        FStar_UInt64.to_string imm;
        "\n"]
  | Vale_Riscv64_Semantics.Addw (dst, s1, s2) ->
      FStar_String.concat ""
        ["    addw ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Riscv64_Semantics.Sub (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sub ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Riscv64_Semantics.Subw (dst, s1, s2) ->
      FStar_String.concat ""
        ["    subw ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Riscv64_Semantics.And (dst, s1, Vale_Riscv64_Semantics.OImm imm) ->
      FStar_String.concat ""
        ["    andi ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        FStar_UInt64.to_string imm;
        "\n"]
  | Vale_Riscv64_Semantics.And (dst, s1, s2) ->
      FStar_String.concat ""
        ["    and ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Riscv64_Semantics.Or (dst, s1, Vale_Riscv64_Semantics.OImm imm) ->
      FStar_String.concat ""
        ["    ori ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        FStar_UInt64.to_string imm;
        "\n"]
  | Vale_Riscv64_Semantics.Or (dst, s1, s2) ->
      FStar_String.concat ""
        ["    or ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Riscv64_Semantics.Xor (dst, s1, Vale_Riscv64_Semantics.OImm imm) ->
      FStar_String.concat ""
        ["    xori ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        FStar_UInt64.to_string imm;
        "\n"]
  | Vale_Riscv64_Semantics.Xor (dst, s1, s2) ->
      FStar_String.concat ""
        ["    xor ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Riscv64_Semantics.Sll (dst, s1, Vale_Riscv64_Semantics.OImm imm) ->
      FStar_String.concat ""
        ["    slli ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        FStar_UInt64.to_string imm;
        "\n"]
  | Vale_Riscv64_Semantics.Sll (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sll ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Riscv64_Semantics.Sllw (dst, s1, Vale_Riscv64_Semantics.OImm imm) ->
      FStar_String.concat ""
        ["    slliw ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        FStar_UInt64.to_string imm;
        "\n"]
  | Vale_Riscv64_Semantics.Sllw (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sllw ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Riscv64_Semantics.Srl (dst, s1, Vale_Riscv64_Semantics.OImm imm) ->
      FStar_String.concat ""
        ["    srli ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        FStar_UInt64.to_string imm;
        "\n"]
  | Vale_Riscv64_Semantics.Srl (dst, s1, s2) ->
      FStar_String.concat ""
        ["    srl ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Riscv64_Semantics.Srlw (dst, s1, Vale_Riscv64_Semantics.OImm imm) ->
      FStar_String.concat ""
        ["    srliw ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        FStar_UInt64.to_string imm;
        "\n"]
  | Vale_Riscv64_Semantics.Srlw (dst, s1, s2) ->
      FStar_String.concat ""
        ["    srlw ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Riscv64_Semantics.Sra (dst, s1, Vale_Riscv64_Semantics.OImm imm) ->
      FStar_String.concat ""
        ["    srai ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        FStar_UInt64.to_string imm;
        "\n"]
  | Vale_Riscv64_Semantics.Sra (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sra ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Riscv64_Semantics.Sraw (dst, s1, Vale_Riscv64_Semantics.OImm imm) ->
      FStar_String.concat ""
        ["    sraiw ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        FStar_UInt64.to_string imm;
        "\n"]
  | Vale_Riscv64_Semantics.Sraw (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sraw ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Riscv64_Semantics.Slt (dst, s1, Vale_Riscv64_Semantics.OImm imm) ->
      FStar_String.concat ""
        ["    slti ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        FStar_UInt64.to_string imm;
        "\n"]
  | Vale_Riscv64_Semantics.Slt (dst, s1, s2) ->
      FStar_String.concat ""
        ["    slt ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Riscv64_Semantics.Sltu (dst, s1, Vale_Riscv64_Semantics.OImm imm) ->
      FStar_String.concat ""
        ["    sltiu ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        FStar_UInt64.to_string imm;
        "\n"]
  | Vale_Riscv64_Semantics.Sltu (dst, s1, s2) ->
      FStar_String.concat ""
        ["    sltu ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Riscv64_Semantics.Ld (dst, base, off) ->
      FStar_String.concat ""
        ["    ld ";
        print_reg dst;
        ", ";
        Prims.string_of_int off;
        "(";
        print_reg base;
        ")\n"]
  | Vale_Riscv64_Semantics.Lw (dst, base, off) ->
      FStar_String.concat ""
        ["    lw ";
        print_reg dst;
        ", ";
        Prims.string_of_int off;
        "(";
        print_reg base;
        ")\n"]
  | Vale_Riscv64_Semantics.Lwu (dst, base, off) ->
      FStar_String.concat ""
        ["    lwu ";
        print_reg dst;
        ", ";
        Prims.string_of_int off;
        "(";
        print_reg base;
        ")\n"]
  | Vale_Riscv64_Semantics.Lh (dst, base, off) ->
      FStar_String.concat ""
        ["    lh ";
        print_reg dst;
        ", ";
        Prims.string_of_int off;
        "(";
        print_reg base;
        ")\n"]
  | Vale_Riscv64_Semantics.Lhu (dst, base, off) ->
      FStar_String.concat ""
        ["    lhu ";
        print_reg dst;
        ", ";
        Prims.string_of_int off;
        "(";
        print_reg base;
        ")\n"]
  | Vale_Riscv64_Semantics.Lb (dst, base, off) ->
      FStar_String.concat ""
        ["    lb ";
        print_reg dst;
        ", ";
        Prims.string_of_int off;
        "(";
        print_reg base;
        ")\n"]
  | Vale_Riscv64_Semantics.Lbu (dst, base, off) ->
      FStar_String.concat ""
        ["    lbu ";
        print_reg dst;
        ", ";
        Prims.string_of_int off;
        "(";
        print_reg base;
        ")\n"]
  | Vale_Riscv64_Semantics.Sd (src, base, off) ->
      FStar_String.concat ""
        ["    sd ";
        print_reg src;
        ", ";
        Prims.string_of_int off;
        "(";
        print_reg base;
        ")\n"]
  | Vale_Riscv64_Semantics.Sw (src, base, off) ->
      FStar_String.concat ""
        ["    sw ";
        print_reg src;
        ", ";
        Prims.string_of_int off;
        "(";
        print_reg base;
        ")\n"]
  | Vale_Riscv64_Semantics.Sh (src, base, off) ->
      FStar_String.concat ""
        ["    sh ";
        print_reg src;
        ", ";
        Prims.string_of_int off;
        "(";
        print_reg base;
        ")\n"]
  | Vale_Riscv64_Semantics.Sb (src, base, off) ->
      FStar_String.concat ""
        ["    sb ";
        print_reg src;
        ", ";
        Prims.string_of_int off;
        "(";
        print_reg base;
        ")\n"]
  | Vale_Riscv64_Semantics.Beq (s1, s2, target) ->
      FStar_String.concat ""
        ["    beq "; print_reg s1; ", "; print_reg s2; ", "; target; "\n"]
  | Vale_Riscv64_Semantics.Bne (s1, s2, target) ->
      FStar_String.concat ""
        ["    bne "; print_reg s1; ", "; print_reg s2; ", "; target; "\n"]
  | Vale_Riscv64_Semantics.Blt (s1, s2, target) ->
      FStar_String.concat ""
        ["    blt "; print_reg s1; ", "; print_reg s2; ", "; target; "\n"]
  | Vale_Riscv64_Semantics.Bge (s1, s2, target) ->
      FStar_String.concat ""
        ["    bge "; print_reg s1; ", "; print_reg s2; ", "; target; "\n"]
  | Vale_Riscv64_Semantics.Bltu (s1, s2, target) ->
      FStar_String.concat ""
        ["    bltu "; print_reg s1; ", "; print_reg s2; ", "; target; "\n"]
  | Vale_Riscv64_Semantics.Bgeu (s1, s2, target) ->
      FStar_String.concat ""
        ["    bgeu "; print_reg s1; ", "; print_reg s2; ", "; target; "\n"]
  | Vale_Riscv64_Semantics.Jal (dst, target) ->
      FStar_String.concat "" ["    jal "; print_reg dst; ", "; target; "\n"]
  | Vale_Riscv64_Semantics.Jalr (dst, base, off) ->
      FStar_String.concat ""
        ["    jalr ";
        print_reg dst;
        ", ";
        Prims.string_of_int off;
        "(";
        print_reg base;
        ")\n"]
  | Vale_Riscv64_Semantics.Ret -> "    ret\n"
  | Vale_Riscv64_Semantics.Call target ->
      FStar_String.concat "" ["    call "; target; "\n"]
  | Vale_Riscv64_Semantics.Ecall -> "    ecall\n"
  | Vale_Riscv64_Semantics.Lui (dst, imm) ->
      FStar_String.concat ""
        ["    lui "; print_reg dst; ", "; FStar_UInt64.to_string imm; "\n"]
  | Vale_Riscv64_Semantics.Auipc (dst, imm) ->
      FStar_String.concat ""
        ["    auipc "; print_reg dst; ", "; FStar_UInt64.to_string imm; "\n"]
  | Vale_Riscv64_Semantics.Li (dst, imm) ->
      FStar_String.concat ""
        ["    li "; print_reg dst; ", "; FStar_UInt64.to_string imm; "\n"]
  | Vale_Riscv64_Semantics.Mv (dst, src) ->
      FStar_String.concat ""
        ["    mv "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_Riscv64_Semantics.Mul (dst, s1, s2) ->
      FStar_String.concat ""
        ["    mul ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Riscv64_Semantics.Mulw (dst, s1, s2) ->
      FStar_String.concat ""
        ["    mulw ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Riscv64_Semantics.Mulh (dst, s1, s2) ->
      FStar_String.concat ""
        ["    mulh ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Riscv64_Semantics.Mulhu (dst, s1, s2) ->
      FStar_String.concat ""
        ["    mulhu ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Riscv64_Semantics.Mulhsu (dst, s1, s2) ->
      FStar_String.concat ""
        ["    mulhsu ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Riscv64_Semantics.Div (dst, s1, s2) ->
      FStar_String.concat ""
        ["    div ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Riscv64_Semantics.Divw (dst, s1, s2) ->
      FStar_String.concat ""
        ["    divw ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Riscv64_Semantics.Divu (dst, s1, s2) ->
      FStar_String.concat ""
        ["    divu ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Riscv64_Semantics.Divuw (dst, s1, s2) ->
      FStar_String.concat ""
        ["    divuw ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Riscv64_Semantics.Rem (dst, s1, s2) ->
      FStar_String.concat ""
        ["    rem ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Riscv64_Semantics.Remw (dst, s1, s2) ->
      FStar_String.concat ""
        ["    remw ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Riscv64_Semantics.Remu (dst, s1, s2) ->
      FStar_String.concat ""
        ["    remu ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Riscv64_Semantics.Remuw (dst, s1, s2) ->
      FStar_String.concat ""
        ["    remuw ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Riscv64_Semantics.Andn (dst, s1, s2) ->
      FStar_String.concat ""
        ["    andn ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Riscv64_Semantics.Orn (dst, s1, s2) ->
      FStar_String.concat ""
        ["    orn ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Riscv64_Semantics.Xnor (dst, s1, s2) ->
      FStar_String.concat ""
        ["    xnor ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Riscv64_Semantics.Clz (dst, src) ->
      FStar_String.concat ""
        ["    clz "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_Riscv64_Semantics.Ctz (dst, src) ->
      FStar_String.concat ""
        ["    ctz "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_Riscv64_Semantics.Cpop (dst, src) ->
      FStar_String.concat ""
        ["    cpop "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_Riscv64_Semantics.Max (dst, s1, s2) ->
      FStar_String.concat ""
        ["    max ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Riscv64_Semantics.Maxu (dst, s1, s2) ->
      FStar_String.concat ""
        ["    maxu ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Riscv64_Semantics.Min (dst, s1, s2) ->
      FStar_String.concat ""
        ["    min ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Riscv64_Semantics.Minu (dst, s1, s2) ->
      FStar_String.concat ""
        ["    minu ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Riscv64_Semantics.Rol (dst, s1, s2) ->
      FStar_String.concat ""
        ["    rol ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Riscv64_Semantics.Ror (dst, s1, s2) ->
      FStar_String.concat ""
        ["    ror ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_operand s2;
        "\n"]
  | Vale_Riscv64_Semantics.Rev8 (dst, src) ->
      FStar_String.concat ""
        ["    rev8 "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_Riscv64_Semantics.Aes64es (dst, s1, s2) ->
      FStar_String.concat ""
        ["    aes64es ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Riscv64_Semantics.Aes64esm (dst, s1, s2) ->
      FStar_String.concat ""
        ["    aes64esm ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Riscv64_Semantics.Aes64ds (dst, s1, s2) ->
      FStar_String.concat ""
        ["    aes64ds ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Riscv64_Semantics.Aes64dsm (dst, s1, s2) ->
      FStar_String.concat ""
        ["    aes64dsm ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Riscv64_Semantics.Aes64ks1i (dst, src, rnum) ->
      FStar_String.concat ""
        ["    aes64ks1i ";
        print_reg dst;
        ", ";
        print_reg src;
        ", ";
        FStar_UInt64.to_string rnum;
        "\n"]
  | Vale_Riscv64_Semantics.Aes64ks2 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    aes64ks2 ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        "\n"]
  | Vale_Riscv64_Semantics.Aes64im (dst, src) ->
      FStar_String.concat ""
        ["    aes64im "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_Riscv64_Semantics.Sha256sum0 (dst, src) ->
      FStar_String.concat ""
        ["    sha256sum0 "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_Riscv64_Semantics.Sha256sum1 (dst, src) ->
      FStar_String.concat ""
        ["    sha256sum1 "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_Riscv64_Semantics.Sha256sig0 (dst, src) ->
      FStar_String.concat ""
        ["    sha256sig0 "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_Riscv64_Semantics.Sha256sig1 (dst, src) ->
      FStar_String.concat ""
        ["    sha256sig1 "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_Riscv64_Semantics.Sha512sum0 (dst, src) ->
      FStar_String.concat ""
        ["    sha512sum0 "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_Riscv64_Semantics.Sha512sum1 (dst, src) ->
      FStar_String.concat ""
        ["    sha512sum1 "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_Riscv64_Semantics.Sha512sig0 (dst, src) ->
      FStar_String.concat ""
        ["    sha512sig0 "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_Riscv64_Semantics.Sha512sig1 (dst, src) ->
      FStar_String.concat ""
        ["    sha512sig1 "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_Riscv64_Semantics.Sm4ed (dst, s1, s2, bs) ->
      FStar_String.concat ""
        ["    sm4ed ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        ", ";
        FStar_UInt64.to_string bs;
        "\n"]
  | Vale_Riscv64_Semantics.Sm4ks (dst, s1, s2, bs) ->
      FStar_String.concat ""
        ["    sm4ks ";
        print_reg dst;
        ", ";
        print_reg s1;
        ", ";
        print_reg s2;
        ", ";
        FStar_UInt64.to_string bs;
        "\n"]
  | Vale_Riscv64_Semantics.Sm3p0 (dst, src) ->
      FStar_String.concat ""
        ["    sm3p0 "; print_reg dst; ", "; print_reg src; "\n"]
  | Vale_Riscv64_Semantics.Sm3p1 (dst, src) ->
      FStar_String.concat ""
        ["    sm3p1 "; print_reg dst; ", "; print_reg src; "\n"]
let rec print_code (c : Vale_Riscv64_Semantics.code) : Prims.string=
  match c with
  | Vale_Riscv64_Semantics.Ins i -> print_ins i
  | Vale_Riscv64_Semantics.Block cs -> print_codes cs
  | Vale_Riscv64_Semantics.IfElse (uu___, ct, cf) ->
      FStar_String.concat "" [print_code ct; print_code cf]
  | Vale_Riscv64_Semantics.While (uu___, body) -> print_code body
and print_codes (cs : Vale_Riscv64_Semantics.code Prims.list) : Prims.string=
  match cs with
  | [] -> ""
  | c::rest -> FStar_String.concat "" [print_code c; print_codes rest]
let emit_header (name : Prims.string) : Prims.string=
  FStar_String.concat ""
    ["    .globl ";
    name;
    "\n";
    "    .type ";
    name;
    ", @function\n";
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
    (match Vale_Compiler_Lower_Riscv64.lower_stmts
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
    "    .text\n    .attribute arch, \"rv64gc_zbb1p0_zk1p0\"\n    .p2align 2\n\n" in
  FStar_String.concat ""
    ["// Emitted by Vale verified assembly compiler (RISC-V 64-bit)\n";
    preamble;
    emit_procs_asm m.Vale_Compiler_AST.mod_procs;
    "    .section .note.GNU-stack,\"\",@progbits\n"]
