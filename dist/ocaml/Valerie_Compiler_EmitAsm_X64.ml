open Prims
let print_reg64 (r : Valerie_X64_Machine.reg) : Prims.string=
  match r with
  | Valerie_X64_Machine.Rax -> "%rax"
  | Valerie_X64_Machine.Rbx -> "%rbx"
  | Valerie_X64_Machine.Rcx -> "%rcx"
  | Valerie_X64_Machine.Rdx -> "%rdx"
  | Valerie_X64_Machine.Rsi -> "%rsi"
  | Valerie_X64_Machine.Rdi -> "%rdi"
  | Valerie_X64_Machine.Rbp -> "%rbp"
  | Valerie_X64_Machine.Rsp -> "%rsp"
  | Valerie_X64_Machine.R8 -> "%r8"
  | Valerie_X64_Machine.R9 -> "%r9"
  | Valerie_X64_Machine.R10 -> "%r10"
  | Valerie_X64_Machine.R11 -> "%r11"
  | Valerie_X64_Machine.R12 -> "%r12"
  | Valerie_X64_Machine.R13 -> "%r13"
  | Valerie_X64_Machine.R14 -> "%r14"
  | Valerie_X64_Machine.R15 -> "%r15"
let print_reg32 (r : Valerie_X64_Machine.reg) : Prims.string=
  match r with
  | Valerie_X64_Machine.Rax -> "%eax"
  | Valerie_X64_Machine.Rbx -> "%ebx"
  | Valerie_X64_Machine.Rcx -> "%ecx"
  | Valerie_X64_Machine.Rdx -> "%edx"
  | Valerie_X64_Machine.Rsi -> "%esi"
  | Valerie_X64_Machine.Rdi -> "%edi"
  | Valerie_X64_Machine.Rbp -> "%ebp"
  | Valerie_X64_Machine.Rsp -> "%esp"
  | Valerie_X64_Machine.R8 -> "%r8d"
  | Valerie_X64_Machine.R9 -> "%r9d"
  | Valerie_X64_Machine.R10 -> "%r10d"
  | Valerie_X64_Machine.R11 -> "%r11d"
  | Valerie_X64_Machine.R12 -> "%r12d"
  | Valerie_X64_Machine.R13 -> "%r13d"
  | Valerie_X64_Machine.R14 -> "%r14d"
  | Valerie_X64_Machine.R15 -> "%r15d"
let print_xmm (x : Valerie_X64_Machine.xmm) : Prims.string=
  match x with
  | Valerie_X64_Machine.Xmm0 -> "%xmm0"
  | Valerie_X64_Machine.Xmm1 -> "%xmm1"
  | Valerie_X64_Machine.Xmm2 -> "%xmm2"
  | Valerie_X64_Machine.Xmm3 -> "%xmm3"
  | Valerie_X64_Machine.Xmm4 -> "%xmm4"
  | Valerie_X64_Machine.Xmm5 -> "%xmm5"
  | Valerie_X64_Machine.Xmm6 -> "%xmm6"
  | Valerie_X64_Machine.Xmm7 -> "%xmm7"
  | Valerie_X64_Machine.Xmm8 -> "%xmm8"
  | Valerie_X64_Machine.Xmm9 -> "%xmm9"
  | Valerie_X64_Machine.Xmm10 -> "%xmm10"
  | Valerie_X64_Machine.Xmm11 -> "%xmm11"
  | Valerie_X64_Machine.Xmm12 -> "%xmm12"
  | Valerie_X64_Machine.Xmm13 -> "%xmm13"
  | Valerie_X64_Machine.Xmm14 -> "%xmm14"
  | Valerie_X64_Machine.Xmm15 -> "%xmm15"
let print_ymm (y : Valerie_X64_Machine.ymm) : Prims.string=
  match y with
  | Valerie_X64_Machine.Ymm0 -> "%ymm0"
  | Valerie_X64_Machine.Ymm1 -> "%ymm1"
  | Valerie_X64_Machine.Ymm2 -> "%ymm2"
  | Valerie_X64_Machine.Ymm3 -> "%ymm3"
  | Valerie_X64_Machine.Ymm4 -> "%ymm4"
  | Valerie_X64_Machine.Ymm5 -> "%ymm5"
  | Valerie_X64_Machine.Ymm6 -> "%ymm6"
  | Valerie_X64_Machine.Ymm7 -> "%ymm7"
  | Valerie_X64_Machine.Ymm8 -> "%ymm8"
  | Valerie_X64_Machine.Ymm9 -> "%ymm9"
  | Valerie_X64_Machine.Ymm10 -> "%ymm10"
  | Valerie_X64_Machine.Ymm11 -> "%ymm11"
  | Valerie_X64_Machine.Ymm12 -> "%ymm12"
  | Valerie_X64_Machine.Ymm13 -> "%ymm13"
  | Valerie_X64_Machine.Ymm14 -> "%ymm14"
  | Valerie_X64_Machine.Ymm15 -> "%ymm15"
let print_cond (c : Valerie_X64_Machine.cond_code) : Prims.string=
  match c with
  | Valerie_X64_Machine.CondE -> "e"
  | Valerie_X64_Machine.CondNE -> "ne"
  | Valerie_X64_Machine.CondB -> "b"
  | Valerie_X64_Machine.CondBE -> "be"
  | Valerie_X64_Machine.CondA -> "a"
  | Valerie_X64_Machine.CondAE -> "ae"
  | Valerie_X64_Machine.CondL -> "l"
  | Valerie_X64_Machine.CondLE -> "le"
  | Valerie_X64_Machine.CondG -> "g"
  | Valerie_X64_Machine.CondGE -> "ge"
let print_maddr (m : Valerie_X64_Machine.maddr) : Prims.string=
  match m with
  | Valerie_X64_Machine.MConst addr -> Prims.string_of_int addr
  | Valerie_X64_Machine.MReg (r, offset) ->
      if offset = Prims.int_zero
      then FStar_String.concat "" ["("; print_reg64 r; ")"]
      else
        FStar_String.concat ""
          [Prims.string_of_int offset; "("; print_reg64 r; ")"]
  | Valerie_X64_Machine.MIndex (base, scale, index, offset) ->
      if offset = Prims.int_zero
      then
        FStar_String.concat ""
          ["(";
          print_reg64 base;
          ", ";
          print_reg64 index;
          ", ";
          Prims.string_of_int scale;
          ")"]
      else
        FStar_String.concat ""
          [Prims.string_of_int offset;
          "(";
          print_reg64 base;
          ", ";
          print_reg64 index;
          ", ";
          Prims.string_of_int scale;
          ")"]
  | Valerie_X64_Machine.MRipRel offset ->
      FStar_String.concat "" [Prims.string_of_int offset; "(%rip)"]
let print_operand (op : Valerie_X64_Semantics.operand) : Prims.string=
  match op with
  | Valerie_X64_Semantics.OReg (r, Valerie_X64_Machine.R64) -> print_reg64 r
  | Valerie_X64_Semantics.OReg (r, Valerie_X64_Machine.R32) -> print_reg32 r
  | Valerie_X64_Semantics.OXmm x -> print_xmm x
  | Valerie_X64_Semantics.OYmm y -> print_ymm y
  | Valerie_X64_Semantics.OImm imm ->
      FStar_String.concat "" ["$"; FStar_UInt64.to_string imm]
  | Valerie_X64_Semantics.OMem m -> print_maddr m
let print_ins (i : Valerie_X64_Semantics.ins) : Prims.string=
  match i with
  | Valerie_X64_Semantics.Mov64 (dst, src) ->
      FStar_String.concat ""
        ["    movq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Valerie_X64_Semantics.Mov32 (dst, src) ->
      FStar_String.concat ""
        ["    movl "; print_operand src; ", "; print_reg32 dst; "\n"]
  | Valerie_X64_Semantics.Add64 (dst, src) ->
      FStar_String.concat ""
        ["    addq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Valerie_X64_Semantics.Adc64 (dst, src) ->
      FStar_String.concat ""
        ["    adcq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Valerie_X64_Semantics.Adcx64 (dst, src) ->
      FStar_String.concat ""
        ["    adcx "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Valerie_X64_Semantics.Adox64 (dst, src) ->
      FStar_String.concat ""
        ["    adox "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Valerie_X64_Semantics.Sub64 (dst, src) ->
      FStar_String.concat ""
        ["    subq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Valerie_X64_Semantics.Sbb64 (dst, src) ->
      FStar_String.concat ""
        ["    sbbq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Valerie_X64_Semantics.Xor64 (dst, src) ->
      FStar_String.concat ""
        ["    xorq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Valerie_X64_Semantics.And64 (dst, src) ->
      FStar_String.concat ""
        ["    andq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Valerie_X64_Semantics.Or64 (dst, src) ->
      FStar_String.concat ""
        ["    orq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Valerie_X64_Semantics.Not64 dst ->
      FStar_String.concat "" ["    notq "; print_reg64 dst; "\n"]
  | Valerie_X64_Semantics.Neg64 dst ->
      FStar_String.concat "" ["    negq "; print_reg64 dst; "\n"]
  | Valerie_X64_Semantics.Shl64 (dst, cnt) ->
      FStar_String.concat ""
        ["    shlq "; print_operand cnt; ", "; print_reg64 dst; "\n"]
  | Valerie_X64_Semantics.Shr64 (dst, cnt) ->
      FStar_String.concat ""
        ["    shrq "; print_operand cnt; ", "; print_reg64 dst; "\n"]
  | Valerie_X64_Semantics.Sar64 (dst, cnt) ->
      FStar_String.concat ""
        ["    sarq "; print_operand cnt; ", "; print_reg64 dst; "\n"]
  | Valerie_X64_Semantics.Rol64 (dst, cnt) ->
      FStar_String.concat ""
        ["    rolq "; print_operand cnt; ", "; print_reg64 dst; "\n"]
  | Valerie_X64_Semantics.Ror64 (dst, cnt) ->
      FStar_String.concat ""
        ["    rorq "; print_operand cnt; ", "; print_reg64 dst; "\n"]
  | Valerie_X64_Semantics.Bswap64 dst ->
      FStar_String.concat "" ["    bswapq "; print_reg64 dst; "\n"]
  | Valerie_X64_Semantics.Cmp64 (s1, s2) ->
      FStar_String.concat ""
        ["    cmpq "; print_operand s2; ", "; print_reg64 s1; "\n"]
  | Valerie_X64_Semantics.Test64 (s1, s2) ->
      FStar_String.concat ""
        ["    testq "; print_operand s2; ", "; print_reg64 s1; "\n"]
  | Valerie_X64_Semantics.Lea64 (dst, addr) ->
      FStar_String.concat ""
        ["    leaq "; print_maddr addr; ", "; print_reg64 dst; "\n"]
  | Valerie_X64_Semantics.Imul64 (dst, src) ->
      FStar_String.concat ""
        ["    imulq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Valerie_X64_Semantics.Mul64 src ->
      FStar_String.concat "" ["    mulq "; print_operand src; "\n"]
  | Valerie_X64_Semantics.Mulx64 (hi, lo, src) ->
      FStar_String.concat ""
        ["    mulx ";
        print_operand src;
        ", ";
        print_reg64 lo;
        ", ";
        print_reg64 hi;
        "\n"]
  | Valerie_X64_Semantics.Andn64 (dst, s1, s2) ->
      FStar_String.concat ""
        ["    andnq ";
        print_operand s2;
        ", ";
        print_reg64 s1;
        ", ";
        print_reg64 dst;
        "\n"]
  | Valerie_X64_Semantics.Rorx64 (dst, src, imm) ->
      FStar_String.concat ""
        ["    rorxq $";
        Prims.string_of_int (FStar_UInt8.v imm);
        ", ";
        print_operand src;
        ", ";
        print_reg64 dst;
        "\n"]
  | Valerie_X64_Semantics.Sarx64 (dst, src, cnt) ->
      FStar_String.concat ""
        ["    sarxq ";
        print_reg64 cnt;
        ", ";
        print_operand src;
        ", ";
        print_reg64 dst;
        "\n"]
  | Valerie_X64_Semantics.Shlx64 (dst, src, cnt) ->
      FStar_String.concat ""
        ["    shlxq ";
        print_reg64 cnt;
        ", ";
        print_operand src;
        ", ";
        print_reg64 dst;
        "\n"]
  | Valerie_X64_Semantics.Shrx64 (dst, src, cnt) ->
      FStar_String.concat ""
        ["    shrxq ";
        print_reg64 cnt;
        ", ";
        print_operand src;
        ", ";
        print_reg64 dst;
        "\n"]
  | Valerie_X64_Semantics.Bsf64 (dst, src) ->
      FStar_String.concat ""
        ["    bsfq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Valerie_X64_Semantics.Bsr64 (dst, src) ->
      FStar_String.concat ""
        ["    bsrq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Valerie_X64_Semantics.Tzcnt64 (dst, src) ->
      FStar_String.concat ""
        ["    tzcntq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Valerie_X64_Semantics.Lzcnt64 (dst, src) ->
      FStar_String.concat ""
        ["    lzcntq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Valerie_X64_Semantics.Popcnt64 (dst, src) ->
      FStar_String.concat ""
        ["    popcntq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Valerie_X64_Semantics.Cmov64 (cond, dst, src) ->
      FStar_String.concat ""
        ["    cmov";
        print_cond cond;
        " ";
        print_operand src;
        ", ";
        print_reg64 dst;
        "\n"]
  | Valerie_X64_Semantics.Jmp lbl ->
      FStar_String.concat "" ["    jmp "; lbl; "\n"]
  | Valerie_X64_Semantics.Jcc (cond, lbl) ->
      FStar_String.concat "" ["    j"; print_cond cond; " "; lbl; "\n"]
  | Valerie_X64_Semantics.Call lbl ->
      FStar_String.concat "" ["    call "; lbl; "\n"]
  | Valerie_X64_Semantics.Ret -> "    ret\n"
  | Valerie_X64_Semantics.Push64 src ->
      FStar_String.concat "" ["    pushq "; print_operand src; "\n"]
  | Valerie_X64_Semantics.Pop64 dst ->
      FStar_String.concat "" ["    popq "; print_reg64 dst; "\n"]
  | Valerie_X64_Semantics.Aesenc (dst, src) ->
      FStar_String.concat ""
        ["    aesenc "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Aesenclast (dst, src) ->
      FStar_String.concat ""
        ["    aesenclast "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Aesdec (dst, src) ->
      FStar_String.concat ""
        ["    aesdec "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Aesdeclast (dst, src) ->
      FStar_String.concat ""
        ["    aesdeclast "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Aesimc (dst, src) ->
      FStar_String.concat ""
        ["    aesimc "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Aeskeygenassist (dst, src, rcon) ->
      FStar_String.concat ""
        ["    aeskeygenassist $";
        Prims.string_of_int (FStar_UInt8.v rcon);
        ", ";
        print_xmm src;
        ", ";
        print_xmm dst;
        "\n"]
  | Valerie_X64_Semantics.Vaesenc (dst, s1, s2) ->
      FStar_String.concat ""
        ["    vaesenc ";
        print_xmm s2;
        ", ";
        print_xmm s1;
        ", ";
        print_xmm dst;
        "\n"]
  | Valerie_X64_Semantics.Vaesdec (dst, s1, s2) ->
      FStar_String.concat ""
        ["    vaesdec ";
        print_xmm s2;
        ", ";
        print_xmm s1;
        ", ";
        print_xmm dst;
        "\n"]
  | Valerie_X64_Semantics.Pclmulqdq (dst, src, imm) ->
      FStar_String.concat ""
        ["    pclmulqdq $";
        Prims.string_of_int (FStar_UInt8.v imm);
        ", ";
        print_xmm src;
        ", ";
        print_xmm dst;
        "\n"]
  | Valerie_X64_Semantics.Vpclmulqdq (dst, s1, s2, imm) ->
      FStar_String.concat ""
        ["    vpclmulqdq $";
        Prims.string_of_int (FStar_UInt8.v imm);
        ", ";
        print_xmm s2;
        ", ";
        print_xmm s1;
        ", ";
        print_xmm dst;
        "\n"]
  | Valerie_X64_Semantics.Sha1rnds4 (dst, src, imm) ->
      FStar_String.concat ""
        ["    sha1rnds4 $";
        Prims.string_of_int (FStar_UInt8.v imm);
        ", ";
        print_xmm src;
        ", ";
        print_xmm dst;
        "\n"]
  | Valerie_X64_Semantics.Sha1nexte (dst, src) ->
      FStar_String.concat ""
        ["    sha1nexte "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Sha1msg1 (dst, src) ->
      FStar_String.concat ""
        ["    sha1msg1 "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Sha1msg2 (dst, src) ->
      FStar_String.concat ""
        ["    sha1msg2 "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Sha256rnds2 (dst, src) ->
      FStar_String.concat ""
        ["    sha256rnds2 %xmm0, "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Sha256msg1 (dst, src) ->
      FStar_String.concat ""
        ["    sha256msg1 "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Sha256msg2 (dst, src) ->
      FStar_String.concat ""
        ["    sha256msg2 "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Movdqu (dst, src) ->
      FStar_String.concat ""
        ["    movdqu "; print_operand src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Movdqa (dst, src) ->
      FStar_String.concat ""
        ["    movdqa "; print_operand src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Vmovdqu (dst, src) ->
      FStar_String.concat ""
        ["    vmovdqu "; print_operand src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Vmovdqa (dst, src) ->
      FStar_String.concat ""
        ["    vmovdqa "; print_operand src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Pxor (dst, src) ->
      FStar_String.concat ""
        ["    pxor "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Vpxor (dst, s1, s2) ->
      FStar_String.concat ""
        ["    vpxor ";
        print_xmm s2;
        ", ";
        print_xmm s1;
        ", ";
        print_xmm dst;
        "\n"]
  | Valerie_X64_Semantics.Pand (dst, src) ->
      FStar_String.concat ""
        ["    pand "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Por (dst, src) ->
      FStar_String.concat ""
        ["    por "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Paddd (dst, src) ->
      FStar_String.concat ""
        ["    paddd "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Psubd (dst, src) ->
      FStar_String.concat ""
        ["    psubd "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Paddq (dst, src) ->
      FStar_String.concat ""
        ["    paddq "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Psubq (dst, src) ->
      FStar_String.concat ""
        ["    psubq "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Paddb (dst, src) ->
      FStar_String.concat ""
        ["    paddb "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Psubb (dst, src) ->
      FStar_String.concat ""
        ["    psubb "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Paddw (dst, src) ->
      FStar_String.concat ""
        ["    paddw "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Psubw (dst, src) ->
      FStar_String.concat ""
        ["    psubw "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Pslld (dst, cnt) ->
      FStar_String.concat ""
        ["    pslld $";
        Prims.string_of_int (FStar_UInt8.v cnt);
        ", ";
        print_xmm dst;
        "\n"]
  | Valerie_X64_Semantics.Psrld (dst, cnt) ->
      FStar_String.concat ""
        ["    psrld $";
        Prims.string_of_int (FStar_UInt8.v cnt);
        ", ";
        print_xmm dst;
        "\n"]
  | Valerie_X64_Semantics.Psllq (dst, cnt) ->
      FStar_String.concat ""
        ["    psllq $";
        Prims.string_of_int (FStar_UInt8.v cnt);
        ", ";
        print_xmm dst;
        "\n"]
  | Valerie_X64_Semantics.Psrlq (dst, cnt) ->
      FStar_String.concat ""
        ["    psrlq $";
        Prims.string_of_int (FStar_UInt8.v cnt);
        ", ";
        print_xmm dst;
        "\n"]
  | Valerie_X64_Semantics.Pshufd (dst, src, imm) ->
      FStar_String.concat ""
        ["    pshufd $";
        Prims.string_of_int (FStar_UInt8.v imm);
        ", ";
        print_xmm src;
        ", ";
        print_xmm dst;
        "\n"]
  | Valerie_X64_Semantics.Pshufb (dst, src) ->
      FStar_String.concat ""
        ["    pshufb "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Palignr (dst, src, imm) ->
      FStar_String.concat ""
        ["    palignr $";
        Prims.string_of_int (FStar_UInt8.v imm);
        ", ";
        print_xmm src;
        ", ";
        print_xmm dst;
        "\n"]
  | Valerie_X64_Semantics.Punpcklbw (dst, src) ->
      FStar_String.concat ""
        ["    punpcklbw "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Punpckhbw (dst, src) ->
      FStar_String.concat ""
        ["    punpckhbw "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Punpcklwd (dst, src) ->
      FStar_String.concat ""
        ["    punpcklwd "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Punpckhwd (dst, src) ->
      FStar_String.concat ""
        ["    punpckhwd "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Punpckldq (dst, src) ->
      FStar_String.concat ""
        ["    punpckldq "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Punpckhdq (dst, src) ->
      FStar_String.concat ""
        ["    punpckhdq "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Punpcklqdq (dst, src) ->
      FStar_String.concat ""
        ["    punpcklqdq "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Punpckhqdq (dst, src) ->
      FStar_String.concat ""
        ["    punpckhqdq "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Pcmpeqd (dst, src) ->
      FStar_String.concat ""
        ["    pcmpeqd "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Pcmpeqb (dst, src) ->
      FStar_String.concat ""
        ["    pcmpeqb "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Valerie_X64_Semantics.Pextrq (dst, src, imm) ->
      FStar_String.concat ""
        ["    pextrq $";
        Prims.string_of_int (FStar_UInt8.v imm);
        ", ";
        print_xmm src;
        ", ";
        print_reg64 dst;
        "\n"]
  | Valerie_X64_Semantics.Pextrd (dst, src, imm) ->
      FStar_String.concat ""
        ["    pextrd $";
        Prims.string_of_int (FStar_UInt8.v imm);
        ", ";
        print_xmm src;
        ", ";
        print_reg32 dst;
        "\n"]
  | Valerie_X64_Semantics.Pinsrd (dst, src, imm) ->
      FStar_String.concat ""
        ["    pinsrd $";
        Prims.string_of_int (FStar_UInt8.v imm);
        ", ";
        print_reg32 src;
        ", ";
        print_xmm dst;
        "\n"]
  | Valerie_X64_Semantics.Pinsrq (dst, src, imm) ->
      FStar_String.concat ""
        ["    pinsrq $";
        Prims.string_of_int (FStar_UInt8.v imm);
        ", ";
        print_reg64 src;
        ", ";
        print_xmm dst;
        "\n"]
let rec print_code (c : Valerie_X64_Semantics.code) : Prims.string=
  match c with
  | Valerie_X64_Semantics.Ins i -> print_ins i
  | Valerie_X64_Semantics.Block cs -> print_codes cs
  | Valerie_X64_Semantics.IfElse (uu___, c1, c2) ->
      FStar_String.concat "" [print_code c1; print_code c2]
  | Valerie_X64_Semantics.While (uu___, body) -> print_code body
and print_codes (cs : Valerie_X64_Semantics.code Prims.list) : Prims.string=
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
    "    .p2align 4, 0x90\n";
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
    (match Valerie_Compiler_Lower_X64.lower_stmts
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
  (has_crypto : Prims.bool) : Prims.string=
  let text_sec = "    .text\n    .p2align 4, 0x90\n\n" in
  FStar_String.concat ""
    ["// Emitted by Vale verified assembly compiler (x86_64 AT&T)\n";
    text_sec;
    emit_procs_asm m.Valerie_Compiler_AST.mod_procs;
    "    .section .note.GNU-stack,\"\",@progbits\n"]
