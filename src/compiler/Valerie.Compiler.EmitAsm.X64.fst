module Valerie.Compiler.EmitAsm.X64

open FStar.String
open Valerie.Defs
open FStar.UInt64
open Valerie.X64.Machine
open Valerie.X64.Semantics
open Valerie.Compiler.AST
open Valerie.Compiler.Lower.X64

let print_reg64 (r: reg) : string =
  match r with
  | Rax -> "%rax"
  | Rbx -> "%rbx"
  | Rcx -> "%rcx"
  | Rdx -> "%rdx"
  | Rsi -> "%rsi"
  | Rdi -> "%rdi"
  | Rbp -> "%rbp"
  | Rsp -> "%rsp"
  | R8  -> "%r8"
  | R9  -> "%r9"
  | R10 -> "%r10"
  | R11 -> "%r11"
  | R12 -> "%r12"
  | R13 -> "%r13"
  | R14 -> "%r14"
  | R15 -> "%r15"

let print_reg32 (r: reg) : string =
  match r with
  | Rax -> "%eax"
  | Rbx -> "%ebx"
  | Rcx -> "%ecx"
  | Rdx -> "%edx"
  | Rsi -> "%esi"
  | Rdi -> "%edi"
  | Rbp -> "%ebp"
  | Rsp -> "%esp"
  | R8  -> "%r8d"
  | R9  -> "%r9d"
  | R10 -> "%r10d"
  | R11 -> "%r11d"
  | R12 -> "%r12d"
  | R13 -> "%r13d"
  | R14 -> "%r14d"
  | R15 -> "%r15d"

let print_xmm (x: xmm) : string =
  match x with
  | Xmm0  -> "%xmm0"
  | Xmm1  -> "%xmm1"
  | Xmm2  -> "%xmm2"
  | Xmm3  -> "%xmm3"
  | Xmm4  -> "%xmm4"
  | Xmm5  -> "%xmm5"
  | Xmm6  -> "%xmm6"
  | Xmm7  -> "%xmm7"
  | Xmm8  -> "%xmm8"
  | Xmm9  -> "%xmm9"
  | Xmm10 -> "%xmm10"
  | Xmm11 -> "%xmm11"
  | Xmm12 -> "%xmm12"
  | Xmm13 -> "%xmm13"
  | Xmm14 -> "%xmm14"
  | Xmm15 -> "%xmm15"

let print_ymm (y: ymm) : string =
  match y with
  | Ymm0  -> "%ymm0"
  | Ymm1  -> "%ymm1"
  | Ymm2  -> "%ymm2"
  | Ymm3  -> "%ymm3"
  | Ymm4  -> "%ymm4"
  | Ymm5  -> "%ymm5"
  | Ymm6  -> "%ymm6"
  | Ymm7  -> "%ymm7"
  | Ymm8  -> "%ymm8"
  | Ymm9  -> "%ymm9"
  | Ymm10 -> "%ymm10"
  | Ymm11 -> "%ymm11"
  | Ymm12 -> "%ymm12"
  | Ymm13 -> "%ymm13"
  | Ymm14 -> "%ymm14"
  | Ymm15 -> "%ymm15"

let print_cond (c: cond_code) : string =
  match c with
  | CondE  -> "e"
  | CondNE -> "ne"
  | CondB  -> "b"
  | CondBE -> "be"
  | CondA  -> "a"
  | CondAE -> "ae"
  | CondL  -> "l"
  | CondLE -> "le"
  | CondG  -> "g"
  | CondGE -> "ge"

let print_maddr (m: maddr) : string =
  match m with
  | MConst addr -> string_of_int addr
  | MReg r offset ->
      if offset = 0 then concat "" ["("; print_reg64 r; ")"]
      else concat "" [string_of_int offset; "("; print_reg64 r; ")"]
  | MIndex base scale index offset ->
      if offset = 0 then concat "" ["("; print_reg64 base; ", "; print_reg64 index; ", "; string_of_int scale; ")"]
      else concat "" [string_of_int offset; "("; print_reg64 base; ", "; print_reg64 index; ", "; string_of_int scale; ")"]
  | MRipRel offset ->
      concat "" [string_of_int offset; "(%rip)"]

let print_operand (op: Valerie.X64.Semantics.operand) : string =
  match op with
  | Valerie.X64.Semantics.OReg r R64 -> print_reg64 r
  | Valerie.X64.Semantics.OReg r R32 -> print_reg32 r
  | Valerie.X64.Semantics.OXmm x -> print_xmm x
  | Valerie.X64.Semantics.OYmm y -> print_ymm y
  | Valerie.X64.Semantics.OImm imm -> concat "" ["$"; FStar.UInt64.to_string imm]
  | Valerie.X64.Semantics.OMem m -> print_maddr m

let print_ins (i: ins) : string =
  match i with
  // Scalar Arithmetic & Logic (AT&T syntax: op src, dst)
  | Mov64 dst src -> concat "" ["    movq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Mov32 dst src -> concat "" ["    movl "; print_operand src; ", "; print_reg32 dst; "\n"]
  | Add64 dst src -> concat "" ["    addq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Adc64 dst src -> concat "" ["    adcq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Adcx64 dst src -> concat "" ["    adcx "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Adox64 dst src -> concat "" ["    adox "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Sub64 dst src -> concat "" ["    subq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Sbb64 dst src -> concat "" ["    sbbq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Xor64 dst src -> concat "" ["    xorq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | And64 dst src -> concat "" ["    andq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Or64 dst src -> concat "" ["    orq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Not64 dst -> concat "" ["    notq "; print_reg64 dst; "\n"]
  | Neg64 dst -> concat "" ["    negq "; print_reg64 dst; "\n"]
  | Shl64 dst cnt -> concat "" ["    shlq "; print_operand cnt; ", "; print_reg64 dst; "\n"]
  | Shr64 dst cnt -> concat "" ["    shrq "; print_operand cnt; ", "; print_reg64 dst; "\n"]
  | Sar64 dst cnt -> concat "" ["    sarq "; print_operand cnt; ", "; print_reg64 dst; "\n"]
  | Rol64 dst cnt -> concat "" ["    rolq "; print_operand cnt; ", "; print_reg64 dst; "\n"]
  | Ror64 dst cnt -> concat "" ["    rorq "; print_operand cnt; ", "; print_reg64 dst; "\n"]
  | Bswap64 dst -> concat "" ["    bswapq "; print_reg64 dst; "\n"]
  | Cmp64 s1 s2 -> concat "" ["    cmpq "; print_operand s2; ", "; print_reg64 s1; "\n"]
  | Test64 s1 s2 -> concat "" ["    testq "; print_operand s2; ", "; print_reg64 s1; "\n"]
  | Lea64 dst addr -> concat "" ["    leaq "; print_maddr addr; ", "; print_reg64 dst; "\n"]
  | Imul64 dst src -> concat "" ["    imulq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Mul64 src -> concat "" ["    mulq "; print_operand src; "\n"]
  | Mulx64 hi lo src -> concat "" ["    mulx "; print_operand src; ", "; print_reg64 lo; ", "; print_reg64 hi; "\n"]

  // BMI1 / BMI2
  | Andn64 dst s1 s2 -> concat "" ["    andnq "; print_operand s2; ", "; print_reg64 s1; ", "; print_reg64 dst; "\n"]
  | Rorx64 dst src imm -> concat "" ["    rorxq $"; string_of_int (UInt8.v imm); ", "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Sarx64 dst src cnt -> concat "" ["    sarxq "; print_reg64 cnt; ", "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Shlx64 dst src cnt -> concat "" ["    shlxq "; print_reg64 cnt; ", "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Shrx64 dst src cnt -> concat "" ["    shrxq "; print_reg64 cnt; ", "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Bsf64 dst src -> concat "" ["    bsfq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Bsr64 dst src -> concat "" ["    bsrq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Tzcnt64 dst src -> concat "" ["    tzcntq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Lzcnt64 dst src -> concat "" ["    lzcntq "; print_operand src; ", "; print_reg64 dst; "\n"]
  | Popcnt64 dst src -> concat "" ["    popcntq "; print_operand src; ", "; print_reg64 dst; "\n"]

  // Conditional Moves
  | Cmov64 cond dst src -> concat "" ["    cmov"; print_cond cond; " "; print_operand src; ", "; print_reg64 dst; "\n"]

  // Control Flow
  | Jmp lbl -> concat "" ["    jmp "; lbl; "\n"]
  | Jcc cond lbl -> concat "" ["    j"; print_cond cond; " "; lbl; "\n"]
  | Call lbl -> concat "" ["    call "; lbl; "\n"]
  | Ret -> "    ret\n"
  | Push64 src -> concat "" ["    pushq "; print_operand src; "\n"]
  | Pop64 dst -> concat "" ["    popq "; print_reg64 dst; "\n"]

  // AES-NI & PCLMULQDQ
  | Aesenc dst src -> concat "" ["    aesenc "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Aesenclast dst src -> concat "" ["    aesenclast "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Aesdec dst src -> concat "" ["    aesdec "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Aesdeclast dst src -> concat "" ["    aesdeclast "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Aesimc dst src -> concat "" ["    aesimc "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Aeskeygenassist dst src rcon -> concat "" ["    aeskeygenassist $"; string_of_int (UInt8.v rcon); ", "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Vaesenc dst s1 s2 -> concat "" ["    vaesenc "; print_xmm s2; ", "; print_xmm s1; ", "; print_xmm dst; "\n"]
  | Vaesdec dst s1 s2 -> concat "" ["    vaesdec "; print_xmm s2; ", "; print_xmm s1; ", "; print_xmm dst; "\n"]
  | Pclmulqdq dst src imm -> concat "" ["    pclmulqdq $"; string_of_int (UInt8.v imm); ", "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Vpclmulqdq dst s1 s2 imm -> concat "" ["    vpclmulqdq $"; string_of_int (UInt8.v imm); ", "; print_xmm s2; ", "; print_xmm s1; ", "; print_xmm dst; "\n"]

  // SHA-NI
  | Sha1rnds4 dst src imm -> concat "" ["    sha1rnds4 $"; string_of_int (UInt8.v imm); ", "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Sha1nexte dst src -> concat "" ["    sha1nexte "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Sha1msg1 dst src -> concat "" ["    sha1msg1 "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Sha1msg2 dst src -> concat "" ["    sha1msg2 "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Sha256rnds2 dst src -> concat "" ["    sha256rnds2 %xmm0, "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Sha256msg1 dst src -> concat "" ["    sha256msg1 "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Sha256msg2 dst src -> concat "" ["    sha256msg2 "; print_xmm src; ", "; print_xmm dst; "\n"]

  // SSE / AVX
  | Movdqu dst src -> concat "" ["    movdqu "; print_operand src; ", "; print_xmm dst; "\n"]
  | Movdqa dst src -> concat "" ["    movdqa "; print_operand src; ", "; print_xmm dst; "\n"]
  | Vmovdqu dst src -> concat "" ["    vmovdqu "; print_operand src; ", "; print_xmm dst; "\n"]
  | Vmovdqa dst src -> concat "" ["    vmovdqa "; print_operand src; ", "; print_xmm dst; "\n"]
  | Pxor dst src -> concat "" ["    pxor "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Vpxor dst s1 s2 -> concat "" ["    vpxor "; print_xmm s2; ", "; print_xmm s1; ", "; print_xmm dst; "\n"]
  | Pand dst src -> concat "" ["    pand "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Por dst src -> concat "" ["    por "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Paddd dst src -> concat "" ["    paddd "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Psubd dst src -> concat "" ["    psubd "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Paddq dst src -> concat "" ["    paddq "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Psubq dst src -> concat "" ["    psubq "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Paddb dst src -> concat "" ["    paddb "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Psubb dst src -> concat "" ["    psubb "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Paddw dst src -> concat "" ["    paddw "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Psubw dst src -> concat "" ["    psubw "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Pslld dst cnt -> concat "" ["    pslld $"; string_of_int (UInt8.v cnt); ", "; print_xmm dst; "\n"]
  | Psrld dst cnt -> concat "" ["    psrld $"; string_of_int (UInt8.v cnt); ", "; print_xmm dst; "\n"]
  | Psllq dst cnt -> concat "" ["    psllq $"; string_of_int (UInt8.v cnt); ", "; print_xmm dst; "\n"]
  | Psrlq dst cnt -> concat "" ["    psrlq $"; string_of_int (UInt8.v cnt); ", "; print_xmm dst; "\n"]
  | Pshufd dst src imm -> concat "" ["    pshufd $"; string_of_int (UInt8.v imm); ", "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Pshufb dst src -> concat "" ["    pshufb "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Palignr dst src imm -> concat "" ["    palignr $"; string_of_int (UInt8.v imm); ", "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Punpcklbw dst src -> concat "" ["    punpcklbw "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Punpckhbw dst src -> concat "" ["    punpckhbw "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Punpcklwd dst src -> concat "" ["    punpcklwd "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Punpckhwd dst src -> concat "" ["    punpckhwd "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Punpckldq dst src -> concat "" ["    punpckldq "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Punpckhdq dst src -> concat "" ["    punpckhdq "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Punpcklqdq dst src -> concat "" ["    punpcklqdq "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Punpckhqdq dst src -> concat "" ["    punpckhqdq "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Pcmpeqd dst src -> concat "" ["    pcmpeqd "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Pcmpeqb dst src -> concat "" ["    pcmpeqb "; print_xmm src; ", "; print_xmm dst; "\n"]
  | Pextrq dst src imm -> concat "" ["    pextrq $"; string_of_int (UInt8.v imm); ", "; print_xmm src; ", "; print_reg64 dst; "\n"]
  | Pextrd dst src imm -> concat "" ["    pextrd $"; string_of_int (UInt8.v imm); ", "; print_xmm src; ", "; print_reg32 dst; "\n"]
  | Pinsrd dst src imm -> concat "" ["    pinsrd $"; string_of_int (UInt8.v imm); ", "; print_reg32 src; ", "; print_xmm dst; "\n"]
  | Pinsrq dst src imm -> concat "" ["    pinsrq $"; string_of_int (UInt8.v imm); ", "; print_reg64 src; ", "; print_xmm dst; "\n"]

let rec print_code (c: code) : Tot string (decreases c) =
  match c with
  | Ins i -> print_ins i
  | Block cs -> print_codes cs
  | IfElse _ c1 c2 -> concat "" [print_code c1; print_code c2]
  | While _ body -> print_code body

and print_codes (cs: list code) : Tot string (decreases cs) =
  match cs with
  | [] -> ""
  | c :: rest -> concat "" [print_code c; print_codes rest]

let emit_header (name: string) : string =
  concat "" [
    "    .globl "; name; "\n";
    "    .type "; name; ", @function\n";
    "    .p2align 4, 0x90\n";
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
    match Valerie.Compiler.Lower.X64.lower_stmts p.proc_body with
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
  | p :: rest -> concat "" [emit_proc_asm p; emit_procs_asm rest]

let emit_module_asm (m: module_decl) (has_crypto: bool) : string =
  let text_sec = concat "" [
    "    .text\n";
    "    .p2align 4, 0x90\n\n"
  ] in
  concat "" [
    "// Emitted by Vale verified assembly compiler (x86_64 AT&T)\n";
    text_sec;
    emit_procs_asm m.mod_procs;
    "    .section .note.GNU-stack,\"\",@progbits\n"
  ]
