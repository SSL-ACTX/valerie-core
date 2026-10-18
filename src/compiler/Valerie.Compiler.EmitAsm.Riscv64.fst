module Valerie.Compiler.EmitAsm.Riscv64

open FStar.String
open Valerie.Defs
open FStar.UInt64
open Valerie.Riscv64.Machine
open Valerie.Riscv64.Semantics
open Valerie.Compiler.AST
open Valerie.Compiler.Lower.Riscv64

let print_reg (r: reg) : string =
  match r with
  | Zero -> "zero"
  | RA   -> "ra"
  | SP   -> "sp"
  | GP   -> "gp"
  | TP   -> "tp"
  | T0   -> "t0"
  | T1   -> "t1"
  | T2   -> "t2"
  | S0   -> "s0"
  | S1   -> "s1"
  | A0   -> "a0"
  | A1   -> "a1"
  | A2   -> "a2"
  | A3   -> "a3"
  | A4   -> "a4"
  | A5   -> "a5"
  | A6   -> "a6"
  | A7   -> "a7"
  | S2   -> "s2"
  | S3   -> "s3"
  | S4   -> "s4"
  | S5   -> "s5"
  | S6   -> "s6"
  | S7   -> "s7"
  | S8   -> "s8"
  | S9   -> "s9"
  | S10  -> "s10"
  | S11  -> "s11"
  | T3   -> "t3"
  | T4   -> "t4"
  | T5   -> "t5"
  | T6   -> "t6"

let print_operand (op: Valerie.Riscv64.Semantics.operand) : string =
  match op with
  | Valerie.Riscv64.Semantics.OReg r -> print_reg r
  | Valerie.Riscv64.Semantics.OImm imm -> FStar.UInt64.to_string imm
  | Valerie.Riscv64.Semantics.OMem base off -> concat "" [string_of_int off; "("; print_reg base; ")"]
  | Valerie.Riscv64.Semantics.OLabel lbl -> lbl

let print_ins (i: ins) : string =
  match i with
  | Add dst s1 (Valerie.Riscv64.Semantics.OImm imm) ->
      concat "" ["    addi "; print_reg dst; ", "; print_reg s1; ", "; FStar.UInt64.to_string imm; "\n"]
  | Add dst s1 s2 ->
      concat "" ["    add "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Addw dst s1 (Valerie.Riscv64.Semantics.OImm imm) ->
      concat "" ["    addiw "; print_reg dst; ", "; print_reg s1; ", "; FStar.UInt64.to_string imm; "\n"]
  | Addw dst s1 s2 ->
      concat "" ["    addw "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Sub dst s1 s2 ->
      concat "" ["    sub "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Subw dst s1 s2 ->
      concat "" ["    subw "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | And dst s1 (Valerie.Riscv64.Semantics.OImm imm) ->
      concat "" ["    andi "; print_reg dst; ", "; print_reg s1; ", "; FStar.UInt64.to_string imm; "\n"]
  | And dst s1 s2 ->
      concat "" ["    and "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Or dst s1 (Valerie.Riscv64.Semantics.OImm imm) ->
      concat "" ["    ori "; print_reg dst; ", "; print_reg s1; ", "; FStar.UInt64.to_string imm; "\n"]
  | Or dst s1 s2 ->
      concat "" ["    or "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Xor dst s1 (Valerie.Riscv64.Semantics.OImm imm) ->
      concat "" ["    xori "; print_reg dst; ", "; print_reg s1; ", "; FStar.UInt64.to_string imm; "\n"]
  | Xor dst s1 s2 ->
      concat "" ["    xor "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Sll dst s1 (Valerie.Riscv64.Semantics.OImm imm) ->
      concat "" ["    slli "; print_reg dst; ", "; print_reg s1; ", "; FStar.UInt64.to_string imm; "\n"]
  | Sll dst s1 s2 ->
      concat "" ["    sll "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Sllw dst s1 (Valerie.Riscv64.Semantics.OImm imm) ->
      concat "" ["    slliw "; print_reg dst; ", "; print_reg s1; ", "; FStar.UInt64.to_string imm; "\n"]
  | Sllw dst s1 s2 ->
      concat "" ["    sllw "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Srl dst s1 (Valerie.Riscv64.Semantics.OImm imm) ->
      concat "" ["    srli "; print_reg dst; ", "; print_reg s1; ", "; FStar.UInt64.to_string imm; "\n"]
  | Srl dst s1 s2 ->
      concat "" ["    srl "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Srlw dst s1 (Valerie.Riscv64.Semantics.OImm imm) ->
      concat "" ["    srliw "; print_reg dst; ", "; print_reg s1; ", "; FStar.UInt64.to_string imm; "\n"]
  | Srlw dst s1 s2 ->
      concat "" ["    srlw "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Sra dst s1 (Valerie.Riscv64.Semantics.OImm imm) ->
      concat "" ["    srai "; print_reg dst; ", "; print_reg s1; ", "; FStar.UInt64.to_string imm; "\n"]
  | Sra dst s1 s2 ->
      concat "" ["    sra "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Sraw dst s1 (Valerie.Riscv64.Semantics.OImm imm) ->
      concat "" ["    sraiw "; print_reg dst; ", "; print_reg s1; ", "; FStar.UInt64.to_string imm; "\n"]
  | Sraw dst s1 s2 ->
      concat "" ["    sraw "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Slt dst s1 (Valerie.Riscv64.Semantics.OImm imm) ->
      concat "" ["    slti "; print_reg dst; ", "; print_reg s1; ", "; FStar.UInt64.to_string imm; "\n"]
  | Slt dst s1 s2 ->
      concat "" ["    slt "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Sltu dst s1 (Valerie.Riscv64.Semantics.OImm imm) ->
      concat "" ["    sltiu "; print_reg dst; ", "; print_reg s1; ", "; FStar.UInt64.to_string imm; "\n"]
  | Sltu dst s1 s2 ->
      concat "" ["    sltu "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Ld dst base off ->
      concat "" ["    ld "; print_reg dst; ", "; string_of_int off; "("; print_reg base; ")\n"]
  | Lw dst base off ->
      concat "" ["    lw "; print_reg dst; ", "; string_of_int off; "("; print_reg base; ")\n"]
  | Lwu dst base off ->
      concat "" ["    lwu "; print_reg dst; ", "; string_of_int off; "("; print_reg base; ")\n"]
  | Lh dst base off ->
      concat "" ["    lh "; print_reg dst; ", "; string_of_int off; "("; print_reg base; ")\n"]
  | Lhu dst base off ->
      concat "" ["    lhu "; print_reg dst; ", "; string_of_int off; "("; print_reg base; ")\n"]
  | Lb dst base off ->
      concat "" ["    lb "; print_reg dst; ", "; string_of_int off; "("; print_reg base; ")\n"]
  | Lbu dst base off ->
      concat "" ["    lbu "; print_reg dst; ", "; string_of_int off; "("; print_reg base; ")\n"]
  | Sd src base off ->
      concat "" ["    sd "; print_reg src; ", "; string_of_int off; "("; print_reg base; ")\n"]
  | Sw src base off ->
      concat "" ["    sw "; print_reg src; ", "; string_of_int off; "("; print_reg base; ")\n"]
  | Sh src base off ->
      concat "" ["    sh "; print_reg src; ", "; string_of_int off; "("; print_reg base; ")\n"]
  | Sb src base off ->
      concat "" ["    sb "; print_reg src; ", "; string_of_int off; "("; print_reg base; ")\n"]
  | Beq s1 s2 target ->
      concat "" ["    beq "; print_reg s1; ", "; print_reg s2; ", "; target; "\n"]
  | Bne s1 s2 target ->
      concat "" ["    bne "; print_reg s1; ", "; print_reg s2; ", "; target; "\n"]
  | Blt s1 s2 target ->
      concat "" ["    blt "; print_reg s1; ", "; print_reg s2; ", "; target; "\n"]
  | Bge s1 s2 target ->
      concat "" ["    bge "; print_reg s1; ", "; print_reg s2; ", "; target; "\n"]
  | Bltu s1 s2 target ->
      concat "" ["    bltu "; print_reg s1; ", "; print_reg s2; ", "; target; "\n"]
  | Bgeu s1 s2 target ->
      concat "" ["    bgeu "; print_reg s1; ", "; print_reg s2; ", "; target; "\n"]
  | Jal dst target ->
      concat "" ["    jal "; print_reg dst; ", "; target; "\n"]
  | Jalr dst base off ->
      concat "" ["    jalr "; print_reg dst; ", "; string_of_int off; "("; print_reg base; ")\n"]
  | Ret -> "    ret\n"
  | Call target -> concat "" ["    call "; target; "\n"]
  | Ecall -> "    ecall\n"
  | Lui dst imm ->
      concat "" ["    lui "; print_reg dst; ", "; FStar.UInt64.to_string imm; "\n"]
  | Auipc dst imm ->
      concat "" ["    auipc "; print_reg dst; ", "; FStar.UInt64.to_string imm; "\n"]
  | Li dst imm ->
      concat "" ["    li "; print_reg dst; ", "; FStar.UInt64.to_string imm; "\n"]
  | Mv dst src ->
      concat "" ["    mv "; print_reg dst; ", "; print_reg src; "\n"]
  | Mul dst s1 s2 ->
      concat "" ["    mul "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Mulw dst s1 s2 ->
      concat "" ["    mulw "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Mulh dst s1 s2 ->
      concat "" ["    mulh "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Mulhu dst s1 s2 ->
      concat "" ["    mulhu "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Mulhsu dst s1 s2 ->
      concat "" ["    mulhsu "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Div dst s1 s2 ->
      concat "" ["    div "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Divw dst s1 s2 ->
      concat "" ["    divw "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Divu dst s1 s2 ->
      concat "" ["    divu "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Divuw dst s1 s2 ->
      concat "" ["    divuw "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Rem dst s1 s2 ->
      concat "" ["    rem "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Remw dst s1 s2 ->
      concat "" ["    remw "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Remu dst s1 s2 ->
      concat "" ["    remu "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Remuw dst s1 s2 ->
      concat "" ["    remuw "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Andn dst s1 s2 ->
      concat "" ["    andn "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Orn dst s1 s2 ->
      concat "" ["    orn "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Xnor dst s1 s2 ->
      concat "" ["    xnor "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Clz dst src ->
      concat "" ["    clz "; print_reg dst; ", "; print_reg src; "\n"]
  | Ctz dst src ->
      concat "" ["    ctz "; print_reg dst; ", "; print_reg src; "\n"]
  | Cpop dst src ->
      concat "" ["    cpop "; print_reg dst; ", "; print_reg src; "\n"]
  | Max dst s1 s2 ->
      concat "" ["    max "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Maxu dst s1 s2 ->
      concat "" ["    maxu "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Min dst s1 s2 ->
      concat "" ["    min "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Minu dst s1 s2 ->
      concat "" ["    minu "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Rol dst s1 s2 ->
      concat "" ["    rol "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Ror dst s1 s2 ->
      concat "" ["    ror "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Rev8 dst src ->
      concat "" ["    rev8 "; print_reg dst; ", "; print_reg src; "\n"]
  | Aes64es dst s1 s2 ->
      concat "" ["    aes64es "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Aes64esm dst s1 s2 ->
      concat "" ["    aes64esm "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Aes64ds dst s1 s2 ->
      concat "" ["    aes64ds "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Aes64dsm dst s1 s2 ->
      concat "" ["    aes64dsm "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Aes64ks1i dst src rnum ->
      concat "" ["    aes64ks1i "; print_reg dst; ", "; print_reg src; ", "; FStar.UInt64.to_string rnum; "\n"]
  | Aes64ks2 dst s1 s2 ->
      concat "" ["    aes64ks2 "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Aes64im dst src ->
      concat "" ["    aes64im "; print_reg dst; ", "; print_reg src; "\n"]
  | Sha256sum0 dst src ->
      concat "" ["    sha256sum0 "; print_reg dst; ", "; print_reg src; "\n"]
  | Sha256sum1 dst src ->
      concat "" ["    sha256sum1 "; print_reg dst; ", "; print_reg src; "\n"]
  | Sha256sig0 dst src ->
      concat "" ["    sha256sig0 "; print_reg dst; ", "; print_reg src; "\n"]
  | Sha256sig1 dst src ->
      concat "" ["    sha256sig1 "; print_reg dst; ", "; print_reg src; "\n"]
  | Sha512sum0 dst src ->
      concat "" ["    sha512sum0 "; print_reg dst; ", "; print_reg src; "\n"]
  | Sha512sum1 dst src ->
      concat "" ["    sha512sum1 "; print_reg dst; ", "; print_reg src; "\n"]
  | Sha512sig0 dst src ->
      concat "" ["    sha512sig0 "; print_reg dst; ", "; print_reg src; "\n"]
  | Sha512sig1 dst src ->
      concat "" ["    sha512sig1 "; print_reg dst; ", "; print_reg src; "\n"]
  | Sm4ed dst s1 s2 bs ->
      concat "" ["    sm4ed "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; ", "; FStar.UInt64.to_string bs; "\n"]
  | Sm4ks dst s1 s2 bs ->
      concat "" ["    sm4ks "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; ", "; FStar.UInt64.to_string bs; "\n"]
  | Sm3p0 dst src ->
      concat "" ["    sm3p0 "; print_reg dst; ", "; print_reg src; "\n"]
  | Sm3p1 dst src ->
      concat "" ["    sm3p1 "; print_reg dst; ", "; print_reg src; "\n"]

let rec print_code (c: code) : Tot string (decreases c) =
  match c with
  | Ins i -> print_ins i
  | Block cs -> print_codes cs
  | IfElse _ ct cf -> concat "" [print_code ct; print_code cf]
  | While _ body -> print_code body

and print_codes (cs: list code) : Tot string (decreases cs) =
  match cs with
  | [] -> ""
  | c :: rest -> concat "" [print_code c; print_codes rest]

let emit_header (name: string) : string =
  concat "" [
    "    .globl "; name; "\n";
    "    .type "; name; ", @function\n";
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
    match Valerie.Compiler.Lower.Riscv64.lower_stmts p.proc_body with
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

let emit_module_asm (m: module_decl) (comments: bool) : string =
  let preamble = concat "" [
    "    .text\n";
    "    .attribute arch, \"rv64gc_zbb1p0_zk1p0\"\n";
    "    .p2align 2\n\n"
  ] in
  concat "" [
    "// Emitted by Vale verified assembly compiler (RISC-V 64-bit)\n";
    preamble;
    emit_procs_asm m.mod_procs;
    "    .section .note.GNU-stack,\"\",@progbits\n"
  ]
