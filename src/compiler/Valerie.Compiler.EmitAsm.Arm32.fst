module Valerie.Compiler.EmitAsm.Arm32

open FStar.String
open Valerie.Defs
open FStar.UInt32
open Valerie.Arm32.Machine
open Valerie.Arm32.Semantics
open Valerie.Compiler.AST
open Valerie.Compiler.Lower.Arm32

let print_reg (r: reg) : string =
  match r with
  | R0  -> "r0"
  | R1  -> "r1"
  | R2  -> "r2"
  | R3  -> "r3"
  | R4  -> "r4"
  | R5  -> "r5"
  | R6  -> "r6"
  | R7  -> "r7"
  | R8  -> "r8"
  | R9  -> "r9"
  | R10 -> "r10"
  | R11 -> "r11"
  | R12 -> "r12"
  | SP  -> "sp"
  | LR  -> "lr"
  | PC  -> "pc"

let print_sreg (s: sreg) : string =
  match s with
  | S0  -> "s0"  | S1  -> "s1"  | S2  -> "s2"  | S3  -> "s3"
  | S4  -> "s4"  | S5  -> "s5"  | S6  -> "s6"  | S7  -> "s7"
  | S8  -> "s8"  | S9  -> "s9"  | S10 -> "s10" | S11 -> "s11"
  | S12 -> "s12" | S13 -> "s13" | S14 -> "s14" | S15 -> "s15"
  | S16 -> "s16" | S17 -> "s17" | S18 -> "s18" | S19 -> "s19"
  | S20 -> "s20" | S21 -> "s21" | S22 -> "s22" | S23 -> "s23"
  | S24 -> "s24" | S25 -> "s25" | S26 -> "s26" | S27 -> "s27"
  | S28 -> "s28" | S29 -> "s29" | S30 -> "s30" | S31 -> "s31"

let print_dreg (d: dreg) : string =
  match d with
  | D0  -> "d0"  | D1  -> "d1"  | D2  -> "d2"  | D3  -> "d3"
  | D4  -> "d4"  | D5  -> "d5"  | D6  -> "d6"  | D7  -> "d7"
  | D8  -> "d8"  | D9  -> "d9"  | D10 -> "d10" | D11 -> "d11"
  | D12 -> "d12" | D13 -> "d13" | D14 -> "d14" | D15 -> "d15"
  | D16 -> "d16" | D17 -> "d17" | D18 -> "d18" | D19 -> "d19"
  | D20 -> "d20" | D21 -> "d21" | D22 -> "d22" | D23 -> "d23"
  | D24 -> "d24" | D25 -> "d25" | D26 -> "d26" | D27 -> "d27"
  | D28 -> "d28" | D29 -> "d29" | D30 -> "d30" | D31 -> "d31"

let print_qreg (q: qreg) : string =
  match q with
  | Q0  -> "q0"  | Q1  -> "q1"  | Q2  -> "q2"  | Q3  -> "q3"
  | Q4  -> "q4"  | Q5  -> "q5"  | Q6  -> "q6"  | Q7  -> "q7"
  | Q8  -> "q8"  | Q9  -> "q9"  | Q10 -> "q10" | Q11 -> "q11"
  | Q12 -> "q12" | Q13 -> "q13" | Q14 -> "q14" | Q15 -> "q15"

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
  | CondAL -> ""

let print_maddr (m: maddr) : string =
  match m with
  | MBase r ->
      concat "" ["["; print_reg r; "]"]
  | MBaseOffset r off ->
      if off = 0 then concat "" ["["; print_reg r; "]"]
      else concat "" ["["; print_reg r; ", #"; string_of_int off; "]"]
  | MPreIndexed r off ->
      concat "" ["["; print_reg r; ", #"; string_of_int off; "]!"]
  | MPostIndexed r off ->
      concat "" ["["; print_reg r; "], #"; string_of_int off]
  | MRegOffset base off ->
      concat "" ["["; print_reg base; ", "; print_reg off; "]"]

let rec print_reg_list (rs: list reg) : string =
  match rs with
  | [] -> ""
  | [r] -> print_reg r
  | r :: rest -> concat "" [print_reg r; ", "; print_reg_list rest]

let rec print_sreg_list (ss: list sreg) : string =
  match ss with
  | [] -> ""
  | [s] -> print_sreg s
  | s :: rest -> concat "" [print_sreg s; ", "; print_sreg_list rest]

let print_operand (op: Valerie.Arm32.Semantics.operand) : string =
  match op with
  | Valerie.Arm32.Semantics.OReg r -> print_reg r
  | Valerie.Arm32.Semantics.OSreg s -> print_sreg s
  | Valerie.Arm32.Semantics.ODreg d -> print_dreg d
  | Valerie.Arm32.Semantics.OQreg q -> print_qreg q
  | Valerie.Arm32.Semantics.OImm imm -> concat "" ["#"; FStar.UInt32.to_string imm]
  | Valerie.Arm32.Semantics.OMem m -> print_maddr m
  | Valerie.Arm32.Semantics.ORegList rs -> concat "" ["{"; print_reg_list rs; "}"]

let print_ins (i: ins) : string =
  match i with
  | Mov dst src ->
      concat "" ["    mov "; print_reg dst; ", "; print_operand src; "\n"]
  | Movs dst src ->
      concat "" ["    movs "; print_reg dst; ", "; print_operand src; "\n"]
  | Movw dst imm ->
      concat "" ["    movw "; print_reg dst; ", #"; FStar.UInt32.to_string imm; "\n"]
  | Movt dst imm ->
      concat "" ["    movt "; print_reg dst; ", #"; FStar.UInt32.to_string imm; "\n"]
  | Add dst s1 s2 ->
      concat "" ["    add "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Adds dst s1 s2 ->
      concat "" ["    adds "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Adc dst s1 s2 ->
      concat "" ["    adc "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Adcs dst s1 s2 ->
      concat "" ["    adcs "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Sub dst s1 s2 ->
      concat "" ["    sub "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Subs dst s1 s2 ->
      concat "" ["    subs "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Sbc dst s1 s2 ->
      concat "" ["    sbc "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Sbcs dst s1 s2 ->
      concat "" ["    sbcs "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Rsb dst s1 s2 ->
      concat "" ["    rsb "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Rsbs dst s1 s2 ->
      concat "" ["    rsbs "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Rsc dst s1 s2 ->
      concat "" ["    rsc "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Rscs dst s1 s2 ->
      concat "" ["    rscs "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Mul dst s1 s2 ->
      concat "" ["    mul "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; "\n"]
  | Mla dst s1 s2 ra ->
      concat "" ["    mla "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; ", "; print_reg ra; "\n"]
  | Mls dst s1 s2 ra ->
      concat "" ["    mls "; print_reg dst; ", "; print_reg s1; ", "; print_reg s2; ", "; print_reg ra; "\n"]
  | Smull rdlo rdhi rn rm ->
      concat "" ["    smull "; print_reg rdlo; ", "; print_reg rdhi; ", "; print_reg rn; ", "; print_reg rm; "\n"]
  | Umull rdlo rdhi rn rm ->
      concat "" ["    umull "; print_reg rdlo; ", "; print_reg rdhi; ", "; print_reg rn; ", "; print_reg rm; "\n"]
  | Sdiv dst rn rm ->
      concat "" ["    sdiv "; print_reg dst; ", "; print_reg rn; ", "; print_reg rm; "\n"]
  | Udiv dst rn rm ->
      concat "" ["    udiv "; print_reg dst; ", "; print_reg rn; ", "; print_reg rm; "\n"]
  | And dst s1 s2 ->
      concat "" ["    and "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Ands dst s1 s2 ->
      concat "" ["    ands "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Orr dst s1 s2 ->
      concat "" ["    orr "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Orrs dst s1 s2 ->
      concat "" ["    orrs "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Eor dst s1 s2 ->
      concat "" ["    eor "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Eors dst s1 s2 ->
      concat "" ["    eors "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Bic dst s1 s2 ->
      concat "" ["    bic "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Bics dst s1 s2 ->
      concat "" ["    bics "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Orn dst s1 s2 ->
      concat "" ["    orn "; print_reg dst; ", "; print_reg s1; ", "; print_operand s2; "\n"]
  | Mvn dst src ->
      concat "" ["    mvn "; print_reg dst; ", "; print_operand src; "\n"]
  | Mvns dst src ->
      concat "" ["    mvns "; print_reg dst; ", "; print_operand src; "\n"]
  | Lsl dst rn rm ->
      concat "" ["    lsl "; print_reg dst; ", "; print_reg rn; ", "; print_operand rm; "\n"]
  | Lsr dst rn rm ->
      concat "" ["    lsr "; print_reg dst; ", "; print_reg rn; ", "; print_operand rm; "\n"]
  | Asr dst rn rm ->
      concat "" ["    asr "; print_reg dst; ", "; print_reg rn; ", "; print_operand rm; "\n"]
  | Ror dst rn rm ->
      concat "" ["    ror "; print_reg dst; ", "; print_reg rn; ", "; print_operand rm; "\n"]
  | Clz dst src ->
      concat "" ["    clz "; print_reg dst; ", "; print_reg src; "\n"]
  | Rbit dst src ->
      concat "" ["    rbit "; print_reg dst; ", "; print_reg src; "\n"]
  | Rev dst src ->
      concat "" ["    rev "; print_reg dst; ", "; print_reg src; "\n"]
  | Rev16 dst src ->
      concat "" ["    rev16 "; print_reg dst; ", "; print_reg src; "\n"]
  | Revsh dst src ->
      concat "" ["    revsh "; print_reg dst; ", "; print_reg src; "\n"]
  | Ubfx dst rn lsb width ->
      concat "" ["    ubfx "; print_reg dst; ", "; print_reg rn; ", #"; FStar.UInt32.to_string lsb; ", #"; FStar.UInt32.to_string width; "\n"]
  | Sbfx dst rn lsb width ->
      concat "" ["    sbfx "; print_reg dst; ", "; print_reg rn; ", #"; FStar.UInt32.to_string lsb; ", #"; FStar.UInt32.to_string width; "\n"]
  | Cmp rn op ->
      concat "" ["    cmp "; print_reg rn; ", "; print_operand op; "\n"]
  | Cmn rn op ->
      concat "" ["    cmn "; print_reg rn; ", "; print_operand op; "\n"]
  | Tst rn op ->
      concat "" ["    tst "; print_reg rn; ", "; print_operand op; "\n"]
  | Teq rn op ->
      concat "" ["    teq "; print_reg rn; ", "; print_operand op; "\n"]
  | Sxtb dst src ->
      concat "" ["    sxtb "; print_reg dst; ", "; print_reg src; "\n"]
  | Sxth dst src ->
      concat "" ["    sxth "; print_reg dst; ", "; print_reg src; "\n"]
  | Uxtb dst src ->
      concat "" ["    uxtb "; print_reg dst; ", "; print_reg src; "\n"]
  | Uxth dst src ->
      concat "" ["    uxth "; print_reg dst; ", "; print_reg src; "\n"]
  | Ldr dst mem ->
      concat "" ["    ldr "; print_reg dst; ", "; print_maddr mem; "\n"]
  | Ldrb dst mem ->
      concat "" ["    ldrb "; print_reg dst; ", "; print_maddr mem; "\n"]
  | Ldrh dst mem ->
      concat "" ["    ldrh "; print_reg dst; ", "; print_maddr mem; "\n"]
  | Ldrsb dst mem ->
      concat "" ["    ldrsb "; print_reg dst; ", "; print_maddr mem; "\n"]
  | Ldrsh dst mem ->
      concat "" ["    ldrsh "; print_reg dst; ", "; print_maddr mem; "\n"]
  | Ldrd rt rt2 mem ->
      concat "" ["    ldrd "; print_reg rt; ", "; print_reg rt2; ", "; print_maddr mem; "\n"]
  | Str src mem ->
      concat "" ["    str "; print_reg src; ", "; print_maddr mem; "\n"]
  | Strb src mem ->
      concat "" ["    strb "; print_reg src; ", "; print_maddr mem; "\n"]
  | Strh src mem ->
      concat "" ["    strh "; print_reg src; ", "; print_maddr mem; "\n"]
  | Strd rt rt2 mem ->
      concat "" ["    strd "; print_reg rt; ", "; print_reg rt2; ", "; print_maddr mem; "\n"]
  | Push rs ->
      concat "" ["    push {"; print_reg_list rs; "}\n"]
  | Pop rs ->
      concat "" ["    pop {"; print_reg_list rs; "}\n"]
  | B target ->
      concat "" ["    b "; target; "\n"]
  | Bc cond target ->
      concat "" ["    b"; print_cond cond; " "; target; "\n"]
  | Bl target ->
      concat "" ["    bl "; target; "\n"]
  | Bx target ->
      concat "" ["    bx "; print_reg target; "\n"]
  | Blx target ->
      concat "" ["    blx "; print_reg target; "\n"]
  | Vmov dst src ->
      concat "" ["    vmov "; print_operand dst; ", "; print_operand src; "\n"]
  | VaddF32 dst s1 s2 ->
      concat "" ["    vadd.f32 "; print_sreg dst; ", "; print_sreg s1; ", "; print_sreg s2; "\n"]
  | VaddF64 dst s1 s2 ->
      concat "" ["    vadd.f64 "; print_dreg dst; ", "; print_dreg s1; ", "; print_dreg s2; "\n"]
  | VsubF32 dst s1 s2 ->
      concat "" ["    vsub.f32 "; print_sreg dst; ", "; print_sreg s1; ", "; print_sreg s2; "\n"]
  | VsubF64 dst s1 s2 ->
      concat "" ["    vsub.f64 "; print_dreg dst; ", "; print_dreg s1; ", "; print_dreg s2; "\n"]
  | VmulF32 dst s1 s2 ->
      concat "" ["    vmul.f32 "; print_sreg dst; ", "; print_sreg s1; ", "; print_sreg s2; "\n"]
  | VmulF64 dst s1 s2 ->
      concat "" ["    vmul.f64 "; print_dreg dst; ", "; print_dreg s1; ", "; print_dreg s2; "\n"]
  | Vldr dst mem ->
      concat "" ["    vldr "; print_sreg dst; ", "; print_maddr mem; "\n"]
  | Vstr src mem ->
      concat "" ["    vstr "; print_sreg src; ", "; print_maddr mem; "\n"]
  | Vpush sregs ->
      concat "" ["    vpush {"; print_sreg_list sregs; "}\n"]
  | Vpop sregs ->
      concat "" ["    vpop {"; print_sreg_list sregs; "}\n"]

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
    "    .global "; name; "\n";
    "    .type "; name; ", %function\n";
    name; ":\n"
  ]

let emit_footer (name: string) : string =
  concat "" [
    "    bx lr\n";
    "    .size "; name; ", .-"; name; "\n\n"
  ]

let emit_proc_asm (p: proc_decl) : string =
  if p.proc_is_ins then ""
  else
    match Valerie.Compiler.Lower.Arm32.lower_stmts p.proc_body with
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
    "    .syntax unified\n";
    "    .arch armv7-a\n";
    "    .arch_extension idiv\n";
    "    .fpu vfpv3-d16\n";
    "    .arm\n";
    "    .text\n\n"
  ] in
  concat "" [
    "// Emitted by Vale verified assembly compiler (ARMv7-A)\n";
    preamble;
    emit_procs_asm m.mod_procs;
    "    .section .note.GNU-stack,\"\",%progbits\n"
  ]
