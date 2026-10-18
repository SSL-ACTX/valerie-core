module Valerie.Compiler.Lower.Riscv64

open Valerie.Defs
open FStar.UInt64
module U64 = FStar.UInt64
open Valerie.Compiler.AST
open Valerie.Riscv64.Machine
module Sem = Valerie.Riscv64.Semantics

let reg_of_operand (r:reg_operand) : reg =
  match r with
  | RegNumbered n ->
      if n = 0ul then Zero
      else if n = 1ul then RA
      else if n = 2ul then SP
      else if n = 3ul then GP
      else if n = 4ul then TP
      else if n = 5ul then T0
      else if n = 6ul then T1
      else if n = 7ul then T2
      else if n = 8ul then S0
      else if n = 9ul then S1
      else if n = 10ul then A0
      else if n = 11ul then A1
      else if n = 12ul then A2
      else if n = 13ul then A3
      else if n = 14ul then A4
      else if n = 15ul then A5
      else if n = 16ul then A6
      else if n = 17ul then A7
      else if n = 18ul then S2
      else if n = 19ul then S3
      else if n = 20ul then S4
      else if n = 21ul then S5
      else if n = 22ul then S6
      else if n = 23ul then S7
      else if n = 24ul then S8
      else if n = 25ul then S9
      else if n = 26ul then S10
      else if n = 27ul then S11
      else if n = 28ul then T3
      else if n = 29ul then T4
      else if n = 30ul then T5
      else T6

let lower_operand (op:operand) : Sem.operand =
  match op with
  | OReg r -> Sem.OReg (reg_of_operand r)
  | OImm i -> Sem.OImm i
  | OMemBuffer _ _ -> Sem.OImm 0uL

let lower_ins (op:ins_opcode) (args:list operand) : option Sem.ins =
  match op, args with
  | OpMov, [dst; src] ->
      (match dst, src with
       | OReg rd, OReg rs -> Some (Sem.Mv (reg_of_operand rd) (reg_of_operand rs))
       | OReg rd, OImm imm -> Some (Sem.Li (reg_of_operand rd) imm)
       | _ -> None)
  | OpAdd, [dst; src1; src2] ->
      (match dst, src1 with
       | OReg rd, OReg rs1 -> Some (Sem.Add (reg_of_operand rd) (reg_of_operand rs1) (lower_operand src2))
       | _ -> None)
  | OpAdd, [dst; src] ->
      (match dst with
       | OReg rd -> Some (Sem.Add (reg_of_operand rd) (reg_of_operand rd) (lower_operand src))
       | _ -> None)
  | OpSub, [dst; src1; src2] ->
      (match dst, src1 with
       | OReg rd, OReg rs1 -> Some (Sem.Sub (reg_of_operand rd) (reg_of_operand rs1) (lower_operand src2))
       | _ -> None)
  | OpSub, [dst; src] ->
      (match dst with
       | OReg rd -> Some (Sem.Sub (reg_of_operand rd) (reg_of_operand rd) (lower_operand src))
       | _ -> None)
  | OpXor, [dst; src1; src2] ->
      (match dst, src1 with
       | OReg rd, OReg rs1 -> Some (Sem.Xor (reg_of_operand rd) (reg_of_operand rs1) (lower_operand src2))
       | _ -> None)
  | OpXor, [dst; src] ->
      (match dst with
       | OReg rd -> Some (Sem.Xor (reg_of_operand rd) (reg_of_operand rd) (lower_operand src))
       | _ -> None)
  | OpAnd, [dst; src1; src2] ->
      (match dst, src1 with
       | OReg rd, OReg rs1 -> Some (Sem.And (reg_of_operand rd) (reg_of_operand rs1) (lower_operand src2))
       | _ -> None)
  | OpAnd, [dst; src] ->
      (match dst with
       | OReg rd -> Some (Sem.And (reg_of_operand rd) (reg_of_operand rd) (lower_operand src))
       | _ -> None)
  | OpOr, [dst; src1; src2] ->
      (match dst, src1 with
       | OReg rd, OReg rs1 -> Some (Sem.Or (reg_of_operand rd) (reg_of_operand rs1) (lower_operand src2))
       | _ -> None)
  | OpOr, [dst; src] ->
      (match dst with
       | OReg rd -> Some (Sem.Or (reg_of_operand rd) (reg_of_operand rd) (lower_operand src))
       | _ -> None)
  | OpRev, [dst; src] ->
      (match dst, src with
       | OReg rd, OReg rs -> Some (Sem.Rev8 (reg_of_operand rd) (reg_of_operand rs))
       | _ -> None)
  | _ -> None

let reg_of_name (s: string) : option reg =
  if s = "zero" || s = "x0" then Some Zero
  else if s = "ra" || s = "x1" then Some RA
  else if s = "sp" || s = "x2" then Some SP
  else if s = "gp" || s = "x3" then Some GP
  else if s = "tp" || s = "x4" then Some TP
  else if s = "t0" || s = "x5" then Some T0
  else if s = "t1" || s = "x6" then Some T1
  else if s = "t2" || s = "x7" then Some T2
  else if s = "s0" || s = "fp" || s = "x8" then Some S0
  else if s = "s1" || s = "x9" then Some S1
  else if s = "a0" || s = "x10" then Some A0
  else if s = "a1" || s = "x11" then Some A1
  else if s = "a2" || s = "x12" then Some A2
  else if s = "a3" || s = "x13" then Some A3
  else if s = "a4" || s = "x14" then Some A4
  else if s = "a5" || s = "x15" then Some A5
  else if s = "a6" || s = "x16" then Some A6
  else if s = "a7" || s = "x17" then Some A7
  else if s = "s2" || s = "x18" then Some S2
  else if s = "s3" || s = "x19" then Some S3
  else if s = "s4" || s = "x20" then Some S4
  else if s = "s5" || s = "x21" then Some S5
  else if s = "s6" || s = "x22" then Some S6
  else if s = "s7" || s = "x23" then Some S7
  else if s = "s8" || s = "x24" then Some S8
  else if s = "s9" || s = "x25" then Some S9
  else if s = "s10" || s = "x26" then Some S10
  else if s = "s11" || s = "x27" then Some S11
  else if s = "t3" || s = "x28" then Some T3
  else if s = "t4" || s = "x29" then Some T4
  else if s = "t5" || s = "x30" then Some T5
  else if s = "t6" || s = "x31" then Some T6
  else None

let reg_of_expr (e: expr) : option reg =
  match e with
  | EVar s -> reg_of_name s
  | _ -> None

let operand_of_expr (e: expr) : option Sem.operand =
  match e with
  | EVar s ->
      (match reg_of_name s with
       | Some r -> Some (Sem.OReg r)
       | None -> None)
  | EConst n ->
      if n >= 0 && n <= 18446744073709551615 then
        Some (Sem.OImm (FStar.UInt64.uint_to_t n))
      else None
  | _ -> None

let lower_call (target: id) (args: list expr) : option Sem.ins =
  match target, args with
  | "Add", [dst; s1; s2]
  | "Addi", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rs1 ->
           (match operand_of_expr s2 with
            | Some op -> Some (Sem.Add rd rs1 op)
            | None -> None)
       | _ -> None)
  | "Sub", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rs1 ->
           (match operand_of_expr s2 with
            | Some op -> Some (Sem.Sub rd rs1 op)
            | None -> None)
       | _ -> None)
  | "Xor", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rs1 ->
           (match operand_of_expr s2 with
            | Some op -> Some (Sem.Xor rd rs1 op)
            | None -> None)
       | _ -> None)
  | "And", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rs1 ->
           (match operand_of_expr s2 with
            | Some op -> Some (Sem.And rd rs1 op)
            | None -> None)
       | _ -> None)
  | "Or", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rs1 ->
           (match operand_of_expr s2 with
            | Some op -> Some (Sem.Or rd rs1 op)
            | None -> None)
       | _ -> None)
  | "Rev8", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Rev8 rd rs)
       | _ -> None)
  | "Aes64esm", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rs1, Some rs2 -> Some (Sem.Aes64esm rd rs1 rs2)
       | _ -> None)
  | "Aes64dsm", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rs1, Some rs2 -> Some (Sem.Aes64dsm rd rs1 rs2)
       | _ -> None)
  | "Sha256sum0", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Sha256sum0 rd rs)
       | _ -> None)
  | "Sha256sum1", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Sha256sum1 rd rs)
       | _ -> None)

  | "Addw", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rs1 ->
           (match operand_of_expr s2 with
            | Some op -> Some (Sem.Addw rd rs1 op)
            | None -> None)
       | _ -> None)
  | "Subw", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rs1 ->
           (match operand_of_expr s2 with
            | Some op -> Some (Sem.Subw rd rs1 op)
            | None -> None)
       | _ -> None)
  | "Sll", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rs1 ->
           (match operand_of_expr s2 with
            | Some op -> Some (Sem.Sll rd rs1 op)
            | None -> None)
       | _ -> None)
  | "Sllw", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rs1 ->
           (match operand_of_expr s2 with
            | Some op -> Some (Sem.Sllw rd rs1 op)
            | None -> None)
       | _ -> None)
  | "Srl", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rs1 ->
           (match operand_of_expr s2 with
            | Some op -> Some (Sem.Srl rd rs1 op)
            | None -> None)
       | _ -> None)
  | "Srlw", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rs1 ->
           (match operand_of_expr s2 with
            | Some op -> Some (Sem.Srlw rd rs1 op)
            | None -> None)
       | _ -> None)
  | "Sra", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rs1 ->
           (match operand_of_expr s2 with
            | Some op -> Some (Sem.Sra rd rs1 op)
            | None -> None)
       | _ -> None)
  | "Sraw", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rs1 ->
           (match operand_of_expr s2 with
            | Some op -> Some (Sem.Sraw rd rs1 op)
            | None -> None)
       | _ -> None)
  | "Slt", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rs1 ->
           (match operand_of_expr s2 with
            | Some op -> Some (Sem.Slt rd rs1 op)
            | None -> None)
       | _ -> None)
  | "Sltu", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rs1 ->
           (match operand_of_expr s2 with
            | Some op -> Some (Sem.Sltu rd rs1 op)
            | None -> None)
       | _ -> None)
  | "Mul", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rs1, Some rs2 -> Some (Sem.Mul rd rs1 rs2)
       | _ -> None)
  | "Mulw", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rs1, Some rs2 -> Some (Sem.Mulw rd rs1 rs2)
       | _ -> None)
  | "Mulh", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rs1, Some rs2 -> Some (Sem.Mulh rd rs1 rs2)
       | _ -> None)
  | "Mulhu", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rs1, Some rs2 -> Some (Sem.Mulhu rd rs1 rs2)
       | _ -> None)
  | "Mulhsu", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rs1, Some rs2 -> Some (Sem.Mulhsu rd rs1 rs2)
       | _ -> None)
  | "Div", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rs1, Some rs2 -> Some (Sem.Div rd rs1 rs2)
       | _ -> None)
  | "Divw", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rs1, Some rs2 -> Some (Sem.Divw rd rs1 rs2)
       | _ -> None)
  | "Divu", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rs1, Some rs2 -> Some (Sem.Divu rd rs1 rs2)
       | _ -> None)
  | "Divuw", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rs1, Some rs2 -> Some (Sem.Divuw rd rs1 rs2)
       | _ -> None)
  | "Rem", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rs1, Some rs2 -> Some (Sem.Rem rd rs1 rs2)
       | _ -> None)
  | "Remw", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rs1, Some rs2 -> Some (Sem.Remw rd rs1 rs2)
       | _ -> None)
  | "Remu", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rs1, Some rs2 -> Some (Sem.Remu rd rs1 rs2)
       | _ -> None)
  | "Remuw", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rs1, Some rs2 -> Some (Sem.Remuw rd rs1 rs2)
       | _ -> None)
  | "Andn", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rs1, Some rs2 -> Some (Sem.Andn rd rs1 rs2)
       | _ -> None)
  | "Orn", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rs1, Some rs2 -> Some (Sem.Orn rd rs1 rs2)
       | _ -> None)
  | "Xnor", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rs1, Some rs2 -> Some (Sem.Xnor rd rs1 rs2)
       | _ -> None)
  | "Clz", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Clz rd rs)
       | _ -> None)
  | "Ctz", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Ctz rd rs)
       | _ -> None)
  | "Cpop", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Cpop rd rs)
       | _ -> None)
  | "Max", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rs1, Some rs2 -> Some (Sem.Max rd rs1 rs2)
       | _ -> None)
  | "Maxu", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rs1, Some rs2 -> Some (Sem.Maxu rd rs1 rs2)
       | _ -> None)
  | "Min", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rs1, Some rs2 -> Some (Sem.Min rd rs1 rs2)
       | _ -> None)
  | "Minu", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rs1, Some rs2 -> Some (Sem.Minu rd rs1 rs2)
       | _ -> None)
  | "Rol", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rs1, Some rs2 -> Some (Sem.Rol rd rs1 rs2)
       | _ -> None)
  | "Ror", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rs1 ->
           (match operand_of_expr s2 with
            | Some op -> Some (Sem.Ror rd rs1 op)
            | None -> None)
       | _ -> None)
  | "Ld", [dst; base; EConst offset] ->
      (match reg_of_expr dst, reg_of_expr base with
       | Some rd, Some rb -> Some (Sem.Ld rd rb offset)
       | _ -> None)
  | "Ld", [dst; base] ->
      (match reg_of_expr dst, reg_of_expr base with
       | Some rd, Some rb -> Some (Sem.Ld rd rb 0)
       | _ -> None)
  | "Lw", [dst; base; EConst offset] ->
      (match reg_of_expr dst, reg_of_expr base with
       | Some rd, Some rb -> Some (Sem.Lw rd rb offset)
       | _ -> None)
  | "Lw", [dst; base] ->
      (match reg_of_expr dst, reg_of_expr base with
       | Some rd, Some rb -> Some (Sem.Lw rd rb 0)
       | _ -> None)
  | "Lwu", [dst; base; EConst offset] ->
      (match reg_of_expr dst, reg_of_expr base with
       | Some rd, Some rb -> Some (Sem.Lwu rd rb offset)
       | _ -> None)
  | "Lwu", [dst; base] ->
      (match reg_of_expr dst, reg_of_expr base with
       | Some rd, Some rb -> Some (Sem.Lwu rd rb 0)
       | _ -> None)
  | "Lh", [dst; base; EConst offset] ->
      (match reg_of_expr dst, reg_of_expr base with
       | Some rd, Some rb -> Some (Sem.Lh rd rb offset)
       | _ -> None)
  | "Lh", [dst; base] ->
      (match reg_of_expr dst, reg_of_expr base with
       | Some rd, Some rb -> Some (Sem.Lh rd rb 0)
       | _ -> None)
  | "Lhu", [dst; base; EConst offset] ->
      (match reg_of_expr dst, reg_of_expr base with
       | Some rd, Some rb -> Some (Sem.Lhu rd rb offset)
       | _ -> None)
  | "Lhu", [dst; base] ->
      (match reg_of_expr dst, reg_of_expr base with
       | Some rd, Some rb -> Some (Sem.Lhu rd rb 0)
       | _ -> None)
  | "Lb", [dst; base; EConst offset] ->
      (match reg_of_expr dst, reg_of_expr base with
       | Some rd, Some rb -> Some (Sem.Lb rd rb offset)
       | _ -> None)
  | "Lb", [dst; base] ->
      (match reg_of_expr dst, reg_of_expr base with
       | Some rd, Some rb -> Some (Sem.Lb rd rb 0)
       | _ -> None)
  | "Lbu", [dst; base; EConst offset] ->
      (match reg_of_expr dst, reg_of_expr base with
       | Some rd, Some rb -> Some (Sem.Lbu rd rb offset)
       | _ -> None)
  | "Lbu", [dst; base] ->
      (match reg_of_expr dst, reg_of_expr base with
       | Some rd, Some rb -> Some (Sem.Lbu rd rb 0)
       | _ -> None)
  | "Sd", [src; base; EConst offset]
  | "Sd", [base; EConst offset; src] ->
      (match reg_of_expr src, reg_of_expr base with
       | Some rs, Some rb -> Some (Sem.Sd rs rb offset)
       | _ -> None)
  | "Sd", [src; base] ->
      (match reg_of_expr src, reg_of_expr base with
       | Some rs, Some rb -> Some (Sem.Sd rs rb 0)
       | _ -> None)
  | "Sw", [src; base; EConst offset]
  | "Sw", [base; EConst offset; src] ->
      (match reg_of_expr src, reg_of_expr base with
       | Some rs, Some rb -> Some (Sem.Sw rs rb offset)
       | _ -> None)
  | "Sw", [src; base] ->
      (match reg_of_expr src, reg_of_expr base with
       | Some rs, Some rb -> Some (Sem.Sw rs rb 0)
       | _ -> None)
  | "Sh", [src; base; EConst offset]
  | "Sh", [base; EConst offset; src] ->
      (match reg_of_expr src, reg_of_expr base with
       | Some rs, Some rb -> Some (Sem.Sh rs rb offset)
       | _ -> None)
  | "Sh", [src; base] ->
      (match reg_of_expr src, reg_of_expr base with
       | Some rs, Some rb -> Some (Sem.Sh rs rb 0)
       | _ -> None)
  | "Sb", [src; base; EConst offset]
  | "Sb", [base; EConst offset; src] ->
      (match reg_of_expr src, reg_of_expr base with
       | Some rs, Some rb -> Some (Sem.Sb rs rb offset)
       | _ -> None)
  | "Sb", [src; base] ->
      (match reg_of_expr src, reg_of_expr base with
       | Some rs, Some rb -> Some (Sem.Sb rs rb 0)
       | _ -> None)
  | "Beq", [s1; s2; EVar target] ->
      (match reg_of_expr s1, reg_of_expr s2 with
       | Some rs1, Some rs2 -> Some (Sem.Beq rs1 rs2 target)
       | _ -> None)
  | "Bne", [s1; s2; EVar target] ->
      (match reg_of_expr s1, reg_of_expr s2 with
       | Some rs1, Some rs2 -> Some (Sem.Bne rs1 rs2 target)
       | _ -> None)
  | "Blt", [s1; s2; EVar target] ->
      (match reg_of_expr s1, reg_of_expr s2 with
       | Some rs1, Some rs2 -> Some (Sem.Blt rs1 rs2 target)
       | _ -> None)
  | "Bge", [s1; s2; EVar target] ->
      (match reg_of_expr s1, reg_of_expr s2 with
       | Some rs1, Some rs2 -> Some (Sem.Bge rs1 rs2 target)
       | _ -> None)
  | "Bltu", [s1; s2; EVar target] ->
      (match reg_of_expr s1, reg_of_expr s2 with
       | Some rs1, Some rs2 -> Some (Sem.Bltu rs1 rs2 target)
       | _ -> None)
  | "Bgeu", [s1; s2; EVar target] ->
      (match reg_of_expr s1, reg_of_expr s2 with
       | Some rs1, Some rs2 -> Some (Sem.Bgeu rs1 rs2 target)
       | _ -> None)
  | "Jal", [dst; EVar target] ->
      (match reg_of_expr dst with
       | Some rd -> Some (Sem.Jal rd target)
       | _ -> None)
  | "Jalr", [dst; base; EConst offset] ->
      (match reg_of_expr dst, reg_of_expr base with
       | Some rd, Some rb -> Some (Sem.Jalr rd rb offset)
       | _ -> None)
  | "Ret", [] -> Some Sem.Ret
  | "Ecall", [] -> Some Sem.Ecall
  | "Lui", [dst; EConst imm] ->
      (match reg_of_expr dst with
       | Some rd ->
           if imm >= 0 && imm <= 18446744073709551615 then Some (Sem.Lui rd (FStar.UInt64.uint_to_t imm))
           else None
       | _ -> None)
  | "Auipc", [dst; EConst imm] ->
      (match reg_of_expr dst with
       | Some rd ->
           if imm >= 0 && imm <= 18446744073709551615 then Some (Sem.Auipc rd (FStar.UInt64.uint_to_t imm))
           else None
       | _ -> None)
  | "Aes64ks1i", [dst; src; EConst rnum] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs ->
           if rnum >= 0 && rnum <= 18446744073709551615 then Some (Sem.Aes64ks1i rd rs (FStar.UInt64.uint_to_t rnum))
           else None
       | _ -> None)
  | "Aes64ks2", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rs1, Some rs2 -> Some (Sem.Aes64ks2 rd rs1 rs2)
       | _ -> None)
  | "Aes64im", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Aes64im rd rs)
       | _ -> None)
  | "Sha256sig0", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Sha256sig0 rd rs)
       | _ -> None)
  | "Sha256sig1", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Sha256sig1 rd rs)
       | _ -> None)
  | "Sha512sum0", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Sha512sum0 rd rs)
       | _ -> None)
  | "Sha512sum1", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Sha512sum1 rd rs)
       | _ -> None)
  | "Sha512sig0", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Sha512sig0 rd rs)
       | _ -> None)
  | "Sha512sig1", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Sha512sig1 rd rs)
       | _ -> None)
  | "Sm4ed", [dst; s1; s2; EConst bs] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rs1, Some rs2 ->
           if bs >= 0 && bs <= 18446744073709551615 then Some (Sem.Sm4ed rd rs1 rs2 (FStar.UInt64.uint_to_t bs))
           else None
       | _ -> None)
  | "Sm4ks", [dst; s1; s2; EConst bs] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rs1, Some rs2 ->
           if bs >= 0 && bs <= 18446744073709551615 then Some (Sem.Sm4ks rd rs1 rs2 (FStar.UInt64.uint_to_t bs))
           else None
       | _ -> None)
  | "Sm3p0", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Sm3p0 rd rs)
       | _ -> None)
  | "Sm3p1", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Sm3p1 rd rs)
       | _ -> None)
  | _ -> None

let rec lower_stmt (s:stmt) : Tot (option Sem.code) (decreases s) =
  match s with
  | SIns op args ->
      (match lower_ins op args with
       | Some i -> Some (Sem.Ins i)
       | None -> None)
  | SBlock stmts -> lower_stmts stmts
  | SCall target args ->
      (match lower_call target args with
       | Some i -> Some (Sem.Ins i)
       | None -> Some (Sem.Ins (Sem.Call target)))
  | SAssert _ | SAssume _ -> Some (Sem.Block [])
  | _ -> None

and lower_stmts (stmts:list stmt) : Tot (option Sem.code) (decreases stmts) =
  match stmts with
  | [] -> Some (Sem.Block [])
  | [s] -> lower_stmt s
  | s :: rest ->
      (match lower_stmt s, lower_stmts rest with
       | Some c1, Some (Sem.Block cs) -> Some (Sem.Block (c1 :: cs))
       | Some c1, Some c2 -> Some (Sem.Block [c1; c2])
       | _ -> None)
