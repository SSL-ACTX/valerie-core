module Valerie.Compiler.Lower.Arm32

open Valerie.Defs
open FStar.UInt32
module U32 = FStar.UInt32
open Valerie.Compiler.AST
open Valerie.Arm32.Machine
module Sem = Valerie.Arm32.Semantics

let reg_of_operand (r:reg_operand) : reg =
  match r with
  | RegNumbered n ->
      if n = 0ul then R0
      else if n = 1ul then R1
      else if n = 2ul then R2
      else if n = 3ul then R3
      else if n = 4ul then R4
      else if n = 5ul then R5
      else if n = 6ul then R6
      else if n = 7ul then R7
      else if n = 8ul then R8
      else if n = 9ul then R9
      else if n = 10ul then R10
      else if n = 11ul then R11
      else if n = 12ul then R12
      else if n = 13ul then SP
      else if n = 14ul then LR
      else PC

let lower_operand (op:operand) : Sem.operand =
  match op with
  | OReg r -> Sem.OReg (reg_of_operand r)
  | OImm i -> Sem.OImm (Int.Cast.uint64_to_uint32 i)
  | OMemBuffer _ _ -> Sem.OImm 0ul

let lower_ins (op:ins_opcode) (args:list operand) : option Sem.ins =
  match op, args with
  | OpMov, [dst; src] ->
      (match dst with
       | OReg r -> Some (Sem.Mov (reg_of_operand r) (lower_operand src))
       | _ -> None)
  | OpAdd, [dst; src1; src2] ->
      (match dst, src1 with
       | OReg rd, OReg rn -> Some (Sem.Add (reg_of_operand rd) (reg_of_operand rn) (lower_operand src2))
       | _ -> None)
  | OpAdd, [dst; src] ->
      (match dst with
       | OReg rd -> Some (Sem.Add (reg_of_operand rd) (reg_of_operand rd) (lower_operand src))
       | _ -> None)
  | OpSub, [dst; src1; src2] ->
      (match dst, src1 with
       | OReg rd, OReg rn -> Some (Sem.Sub (reg_of_operand rd) (reg_of_operand rn) (lower_operand src2))
       | _ -> None)
  | OpSub, [dst; src] ->
      (match dst with
       | OReg rd -> Some (Sem.Sub (reg_of_operand rd) (reg_of_operand rd) (lower_operand src))
       | _ -> None)
  | OpXor, [dst; src1; src2] ->
      (match dst, src1 with
       | OReg rd, OReg rn -> Some (Sem.Eor (reg_of_operand rd) (reg_of_operand rn) (lower_operand src2))
       | _ -> None)
  | OpXor, [dst; src] ->
      (match dst with
       | OReg rd -> Some (Sem.Eor (reg_of_operand rd) (reg_of_operand rd) (lower_operand src))
       | _ -> None)
  | OpAnd, [dst; src1; src2] ->
      (match dst, src1 with
       | OReg rd, OReg rn -> Some (Sem.And (reg_of_operand rd) (reg_of_operand rn) (lower_operand src2))
       | _ -> None)
  | OpAnd, [dst; src] ->
      (match dst with
       | OReg rd -> Some (Sem.And (reg_of_operand rd) (reg_of_operand rd) (lower_operand src))
       | _ -> None)
  | OpOr, [dst; src1; src2] ->
      (match dst, src1 with
       | OReg rd, OReg rn -> Some (Sem.Orr (reg_of_operand rd) (reg_of_operand rn) (lower_operand src2))
       | _ -> None)
  | OpOr, [dst; src] ->
      (match dst with
       | OReg rd -> Some (Sem.Orr (reg_of_operand rd) (reg_of_operand rd) (lower_operand src))
       | _ -> None)
  | OpRev, [dst; src] ->
      (match dst, src with
       | OReg rd, OReg rn -> Some (Sem.Rev (reg_of_operand rd) (reg_of_operand rn))
       | _ -> None)
  | _ -> None

let reg_of_name (s: string) : option reg =
  if s = "r0" || s = "a1" || s = "state" then Some R0
  else if s = "r1" || s = "a2" then Some R1
  else if s = "r2" || s = "a3" then Some R2
  else if s = "r3" || s = "a4" then Some R3
  else if s = "r4" || s = "v1" then Some R4
  else if s = "r5" || s = "v2" then Some R5
  else if s = "r6" || s = "v3" then Some R6
  else if s = "r7" || s = "v4" then Some R7
  else if s = "r8" || s = "v5" then Some R8
  else if s = "r9" || s = "v6" || s = "sb" then Some R9
  else if s = "r10" || s = "v7" || s = "sl" then Some R10
  else if s = "r11" || s = "v8" || s = "fp" then Some R11
  else if s = "r12" || s = "ip" then Some R12
  else if s = "r13" || s = "sp" then Some SP
  else if s = "r14" || s = "lr" then Some LR
  else if s = "r15" || s = "pc" then Some PC
  else None

let reg_of_expr (e: expr) : option reg =
  match e with
  | EVar s -> reg_of_name s
  | _ -> None


let cond_of_name (s: string) : option cond_code =
  if s = "eq" || s = "EQ" then Some CondEQ
  else if s = "ne" || s = "NE" then Some CondNE
  else if s = "cs" || s = "CS" || s = "hs" || s = "HS" then Some CondCS
  else if s = "cc" || s = "CC" || s = "lo" || s = "LO" then Some CondCC
  else if s = "mi" || s = "MI" then Some CondMI
  else if s = "pl" || s = "PL" then Some CondPL
  else if s = "vs" || s = "VS" then Some CondVS
  else if s = "vc" || s = "VC" then Some CondVC
  else if s = "hi" || s = "HI" then Some CondHI
  else if s = "ls" || s = "LS" then Some CondLS
  else if s = "ge" || s = "GE" then Some CondGE
  else if s = "lt" || s = "LT" then Some CondLT
  else if s = "gt" || s = "GT" then Some CondGT
  else if s = "le" || s = "LE" then Some CondLE
  else if s = "al" || s = "AL" then Some CondAL
  else None

let cond_of_expr (e: expr) : option cond_code =
  match e with
  | EVar s -> cond_of_name s
  | _ -> None

let sreg_of_name (s: string) : option sreg =
  if s = "s0" then Some S0 else if s = "s1" then Some S1
  else if s = "s2" then Some S2 else if s = "s3" then Some S3
  else if s = "s4" then Some S4 else if s = "s5" then Some S5
  else if s = "s6" then Some S6 else if s = "s7" then Some S7
  else if s = "s8" then Some S8 else if s = "s9" then Some S9
  else if s = "s10" then Some S10 else if s = "s11" then Some S11
  else if s = "s12" then Some S12 else if s = "s13" then Some S13
  else if s = "s14" then Some S14 else if s = "s15" then Some S15
  else if s = "s16" then Some S16 else if s = "s17" then Some S17
  else if s = "s18" then Some S18 else if s = "s19" then Some S19
  else if s = "s20" then Some S20 else if s = "s21" then Some S21
  else if s = "s22" then Some S22 else if s = "s23" then Some S23
  else if s = "s24" then Some S24 else if s = "s25" then Some S25
  else if s = "s26" then Some S26 else if s = "s27" then Some S27
  else if s = "s28" then Some S28 else if s = "s29" then Some S29
  else if s = "s30" then Some S30 else if s = "s31" then Some S31
  else None

let sreg_of_expr (e: expr) : option sreg =
  match e with
  | EVar s -> sreg_of_name s
  | _ -> None

let dreg_of_name (s: string) : option dreg =
  if s = "d0" then Some D0 else if s = "d1" then Some D1
  else if s = "d2" then Some D2 else if s = "d3" then Some D3
  else if s = "d4" then Some D4 else if s = "d5" then Some D5
  else if s = "d6" then Some D6 else if s = "d7" then Some D7
  else if s = "d8" then Some D8 else if s = "d9" then Some D9
  else if s = "d10" then Some D10 else if s = "d11" then Some D11
  else if s = "d12" then Some D12 else if s = "d13" then Some D13
  else if s = "d14" then Some D14 else if s = "d15" then Some D15
  else if s = "d16" then Some D16 else if s = "d17" then Some D17
  else if s = "d18" then Some D18 else if s = "d19" then Some D19
  else if s = "d20" then Some D20 else if s = "d21" then Some D21
  else if s = "d22" then Some D22 else if s = "d23" then Some D23
  else if s = "d24" then Some D24 else if s = "d25" then Some D25
  else if s = "d26" then Some D26 else if s = "d27" then Some D27
  else if s = "d28" then Some D28 else if s = "d29" then Some D29
  else if s = "d30" then Some D30 else if s = "d31" then Some D31
  else None

let dreg_of_expr (e: expr) : option dreg =
  match e with
  | EVar s -> dreg_of_name s
  | _ -> None

let operand_of_expr (e: expr) : option Sem.operand =
  match e with
  | EVar s ->
      (match reg_of_name s with
       | Some r -> Some (Sem.OReg r)
       | None -> None)
  | EConst n ->
      if n >= 0 && n <= 4294967295 then
        Some (Sem.OImm (FStar.UInt32.uint_to_t n))
      else None
  | _ -> None

let lower_call (target: id) (args: list expr) : option Sem.ins =
  match target, args with
  | "Add", [dst; s1; s2]
  | "Add32", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 with
            | Some op -> Some (Sem.Add rd rn op)
            | None -> None)
       | _ -> None)
  | "Sub", [dst; s1; s2]
  | "Sub32", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 with
            | Some op -> Some (Sem.Sub rd rn op)
            | None -> None)
       | _ -> None)
  | "Xor", [dst; s1; s2]
  | "Xor32", [dst; s1; s2]
  | "Eor", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 with
            | Some op -> Some (Sem.Eor rd rn op)
            | None -> None)
       | _ -> None)
  | "And", [dst; s1; s2]
  | "And32", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 with
            | Some op -> Some (Sem.And rd rn op)
            | None -> None)
       | _ -> None)
  | "Or", [dst; s1; s2]
  | "Orr", [dst; s1; s2]
  | "Or32", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 with
            | Some op -> Some (Sem.Orr rd rn op)
            | None -> None)
       | _ -> None)
  | "Mov", [dst; src]
  | "Mov32", [dst; src]
  | "MovImm", [dst; src] ->
      (match reg_of_expr dst with
       | Some rd ->
           (match operand_of_expr src with
            | Some op -> Some (Sem.Mov rd op)
            | None -> None)
       | _ -> None)
  | "Rev", [dst; src]
  | "Rev32", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Rev rd rs)
       | _ -> None)
  | "Ror", [dst; s1; s2]
  | "Ror32", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 with
            | Some op -> Some (Sem.Ror rd rn op)
            | None -> None)
       | _ -> None)
  | "Rol32", [dst; s1; EConst amt] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           let ror_amt = (32 - (amt % 32)) % 32 in
           if ror_amt >= 0 && ror_amt <= 31 then
             Some (Sem.Ror rd rn (Sem.OImm (FStar.UInt32.uint_to_t ror_amt)))
           else None
       | _ -> None)

  | "Movs", [dst; src] ->
      (match reg_of_expr dst, operand_of_expr src with
       | Some rd, Some op -> Some (Sem.Movs rd op)
       | _ -> None)
  | "Movw", [dst; EConst imm] ->
      (match reg_of_expr dst with
       | Some rd ->
           if imm >= 0 && imm <= 65535 then
             Some (Sem.Movw rd (FStar.UInt32.uint_to_t imm))
           else None
       | _ -> None)
  | "Movt", [dst; EConst imm] ->
      (match reg_of_expr dst with
       | Some rd ->
           if imm >= 0 && imm <= 65535 then
             Some (Sem.Movt rd (FStar.UInt32.uint_to_t imm))
           else None
       | _ -> None)
  | "Adds", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, operand_of_expr s2 with
       | Some rd, Some rn, Some op -> Some (Sem.Adds rd rn op)
       | _ -> None)
  | "Adc", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, operand_of_expr s2 with
       | Some rd, Some rn, Some op -> Some (Sem.Adc rd rn op)
       | _ -> None)
  | "Adcs", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, operand_of_expr s2 with
       | Some rd, Some rn, Some op -> Some (Sem.Adcs rd rn op)
       | _ -> None)
  | "Subs", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, operand_of_expr s2 with
       | Some rd, Some rn, Some op -> Some (Sem.Subs rd rn op)
       | _ -> None)
  | "Sbc", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, operand_of_expr s2 with
       | Some rd, Some rn, Some op -> Some (Sem.Sbc rd rn op)
       | _ -> None)
  | "Sbcs", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, operand_of_expr s2 with
       | Some rd, Some rn, Some op -> Some (Sem.Sbcs rd rn op)
       | _ -> None)
  | "Rsb", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, operand_of_expr s2 with
       | Some rd, Some rn, Some op -> Some (Sem.Rsb rd rn op)
       | _ -> None)
  | "Rsbs", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, operand_of_expr s2 with
       | Some rd, Some rn, Some op -> Some (Sem.Rsbs rd rn op)
       | _ -> None)
  | "Rsc", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, operand_of_expr s2 with
       | Some rd, Some rn, Some op -> Some (Sem.Rsc rd rn op)
       | _ -> None)
  | "Rscs", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, operand_of_expr s2 with
       | Some rd, Some rn, Some op -> Some (Sem.Rscs rd rn op)
       | _ -> None)
  | "Mul", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rn, Some rm -> Some (Sem.Mul rd rn rm)
       | _ -> None)
  | "Mla", [dst; s1; s2; ra] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2, reg_of_expr ra with
       | Some rd, Some rn, Some rm, Some r_a -> Some (Sem.Mla rd rn rm r_a)
       | _ -> None)
  | "Mls", [dst; s1; s2; ra] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2, reg_of_expr ra with
       | Some rd, Some rn, Some rm, Some r_a -> Some (Sem.Mls rd rn rm r_a)
       | _ -> None)
  | "Smull", [rdlo; rdhi; rn; rm] ->
      (match reg_of_expr rdlo, reg_of_expr rdhi, reg_of_expr rn, reg_of_expr rm with
       | Some rlo, Some rhi, Some r_n, Some r_m -> Some (Sem.Smull rlo rhi r_n r_m)
       | _ -> None)
  | "Umull", [rdlo; rdhi; rn; rm] ->
      (match reg_of_expr rdlo, reg_of_expr rdhi, reg_of_expr rn, reg_of_expr rm with
       | Some rlo, Some rhi, Some r_n, Some r_m -> Some (Sem.Umull rlo rhi r_n r_m)
       | _ -> None)
  | "Sdiv", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rn, Some rm -> Some (Sem.Sdiv rd rn rm)
       | _ -> None)
  | "Udiv", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rn, Some rm -> Some (Sem.Udiv rd rn rm)
       | _ -> None)
  | "Ands", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, operand_of_expr s2 with
       | Some rd, Some rn, Some op -> Some (Sem.Ands rd rn op)
       | _ -> None)
  | "Orrs", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, operand_of_expr s2 with
       | Some rd, Some rn, Some op -> Some (Sem.Orrs rd rn op)
       | _ -> None)
  | "Eors", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, operand_of_expr s2 with
       | Some rd, Some rn, Some op -> Some (Sem.Eors rd rn op)
       | _ -> None)
  | "Bic", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, operand_of_expr s2 with
       | Some rd, Some rn, Some op -> Some (Sem.Bic rd rn op)
       | _ -> None)
  | "Bics", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, operand_of_expr s2 with
       | Some rd, Some rn, Some op -> Some (Sem.Bics rd rn op)
       | _ -> None)
  | "Orn", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, operand_of_expr s2 with
       | Some rd, Some rn, Some op -> Some (Sem.Orn rd rn op)
       | _ -> None)
  | "Mvn", [dst; src] ->
      (match reg_of_expr dst, operand_of_expr src with
       | Some rd, Some op -> Some (Sem.Mvn rd op)
       | _ -> None)
  | "Mvns", [dst; src] ->
      (match reg_of_expr dst, operand_of_expr src with
       | Some rd, Some op -> Some (Sem.Mvns rd op)
       | _ -> None)
  | "Lsl", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, operand_of_expr s2 with
       | Some rd, Some rn, Some op -> Some (Sem.Lsl rd rn op)
       | _ -> None)
  | "Lsr", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, operand_of_expr s2 with
       | Some rd, Some rn, Some op -> Some (Sem.Lsr rd rn op)
       | _ -> None)
  | "Asr", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, operand_of_expr s2 with
       | Some rd, Some rn, Some op -> Some (Sem.Asr rd rn op)
       | _ -> None)
  | "Clz", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Clz rd rs)
       | _ -> None)
  | "Rbit", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Rbit rd rs)
       | _ -> None)
  | "Rev16", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Rev16 rd rs)
       | _ -> None)
  | "Revsh", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Revsh rd rs)
       | _ -> None)
  | "Ubfx", [dst; rn; EConst lsb; EConst width] ->
      (match reg_of_expr dst, reg_of_expr rn with
       | Some rd, Some r_n ->
           if lsb >= 0 && lsb <= 4294967295 && width >= 0 && width <= 4294967295 then
             Some (Sem.Ubfx rd r_n (FStar.UInt32.uint_to_t lsb) (FStar.UInt32.uint_to_t width))
           else None
       | _ -> None)
  | "Sbfx", [dst; rn; EConst lsb; EConst width] ->
      (match reg_of_expr dst, reg_of_expr rn with
       | Some rd, Some r_n ->
           if lsb >= 0 && lsb <= 4294967295 && width >= 0 && width <= 4294967295 then
             Some (Sem.Sbfx rd r_n (FStar.UInt32.uint_to_t lsb) (FStar.UInt32.uint_to_t width))
           else None
       | _ -> None)
  | "Cmp", [rn; op] ->
      (match reg_of_expr rn, operand_of_expr op with
       | Some r_n, Some o -> Some (Sem.Cmp r_n o)
       | _ -> None)
  | "Cmn", [rn; op] ->
      (match reg_of_expr rn, operand_of_expr op with
       | Some r_n, Some o -> Some (Sem.Cmn r_n o)
       | _ -> None)
  | "Tst", [rn; op] ->
      (match reg_of_expr rn, operand_of_expr op with
       | Some r_n, Some o -> Some (Sem.Tst r_n o)
       | _ -> None)
  | "Teq", [rn; op] ->
      (match reg_of_expr rn, operand_of_expr op with
       | Some r_n, Some o -> Some (Sem.Teq r_n o)
       | _ -> None)
  | "Sxtb", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Sxtb rd rs)
       | _ -> None)
  | "Sxth", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Sxth rd rs)
       | _ -> None)
  | "Uxtb", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Uxtb rd rs)
       | _ -> None)
  | "Uxth", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Uxth rd rs)
       | _ -> None)
  | "Ldr", [dst; buf; EConst offset] ->
      (match reg_of_expr dst, reg_of_expr buf with
       | Some rd, Some rb -> Some (Sem.Ldr rd (MBaseOffset rb offset))
       | _ -> None)
  | "Ldr", [dst; buf] ->
      (match reg_of_expr dst, reg_of_expr buf with
       | Some rd, Some rb -> Some (Sem.Ldr rd (MBase rb))
       | _ -> None)
  | "Ldrb", [dst; buf; EConst offset] ->
      (match reg_of_expr dst, reg_of_expr buf with
       | Some rd, Some rb -> Some (Sem.Ldrb rd (MBaseOffset rb offset))
       | _ -> None)
  | "Ldrh", [dst; buf; EConst offset] ->
      (match reg_of_expr dst, reg_of_expr buf with
       | Some rd, Some rb -> Some (Sem.Ldrh rd (MBaseOffset rb offset))
       | _ -> None)
  | "Ldrsb", [dst; buf; EConst offset] ->
      (match reg_of_expr dst, reg_of_expr buf with
       | Some rd, Some rb -> Some (Sem.Ldrsb rd (MBaseOffset rb offset))
       | _ -> None)
  | "Ldrsh", [dst; buf; EConst offset] ->
      (match reg_of_expr dst, reg_of_expr buf with
       | Some rd, Some rb -> Some (Sem.Ldrsh rd (MBaseOffset rb offset))
       | _ -> None)
  | "Ldrd", [rt; rt2; buf; EConst offset] ->
      (match reg_of_expr rt, reg_of_expr rt2, reg_of_expr buf with
       | Some r1, Some r2, Some rb -> Some (Sem.Ldrd r1 r2 (MBaseOffset rb offset))
       | _ -> None)
  | "Str", [src; buf; EConst offset]
  | "Str", [buf; EConst offset; src] ->
      (match reg_of_expr src, reg_of_expr buf with
       | Some rs, Some rb -> Some (Sem.Str rs (MBaseOffset rb offset))
       | _ -> None)
  | "Str", [src; buf] ->
      (match reg_of_expr src, reg_of_expr buf with
       | Some rs, Some rb -> Some (Sem.Str rs (MBase rb))
       | _ -> None)
  | "Strb", [src; buf; EConst offset]
  | "Strb", [buf; EConst offset; src] ->
      (match reg_of_expr src, reg_of_expr buf with
       | Some rs, Some rb -> Some (Sem.Strb rs (MBaseOffset rb offset))
       | _ -> None)
  | "Strh", [src; buf; EConst offset]
  | "Strh", [buf; EConst offset; src] ->
      (match reg_of_expr src, reg_of_expr buf with
       | Some rs, Some rb -> Some (Sem.Strh rs (MBaseOffset rb offset))
       | _ -> None)
  | "Strd", [rt; rt2; buf; EConst offset] ->
      (match reg_of_expr rt, reg_of_expr rt2, reg_of_expr buf with
       | Some r1, Some r2, Some rb -> Some (Sem.Strd r1 r2 (MBaseOffset rb offset))
       | _ -> None)
  | "Push", [r] ->
      (match reg_of_expr r with
       | Some rd -> Some (Sem.Push [rd])
       | _ -> None)
  | "Pop", [r] ->
      (match reg_of_expr r with
       | Some rd -> Some (Sem.Pop [rd])
       | _ -> None)
  | "Bc", [c; EVar target] ->
      (match cond_of_expr c with
       | Some cond -> Some (Sem.Bc cond target)
       | _ -> None)
  | "Bx", [target] ->
      (match reg_of_expr target with
       | Some rt -> Some (Sem.Bx rt)
       | _ -> None)
  | "Blx", [target] ->
      (match reg_of_expr target with
       | Some rt -> Some (Sem.Blx rt)
       | _ -> None)
  | "Vmov", [dst; src] ->
      (match operand_of_expr dst, operand_of_expr src with
       | Some od, Some os -> Some (Sem.Vmov od os)
       | _ -> None)
  | "VaddF32", [dst; s1; s2] ->
      (match sreg_of_expr dst, sreg_of_expr s1, sreg_of_expr s2 with
       | Some sd, Some s_1, Some s_2 -> Some (Sem.VaddF32 sd s_1 s_2)
       | _ -> None)
  | "VaddF64", [dst; s1; s2] ->
      (match dreg_of_expr dst, dreg_of_expr s1, dreg_of_expr s2 with
       | Some dd, Some d_1, Some d_2 -> Some (Sem.VaddF64 dd d_1 d_2)
       | _ -> None)
  | "VsubF32", [dst; s1; s2] ->
      (match sreg_of_expr dst, sreg_of_expr s1, sreg_of_expr s2 with
       | Some sd, Some s_1, Some s_2 -> Some (Sem.VsubF32 sd s_1 s_2)
       | _ -> None)
  | "VsubF64", [dst; s1; s2] ->
      (match dreg_of_expr dst, dreg_of_expr s1, dreg_of_expr s2 with
       | Some dd, Some d_1, Some d_2 -> Some (Sem.VsubF64 dd d_1 d_2)
       | _ -> None)
  | "VmulF32", [dst; s1; s2] ->
      (match sreg_of_expr dst, sreg_of_expr s1, sreg_of_expr s2 with
       | Some sd, Some s_1, Some s_2 -> Some (Sem.VmulF32 sd s_1 s_2)
       | _ -> None)
  | "VmulF64", [dst; s1; s2] ->
      (match dreg_of_expr dst, dreg_of_expr s1, dreg_of_expr s2 with
       | Some dd, Some d_1, Some d_2 -> Some (Sem.VmulF64 dd d_1 d_2)
       | _ -> None)
  | "Vldr", [dst; buf; EConst offset] ->
      (match sreg_of_expr dst, reg_of_expr buf with
       | Some sd, Some rb -> Some (Sem.Vldr sd (MBaseOffset rb offset))
       | _ -> None)
  | "Vstr", [src; buf; EConst offset] ->
      (match sreg_of_expr src, reg_of_expr buf with
       | Some sd, Some rb -> Some (Sem.Vstr sd (MBaseOffset rb offset))
       | _ -> None)
  | "Vpush", [r] ->
      (match sreg_of_expr r with
       | Some sd -> Some (Sem.Vpush [sd])
       | _ -> None)
  | "Vpop", [r] ->
      (match sreg_of_expr r with
       | Some sd -> Some (Sem.Vpop [sd])
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
       | None -> Some (Sem.Ins (Sem.Bl target)))
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
