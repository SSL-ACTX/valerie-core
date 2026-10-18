module Valerie.Compiler.Lower.AArch64

open Valerie.Defs
open FStar.UInt64
module U64 = FStar.UInt64
open Valerie.Compiler.AST
open Valerie.AArch64.Machine
module Sem = Valerie.AArch64.Semantics

let reg_of_operand (r:reg_operand) : reg =
  match r with
  | RegNumbered n ->
      if n = 0ul then X0
      else if n = 1ul then X1
      else if n = 2ul then X2
      else if n = 3ul then X3
      else if n = 4ul then X4
      else if n = 5ul then X5
      else if n = 6ul then X6
      else if n = 7ul then X7
      else if n = 8ul then X8
      else if n = 9ul then X9
      else if n = 10ul then X10
      else if n = 11ul then X11
      else if n = 12ul then X12
      else if n = 13ul then X13
      else if n = 14ul then X14
      else if n = 15ul then X15
      else if n = 16ul then X16
      else if n = 17ul then X17
      else if n = 18ul then X18
      else if n = 19ul then X19
      else if n = 20ul then X20
      else if n = 21ul then X21
      else if n = 22ul then X22
      else if n = 23ul then X23
      else if n = 24ul then X24
      else if n = 25ul then X25
      else if n = 26ul then X26
      else if n = 27ul then X27
      else if n = 28ul then X28
      else if n = 29ul then X29
      else if n = 30ul then X30
      else SP

let lower_operand (op:operand) : Sem.operand =
  match op with
  | OReg r -> Sem.OReg (reg_of_operand r) W64
  | OImm i -> Sem.OImm i
  | OMemBuffer r offset -> Sem.OMem (MOffset (reg_of_operand r) (FStar.UInt32.v offset))

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
  | OpSub, [dst; src1; src2] ->
      (match dst, src1 with
       | OReg rd, OReg rn -> Some (Sem.Sub (reg_of_operand rd) (reg_of_operand rn) (lower_operand src2))
       | _ -> None)
  | OpXor, [dst; src1; src2] ->
      (match dst, src1 with
       | OReg rd, OReg rn -> Some (Sem.Eor (reg_of_operand rd) (reg_of_operand rn) (lower_operand src2))
       | _ -> None)
  | OpAnd, [dst; src1; src2] ->
      (match dst, src1 with
       | OReg rd, OReg rn -> Some (Sem.And (reg_of_operand rd) (reg_of_operand rn) (lower_operand src2))
       | _ -> None)
  | OpRev, [dst; src] ->
      (match dst, src with
       | OReg rd, OReg rn -> Some (Sem.Rev64 (reg_of_operand rd) (reg_of_operand rn))
       | _ -> None)
  | _ -> None

let reg_of_name (s: string) : option reg =
  if s = "r0" || s = "x0" || s = "w0" || s = "state" then Some X0
  else if s = "r1" || s = "x1" || s = "w1" then Some X1
  else if s = "r2" || s = "x2" || s = "w2" then Some X2
  else if s = "r3" || s = "x3" || s = "w3" then Some X3
  else if s = "r4" || s = "x4" || s = "w4" then Some X4
  else if s = "r5" || s = "x5" || s = "w5" then Some X5
  else if s = "r6" || s = "x6" || s = "w6" then Some X6
  else if s = "r7" || s = "x7" || s = "w7" then Some X7
  else if s = "r8" || s = "x8" || s = "w8" then Some X8
  else if s = "r9" || s = "x9" || s = "w9" then Some X9
  else if s = "r10" || s = "x10" || s = "w10" then Some X10
  else if s = "r11" || s = "x11" || s = "w11" then Some X11
  else if s = "r12" || s = "x12" || s = "w12" then Some X12
  else if s = "r13" || s = "x13" || s = "w13" then Some X13
  else if s = "r14" || s = "x14" || s = "w14" then Some X14
  else if s = "r15" || s = "x15" || s = "w15" then Some X15
  else if s = "r16" || s = "x16" || s = "w16" then Some X16
  else if s = "r17" || s = "x17" || s = "w17" then Some X17
  else if s = "r18" || s = "x18" || s = "w18" then Some X18
  else if s = "r19" || s = "x19" || s = "w19" then Some X19
  else if s = "r20" || s = "x20" || s = "w20" then Some X20
  else if s = "r21" || s = "x21" || s = "w21" then Some X21
  else if s = "r22" || s = "x22" || s = "w22" then Some X22
  else if s = "r23" || s = "x23" || s = "w23" then Some X23
  else if s = "r24" || s = "x24" || s = "w24" then Some X24
  else if s = "r25" || s = "x25" || s = "w25" then Some X25
  else if s = "r26" || s = "x26" || s = "w26" then Some X26
  else if s = "r27" || s = "x27" || s = "w27" then Some X27
  else if s = "r28" || s = "x28" || s = "w28" then Some X28
  else if s = "r29" || s = "x29" || s = "w29" then Some X29
  else if s = "r30" || s = "x30" || s = "w30" then Some X30
  else if s = "sp" || s = "rsp" || s = "wsp" then Some SP
  else None

let reg_of_expr (e: expr) : option reg =
  match e with
  | EVar s -> reg_of_name s
  | _ -> None

let vec_of_name (s: string) : option vec =
  if s = "v0" || s = "q0" || s = "d0" then Some V0
  else if s = "v1" || s = "q1" || s = "d1" then Some V1
  else if s = "v2" || s = "q2" || s = "d2" then Some V2
  else if s = "v3" || s = "q3" || s = "d3" then Some V3
  else if s = "v4" || s = "q4" || s = "d4" then Some V4
  else if s = "v5" || s = "q5" || s = "d5" then Some V5
  else if s = "v6" || s = "q6" || s = "d6" then Some V6
  else if s = "v7" || s = "q7" || s = "d7" then Some V7
  else if s = "v8" || s = "q8" || s = "d8" then Some V8
  else if s = "v9" || s = "q9" || s = "d9" then Some V9
  else if s = "v10" || s = "q10" || s = "d10" then Some V10
  else if s = "v11" || s = "q11" || s = "d11" then Some V11
  else if s = "v12" || s = "q12" || s = "d12" then Some V12
  else if s = "v13" || s = "q13" || s = "d13" then Some V13
  else if s = "v14" || s = "q14" || s = "d14" then Some V14
  else if s = "v15" || s = "q15" || s = "d15" then Some V15
  else if s = "v16" || s = "q16" || s = "d16" then Some V16
  else if s = "v17" || s = "q17" || s = "d17" then Some V17
  else if s = "v18" || s = "q18" || s = "d18" then Some V18
  else if s = "v19" || s = "q19" || s = "d19" then Some V19
  else if s = "v20" || s = "q20" || s = "d20" then Some V20
  else if s = "v21" || s = "q21" || s = "d21" then Some V21
  else if s = "v22" || s = "q22" || s = "d22" then Some V22
  else if s = "v23" || s = "q23" || s = "d23" then Some V23
  else if s = "v24" || s = "q24" || s = "d24" then Some V24
  else if s = "v25" || s = "q25" || s = "d25" then Some V25
  else if s = "v26" || s = "q26" || s = "d26" then Some V26
  else if s = "v27" || s = "q27" || s = "d27" then Some V27
  else if s = "v28" || s = "q28" || s = "d28" then Some V28
  else if s = "v29" || s = "q29" || s = "d29" then Some V29
  else if s = "v30" || s = "q30" || s = "d30" then Some V30
  else if s = "v31" || s = "q31" || s = "d31" then Some V31
  else None

let vec_of_expr (e: expr) : option vec =
  match e with
  | EVar s -> vec_of_name s
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
  else if s = "nv" || s = "NV" then Some CondNV
  else None

let cond_of_expr (e: expr) : option cond_code =
  match e with
  | EVar s -> cond_of_name s
  | _ -> None

let operand_of_expr (e: expr) (w: reg_width) : option Sem.operand =
  match e with
  | EVar s ->
      (match reg_of_name s with
       | Some r -> Some (Sem.OReg r w)
       | None -> None)
  | EConst n ->
      if n >= 0 && n <= 18446744073709551615 then
        Some (Sem.OImm (FStar.UInt64.uint_to_t n))
      else None
  | _ -> None

let lower_call (target: id) (args: list expr) : option Sem.ins =
  match target, args with
  | "Load32", [dst; buf; EConst offset]
  | "Ldr32", [dst; buf; EConst offset] ->
      (match reg_of_expr dst, reg_of_expr buf with
       | Some rd, Some rb -> Some (Sem.Ldr32 rd (MOffset rb offset))
       | _ -> None)
  | "Store32", [buf; EConst offset; src]
  | "Str32", [buf; EConst offset; src] ->
      (match reg_of_expr buf, reg_of_expr src with
       | Some rb, Some rs -> Some (Sem.Str32 rs (MOffset rb offset))
       | _ -> None)
  | "Add32", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 W32 with
            | Some op -> Some (Sem.Add32 rd rn op)
            | None -> None)
       | _ -> None)
  | "Add", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 W64 with
            | Some op -> Some (Sem.Add rd rn op)
            | None -> None)
       | _ -> None)
  | "Sub32", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 W32 with
            | Some op -> Some (Sem.Sub32 rd rn op)
            | None -> None)
       | _ -> None)
  | "Sub", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 W64 with
            | Some op -> Some (Sem.Sub rd rn op)
            | None -> None)
       | _ -> None)
  | "Xor32", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 W32 with
            | Some op -> Some (Sem.Eor32 rd rn op)
            | None -> None)
       | _ -> None)
  | "Xor", [dst; s1; s2]
  | "Eor", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 W64 with
            | Some op -> Some (Sem.Eor rd rn op)
            | None -> None)
       | _ -> None)
  | "And32", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 W32 with
            | Some op -> Some (Sem.And32 rd rn op)
            | None -> None)
       | _ -> None)
  | "And", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 W64 with
            | Some op -> Some (Sem.And rd rn op)
            | None -> None)
       | _ -> None)
  | "Or32", [dst; s1; s2]
  | "Orr32", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 W32 with
            | Some op -> Some (Sem.Orr32 rd rn op)
            | None -> None)
       | _ -> None)
  | "Rol32", [dst; s1; EConst amt] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           let ror_amt = (32 - (amt % 32)) % 32 in
           if ror_amt >= 0 && ror_amt <= 31 then
             Some (Sem.Ror32 rd rn (Sem.OImm (FStar.UInt64.uint_to_t ror_amt)))
           else None
       | _ -> None)
  | "Ror32", [dst; s1; EConst amt] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           let ror_amt = amt % 32 in
           if ror_amt >= 0 && ror_amt <= 31 then
             Some (Sem.Ror32 rd rn (Sem.OImm (FStar.UInt64.uint_to_t ror_amt)))
           else None
       | _ -> None)
  | "Rol", [dst; s1; EConst amt] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           let ror_amt = (64 - (amt % 64)) % 64 in
           if ror_amt >= 0 && ror_amt <= 63 then
             Some (Sem.Ror rd rn (Sem.OImm (FStar.UInt64.uint_to_t ror_amt)))
           else None
       | _ -> None)
  | "Ror", [dst; s1; EConst amt] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           let ror_amt = amt % 64 in
           if ror_amt >= 0 && ror_amt <= 63 then
             Some (Sem.Ror rd rn (Sem.OImm (FStar.UInt64.uint_to_t ror_amt)))
           else None
       | _ -> None)
  | "Ror", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 W64 with
            | Some op -> Some (Sem.Ror rd rn op)
            | None -> None)
       | _ -> None)
  | "FmovFromGpr", [dst; src] ->
      (match vec_of_expr dst, reg_of_expr src with
       | Some vd, Some rs -> Some (Sem.FmovGprToVec vd rs)
       | _ -> None)
  | "FmovToGpr", [dst; src] ->
      (match reg_of_expr dst, vec_of_expr src with
       | Some rd, Some vs -> Some (Sem.FmovVecToGpr rd vs)
       | _ -> None)
  | "MovVecLane", [dst; src] ->
      (match vec_of_expr dst, reg_of_expr src with
       | Some vd, Some rs -> Some (Sem.InsLane vd 1 rs)
       | _ -> None)
  | "MovVecLane", [dst; EConst lane; src]
  | "InsLane", [dst; EConst lane; src] ->
      (match vec_of_expr dst, reg_of_expr src with
       | Some vd, Some rs ->
           if lane >= 0 && lane < 2 then Some (Sem.InsLane vd lane rs)
           else None
       | _ -> None)
  | "MovLaneToGpr", [dst; src] ->
      (match reg_of_expr dst, vec_of_expr src with
       | Some rd, Some vs -> Some (Sem.Umov rd vs 1)
       | _ -> None)
  | "MovLaneToGpr", [dst; src; EConst lane]
  | "Umov", [dst; src; EConst lane] ->
      (match reg_of_expr dst, vec_of_expr src with
       | Some rd, Some vs ->
           if lane >= 0 && lane < 2 then Some (Sem.Umov rd vs lane)
           else None
       | _ -> None)
  | "MovImm", [dst; EConst imm] ->
      (match reg_of_expr dst with
       | Some rd ->
           if imm >= 0 && imm <= 18446744073709551615 then
             Some (Sem.Mov rd (Sem.OImm (FStar.UInt64.uint_to_t imm)))
           else None
       | _ -> None)
  | "Mov", [dst; src] ->
      (match reg_of_expr dst, operand_of_expr src W64 with
       | Some rd, Some op -> Some (Sem.Mov rd op)
       | _ -> None)
  | "Rbit", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Rbit rd rs)
       | _ -> None)
  | "Rev", [dst; src]
  | "Rev64", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Rev64 rd rs)
       | _ -> None)
  | "Rev32", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Rev32 rd rs)
       | _ -> None)
  | "Rev16", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Rev16 rd rs)
       | _ -> None)
  | "Clz", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Clz rd rs)
       | _ -> None)
  | "DupVec", [dst; src]
  | "Dup", [dst; src] ->
      (match vec_of_expr dst, reg_of_expr src with
       | Some vd, Some rs -> Some (Sem.Dup vd rs)
       | _ -> None)
  | "Pmull", [dst; s1; s2] ->
      (match vec_of_expr dst, vec_of_expr s1, vec_of_expr s2 with
       | Some vd, Some v1, Some v2 -> Some (Sem.Pmull vd v1 v2)
       | _ -> None)
  | "Pmull2", [dst; s1; s2] ->
      (match vec_of_expr dst, vec_of_expr s1, vec_of_expr s2 with
       | Some vd, Some v1, Some v2 -> Some (Sem.Pmull2 vd v1 v2)
       | _ -> None)
  | "Veor", [dst; src] ->
      (match vec_of_expr dst, vec_of_expr src with
       | Some vd, Some vs -> Some (Sem.Veor vd vd vs)
       | _ -> None)
  | "Veor", [dst; s1; s2] ->
      (match vec_of_expr dst, vec_of_expr s1, vec_of_expr s2 with
       | Some vd, Some v1, Some v2 -> Some (Sem.Veor vd v1 v2)
       | _ -> None)
  | "Vext8", [dst; s1; s2] ->
      (match vec_of_expr dst, vec_of_expr s1, vec_of_expr s2 with
       | Some vd, Some v1, Some v2 -> Some (Sem.Vext8 vd v1 v2 8)
       | _ -> None)
  | "Vext8", [dst; s1; s2; EConst idx]
  | "Vext", [dst; s1; s2; EConst idx] ->
      (match vec_of_expr dst, vec_of_expr s1, vec_of_expr s2 with
       | Some vd, Some v1, Some v2 ->
           if idx >= 0 && idx < 16 then Some (Sem.Vext8 vd v1 v2 idx)
           else None
       | _ -> None)
  | "Aese", [dst; src] ->
      (match vec_of_expr dst, vec_of_expr src with
       | Some vd, Some vs -> Some (Sem.Aese vd vs)
       | _ -> None)
  | "Aesd", [dst; src] ->
      (match vec_of_expr dst, vec_of_expr src with
       | Some vd, Some vs -> Some (Sem.Aesd vd vs)
       | _ -> None)
  | "Aesmc", [dst; src] ->
      (match vec_of_expr dst, vec_of_expr src with
       | Some vd, Some vs -> Some (Sem.Aesmc vd vs)
       | _ -> None)
  | "Aesimc", [dst; src] ->
      (match vec_of_expr dst, vec_of_expr src with
       | Some vd, Some vs -> Some (Sem.Aesimc vd vs)
       | _ -> None)
  | "Sha1c", [dst; s1; s2] ->
      (match vec_of_expr dst, vec_of_expr s1, vec_of_expr s2 with
       | Some vd, Some v1, Some v2 -> Some (Sem.Sha1c vd v1 v2)
       | _ -> None)
  | "Sha1p", [dst; s1; s2] ->
      (match vec_of_expr dst, vec_of_expr s1, vec_of_expr s2 with
       | Some vd, Some v1, Some v2 -> Some (Sem.Sha1p vd v1 v2)
       | _ -> None)
  | "Sha1m", [dst; s1; s2] ->
      (match vec_of_expr dst, vec_of_expr s1, vec_of_expr s2 with
       | Some vd, Some v1, Some v2 -> Some (Sem.Sha1m vd v1 v2)
       | _ -> None)
  | "Sha1h", [dst; src] ->
      (match vec_of_expr dst, vec_of_expr src with
       | Some vd, Some vs -> Some (Sem.Sha1h vd vs)
       | _ -> None)
  | "Sha1su0", [dst; s1; s2] ->
      (match vec_of_expr dst, vec_of_expr s1, vec_of_expr s2 with
       | Some vd, Some v1, Some v2 -> Some (Sem.Sha1su0 vd v1 v2)
       | _ -> None)
  | "Sha1su1", [dst; src] ->
      (match vec_of_expr dst, vec_of_expr src with
       | Some vd, Some vs -> Some (Sem.Sha1su1 vd vs)
       | _ -> None)
  | "Sha256h", [dst; s1; s2] ->
      (match vec_of_expr dst, vec_of_expr s1, vec_of_expr s2 with
       | Some vd, Some v1, Some v2 -> Some (Sem.Sha256h vd v1 v2)
       | _ -> None)
  | "Sha256h2", [dst; s1; s2] ->
      (match vec_of_expr dst, vec_of_expr s1, vec_of_expr s2 with
       | Some vd, Some v1, Some v2 -> Some (Sem.Sha256h2 vd v1 v2)
       | _ -> None)
  | "Sha256su0", [dst; src] ->
      (match vec_of_expr dst, vec_of_expr src with
       | Some vd, Some vs -> Some (Sem.Sha256su0 vd vs)
       | _ -> None)
  | "Sha256su1", [dst; s1; s2] ->
      (match vec_of_expr dst, vec_of_expr s1, vec_of_expr s2 with
       | Some vd, Some v1, Some v2 -> Some (Sem.Sha256su1 vd v1 v2)
       | _ -> None)

  | "Adds", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 W64 with
            | Some op -> Some (Sem.Adds rd rn op)
            | None -> None)
       | _ -> None)
  | "Subs", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 W64 with
            | Some op -> Some (Sem.Subs rd rn op)
            | None -> None)
       | _ -> None)
  | "Adc", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rn, Some rm -> Some (Sem.Adc rd rn rm)
       | _ -> None)
  | "Adcs", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rn, Some rm -> Some (Sem.Adcs rd rn rm)
       | _ -> None)
  | "Sbc", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rn, Some rm -> Some (Sem.Sbc rd rn rm)
       | _ -> None)
  | "Sbcs", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rn, Some rm -> Some (Sem.Sbcs rd rn rm)
       | _ -> None)
  | "Cmp", [s1; s2] ->
      (match reg_of_expr s1 with
       | Some rn ->
           (match operand_of_expr s2 W64 with
            | Some op -> Some (Sem.Cmp rn op)
            | None -> None)
       | _ -> None)
  | "Cmn", [s1; s2] ->
      (match reg_of_expr s1 with
       | Some rn ->
           (match operand_of_expr s2 W64 with
            | Some op -> Some (Sem.Cmn rn op)
            | None -> None)
       | _ -> None)
  | "Tst", [s1; s2] ->
      (match reg_of_expr s1 with
       | Some rn ->
           (match operand_of_expr s2 W64 with
            | Some op -> Some (Sem.Tst rn op)
            | None -> None)
       | _ -> None)
  | "Neg", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Neg rd rs)
       | _ -> None)
  | "Negs", [dst; src] ->
      (match reg_of_expr dst, reg_of_expr src with
       | Some rd, Some rs -> Some (Sem.Negs rd rs)
       | _ -> None)
  | "Mvn", [dst; src] ->
      (match reg_of_expr dst with
       | Some rd ->
           (match operand_of_expr src W64 with
            | Some op -> Some (Sem.Mvn rd op)
            | None -> None)
       | _ -> None)
  | "Mul", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rn, Some rm -> Some (Sem.Mul rd rn rm)
       | _ -> None)
  | "Madd", [dst; s1; s2; s3] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2, reg_of_expr s3 with
       | Some rd, Some rn, Some rm, Some ra -> Some (Sem.Madd rd rn rm ra)
       | _ -> None)
  | "Msub", [dst; s1; s2; s3] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2, reg_of_expr s3 with
       | Some rd, Some rn, Some rm, Some ra -> Some (Sem.Msub rd rn rm ra)
       | _ -> None)
  | "Smulh", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rn, Some rm -> Some (Sem.Smulh rd rn rm)
       | _ -> None)
  | "Umulh", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rn, Some rm -> Some (Sem.Umulh rd rn rm)
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
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 W64 with
            | Some op -> Some (Sem.Ands rd rn op)
            | None -> None)
       | _ -> None)
  | "Orr", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 W64 with
            | Some op -> Some (Sem.Orr rd rn op)
            | None -> None)
       | _ -> None)
  | "Orn", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 W64 with
            | Some op -> Some (Sem.Orn rd rn op)
            | None -> None)
       | _ -> None)
  | "Eon", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 W64 with
            | Some op -> Some (Sem.Eon rd rn op)
            | None -> None)
       | _ -> None)
  | "Bic", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 W64 with
            | Some op -> Some (Sem.Bic rd rn op)
            | None -> None)
       | _ -> None)
  | "Bics", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 W64 with
            | Some op -> Some (Sem.Bics rd rn op)
            | None -> None)
       | _ -> None)
  | "Lsl", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 W64 with
            | Some op -> Some (Sem.Lsl rd rn op)
            | None -> None)
       | _ -> None)
  | "Lsr", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 W64 with
            | Some op -> Some (Sem.Lsr rd rn op)
            | None -> None)
       | _ -> None)
  | "Asr", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1 with
       | Some rd, Some rn ->
           (match operand_of_expr s2 W64 with
            | Some op -> Some (Sem.Asr rd rn op)
            | None -> None)
       | _ -> None)
  | "Extr", [dst; s1; s2; EConst lsb] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2 with
       | Some rd, Some rn, Some rm ->
           if lsb >= 0 && lsb <= 63 then Some (Sem.Extr rd rn rm lsb)
           else None
       | _ -> None)
  | "Csel", [dst; s1; s2; c] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2, cond_of_expr c with
       | Some rd, Some rn, Some rm, Some cond -> Some (Sem.Csel rd rn rm cond)
       | _ -> None)
  | "Cset", [dst; c] ->
      (match reg_of_expr dst, cond_of_expr c with
       | Some rd, Some cond -> Some (Sem.Cset rd cond)
       | _ -> None)
  | "Csetm", [dst; c] ->
      (match reg_of_expr dst, cond_of_expr c with
       | Some rd, Some cond -> Some (Sem.Csetm rd cond)
       | _ -> None)
  | "Cinc", [dst; s1; c] ->
      (match reg_of_expr dst, reg_of_expr s1, cond_of_expr c with
       | Some rd, Some rn, Some cond -> Some (Sem.Cinc rd rn cond)
       | _ -> None)
  | "Cinv", [dst; s1; c] ->
      (match reg_of_expr dst, reg_of_expr s1, cond_of_expr c with
       | Some rd, Some rn, Some cond -> Some (Sem.Cinv rd rn cond)
       | _ -> None)
  | "Cneg", [dst; s1; c] ->
      (match reg_of_expr dst, reg_of_expr s1, cond_of_expr c with
       | Some rd, Some rn, Some cond -> Some (Sem.Cneg rd rn cond)
       | _ -> None)
  | "Csinc", [dst; s1; s2; c] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2, cond_of_expr c with
       | Some rd, Some rn, Some rm, Some cond -> Some (Sem.Csinc rd rn rm cond)
       | _ -> None)
  | "Csinv", [dst; s1; s2; c] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2, cond_of_expr c with
       | Some rd, Some rn, Some rm, Some cond -> Some (Sem.Csinv rd rn rm cond)
       | _ -> None)
  | "Csneg", [dst; s1; s2; c] ->
      (match reg_of_expr dst, reg_of_expr s1, reg_of_expr s2, cond_of_expr c with
       | Some rd, Some rn, Some rm, Some cond -> Some (Sem.Csneg rd rn rm cond)
       | _ -> None)
  | "Movz", [dst; EConst imm; EConst shift] ->
      (match reg_of_expr dst with
       | Some rd ->
           if imm >= 0 && imm <= 18446744073709551615 && shift >= 0 then
             Some (Sem.Movz rd (FStar.UInt64.uint_to_t imm) shift)
           else None
       | _ -> None)
  | "Movn", [dst; EConst imm; EConst shift] ->
      (match reg_of_expr dst with
       | Some rd ->
           if imm >= 0 && imm <= 18446744073709551615 && shift >= 0 then
             Some (Sem.Movn rd (FStar.UInt64.uint_to_t imm) shift)
           else None
       | _ -> None)
  | "Movk", [dst; EConst imm; EConst shift] ->
      (match reg_of_expr dst with
       | Some rd ->
           if imm >= 0 && imm <= 18446744073709551615 && shift >= 0 then
             Some (Sem.Movk rd (FStar.UInt64.uint_to_t imm) shift)
           else None
       | _ -> None)
  | "Adrp", [dst; EVar lbl] ->
      (match reg_of_expr dst with
       | Some rd -> Some (Sem.Adrp rd lbl)
       | _ -> None)
  | "Ldr", [dst; buf; EConst offset] ->
      (match reg_of_expr dst, reg_of_expr buf with
       | Some rd, Some rb -> Some (Sem.Ldr rd (MOffset rb offset))
       | _ -> None)
  | "Ldr", [dst; buf] ->
      (match reg_of_expr dst, reg_of_expr buf with
       | Some rd, Some rb -> Some (Sem.Ldr rd (MReg rb))
       | _ -> None)
  | "Ldrb", [dst; buf; EConst offset] ->
      (match reg_of_expr dst, reg_of_expr buf with
       | Some rd, Some rb -> Some (Sem.Ldrb rd (MOffset rb offset))
       | _ -> None)
  | "Ldrh", [dst; buf; EConst offset] ->
      (match reg_of_expr dst, reg_of_expr buf with
       | Some rd, Some rb -> Some (Sem.Ldrh rd (MOffset rb offset))
       | _ -> None)
  | "Ldrsb", [dst; buf; EConst offset] ->
      (match reg_of_expr dst, reg_of_expr buf with
       | Some rd, Some rb -> Some (Sem.Ldrsb rd (MOffset rb offset))
       | _ -> None)
  | "Ldrsh", [dst; buf; EConst offset] ->
      (match reg_of_expr dst, reg_of_expr buf with
       | Some rd, Some rb -> Some (Sem.Ldrsh rd (MOffset rb offset))
       | _ -> None)
  | "Ldrsw", [dst; buf; EConst offset] ->
      (match reg_of_expr dst, reg_of_expr buf with
       | Some rd, Some rb -> Some (Sem.Ldrsw rd (MOffset rb offset))
       | _ -> None)
  | "Str", [buf; EConst offset; src]
  | "Str", [src; buf; EConst offset] ->
      (match reg_of_expr src, reg_of_expr buf with
       | Some rs, Some rb -> Some (Sem.Str rs (MOffset rb offset))
       | _ -> None)
  | "Str", [src; buf] ->
      (match reg_of_expr src, reg_of_expr buf with
       | Some rs, Some rb -> Some (Sem.Str rs (MReg rb))
       | _ -> None)
  | "Strb", [buf; EConst offset; src]
  | "Strb", [src; buf; EConst offset] ->
      (match reg_of_expr src, reg_of_expr buf with
       | Some rs, Some rb -> Some (Sem.Strb rs (MOffset rb offset))
       | _ -> None)
  | "Strh", [buf; EConst offset; src]
  | "Strh", [src; buf; EConst offset] ->
      (match reg_of_expr src, reg_of_expr buf with
       | Some rs, Some rb -> Some (Sem.Strh rs (MOffset rb offset))
       | _ -> None)
  | "Ldp", [d1; d2; buf; EConst offset] ->
      (match reg_of_expr d1, reg_of_expr d2, reg_of_expr buf with
       | Some rd1, Some rd2, Some rb -> Some (Sem.Ldp rd1 rd2 (MOffset rb offset))
       | _ -> None)
  | "Stp", [s1; s2; buf; EConst offset] ->
      (match reg_of_expr s1, reg_of_expr s2, reg_of_expr buf with
       | Some rs1, Some rs2, Some rb -> Some (Sem.Stp rs1 rs2 (MOffset rb offset))
       | _ -> None)
  | "B", [EVar lbl] -> Some (Sem.B lbl)
  | "BCond", [c; EVar lbl] ->
      (match cond_of_expr c with
       | Some cond -> Some (Sem.BCond cond lbl)
       | _ -> None)
  | "Blr", [r] ->
      (match reg_of_expr r with
       | Some rd -> Some (Sem.Blr rd)
       | _ -> None)
  | "Ret", [] -> Some Sem.Ret
  | "Cbz", [r; EVar lbl] ->
      (match reg_of_expr r with
       | Some rd -> Some (Sem.Cbz rd lbl)
       | _ -> None)
  | "Cbnz", [r; EVar lbl] ->
      (match reg_of_expr r with
       | Some rd -> Some (Sem.Cbnz rd lbl)
       | _ -> None)
  | "Fadd", [dst; s1; s2] ->
      (match vec_of_expr dst, vec_of_expr s1, vec_of_expr s2 with
       | Some vd, Some v1, Some v2 -> Some (Sem.Fadd vd v1 v2)
       | _ -> None)
  | "Fsub", [dst; s1; s2] ->
      (match vec_of_expr dst, vec_of_expr s1, vec_of_expr s2 with
       | Some vd, Some v1, Some v2 -> Some (Sem.Fsub vd v1 v2)
       | _ -> None)
  | "Fmul", [dst; s1; s2] ->
      (match vec_of_expr dst, vec_of_expr s1, vec_of_expr s2 with
       | Some vd, Some v1, Some v2 -> Some (Sem.Fmul vd v1 v2)
       | _ -> None)
  | "Fdiv", [dst; s1; s2] ->
      (match vec_of_expr dst, vec_of_expr s1, vec_of_expr s2 with
       | Some vd, Some v1, Some v2 -> Some (Sem.Fdiv vd v1 v2)
       | _ -> None)
  | "Smov", [dst; src; EConst lane] ->
      (match reg_of_expr dst, vec_of_expr src with
       | Some rd, Some vs ->
           if lane >= 0 && lane < 2 then Some (Sem.Smov rd vs lane)
           else None
       | _ -> None)
  | "Vadd", [dst; s1; s2] ->
      (match vec_of_expr dst, vec_of_expr s1, vec_of_expr s2 with
       | Some vd, Some v1, Some v2 -> Some (Sem.Vadd vd v1 v2 B16)
       | _ -> None)
  | "Vsub", [dst; s1; s2] ->
      (match vec_of_expr dst, vec_of_expr s1, vec_of_expr s2 with
       | Some vd, Some v1, Some v2 -> Some (Sem.Vsub vd v1 v2 B16)
       | _ -> None)
  | "Vmul", [dst; s1; s2] ->
      (match vec_of_expr dst, vec_of_expr s1, vec_of_expr s2 with
       | Some vd, Some v1, Some v2 -> Some (Sem.Vmul vd v1 v2 B16)
       | _ -> None)
  | "Vand", [dst; s1; s2] ->
      (match vec_of_expr dst, vec_of_expr s1, vec_of_expr s2 with
       | Some vd, Some v1, Some v2 -> Some (Sem.Vand vd v1 v2)
       | _ -> None)
  | "Vorr", [dst; s1; s2] ->
      (match vec_of_expr dst, vec_of_expr s1, vec_of_expr s2 with
       | Some vd, Some v1, Some v2 -> Some (Sem.Vorr vd v1 v2)
       | _ -> None)
  | "Vrev64", [dst; src] ->
      (match vec_of_expr dst, vec_of_expr src with
       | Some vd, Some vs -> Some (Sem.Vrev64 vd vs B16)
       | _ -> None)
  | "Vrev32", [dst; src] ->
      (match vec_of_expr dst, vec_of_expr src with
       | Some vd, Some vs -> Some (Sem.Vrev32 vd vs B16)
       | _ -> None)
  | "Vrev16", [dst; src] ->
      (match vec_of_expr dst, vec_of_expr src with
       | Some vd, Some vs -> Some (Sem.Vrev16 vd vs B16)
       | _ -> None)
  | "Eor3", [dst; s1; s2; s3] ->
      (match vec_of_expr dst, vec_of_expr s1, vec_of_expr s2, vec_of_expr s3 with
       | Some vd, Some v1, Some v2, Some v3 -> Some (Sem.Eor3 vd v1 v2 v3)
       | _ -> None)
  | "Rax1", [dst; s1; s2] ->
      (match vec_of_expr dst, vec_of_expr s1, vec_of_expr s2 with
       | Some vd, Some v1, Some v2 -> Some (Sem.Rax1 vd v1 v2)
       | _ -> None)
  | "Xar", [dst; s1; s2; EConst imm] ->
      (match vec_of_expr dst, vec_of_expr s1, vec_of_expr s2 with
       | Some vd, Some v1, Some v2 ->
           if imm >= 0 && imm <= 63 then Some (Sem.Xar vd v1 v2 imm)
           else None
       | _ -> None)
  | "Ld1", [dst; buf; EConst offset] ->
      (match vec_of_expr dst, reg_of_expr buf with
       | Some vd, Some rb -> Some (Sem.Ld1 vd B16 (MOffset rb offset))
       | _ -> None)
  | "Ld1", [dst; buf] ->
      (match vec_of_expr dst, reg_of_expr buf with
       | Some vd, Some rb -> Some (Sem.Ld1 vd B16 (MReg rb))
       | _ -> None)
  | "St1", [src; buf; EConst offset] ->
      (match vec_of_expr src, reg_of_expr buf with
       | Some vs, Some rb -> Some (Sem.St1 vs B16 (MOffset rb offset))
       | _ -> None)
  | "St1", [src; buf] ->
      (match vec_of_expr src, reg_of_expr buf with
       | Some vs, Some rb -> Some (Sem.St1 vs B16 (MReg rb))
       | _ -> None)
  | "MsrDit", [EConst n] ->
      Some (Sem.MsrDit (n <> 0))
  | "MsrDit", [EVar s] ->
      Some (Sem.MsrDit (s = "1" || s = "true" || s = "True"))
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
