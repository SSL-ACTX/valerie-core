module Valerie.Compiler.Lower.X64

open Valerie.Defs
open FStar.UInt64
module U64 = FStar.UInt64
open Valerie.Compiler.AST
open Valerie.X64.Machine
module Sem = Valerie.X64.Semantics

let reg_of_operand (r:reg_operand) : reg =
  match r with
  | RegNumbered n ->
      if n = 0ul then Rax
      else if n = 1ul then Rbx
      else if n = 2ul then Rcx
      else if n = 3ul then Rdx
      else if n = 4ul then Rsi
      else if n = 5ul then Rdi
      else if n = 6ul then Rbp
      else if n = 7ul then Rsp
      else if n = 8ul then R8
      else if n = 9ul then R9
      else if n = 10ul then R10
      else if n = 11ul then R11
      else if n = 12ul then R12
      else if n = 13ul then R13
      else if n = 14ul then R14
      else R15

let lower_operand (op:operand) : Sem.operand =
  match op with
  | OReg r -> Sem.OReg (reg_of_operand r) R64
  | OImm i -> Sem.OImm i
  | OMemBuffer _ _ -> Sem.OImm 0uL

let lower_ins (op:ins_opcode) (args:list operand) : option Sem.ins =
  match op, args with
  | OpMov, [dst; src] ->
      (match dst with
       | OReg r -> Some (Sem.Mov64 (reg_of_operand r) (lower_operand src))
       | _ -> None)
  | OpAdd, [dst; src] ->
      (match dst with
       | OReg rd -> Some (Sem.Add64 (reg_of_operand rd) (lower_operand src))
       | _ -> None)
  | OpSub, [dst; src] ->
      (match dst with
       | OReg rd -> Some (Sem.Sub64 (reg_of_operand rd) (lower_operand src))
       | _ -> None)
  | OpXor, [dst; src] ->
      (match dst with
       | OReg rd -> Some (Sem.Xor64 (reg_of_operand rd) (lower_operand src))
       | _ -> None)
  | OpAnd, [dst; src] ->
      (match dst with
       | OReg rd -> Some (Sem.And64 (reg_of_operand rd) (lower_operand src))
       | _ -> None)
  | _ -> None

let reg_of_name (s: string) : option reg =
  if s = "rax" then Some Rax
  else if s = "rbx" then Some Rbx
  else if s = "rcx" then Some Rcx
  else if s = "rdx" then Some Rdx
  else if s = "rsi" then Some Rsi
  else if s = "rdi" then Some Rdi
  else if s = "rbp" then Some Rbp
  else if s = "rsp" then Some Rsp
  else if s = "r8" then Some R8
  else if s = "r9" then Some R9
  else if s = "r10" then Some R10
  else if s = "r11" then Some R11
  else if s = "r12" then Some R12
  else if s = "r13" then Some R13
  else if s = "r14" then Some R14
  else if s = "r15" then Some R15
  else None

let xmm_of_name (s: string) : option xmm =
  if s = "xmm0" then Some Xmm0
  else if s = "xmm1" then Some Xmm1
  else if s = "xmm2" then Some Xmm2
  else if s = "xmm3" then Some Xmm3
  else if s = "xmm4" then Some Xmm4
  else if s = "xmm5" then Some Xmm5
  else if s = "xmm6" then Some Xmm6
  else if s = "xmm7" then Some Xmm7
  else if s = "xmm8" then Some Xmm8
  else if s = "xmm9" then Some Xmm9
  else if s = "xmm10" then Some Xmm10
  else if s = "xmm11" then Some Xmm11
  else if s = "xmm12" then Some Xmm12
  else if s = "xmm13" then Some Xmm13
  else if s = "xmm14" then Some Xmm14
  else if s = "xmm15" then Some Xmm15
  else None

let reg_of_expr (e: expr) : option reg =
  match e with
  | EVar s -> reg_of_name s
  | _ -> None

let xmm_of_expr (e: expr) : option xmm =
  match e with
  | EVar s -> xmm_of_name s
  | _ -> None


let cond_of_name (s: string) : option cond_code =
  if s = "e" || s = "E" || s = "eq" || s = "EQ" || s = "z" || s = "Z" then Some CondE
  else if s = "ne" || s = "NE" || s = "nz" || s = "NZ" then Some CondNE
  else if s = "b" || s = "B" || s = "c" || s = "C" then Some CondB
  else if s = "be" || s = "BE" || s = "na" || s = "NA" then Some CondBE
  else if s = "a" || s = "A" || s = "nbe" || s = "NBE" then Some CondA
  else if s = "ae" || s = "AE" || s = "nc" || s = "NC" || s = "nb" || s = "NB" then Some CondAE
  else if s = "l" || s = "L" || s = "nge" || s = "NGE" then Some CondL
  else if s = "le" || s = "LE" || s = "ng" || s = "NG" then Some CondLE
  else if s = "g" || s = "G" || s = "nle" || s = "NLE" then Some CondG
  else if s = "ge" || s = "GE" || s = "nl" || s = "NL" then Some CondGE
  else None

let cond_of_expr (e: expr) : option cond_code =
  match e with
  | EVar s -> cond_of_name s
  | _ -> None

let operand_of_expr (e: expr) : option Sem.operand =
  match e with
  | EVar s ->
      (match reg_of_name s with
       | Some r -> Some (Sem.OReg r R64)
       | None -> None)
  | EConst n ->
      if n >= 0 && n <= 18446744073709551615 then
        Some (Sem.OImm (FStar.UInt64.uint_to_t n))
      else None
  | _ -> None

let lower_call (target: id) (args: list expr) : option Sem.ins =
  match target, args with
  | "Mov64", [dst; src]
  | "Mov", [dst; src] ->
      (match reg_of_expr dst, operand_of_expr src with
       | Some rd, Some op -> Some (Sem.Mov64 rd op)
       | _ -> None)
  | "Add64", [dst; src]
  | "Add", [dst; src] ->
      (match reg_of_expr dst, operand_of_expr src with
       | Some rd, Some op -> Some (Sem.Add64 rd op)
       | _ -> None)
  | "Sub64", [dst; src]
  | "Sub", [dst; src] ->
      (match reg_of_expr dst, operand_of_expr src with
       | Some rd, Some op -> Some (Sem.Sub64 rd op)
       | _ -> None)
  | "Xor64", [dst; src]
  | "Xor", [dst; src] ->
      (match reg_of_expr dst, operand_of_expr src with
       | Some rd, Some op -> Some (Sem.Xor64 rd op)
       | _ -> None)
  | "Aesenc", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Aesenc xd xs)
       | _ -> None)
  | "Aesdec", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Aesdec xd xs)
       | _ -> None)
  | "Pxor", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Pxor xd xs)
       | _ -> None)
  | "Paddd", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Paddd xd xs)
       | _ -> None)
  | "Pclmulqdq", [dst; src; EConst imm] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs ->
           if imm >= 0 && imm <= 255 then
             Some (Sem.Pclmulqdq xd xs (FStar.UInt8.uint_to_t imm))
           else None
       | _ -> None)

  | "Mov32", [dst; src] ->
      (match reg_of_expr dst, operand_of_expr src with
       | Some rd, Some op -> Some (Sem.Mov32 rd op)
       | _ -> None)
  | "Adc64", [dst; src]
  | "Adc", [dst; src] ->
      (match reg_of_expr dst, operand_of_expr src with
       | Some rd, Some op -> Some (Sem.Adc64 rd op)
       | _ -> None)
  | "Adcx64", [dst; src]
  | "Adcx", [dst; src] ->
      (match reg_of_expr dst, operand_of_expr src with
       | Some rd, Some op -> Some (Sem.Adcx64 rd op)
       | _ -> None)
  | "Adox64", [dst; src]
  | "Adox", [dst; src] ->
      (match reg_of_expr dst, operand_of_expr src with
       | Some rd, Some op -> Some (Sem.Adox64 rd op)
       | _ -> None)
  | "Sbb64", [dst; src]
  | "Sbb", [dst; src] ->
      (match reg_of_expr dst, operand_of_expr src with
       | Some rd, Some op -> Some (Sem.Sbb64 rd op)
       | _ -> None)
  | "Or64", [dst; src]
  | "Or", [dst; src] ->
      (match reg_of_expr dst, operand_of_expr src with
       | Some rd, Some op -> Some (Sem.Or64 rd op)
       | _ -> None)
  | "Not64", [dst]
  | "Not", [dst] ->
      (match reg_of_expr dst with
       | Some rd -> Some (Sem.Not64 rd)
       | _ -> None)
  | "Neg64", [dst]
  | "Neg", [dst] ->
      (match reg_of_expr dst with
       | Some rd -> Some (Sem.Neg64 rd)
       | _ -> None)
  | "Shl64", [dst; count]
  | "Shl", [dst; count] ->
      (match reg_of_expr dst, operand_of_expr count with
       | Some rd, Some op -> Some (Sem.Shl64 rd op)
       | _ -> None)
  | "Shr64", [dst; count]
  | "Shr", [dst; count] ->
      (match reg_of_expr dst, operand_of_expr count with
       | Some rd, Some op -> Some (Sem.Shr64 rd op)
       | _ -> None)
  | "Sar64", [dst; count]
  | "Sar", [dst; count] ->
      (match reg_of_expr dst, operand_of_expr count with
       | Some rd, Some op -> Some (Sem.Sar64 rd op)
       | _ -> None)
  | "Rol64", [dst; count]
  | "Rol", [dst; count] ->
      (match reg_of_expr dst, operand_of_expr count with
       | Some rd, Some op -> Some (Sem.Rol64 rd op)
       | _ -> None)
  | "Ror64", [dst; count]
  | "Ror", [dst; count] ->
      (match reg_of_expr dst, operand_of_expr count with
       | Some rd, Some op -> Some (Sem.Ror64 rd op)
       | _ -> None)
  | "Bswap64", [dst]
  | "Bswap", [dst] ->
      (match reg_of_expr dst with
       | Some rd -> Some (Sem.Bswap64 rd)
       | _ -> None)
  | "Cmp64", [s1; s2]
  | "Cmp", [s1; s2] ->
      (match reg_of_expr s1, operand_of_expr s2 with
       | Some r1, Some op -> Some (Sem.Cmp64 r1 op)
       | _ -> None)
  | "Test64", [s1; s2]
  | "Test", [s1; s2] ->
      (match reg_of_expr s1, operand_of_expr s2 with
       | Some r1, Some op -> Some (Sem.Test64 r1 op)
       | _ -> None)
  | "Lea64", [dst; base; EConst offset]
  | "Lea", [dst; base; EConst offset] ->
      (match reg_of_expr dst, reg_of_expr base with
       | Some rd, Some rb -> Some (Sem.Lea64 rd (MReg rb offset))
       | _ -> None)
  | "Imul64", [dst; src]
  | "Imul", [dst; src] ->
      (match reg_of_expr dst, operand_of_expr src with
       | Some rd, Some op -> Some (Sem.Imul64 rd op)
       | _ -> None)
  | "Mul64", [src]
  | "Mul", [src] ->
      (match operand_of_expr src with
       | Some op -> Some (Sem.Mul64 op)
       | _ -> None)
  | "Mulx64", [hi; lo; src]
  | "Mulx", [hi; lo; src] ->
      (match reg_of_expr hi, reg_of_expr lo, operand_of_expr src with
       | Some rhi, Some rlo, Some op -> Some (Sem.Mulx64 rhi rlo op)
       | _ -> None)
  | "Andn64", [dst; s1; s2]
  | "Andn", [dst; s1; s2] ->
      (match reg_of_expr dst, reg_of_expr s1, operand_of_expr s2 with
       | Some rd, Some rn, Some op -> Some (Sem.Andn64 rd rn op)
       | _ -> None)
  | "Rorx64", [dst; src; EConst imm]
  | "Rorx", [dst; src; EConst imm] ->
      (match reg_of_expr dst, operand_of_expr src with
       | Some rd, Some op ->
           if imm >= 0 && imm <= 255 then
             Some (Sem.Rorx64 rd op (FStar.UInt8.uint_to_t imm))
           else None
       | _ -> None)
  | "Sarx64", [dst; src; count]
  | "Sarx", [dst; src; count] ->
      (match reg_of_expr dst, operand_of_expr src, reg_of_expr count with
       | Some rd, Some op, Some rc -> Some (Sem.Sarx64 rd op rc)
       | _ -> None)
  | "Shlx64", [dst; src; count]
  | "Shlx", [dst; src; count] ->
      (match reg_of_expr dst, operand_of_expr src, reg_of_expr count with
       | Some rd, Some op, Some rc -> Some (Sem.Shlx64 rd op rc)
       | _ -> None)
  | "Shrx64", [dst; src; count]
  | "Shrx", [dst; src; count] ->
      (match reg_of_expr dst, operand_of_expr src, reg_of_expr count with
       | Some rd, Some op, Some rc -> Some (Sem.Shrx64 rd op rc)
       | _ -> None)
  | "Bsf64", [dst; src]
  | "Bsf", [dst; src] ->
      (match reg_of_expr dst, operand_of_expr src with
       | Some rd, Some op -> Some (Sem.Bsf64 rd op)
       | _ -> None)
  | "Bsr64", [dst; src]
  | "Bsr", [dst; src] ->
      (match reg_of_expr dst, operand_of_expr src with
       | Some rd, Some op -> Some (Sem.Bsr64 rd op)
       | _ -> None)
  | "Tzcnt64", [dst; src]
  | "Tzcnt", [dst; src] ->
      (match reg_of_expr dst, operand_of_expr src with
       | Some rd, Some op -> Some (Sem.Tzcnt64 rd op)
       | _ -> None)
  | "Lzcnt64", [dst; src]
  | "Lzcnt", [dst; src] ->
      (match reg_of_expr dst, operand_of_expr src with
       | Some rd, Some op -> Some (Sem.Lzcnt64 rd op)
       | _ -> None)
  | "Popcnt64", [dst; src]
  | "Popcnt", [dst; src] ->
      (match reg_of_expr dst, operand_of_expr src with
       | Some rd, Some op -> Some (Sem.Popcnt64 rd op)
       | _ -> None)
  | "Cmov64", [c; dst; src]
  | "Cmov", [c; dst; src] ->
      (match cond_of_expr c, reg_of_expr dst, operand_of_expr src with
       | Some cond, Some rd, Some op -> Some (Sem.Cmov64 cond rd op)
       | _ -> None)
  | "Jmp", [EVar target] -> Some (Sem.Jmp target)
  | "Jcc", [c; EVar target] ->
      (match cond_of_expr c with
       | Some cond -> Some (Sem.Jcc cond target)
       | _ -> None)
  | "Ret", [] -> Some Sem.Ret
  | "Push64", [src]
  | "Push", [src] ->
      (match operand_of_expr src with
       | Some op -> Some (Sem.Push64 op)
       | _ -> None)
  | "Pop64", [dst]
  | "Pop", [dst] ->
      (match reg_of_expr dst with
       | Some rd -> Some (Sem.Pop64 rd)
       | _ -> None)
  | "Aesenclast", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Aesenclast xd xs)
       | _ -> None)
  | "Aesdeclast", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Aesdeclast xd xs)
       | _ -> None)
  | "Aesimc", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Aesimc xd xs)
       | _ -> None)
  | "Aeskeygenassist", [dst; src; EConst rcon] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs ->
           if rcon >= 0 && rcon <= 255 then
             Some (Sem.Aeskeygenassist xd xs (FStar.UInt8.uint_to_t rcon))
           else None
       | _ -> None)
  | "Vaesenc", [dst; s1; s2] ->
      (match xmm_of_expr dst, xmm_of_expr s1, xmm_of_expr s2 with
       | Some xd, Some x1, Some x2 -> Some (Sem.Vaesenc xd x1 x2)
       | _ -> None)
  | "Vaesdec", [dst; s1; s2] ->
      (match xmm_of_expr dst, xmm_of_expr s1, xmm_of_expr s2 with
       | Some xd, Some x1, Some x2 -> Some (Sem.Vaesdec xd x1 x2)
       | _ -> None)
  | "Vpclmulqdq", [dst; s1; s2; EConst imm] ->
      (match xmm_of_expr dst, xmm_of_expr s1, xmm_of_expr s2 with
       | Some xd, Some x1, Some x2 ->
           if imm >= 0 && imm <= 255 then
             Some (Sem.Vpclmulqdq xd x1 x2 (FStar.UInt8.uint_to_t imm))
           else None
       | _ -> None)
  | "Sha1rnds4", [dst; src; EConst imm] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs ->
           if imm >= 0 && imm <= 255 then
             Some (Sem.Sha1rnds4 xd xs (FStar.UInt8.uint_to_t imm))
           else None
       | _ -> None)
  | "Sha1nexte", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Sha1nexte xd xs)
       | _ -> None)
  | "Sha1msg1", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Sha1msg1 xd xs)
       | _ -> None)
  | "Sha1msg2", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Sha1msg2 xd xs)
       | _ -> None)
  | "Sha256rnds2", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Sha256rnds2 xd xs)
       | _ -> None)
  | "Sha256msg1", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Sha256msg1 xd xs)
       | _ -> None)
  | "Sha256msg2", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Sha256msg2 xd xs)
       | _ -> None)
  | "Movdqu", [dst; src] ->
      (match xmm_of_expr dst, operand_of_expr src with
       | Some xd, Some op -> Some (Sem.Movdqu xd op)
       | _ -> None)
  | "Movdqa", [dst; src] ->
      (match xmm_of_expr dst, operand_of_expr src with
       | Some xd, Some op -> Some (Sem.Movdqa xd op)
       | _ -> None)
  | "Vmovdqu", [dst; src] ->
      (match xmm_of_expr dst, operand_of_expr src with
       | Some xd, Some op -> Some (Sem.Vmovdqu xd op)
       | _ -> None)
  | "Vmovdqa", [dst; src] ->
      (match xmm_of_expr dst, operand_of_expr src with
       | Some xd, Some op -> Some (Sem.Vmovdqa xd op)
       | _ -> None)
  | "Vpxor", [dst; s1; s2] ->
      (match xmm_of_expr dst, xmm_of_expr s1, xmm_of_expr s2 with
       | Some xd, Some x1, Some x2 -> Some (Sem.Vpxor xd x1 x2)
       | _ -> None)
  | "Pand", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Pand xd xs)
       | _ -> None)
  | "Por", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Por xd xs)
       | _ -> None)
  | "Psubd", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Psubd xd xs)
       | _ -> None)
  | "Paddq", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Paddq xd xs)
       | _ -> None)
  | "Psubq", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Psubq xd xs)
       | _ -> None)
  | "Paddb", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Paddb xd xs)
       | _ -> None)
  | "Psubb", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Psubb xd xs)
       | _ -> None)
  | "Paddw", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Paddw xd xs)
       | _ -> None)
  | "Psubw", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Psubw xd xs)
       | _ -> None)
  | "Pslld", [dst; EConst count] ->
      (match xmm_of_expr dst with
       | Some xd ->
           if count >= 0 && count <= 255 then
             Some (Sem.Pslld xd (FStar.UInt8.uint_to_t count))
           else None
       | _ -> None)
  | "Psrld", [dst; EConst count] ->
      (match xmm_of_expr dst with
       | Some xd ->
           if count >= 0 && count <= 255 then
             Some (Sem.Psrld xd (FStar.UInt8.uint_to_t count))
           else None
       | _ -> None)
  | "Psllq", [dst; EConst count] ->
      (match xmm_of_expr dst with
       | Some xd ->
           if count >= 0 && count <= 255 then
             Some (Sem.Psllq xd (FStar.UInt8.uint_to_t count))
           else None
       | _ -> None)
  | "Psrlq", [dst; EConst count] ->
      (match xmm_of_expr dst with
       | Some xd ->
           if count >= 0 && count <= 255 then
             Some (Sem.Psrlq xd (FStar.UInt8.uint_to_t count))
           else None
       | _ -> None)
  | "Pshufd", [dst; src; EConst imm] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs ->
           if imm >= 0 && imm <= 255 then
             Some (Sem.Pshufd xd xs (FStar.UInt8.uint_to_t imm))
           else None
       | _ -> None)
  | "Pshufb", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Pshufb xd xs)
       | _ -> None)
  | "Palignr", [dst; src; EConst imm] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs ->
           if imm >= 0 && imm <= 255 then
             Some (Sem.Palignr xd xs (FStar.UInt8.uint_to_t imm))
           else None
       | _ -> None)
  | "Punpcklbw", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Punpcklbw xd xs)
       | _ -> None)
  | "Punpckhbw", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Punpckhbw xd xs)
       | _ -> None)
  | "Punpcklwd", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Punpcklwd xd xs)
       | _ -> None)
  | "Punpckhwd", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Punpckhwd xd xs)
       | _ -> None)
  | "Punpckldq", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Punpckldq xd xs)
       | _ -> None)
  | "Punpckhdq", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Punpckhdq xd xs)
       | _ -> None)
  | "Punpcklqdq", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Punpcklqdq xd xs)
       | _ -> None)
  | "Punpckhqdq", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Punpckhqdq xd xs)
       | _ -> None)
  | "Pcmpeqd", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Pcmpeqd xd xs)
       | _ -> None)
  | "Pcmpeqb", [dst; src] ->
      (match xmm_of_expr dst, xmm_of_expr src with
       | Some xd, Some xs -> Some (Sem.Pcmpeqb xd xs)
       | _ -> None)
  | "Pextrq", [dst; src; EConst imm] ->
      (match reg_of_expr dst, xmm_of_expr src with
       | Some rd, Some xs ->
           if imm >= 0 && imm <= 255 then
             Some (Sem.Pextrq rd xs (FStar.UInt8.uint_to_t imm))
           else None
       | _ -> None)
  | "Pextrd", [dst; src; EConst imm] ->
      (match reg_of_expr dst, xmm_of_expr src with
       | Some rd, Some xs ->
           if imm >= 0 && imm <= 255 then
             Some (Sem.Pextrd rd xs (FStar.UInt8.uint_to_t imm))
           else None
       | _ -> None)
  | "Pinsrd", [dst; src; EConst imm] ->
      (match xmm_of_expr dst, reg_of_expr src with
       | Some xd, Some rs ->
           if imm >= 0 && imm <= 255 then
             Some (Sem.Pinsrd xd rs (FStar.UInt8.uint_to_t imm))
           else None
       | _ -> None)
  | "Pinsrq", [dst; src; EConst imm] ->
      (match xmm_of_expr dst, reg_of_expr src with
       | Some xd, Some rs ->
           if imm >= 0 && imm <= 255 then
             Some (Sem.Pinsrq xd rs (FStar.UInt8.uint_to_t imm))
           else None
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
