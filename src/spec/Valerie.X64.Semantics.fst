module Valerie.X64.Semantics

open Valerie.Defs
open FStar.UInt64
module U64 = FStar.UInt64
open FStar.UInt128
module U128 = FStar.UInt128
open Valerie.X64.Machine

type operand =
  | OReg: r:reg -> w:reg_width -> operand
  | OXmm: x:xmm -> operand
  | OYmm: y:ymm -> operand
  | OImm: imm:nat64 -> operand
  | OMem: m:maddr -> operand

type ins =
  // Scalar Arithmetic & Logic
  | Mov64: dst:reg -> src:operand -> ins
  | Mov32: dst:reg -> src:operand -> ins
  | Add64: dst:reg -> src:operand -> ins
  | Adc64: dst:reg -> src:operand -> ins
  | Adcx64: dst:reg -> src:operand -> ins
  | Adox64: dst:reg -> src:operand -> ins
  | Sub64: dst:reg -> src:operand -> ins
  | Sbb64: dst:reg -> src:operand -> ins
  | Xor64: dst:reg -> src:operand -> ins
  | And64: dst:reg -> src:operand -> ins
  | Or64: dst:reg -> src:operand -> ins
  | Not64: dst:reg -> ins
  | Neg64: dst:reg -> ins
  | Shl64: dst:reg -> count:operand -> ins
  | Shr64: dst:reg -> count:operand -> ins
  | Sar64: dst:reg -> count:operand -> ins
  | Rol64: dst:reg -> count:operand -> ins
  | Ror64: dst:reg -> count:operand -> ins
  | Bswap64: dst:reg -> ins
  | Cmp64: s1:reg -> s2:operand -> ins
  | Test64: s1:reg -> s2:operand -> ins
  | Lea64: dst:reg -> addr:maddr -> ins
  | Imul64: dst:reg -> src:operand -> ins
  | Mul64: src:operand -> ins
  | Mulx64: hi:reg -> lo:reg -> src:operand -> ins

  // Bit Manipulation (BMI1 / BMI2)
  | Andn64: dst:reg -> s1:reg -> s2:operand -> ins
  | Rorx64: dst:reg -> src:operand -> imm:nat8 -> ins
  | Sarx64: dst:reg -> src:operand -> count:reg -> ins
  | Shlx64: dst:reg -> src:operand -> count:reg -> ins
  | Shrx64: dst:reg -> src:operand -> count:reg -> ins
  | Bsf64: dst:reg -> src:operand -> ins
  | Bsr64: dst:reg -> src:operand -> ins
  | Tzcnt64: dst:reg -> src:operand -> ins
  | Lzcnt64: dst:reg -> src:operand -> ins
  | Popcnt64: dst:reg -> src:operand -> ins

  // Conditional Moves
  | Cmov64: cond:cond_code -> dst:reg -> src:operand -> ins

  // Control Flow
  | Jmp: label:string -> ins
  | Jcc: cond:cond_code -> label:string -> ins
  | Call: label:string -> ins
  | Ret: ins
  | Push64: src:operand -> ins
  | Pop64: dst:reg -> ins

  // Cryptographic Extensions (AES-NI & PCLMULQDQ)
  | Aesenc: dst:xmm -> src:xmm -> ins
  | Aesenclast: dst:xmm -> src:xmm -> ins
  | Aesdec: dst:xmm -> src:xmm -> ins
  | Aesdeclast: dst:xmm -> src:xmm -> ins
  | Aesimc: dst:xmm -> src:xmm -> ins
  | Aeskeygenassist: dst:xmm -> src:xmm -> rcon:nat8 -> ins
  | Vaesenc: dst:xmm -> s1:xmm -> s2:xmm -> ins
  | Vaesdec: dst:xmm -> s1:xmm -> s2:xmm -> ins
  | Pclmulqdq: dst:xmm -> src:xmm -> imm:nat8 -> ins
  | Vpclmulqdq: dst:xmm -> s1:xmm -> s2:xmm -> imm:nat8 -> ins

  // Cryptographic Extensions (Intel SHA-NI)
  | Sha1rnds4: dst:xmm -> src:xmm -> imm:nat8 -> ins
  | Sha1nexte: dst:xmm -> src:xmm -> ins
  | Sha1msg1: dst:xmm -> src:xmm -> ins
  | Sha1msg2: dst:xmm -> src:xmm -> ins
  | Sha256rnds2: dst:xmm -> src:xmm -> ins
  | Sha256msg1: dst:xmm -> src:xmm -> ins
  | Sha256msg2: dst:xmm -> src:xmm -> ins

  // SSE / AVX Vector SIMD
  | Movdqu: dst:xmm -> src:operand -> ins
  | Movdqa: dst:xmm -> src:operand -> ins
  | Vmovdqu: dst:xmm -> src:operand -> ins
  | Vmovdqa: dst:xmm -> src:operand -> ins
  | Pxor: dst:xmm -> src:xmm -> ins
  | Vpxor: dst:xmm -> s1:xmm -> s2:xmm -> ins
  | Pand: dst:xmm -> src:xmm -> ins
  | Por: dst:xmm -> src:xmm -> ins
  | Paddd: dst:xmm -> src:xmm -> ins
  | Psubd: dst:xmm -> src:xmm -> ins
  | Paddq: dst:xmm -> src:xmm -> ins
  | Psubq: dst:xmm -> src:xmm -> ins
  | Paddb: dst:xmm -> src:xmm -> ins
  | Psubb: dst:xmm -> src:xmm -> ins
  | Paddw: dst:xmm -> src:xmm -> ins
  | Psubw: dst:xmm -> src:xmm -> ins
  | Pslld: dst:xmm -> count:nat8 -> ins
  | Psrld: dst:xmm -> count:nat8 -> ins
  | Psllq: dst:xmm -> count:nat8 -> ins
  | Psrlq: dst:xmm -> count:nat8 -> ins
  | Pshufd: dst:xmm -> src:xmm -> imm:nat8 -> ins
  | Pshufb: dst:xmm -> src:xmm -> ins
  | Palignr: dst:xmm -> src:xmm -> imm:nat8 -> ins
  | Punpcklbw: dst:xmm -> src:xmm -> ins
  | Punpckhbw: dst:xmm -> src:xmm -> ins
  | Punpcklwd: dst:xmm -> src:xmm -> ins
  | Punpckhwd: dst:xmm -> src:xmm -> ins
  | Punpckldq: dst:xmm -> src:xmm -> ins
  | Punpckhdq: dst:xmm -> src:xmm -> ins
  | Punpcklqdq: dst:xmm -> src:xmm -> ins
  | Punpckhqdq: dst:xmm -> src:xmm -> ins
  | Pcmpeqd: dst:xmm -> src:xmm -> ins
  | Pcmpeqb: dst:xmm -> src:xmm -> ins
  | Pextrq: dst:reg -> src:xmm -> imm:nat8 -> ins
  | Pextrd: dst:reg -> src:xmm -> imm:nat8 -> ins
  | Pinsrd: dst:xmm -> src:reg -> imm:nat8 -> ins
  | Pinsrq: dst:xmm -> src:reg -> imm:nat8 -> ins

type ocmp =
  | OEq: o1:operand -> o2:operand -> ocmp
  | ONe: o1:operand -> o2:operand -> ocmp
  | OLt: o1:operand -> o2:operand -> ocmp
  | OGe: o1:operand -> o2:operand -> ocmp

type code =
  | Ins: ins:ins -> code
  | Block: block:list code -> code
  | IfElse: ifCond:ocmp -> ifTrue:code -> ifFalse:code -> code
  | While: whileCond:ocmp -> whileBody:code -> code

let eval_operand (o: operand) (s: state) : nat64 =
  match o with
  | OReg r _ -> s.regs r
  | OImm imm -> imm
  | _ -> 0uL

let eval_ins (i: ins) (s: state) : state =
  match i with
  | Mov64 dst src ->
      let v = eval_operand src s in
      { s with regs = (fun r -> if r = dst then v else s.regs r) }
  | Mov32 dst src ->
      let v = eval_operand src s in
      { s with regs = (fun r -> if r = dst then v else s.regs r) }
  | Add64 dst src ->
      let v1 = s.regs dst in
      let v2 = eval_operand src s in
      let res = U64.add_mod v1 v2 in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Sub64 dst src ->
      let v1 = s.regs dst in
      let v2 = eval_operand src s in
      let res = U64.sub_mod v1 v2 in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Xor64 dst src ->
      let v1 = s.regs dst in
      let v2 = eval_operand src s in
      let res = nat64_xor v1 v2 in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | And64 dst src ->
      let v1 = s.regs dst in
      let v2 = eval_operand src s in
      let res = nat64_and v1 v2 in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Or64 dst src ->
      let v1 = s.regs dst in
      let v2 = eval_operand src s in
      let res = nat64_or v1 v2 in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Pxor dst src ->
      let v1 = s.xmms dst in
      let v2 = s.xmms src in
      let res = nat128_xor v1 v2 in
      { s with xmms = (fun x -> if x = dst then res else s.xmms x) }
  | _ -> s

let eval_ocmp (c: ocmp) (s: state) : bool =
  match c with
  | OEq o1 o2 -> eval_operand o1 s = eval_operand o2 s
  | ONe o1 o2 -> eval_operand o1 s <> eval_operand o2 s
  | OLt o1 o2 -> U64.lt (eval_operand o1 s) (eval_operand o2 s)
  | OGe o1 o2 -> U64.gte (eval_operand o1 s) (eval_operand o2 s)

let rec eval_code (c: code) (fuel: nat) (s: state) : Tot (option state) (decreases %[fuel; c]) =
  if fuel = 0 then None
  else
    match c with
    | Ins i -> Some (eval_ins i s)
    | Block [] -> Some s
    | Block (hd::tl) ->
        (match eval_code hd (fuel - 1) s with
         | None -> None
         | Some s' -> eval_code (Block tl) (fuel - 1) s')
    | IfElse cond ifTrue ifFalse ->
        if eval_ocmp cond s then eval_code ifTrue (fuel - 1) s
        else eval_code ifFalse (fuel - 1) s
    | While cond body ->
        if eval_ocmp cond s then
          (match eval_code body (fuel - 1) s with
           | None -> None
           | Some s' -> eval_code (While cond body) (fuel - 1) s')
        else Some s
