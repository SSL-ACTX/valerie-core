module Valerie.Riscv64.Semantics

open Valerie.Defs
open FStar.UInt64
module U64 = FStar.UInt64
open Valerie.Riscv64.Machine

type operand =
  | OReg: r:reg -> operand
  | OImm: imm:nat64 -> operand
  | OMem: base:reg -> offset:int -> operand
  | OLabel: label:string -> operand

type ins =
  // Base RV64I
  | Add: dst:reg -> s1:reg -> s2:operand -> ins
  | Addw: dst:reg -> s1:reg -> s2:operand -> ins
  | Sub: dst:reg -> s1:reg -> s2:operand -> ins
  | Subw: dst:reg -> s1:reg -> s2:operand -> ins
  | And: dst:reg -> s1:reg -> s2:operand -> ins
  | Or:  dst:reg -> s1:reg -> s2:operand -> ins
  | Xor: dst:reg -> s1:reg -> s2:operand -> ins
  | Sll: dst:reg -> s1:reg -> s2:operand -> ins
  | Sllw: dst:reg -> s1:reg -> s2:operand -> ins
  | Srl: dst:reg -> s1:reg -> s2:operand -> ins
  | Srlw: dst:reg -> s1:reg -> s2:operand -> ins
  | Sra: dst:reg -> s1:reg -> s2:operand -> ins
  | Sraw: dst:reg -> s1:reg -> s2:operand -> ins
  | Slt: dst:reg -> s1:reg -> s2:operand -> ins
  | Sltu: dst:reg -> s1:reg -> s2:operand -> ins
  // Memory
  | Ld: dst:reg -> base:reg -> offset:int -> ins
  | Lw: dst:reg -> base:reg -> offset:int -> ins
  | Lwu: dst:reg -> base:reg -> offset:int -> ins
  | Lh: dst:reg -> base:reg -> offset:int -> ins
  | Lhu: dst:reg -> base:reg -> offset:int -> ins
  | Lb: dst:reg -> base:reg -> offset:int -> ins
  | Lbu: dst:reg -> base:reg -> offset:int -> ins
  | Sd: src:reg -> base:reg -> offset:int -> ins
  | Sw: src:reg -> base:reg -> offset:int -> ins
  | Sh: src:reg -> base:reg -> offset:int -> ins
  | Sb: src:reg -> base:reg -> offset:int -> ins
  // Control flow & jumps
  | Beq: s1:reg -> s2:reg -> target:string -> ins
  | Bne: s1:reg -> s2:reg -> target:string -> ins
  | Blt: s1:reg -> s2:reg -> target:string -> ins
  | Bge: s1:reg -> s2:reg -> target:string -> ins
  | Bltu: s1:reg -> s2:reg -> target:string -> ins
  | Bgeu: s1:reg -> s2:reg -> target:string -> ins
  | Jal: dst:reg -> target:string -> ins
  | Jalr: dst:reg -> base:reg -> offset:int -> ins
  | Ret: ins
  | Call: target:string -> ins
  | Ecall: ins
  // Upper / Moves
  | Lui: dst:reg -> imm:nat64 -> ins
  | Auipc: dst:reg -> imm:nat64 -> ins
  | Li: dst:reg -> imm:nat64 -> ins
  | Mv: dst:reg -> src:reg -> ins
  // RV64M Multiply / Divide
  | Mul: dst:reg -> s1:reg -> s2:reg -> ins
  | Mulw: dst:reg -> s1:reg -> s2:reg -> ins
  | Mulh: dst:reg -> s1:reg -> s2:reg -> ins
  | Mulhu: dst:reg -> s1:reg -> s2:reg -> ins
  | Mulhsu: dst:reg -> s1:reg -> s2:reg -> ins
  | Div: dst:reg -> s1:reg -> s2:reg -> ins
  | Divw: dst:reg -> s1:reg -> s2:reg -> ins
  | Divu: dst:reg -> s1:reg -> s2:reg -> ins
  | Divuw: dst:reg -> s1:reg -> s2:reg -> ins
  | Rem: dst:reg -> s1:reg -> s2:reg -> ins
  | Remw: dst:reg -> s1:reg -> s2:reg -> ins
  | Remu: dst:reg -> s1:reg -> s2:reg -> ins
  | Remuw: dst:reg -> s1:reg -> s2:reg -> ins
  // Zbb Bitmanipulation & Zk Cryptography
  | Andn: dst:reg -> s1:reg -> s2:reg -> ins
  | Orn: dst:reg -> s1:reg -> s2:reg -> ins
  | Xnor: dst:reg -> s1:reg -> s2:reg -> ins
  | Clz: dst:reg -> src:reg -> ins
  | Ctz: dst:reg -> src:reg -> ins
  | Cpop: dst:reg -> src:reg -> ins
  | Max: dst:reg -> s1:reg -> s2:reg -> ins
  | Maxu: dst:reg -> s1:reg -> s2:reg -> ins
  | Min: dst:reg -> s1:reg -> s2:reg -> ins
  | Minu: dst:reg -> s1:reg -> s2:reg -> ins
  | Rol: dst:reg -> s1:reg -> s2:reg -> ins
  | Ror: dst:reg -> s1:reg -> s2:operand -> ins
  | Rev8: dst:reg -> src:reg -> ins
  | Aes64es: dst:reg -> s1:reg -> s2:reg -> ins
  | Aes64esm: dst:reg -> s1:reg -> s2:reg -> ins
  | Aes64ds: dst:reg -> s1:reg -> s2:reg -> ins
  | Aes64dsm: dst:reg -> s1:reg -> s2:reg -> ins
  | Aes64ks1i: dst:reg -> src:reg -> rnum:nat64 -> ins
  | Aes64ks2: dst:reg -> s1:reg -> s2:reg -> ins
  | Aes64im: dst:reg -> src:reg -> ins
  | Sha256sum0: dst:reg -> src:reg -> ins
  | Sha256sum1: dst:reg -> src:reg -> ins
  | Sha256sig0: dst:reg -> src:reg -> ins
  | Sha256sig1: dst:reg -> src:reg -> ins
  | Sha512sum0: dst:reg -> src:reg -> ins
  | Sha512sum1: dst:reg -> src:reg -> ins
  | Sha512sig0: dst:reg -> src:reg -> ins
  | Sha512sig1: dst:reg -> src:reg -> ins
  | Sm4ed: dst:reg -> s1:reg -> s2:reg -> bs:nat64 -> ins
  | Sm4ks: dst:reg -> s1:reg -> s2:reg -> bs:nat64 -> ins
  | Sm3p0: dst:reg -> src:reg -> ins
  | Sm3p1: dst:reg -> src:reg -> ins

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
  | OReg Zero -> 0uL
  | OReg r -> s.regs r
  | OImm imm -> imm
  | _ -> 0uL

let write_reg (dst: reg) (v: nat64) (s: state) : state =
  if dst = Zero then s
  else { s with regs = (fun r -> if r = dst then v else s.regs r) }

let safe_shift64 (amount: nat64) : (s:nat32{FStar.UInt32.v s < 64}) =
  FStar.UInt32.uint_to_t (FStar.UInt64.v amount % 64)

let eval_ins (i: ins) (s: state) : state =
  match i with
  | Add dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand s2 s in
      write_reg dst (U64.add_mod v1 v2) s
  | Addw dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand s2 s in
      let sum = U64.add_mod v1 v2 in
      let low = U64.logand sum 0xffffffffuL in
      let ext = if U64.v low >= 2147483648 then U64.logor low 0xffffffff00000000uL else low in
      write_reg dst ext s
  | Sub dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand s2 s in
      write_reg dst (U64.sub_mod v1 v2) s
  | Subw dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand s2 s in
      let diff = U64.sub_mod v1 v2 in
      let low = U64.logand diff 0xffffffffuL in
      let ext = if U64.v low >= 2147483648 then U64.logor low 0xffffffff00000000uL else low in
      write_reg dst ext s
  | Xor dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand s2 s in
      write_reg dst (nat64_xor v1 v2) s
  | And dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand s2 s in
      write_reg dst (U64.logand v1 v2) s
  | Or dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand s2 s in
      write_reg dst (U64.logor v1 v2) s
  | Sll dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let amt = safe_shift64 (eval_operand s2 s) in
      write_reg dst (U64.shift_left v1 amt) s
  | Srl dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let amt = safe_shift64 (eval_operand s2 s) in
      write_reg dst (U64.shift_right v1 amt) s
  | Sra dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let amt = safe_shift64 (eval_operand s2 s) in
      write_reg dst (U64.shift_right v1 amt) s
  | Slt dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand s2 s in
      let res = if U64.lt v1 v2 then 1uL else 0uL in
      write_reg dst res s
  | Sltu dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand s2 s in
      let res = if U64.lt v1 v2 then 1uL else 0uL in
      write_reg dst res s
  | Lui dst imm ->
      let shifted = U64.shift_left imm 12ul in
      write_reg dst shifted s
  | Auipc dst imm ->
      let shifted = U64.shift_left imm 12ul in
      write_reg dst shifted s
  | Li dst imm ->
      write_reg dst imm s
  | Mv dst src ->
      write_reg dst (eval_operand (OReg src) s) s
  | Mul dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand (OReg s2) s in
      write_reg dst (U64.mul_mod v1 v2) s
  | Mulw dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand (OReg s2) s in
      let prod = U64.mul_mod v1 v2 in
      let low = U64.logand prod 0xffffffffuL in
      let ext = if U64.v low >= 2147483648 then U64.logor low 0xffffffff00000000uL else low in
      write_reg dst ext s
  | Div dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand (OReg s2) s in
      let res = if v2 = 0uL then 0uL else U64.div v1 v2 in
      write_reg dst res s
  | Divu dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand (OReg s2) s in
      let res = if v2 = 0uL then 0uL else U64.div v1 v2 in
      write_reg dst res s
  | Rem dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand (OReg s2) s in
      let res = if v2 = 0uL then 0uL else U64.rem v1 v2 in
      write_reg dst res s
  | Remu dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand (OReg s2) s in
      let res = if v2 = 0uL then 0uL else U64.rem v1 v2 in
      write_reg dst res s
  | Andn dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand (OReg s2) s in
      write_reg dst (U64.logand v1 (U64.lognot v2)) s
  | Orn dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand (OReg s2) s in
      write_reg dst (U64.logor v1 (U64.lognot v2)) s
  | Xnor dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand (OReg s2) s in
      write_reg dst (U64.lognot (nat64_xor v1 v2)) s
  | Clz dst src ->
      write_reg dst 0uL s
  | Ctz dst src ->
      write_reg dst 0uL s
  | Cpop dst src ->
      write_reg dst 0uL s
  | Max dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand (OReg s2) s in
      write_reg dst (if U64.gte v1 v2 then v1 else v2) s
  | Maxu dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand (OReg s2) s in
      write_reg dst (if U64.gte v1 v2 then v1 else v2) s
  | Min dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand (OReg s2) s in
      write_reg dst (if U64.lt v1 v2 then v1 else v2) s
  | Minu dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let v2 = eval_operand (OReg s2) s in
      write_reg dst (if U64.lt v1 v2 then v1 else v2) s
  | Rol dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let shift = U64.v (eval_operand (OReg s2) s) % 64 in
      let res =
        if shift = 0 then v1
        else
          let s1 : (amt:nat32{FStar.UInt32.v amt < 64}) = FStar.UInt32.uint_to_t shift in
          let s2 : (amt:nat32{FStar.UInt32.v amt < 64}) = FStar.UInt32.uint_to_t (64 - shift) in
          U64.logor (U64.shift_left v1 s1) (U64.shift_right v1 s2)
      in
      write_reg dst res s
  | Ror dst s1 s2 ->
      let v1 = eval_operand (OReg s1) s in
      let shift = U64.v (eval_operand s2 s) % 64 in
      let res =
        if shift = 0 then v1
        else
          let s1 : (amt:nat32{FStar.UInt32.v amt < 64}) = FStar.UInt32.uint_to_t shift in
          let s2 : (amt:nat32{FStar.UInt32.v amt < 64}) = FStar.UInt32.uint_to_t (64 - shift) in
          U64.logor (U64.shift_right v1 s1) (U64.shift_left v1 s2)
      in
      write_reg dst res s
  | Rev8 dst src ->
      let v = eval_operand (OReg src) s in
      let b0 = U64.logand v 0xFFuL in
      let b1 = U64.logand (U64.shift_right v 8ul) 0xFFuL in
      let b2 = U64.logand (U64.shift_right v 16ul) 0xFFuL in
      let b3 = U64.logand (U64.shift_right v 24ul) 0xFFuL in
      let b4 = U64.logand (U64.shift_right v 32ul) 0xFFuL in
      let b5 = U64.logand (U64.shift_right v 40ul) 0xFFuL in
      let b6 = U64.logand (U64.shift_right v 48ul) 0xFFuL in
      let b7 = U64.logand (U64.shift_right v 56ul) 0xFFuL in
      let res = U64.logor (U64.shift_left b0 56ul)
        (U64.logor (U64.shift_left b1 48ul)
          (U64.logor (U64.shift_left b2 40ul)
            (U64.logor (U64.shift_left b3 32ul)
              (U64.logor (U64.shift_left b4 24ul)
                (U64.logor (U64.shift_left b5 16ul)
                  (U64.logor (U64.shift_left b6 8ul) b7)))))) in
      write_reg dst res s
  | Ld dst _ _ | Lw dst _ _ | Lwu dst _ _ | Lh dst _ _ | Lhu dst _ _ | Lb dst _ _ | Lbu dst _ _ ->
      write_reg dst 0uL s
  | Sd _ _ _ | Sw _ _ _ | Sh _ _ _ | Sb _ _ _ -> s
  | Beq _ _ _ | Bne _ _ _ | Blt _ _ _ | Bge _ _ _ | Bltu _ _ _ | Bgeu _ _ _ -> s
  | Jal dst _ -> write_reg dst 0uL s
  | Jalr dst _ _ -> write_reg dst 0uL s
  | Ret | Call _ | Ecall -> s
  | Aes64es dst _ _ | Aes64esm dst _ _ | Aes64ds dst _ _ | Aes64dsm dst _ _ -> write_reg dst 0uL s
  | Aes64ks1i dst _ _ | Aes64ks2 dst _ _ | Aes64im dst _ -> write_reg dst 0uL s
  | Sha256sum0 dst _ | Sha256sum1 dst _ | Sha256sig0 dst _ | Sha256sig1 dst _ -> write_reg dst 0uL s
  | Sha512sum0 dst _ | Sha512sum1 dst _ | Sha512sig0 dst _ | Sha512sig1 dst _ -> write_reg dst 0uL s
  | Sm4ed dst _ _ _ | Sm4ks dst _ _ _ | Sm3p0 dst _ | Sm3p1 dst _ -> write_reg dst 0uL s
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
    | IfElse ifCond ifTrue ifFalse ->
        if eval_ocmp ifCond s then
          eval_code ifTrue (fuel - 1) s
        else
          eval_code ifFalse (fuel - 1) s
    | While whileCond whileBody ->
        if eval_ocmp whileCond s then
          match eval_code whileBody (fuel - 1) s with
          | None -> None
          | Some s' -> eval_code (While whileCond whileBody) (fuel - 1) s'
        else
          Some s
