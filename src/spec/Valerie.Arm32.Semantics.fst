module Valerie.Arm32.Semantics

open Valerie.Defs
open FStar.UInt32
module U32 = FStar.UInt32
open Valerie.Arm32.Machine

type operand =
  | OReg: r:reg -> operand
  | OSreg: s:sreg -> operand
  | ODreg: d:dreg -> operand
  | OQreg: q:qreg -> operand
  | OImm: imm:nat32 -> operand
  | OMem: m:maddr -> operand
  | ORegList: regs:list reg -> operand

type ins =
  | Mov: dst:reg -> src:operand -> ins
  | Movs: dst:reg -> src:operand -> ins
  | Movw: dst:reg -> imm:nat32 -> ins
  | Movt: dst:reg -> imm:nat32 -> ins
  | Add: dst:reg -> s1:reg -> s2:operand -> ins
  | Adds: dst:reg -> s1:reg -> s2:operand -> ins
  | Adc: dst:reg -> s1:reg -> s2:operand -> ins
  | Adcs: dst:reg -> s1:reg -> s2:operand -> ins
  | Sub: dst:reg -> s1:reg -> s2:operand -> ins
  | Subs: dst:reg -> s1:reg -> s2:operand -> ins
  | Sbc: dst:reg -> s1:reg -> s2:operand -> ins
  | Sbcs: dst:reg -> s1:reg -> s2:operand -> ins
  | Rsb: dst:reg -> s1:reg -> s2:operand -> ins
  | Rsbs: dst:reg -> s1:reg -> s2:operand -> ins
  | Rsc: dst:reg -> s1:reg -> s2:operand -> ins
  | Rscs: dst:reg -> s1:reg -> s2:operand -> ins
  | Mul: dst:reg -> s1:reg -> s2:reg -> ins
  | Mla: dst:reg -> s1:reg -> s2:reg -> ra:reg -> ins
  | Mls: dst:reg -> s1:reg -> s2:reg -> ra:reg -> ins
  | Smull: rdlo:reg -> rdhi:reg -> rn:reg -> rm:reg -> ins
  | Umull: rdlo:reg -> rdhi:reg -> rn:reg -> rm:reg -> ins
  | Sdiv: dst:reg -> rn:reg -> rm:reg -> ins
  | Udiv: dst:reg -> rn:reg -> rm:reg -> ins
  | And: dst:reg -> s1:reg -> s2:operand -> ins
  | Ands: dst:reg -> s1:reg -> s2:operand -> ins
  | Orr: dst:reg -> s1:reg -> s2:operand -> ins
  | Orrs: dst:reg -> s1:reg -> s2:operand -> ins
  | Eor: dst:reg -> s1:reg -> s2:operand -> ins
  | Eors: dst:reg -> s1:reg -> s2:operand -> ins
  | Bic: dst:reg -> s1:reg -> s2:operand -> ins
  | Bics: dst:reg -> s1:reg -> s2:operand -> ins
  | Orn: dst:reg -> s1:reg -> s2:operand -> ins
  | Mvn: dst:reg -> src:operand -> ins
  | Mvns: dst:reg -> src:operand -> ins
  | Lsl: dst:reg -> rn:reg -> rm:operand -> ins
  | Lsr: dst:reg -> rn:reg -> rm:operand -> ins
  | Asr: dst:reg -> rn:reg -> rm:operand -> ins
  | Ror: dst:reg -> rn:reg -> rm:operand -> ins
  | Clz: dst:reg -> src:reg -> ins
  | Rbit: dst:reg -> src:reg -> ins
  | Rev: dst:reg -> src:reg -> ins
  | Rev16: dst:reg -> src:reg -> ins
  | Revsh: dst:reg -> src:reg -> ins
  | Ubfx: dst:reg -> rn:reg -> lsb:nat32 -> width:nat32 -> ins
  | Sbfx: dst:reg -> rn:reg -> lsb:nat32 -> width:nat32 -> ins
  | Cmp: rn:reg -> op:operand -> ins
  | Cmn: rn:reg -> op:operand -> ins
  | Tst: rn:reg -> op:operand -> ins
  | Teq: rn:reg -> op:operand -> ins
  | Sxtb: dst:reg -> src:reg -> ins
  | Sxth: dst:reg -> src:reg -> ins
  | Uxtb: dst:reg -> src:reg -> ins
  | Uxth: dst:reg -> src:reg -> ins
  | Ldr: dst:reg -> mem:maddr -> ins
  | Ldrb: dst:reg -> mem:maddr -> ins
  | Ldrh: dst:reg -> mem:maddr -> ins
  | Ldrsb: dst:reg -> mem:maddr -> ins
  | Ldrsh: dst:reg -> mem:maddr -> ins
  | Ldrd: rt:reg -> rt2:reg -> mem:maddr -> ins
  | Str: src:reg -> mem:maddr -> ins
  | Strb: src:reg -> mem:maddr -> ins
  | Strh: src:reg -> mem:maddr -> ins
  | Strd: rt:reg -> rt2:reg -> mem:maddr -> ins
  | Push: regs:list reg -> ins
  | Pop: regs:list reg -> ins
  | B: target:string -> ins
  | Bc: cond:cond_code -> target:string -> ins
  | Bl: target:string -> ins
  | Bx: target:reg -> ins
  | Blx: target:reg -> ins
  | Vmov: dst:operand -> src:operand -> ins
  | VaddF32: dst:sreg -> s1:sreg -> s2:sreg -> ins
  | VaddF64: dst:dreg -> s1:dreg -> s2:dreg -> ins
  | VsubF32: dst:sreg -> s1:sreg -> s2:sreg -> ins
  | VsubF64: dst:dreg -> s1:dreg -> s2:dreg -> ins
  | VmulF32: dst:sreg -> s1:sreg -> s2:sreg -> ins
  | VmulF64: dst:dreg -> s1:dreg -> s2:dreg -> ins
  | Vldr: dst:sreg -> mem:maddr -> ins
  | Vstr: src:sreg -> mem:maddr -> ins
  | Vpush: regs:list sreg -> ins
  | Vpop: regs:list sreg -> ins

type ocmp =
  | OEq: o1:operand -> o2:operand -> ocmp
  | ONe: o1:operand -> o2:operand -> ocmp
  | OLt: o1:operand -> o2:operand -> ocmp
  | OGe: o1:operand -> o2:operand -> ocmp

let safe_shift_amount (amount: nat32) : (s:nat32{U32.v s < 32}) =
  U32.uint_to_t (U32.v amount % 32)

let safe_shl (v: nat32) (amt: nat32) : nat32 =
  U32.shift_left v (safe_shift_amount amt)

let safe_shr (v: nat32) (amt: nat32) : nat32 =
  U32.shift_right v (safe_shift_amount amt)

type code =
  | Ins: ins:ins -> code
  | Block: block:list code -> code
  | IfElse: ifCond:ocmp -> ifTrue:code -> ifFalse:code -> code
  | While: whileCond:ocmp -> whileBody:code -> code

let eval_operand (o: operand) (s: state) : nat32 =
  match o with
  | OReg r -> s.regs r
  | OSreg sr -> s.sregs sr
  | OImm imm -> imm
  | _ -> 0ul

let eval_ins (i: ins) (s: state) : state =
  match i with
  | Mov dst src ->
      let v = eval_operand src s in
      { s with regs = (fun r -> if r = dst then v else s.regs r) }
  | Movs dst src ->
      let v = eval_operand src s in
      let fl = { s.flags with z = (v = 0ul); n = (U32.v v >= 2147483648) } in
      { s with regs = (fun r -> if r = dst then v else s.regs r); flags = fl }
  | Movw dst imm ->
      let v = U32.logand imm 0xfffful in
      { s with regs = (fun r -> if r = dst then v else s.regs r) }
  | Movt dst imm ->
      let cur = s.regs dst in
      let v = U32.logor (U32.logand cur 0xfffful) (U32.shift_left imm 16ul) in
      { s with regs = (fun r -> if r = dst then v else s.regs r) }
  | Add dst s1 s2 ->
      let v1 = s.regs s1 in
      let v2 = eval_operand s2 s in
      let res = U32.add_mod v1 v2 in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Adds dst s1 s2 ->
      let v1 = s.regs s1 in
      let v2 = eval_operand s2 s in
      let res = U32.add_mod v1 v2 in
      let fl = { s.flags with z = (res = 0ul); n = (U32.v res >= 2147483648) } in
      { s with regs = (fun r -> if r = dst then res else s.regs r); flags = fl }
  | Adc dst s1 s2 ->
      let v1 = s.regs s1 in
      let v2 = eval_operand s2 s in
      let carry = if s.flags.c then 1ul else 0ul in
      let res = U32.add_mod (U32.add_mod v1 v2) carry in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Adcs dst s1 s2 ->
      let v1 = s.regs s1 in
      let v2 = eval_operand s2 s in
      let carry = if s.flags.c then 1ul else 0ul in
      let res = U32.add_mod (U32.add_mod v1 v2) carry in
      let fl = { s.flags with z = (res = 0ul); n = (U32.v res >= 2147483648) } in
      { s with regs = (fun r -> if r = dst then res else s.regs r); flags = fl }
  | Sub dst s1 s2 ->
      let v1 = s.regs s1 in
      let v2 = eval_operand s2 s in
      let res = U32.sub_mod v1 v2 in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Subs dst s1 s2 ->
      let v1 = s.regs s1 in
      let v2 = eval_operand s2 s in
      let res = U32.sub_mod v1 v2 in
      let fl = { s.flags with z = (res = 0ul); n = (U32.v res >= 2147483648) } in
      { s with regs = (fun r -> if r = dst then res else s.regs r); flags = fl }
  | Sbc dst s1 s2 ->
      let v1 = s.regs s1 in
      let v2 = eval_operand s2 s in
      let not_carry = if s.flags.c then 0ul else 1ul in
      let res = U32.sub_mod (U32.sub_mod v1 v2) not_carry in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Sbcs dst s1 s2 ->
      let v1 = s.regs s1 in
      let v2 = eval_operand s2 s in
      let not_carry = if s.flags.c then 0ul else 1ul in
      let res = U32.sub_mod (U32.sub_mod v1 v2) not_carry in
      let fl = { s.flags with z = (res = 0ul); n = (U32.v res >= 2147483648) } in
      { s with regs = (fun r -> if r = dst then res else s.regs r); flags = fl }
  | Rsb dst s1 s2 ->
      let v1 = s.regs s1 in
      let v2 = eval_operand s2 s in
      let res = U32.sub_mod v2 v1 in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Rsbs dst s1 s2 ->
      let v1 = s.regs s1 in
      let v2 = eval_operand s2 s in
      let res = U32.sub_mod v2 v1 in
      let fl = { s.flags with z = (res = 0ul); n = (U32.v res >= 2147483648) } in
      { s with regs = (fun r -> if r = dst then res else s.regs r); flags = fl }
  | Rsc dst s1 s2 ->
      let v1 = s.regs s1 in
      let v2 = eval_operand s2 s in
      let not_carry = if s.flags.c then 0ul else 1ul in
      let res = U32.sub_mod (U32.sub_mod v2 v1) not_carry in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Rscs dst s1 s2 ->
      let v1 = s.regs s1 in
      let v2 = eval_operand s2 s in
      let not_carry = if s.flags.c then 0ul else 1ul in
      let res = U32.sub_mod (U32.sub_mod v2 v1) not_carry in
      let fl = { s.flags with z = (res = 0ul); n = (U32.v res >= 2147483648) } in
      { s with regs = (fun r -> if r = dst then res else s.regs r); flags = fl }
  | Mul dst s1 s2 ->
      let v1 = s.regs s1 in
      let v2 = s.regs s2 in
      let res = U32.mul_mod v1 v2 in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Mla dst s1 s2 ra ->
      let v1 = s.regs s1 in
      let v2 = s.regs s2 in
      let a = s.regs ra in
      let res = U32.add_mod (U32.mul_mod v1 v2) a in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Mls dst s1 s2 ra ->
      let v1 = s.regs s1 in
      let v2 = s.regs s2 in
      let a = s.regs ra in
      let res = U32.sub_mod a (U32.mul_mod v1 v2) in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Smull rdlo rdhi rn rm ->
      let v1 = Int.Cast.uint32_to_uint64 (s.regs rn) in
      let v2 = Int.Cast.uint32_to_uint64 (s.regs rm) in
      let prod = FStar.UInt64.mul_mod v1 v2 in
      let r_lo = Int.Cast.uint64_to_uint32 (FStar.UInt64.logand prod 0xffffffffuL) in
      let r_hi = Int.Cast.uint64_to_uint32 (FStar.UInt64.shift_right prod 32ul) in
      { s with regs = (fun r -> if r = rdlo then r_lo else if r = rdhi then r_hi else s.regs r) }
  | Umull rdlo rdhi rn rm ->
      let v1 = Int.Cast.uint32_to_uint64 (s.regs rn) in
      let v2 = Int.Cast.uint32_to_uint64 (s.regs rm) in
      let prod = FStar.UInt64.mul_mod v1 v2 in
      let r_lo = Int.Cast.uint64_to_uint32 (FStar.UInt64.logand prod 0xffffffffuL) in
      let r_hi = Int.Cast.uint64_to_uint32 (FStar.UInt64.shift_right prod 32ul) in
      { s with regs = (fun r -> if r = rdlo then r_lo else if r = rdhi then r_hi else s.regs r) }
  | Sdiv dst rn rm ->
      let d = s.regs rm in
      let res = if d = 0ul then 0ul else U32.div (s.regs rn) d in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Udiv dst rn rm ->
      let d = s.regs rm in
      let res = if d = 0ul then 0ul else U32.div (s.regs rn) d in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | And dst s1 s2 ->
      let v1 = s.regs s1 in
      let v2 = eval_operand s2 s in
      let res = U32.logand v1 v2 in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Ands dst s1 s2 ->
      let v1 = s.regs s1 in
      let v2 = eval_operand s2 s in
      let res = U32.logand v1 v2 in
      let fl = { s.flags with z = (res = 0ul); n = (U32.v res >= 2147483648) } in
      { s with regs = (fun r -> if r = dst then res else s.regs r); flags = fl }
  | Orr dst s1 s2 ->
      let v1 = s.regs s1 in
      let v2 = eval_operand s2 s in
      let res = U32.logor v1 v2 in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Orrs dst s1 s2 ->
      let v1 = s.regs s1 in
      let v2 = eval_operand s2 s in
      let res = U32.logor v1 v2 in
      let fl = { s.flags with z = (res = 0ul); n = (U32.v res >= 2147483648) } in
      { s with regs = (fun r -> if r = dst then res else s.regs r); flags = fl }
  | Eor dst s1 s2 ->
      let v1 = s.regs s1 in
      let v2 = eval_operand s2 s in
      let res = U32.logxor v1 v2 in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Eors dst s1 s2 ->
      let v1 = s.regs s1 in
      let v2 = eval_operand s2 s in
      let res = U32.logxor v1 v2 in
      let fl = { s.flags with z = (res = 0ul); n = (U32.v res >= 2147483648) } in
      { s with regs = (fun r -> if r = dst then res else s.regs r); flags = fl }
  | Bic dst s1 s2 ->
      let v1 = s.regs s1 in
      let v2 = eval_operand s2 s in
      let res = U32.logand v1 (U32.lognot v2) in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Bics dst s1 s2 ->
      let v1 = s.regs s1 in
      let v2 = eval_operand s2 s in
      let res = U32.logand v1 (U32.lognot v2) in
      let fl = { s.flags with z = (res = 0ul); n = (U32.v res >= 2147483648) } in
      { s with regs = (fun r -> if r = dst then res else s.regs r); flags = fl }
  | Orn dst s1 s2 ->
      let v1 = s.regs s1 in
      let v2 = eval_operand s2 s in
      let res = U32.logor v1 (U32.lognot v2) in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Mvn dst src ->
      let v = eval_operand src s in
      let res = U32.lognot v in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Mvns dst src ->
      let v = eval_operand src s in
      let res = U32.lognot v in
      let fl = { s.flags with z = (res = 0ul); n = (U32.v res >= 2147483648) } in
      { s with regs = (fun r -> if r = dst then res else s.regs r); flags = fl }
  | Lsl dst rn rm ->
      let v = s.regs rn in
      let res = safe_shl v (eval_operand rm s) in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Lsr dst rn rm ->
      let v = s.regs rn in
      let res = safe_shr v (eval_operand rm s) in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Asr dst rn rm ->
      let v = s.regs rn in
      let res = safe_shr v (eval_operand rm s) in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Ror dst rn rm ->
      let v = s.regs rn in
      let shift = U32.v (eval_operand rm s) % 32 in
      let res =
        if shift = 0 then v
        else
          let s1 : (s:nat32{U32.v s < 32}) = U32.uint_to_t shift in
          let s2 : (s:nat32{U32.v s < 32}) = U32.uint_to_t (32 - shift) in
          U32.logor (U32.shift_right v s1) (U32.shift_left v s2)
      in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Clz dst src ->
      { s with regs = (fun r -> if r = dst then 0ul else s.regs r) }
  | Rbit dst src ->
      { s with regs = (fun r -> if r = dst then 0ul else s.regs r) }
  | Rev dst src ->
      let v = s.regs src in
      let b0 = U32.shift_left (U32.logand v 0xfful) 24ul in
      let b1 = U32.shift_left (U32.logand (U32.shift_right v 8ul) 0xfful) 16ul in
      let b2 = U32.shift_left (U32.logand (U32.shift_right v 16ul) 0xfful) 8ul in
      let b3 = U32.logand (U32.shift_right v 24ul) 0xfful in
      let res = U32.logor (U32.logor b0 b1) (U32.logor b2 b3) in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Rev16 dst src ->
      let v = s.regs src in
      let b0 = U32.shift_left (U32.logand v 0xfful) 8ul in
      let b1 = U32.logand (U32.shift_right v 8ul) 0xfful in
      let b2 = U32.shift_left (U32.logand (U32.shift_right v 16ul) 0xfful) 8ul in
      let b3 = U32.logand (U32.shift_right v 24ul) 0xfful in
      let res = U32.logor (U32.shift_left (U32.logor b2 b3) 16ul) (U32.logor b0 b1) in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Revsh dst src ->
      let v = s.regs src in
      let b0 = U32.shift_left (U32.logand v 0xfful) 8ul in
      let b1 = U32.logand (U32.shift_right v 8ul) 0xfful in
      let res = U32.logor b0 b1 in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Ubfx dst rn lsb width ->
      let v = s.regs rn in
      let shifted = safe_shr v lsb in
      let w_val = U32.v width in
      let mask =
        if w_val >= 32 then 0xfffffffful
        else
          let w_shift : (s:nat32{U32.v s < 32}) = U32.uint_to_t w_val in
          U32.sub_mod (U32.shift_left 1ul w_shift) 1ul
      in
      let res = U32.logand shifted mask in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Sbfx dst rn lsb width ->
      let v = s.regs rn in
      let shifted = safe_shr v lsb in
      let w_val = U32.v width in
      let mask =
        if w_val >= 32 then 0xfffffffful
        else
          let w_shift : (s:nat32{U32.v s < 32}) = U32.uint_to_t w_val in
          U32.sub_mod (U32.shift_left 1ul w_shift) 1ul
      in
      let res = U32.logand shifted mask in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Cmp rn op ->
      let v1 = s.regs rn in
      let v2 = eval_operand op s in
      let diff = U32.sub_mod v1 v2 in
      let fl = {
        n = (U32.v diff >= 2147483648);
        z = (v1 = v2);
        c = (U32.v v1 >= U32.v v2);
        v = false;
      } in
      { s with flags = fl }
  | Cmn rn op ->
      let v1 = s.regs rn in
      let v2 = eval_operand op s in
      let sum = U32.add_mod v1 v2 in
      let fl = {
        n = (U32.v sum >= 2147483648);
        z = (sum = 0ul);
        c = (U32.v sum < U32.v v1);
        v = false;
      } in
      { s with flags = fl }
  | Tst rn op ->
      let v1 = s.regs rn in
      let v2 = eval_operand op s in
      let res = U32.logand v1 v2 in
      let fl = { s.flags with z = (res = 0ul); n = (U32.v res >= 2147483648) } in
      { s with flags = fl }
  | Teq rn op ->
      let v1 = s.regs rn in
      let v2 = eval_operand op s in
      let res = U32.logxor v1 v2 in
      let fl = { s.flags with z = (res = 0ul); n = (U32.v res >= 2147483648) } in
      { s with flags = fl }
  | Sxtb dst src ->
      let v = U32.logand (s.regs src) 0xfful in
      let res = if U32.v v >= 128 then U32.logor v 0xffffff00ul else v in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Sxth dst src ->
      let v = U32.logand (s.regs src) 0xfffful in
      let res = if U32.v v >= 32768 then U32.logor v 0xffff0000ul else v in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Uxtb dst src ->
      let res = U32.logand (s.regs src) 0xfful in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Uxth dst src ->
      let res = U32.logand (s.regs src) 0xfffful in
      { s with regs = (fun r -> if r = dst then res else s.regs r) }
  | Ldr dst _ | Ldrb dst _ | Ldrh dst _ | Ldrsb dst _ | Ldrsh dst _ ->
      { s with regs = (fun r -> if r = dst then 0ul else s.regs r) }
  | Ldrd rt rt2 _ ->
      { s with regs = (fun r -> if r = rt || r = rt2 then 0ul else s.regs r) }
  | Str _ _ | Strb _ _ | Strh _ _ | Strd _ _ _ | Push _ | Pop _ -> s
  | B _ | Bc _ _ | Bl _ | Bx _ | Blx _ -> s
  | Vmov dst src -> s
  | VaddF32 dst s1 s2 | VsubF32 dst s1 s2 | VmulF32 dst s1 s2 -> s
  | VaddF64 dst s1 s2 | VsubF64 dst s1 s2 | VmulF64 dst s1 s2 -> s
  | Vldr _ _ | Vstr _ _ | Vpush _ | Vpop _ -> s

let eval_ocmp (c: ocmp) (s: state) : bool =
  match c with
  | OEq o1 o2 -> (U32.v (eval_operand o1 s) = U32.v (eval_operand o2 s))
  | ONe o1 o2 -> (U32.v (eval_operand o1 s) <> U32.v (eval_operand o2 s))
  | OLt o1 o2 -> (U32.v (eval_operand o1 s) < U32.v (eval_operand o2 s))
  | OGe o1 o2 -> (U32.v (eval_operand o1 s) >= U32.v (eval_operand o2 s))

let rec eval_code (c: code) (fuel: nat) (s: state) : Tot (option state) (decreases fuel) =
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
