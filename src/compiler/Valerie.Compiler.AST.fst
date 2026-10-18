module Valerie.Compiler.AST

open FStar.UInt32
module U32 = FStar.UInt32
open FStar.UInt64
module U64 = FStar.UInt64

type id = string

type arch_target =
  | TargetAArch64
  | TargetArm32
  | TargetX64
  | TargetRiscv64

let arch_target_to_u32 (t: arch_target) : U32.t =
  match t with
  | TargetAArch64 -> 0ul
  | TargetArm32   -> 1ul
  | TargetX64     -> 2ul
  | TargetRiscv64 -> 3ul

let u32_to_arch_target (tag: U32.t) : arch_target =
  if tag = 0ul then TargetAArch64
  else if tag = 1ul then TargetArm32
  else if tag = 2ul then TargetX64
  else TargetRiscv64


type vaf_typ =
  | TVar : id -> vaf_typ
  | TNat : vaf_typ
  | TInt : vaf_typ
  | TBool : vaf_typ
  | TU8 : vaf_typ
  | TU32 : vaf_typ
  | TU64 : vaf_typ
  | TSeq : vaf_typ -> vaf_typ
  | TBuffer : elem_typ:vaf_typ -> size:nat -> vaf_typ
  | TTuple : list vaf_typ -> vaf_typ

/// Low*-clean register operand: uses fixed-width U32 tag instead of string pattern matching
type reg_operand =
  | RegNumbered : U32.t -> reg_operand

/// Low*-clean operand: uses fixed-width machine words
type operand =
  | OReg : reg_operand -> operand
  | OImm : U64.t -> operand
  | OMemBuffer : reg_operand -> U32.t -> operand

type uop =
  | UNot
  | UNeg
  | UOld

type bop =
  | BAdd
  | BSub
  | BMul
  | BDiv
  | BMod
  | BAnd
  | BOr
  | BEq
  | BNe
  | BLt
  | BGt
  | BLe
  | BGe
  | BImply
  | BEquiv

type expr =
  | EVar : id -> expr
  | EConst : int -> expr
  | EBoolConst : bool -> expr
  | EUop : uop -> expr -> expr
  | EBop : bop -> expr -> expr -> expr
  | EApply : id -> list expr -> expr

type spec_kind =
  | SpecRequires : expr -> spec_kind
  | SpecEnsures : expr -> spec_kind
  | SpecModifies : list reg_operand -> spec_kind
  | SpecBufferPerm : reg_operand -> nat -> spec_kind

type pformal_kind =
  | FormalGhost
  | FormalInline
  | FormalIn
  | FormalOut
  | FormalInOut

type pformal = {
  pformal_name : id;
  pformal_typ : vaf_typ;
  pformal_kind : pformal_kind;
}

type ins_opcode =
  | OpMov
  | OpAdd
  | OpSub
  | OpXor
  | OpAnd
  | OpOr
  | OpRev
  | OpLoad
  | OpStore

let opcode_to_u32 (op: ins_opcode) : U32.t =
  match op with
  | OpMov   -> 0ul
  | OpAdd   -> 1ul
  | OpSub   -> 2ul
  | OpXor   -> 3ul
  | OpAnd   -> 4ul
  | OpOr    -> 5ul
  | OpRev   -> 6ul
  | OpLoad  -> 7ul
  | OpStore -> 8ul

let u32_to_opcode (tag: U32.t) : ins_opcode =
  if tag = 0ul then OpMov
  else if tag = 1ul then OpAdd
  else if tag = 2ul then OpSub
  else if tag = 3ul then OpXor
  else if tag = 4ul then OpAnd
  else if tag = 5ul then OpOr
  else if tag = 6ul then OpRev
  else if tag = 7ul then OpLoad
  else OpStore


type stmt =
  | SIns : ins_opcode -> list operand -> stmt
  | SAssign : id -> expr -> stmt
  | SAssert : expr -> stmt
  | SAssume : expr -> stmt
  | SBlock : list stmt -> stmt
  | SIfElse : expr -> list stmt -> list stmt -> stmt
  | SWhile : expr -> list expr -> list stmt -> stmt
  | SCall : id -> list expr -> stmt

type proc_decl = {
  proc_name : id;
  proc_target : arch_target;
  proc_formals : list pformal;
  proc_returns : list pformal;
  proc_specs : list spec_kind;
  proc_body : list stmt;
  proc_is_ins : bool;
  proc_ins_term : string;
  proc_modifies : list id;
}

type module_decl = {
  mod_name : id;
  mod_includes : list id;
  mod_procs : list proc_decl;
}
