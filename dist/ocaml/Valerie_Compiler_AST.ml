open Prims
type id = Prims.string
type arch_target =
  | TargetAArch64 
  | TargetArm32 
  | TargetX64 
  | TargetRiscv64 
let uu___is_TargetAArch64 (projectee : arch_target) : Prims.bool=
  match projectee with | TargetAArch64 -> true | uu___ -> false
let uu___is_TargetArm32 (projectee : arch_target) : Prims.bool=
  match projectee with | TargetArm32 -> true | uu___ -> false
let uu___is_TargetX64 (projectee : arch_target) : Prims.bool=
  match projectee with | TargetX64 -> true | uu___ -> false
let uu___is_TargetRiscv64 (projectee : arch_target) : Prims.bool=
  match projectee with | TargetRiscv64 -> true | uu___ -> false
let arch_target_to_u32 (t : arch_target) : FStar_UInt32.t=
  match t with
  | TargetAArch64 -> Stdint.Uint32.zero
  | TargetArm32 -> Stdint.Uint32.one
  | TargetX64 -> Stdint.Uint32.of_int 2
  | TargetRiscv64 -> Stdint.Uint32.of_int 3
let u32_to_arch_target (tag : FStar_UInt32.t) : arch_target=
  if tag = Stdint.Uint32.zero
  then TargetAArch64
  else
    if tag = Stdint.Uint32.one
    then TargetArm32
    else if tag = (Stdint.Uint32.of_int 2) then TargetX64 else TargetRiscv64
type vaf_typ =
  | TVar of id 
  | TNat 
  | TInt 
  | TBool 
  | TU8 
  | TU32 
  | TU64 
  | TSeq of vaf_typ 
  | TBuffer of vaf_typ * Prims.nat 
  | TTuple of vaf_typ Prims.list 
let uu___is_TVar (projectee : vaf_typ) : Prims.bool=
  match projectee with | TVar _0 -> true | uu___ -> false
let __proj__TVar__item___0 (projectee : vaf_typ) : id=
  match projectee with | TVar _0 -> _0
let uu___is_TNat (projectee : vaf_typ) : Prims.bool=
  match projectee with | TNat -> true | uu___ -> false
let uu___is_TInt (projectee : vaf_typ) : Prims.bool=
  match projectee with | TInt -> true | uu___ -> false
let uu___is_TBool (projectee : vaf_typ) : Prims.bool=
  match projectee with | TBool -> true | uu___ -> false
let uu___is_TU8 (projectee : vaf_typ) : Prims.bool=
  match projectee with | TU8 -> true | uu___ -> false
let uu___is_TU32 (projectee : vaf_typ) : Prims.bool=
  match projectee with | TU32 -> true | uu___ -> false
let uu___is_TU64 (projectee : vaf_typ) : Prims.bool=
  match projectee with | TU64 -> true | uu___ -> false
let uu___is_TSeq (projectee : vaf_typ) : Prims.bool=
  match projectee with | TSeq _0 -> true | uu___ -> false
let __proj__TSeq__item___0 (projectee : vaf_typ) : vaf_typ=
  match projectee with | TSeq _0 -> _0
let uu___is_TBuffer (projectee : vaf_typ) : Prims.bool=
  match projectee with | TBuffer (elem_typ, size) -> true | uu___ -> false
let __proj__TBuffer__item__elem_typ (projectee : vaf_typ) : vaf_typ=
  match projectee with | TBuffer (elem_typ, size) -> elem_typ
let __proj__TBuffer__item__size (projectee : vaf_typ) : Prims.nat=
  match projectee with | TBuffer (elem_typ, size) -> size
let uu___is_TTuple (projectee : vaf_typ) : Prims.bool=
  match projectee with | TTuple _0 -> true | uu___ -> false
let __proj__TTuple__item___0 (projectee : vaf_typ) : vaf_typ Prims.list=
  match projectee with | TTuple _0 -> _0
type reg_operand =
  | RegNumbered of FStar_UInt32.t 
let uu___is_RegNumbered (projectee : reg_operand) : Prims.bool= true
let __proj__RegNumbered__item___0 (projectee : reg_operand) : FStar_UInt32.t=
  match projectee with | RegNumbered _0 -> _0
type operand =
  | OReg of reg_operand 
  | OImm of FStar_UInt64.t 
  | OMemBuffer of reg_operand * FStar_UInt32.t 
let uu___is_OReg (projectee : operand) : Prims.bool=
  match projectee with | OReg _0 -> true | uu___ -> false
let __proj__OReg__item___0 (projectee : operand) : reg_operand=
  match projectee with | OReg _0 -> _0
let uu___is_OImm (projectee : operand) : Prims.bool=
  match projectee with | OImm _0 -> true | uu___ -> false
let __proj__OImm__item___0 (projectee : operand) : FStar_UInt64.t=
  match projectee with | OImm _0 -> _0
let uu___is_OMemBuffer (projectee : operand) : Prims.bool=
  match projectee with | OMemBuffer (_0, _1) -> true | uu___ -> false
let __proj__OMemBuffer__item___0 (projectee : operand) : reg_operand=
  match projectee with | OMemBuffer (_0, _1) -> _0
let __proj__OMemBuffer__item___1 (projectee : operand) : FStar_UInt32.t=
  match projectee with | OMemBuffer (_0, _1) -> _1
type uop =
  | UNot 
  | UNeg 
  | UOld 
let uu___is_UNot (projectee : uop) : Prims.bool=
  match projectee with | UNot -> true | uu___ -> false
let uu___is_UNeg (projectee : uop) : Prims.bool=
  match projectee with | UNeg -> true | uu___ -> false
let uu___is_UOld (projectee : uop) : Prims.bool=
  match projectee with | UOld -> true | uu___ -> false
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
let uu___is_BAdd (projectee : bop) : Prims.bool=
  match projectee with | BAdd -> true | uu___ -> false
let uu___is_BSub (projectee : bop) : Prims.bool=
  match projectee with | BSub -> true | uu___ -> false
let uu___is_BMul (projectee : bop) : Prims.bool=
  match projectee with | BMul -> true | uu___ -> false
let uu___is_BDiv (projectee : bop) : Prims.bool=
  match projectee with | BDiv -> true | uu___ -> false
let uu___is_BMod (projectee : bop) : Prims.bool=
  match projectee with | BMod -> true | uu___ -> false
let uu___is_BAnd (projectee : bop) : Prims.bool=
  match projectee with | BAnd -> true | uu___ -> false
let uu___is_BOr (projectee : bop) : Prims.bool=
  match projectee with | BOr -> true | uu___ -> false
let uu___is_BEq (projectee : bop) : Prims.bool=
  match projectee with | BEq -> true | uu___ -> false
let uu___is_BNe (projectee : bop) : Prims.bool=
  match projectee with | BNe -> true | uu___ -> false
let uu___is_BLt (projectee : bop) : Prims.bool=
  match projectee with | BLt -> true | uu___ -> false
let uu___is_BGt (projectee : bop) : Prims.bool=
  match projectee with | BGt -> true | uu___ -> false
let uu___is_BLe (projectee : bop) : Prims.bool=
  match projectee with | BLe -> true | uu___ -> false
let uu___is_BGe (projectee : bop) : Prims.bool=
  match projectee with | BGe -> true | uu___ -> false
let uu___is_BImply (projectee : bop) : Prims.bool=
  match projectee with | BImply -> true | uu___ -> false
let uu___is_BEquiv (projectee : bop) : Prims.bool=
  match projectee with | BEquiv -> true | uu___ -> false
type expr =
  | EVar of id 
  | EConst of Prims.int 
  | EBoolConst of Prims.bool 
  | EUop of uop * expr 
  | EBop of bop * expr * expr 
  | EApply of id * expr Prims.list 
let uu___is_EVar (projectee : expr) : Prims.bool=
  match projectee with | EVar _0 -> true | uu___ -> false
let __proj__EVar__item___0 (projectee : expr) : id=
  match projectee with | EVar _0 -> _0
let uu___is_EConst (projectee : expr) : Prims.bool=
  match projectee with | EConst _0 -> true | uu___ -> false
let __proj__EConst__item___0 (projectee : expr) : Prims.int=
  match projectee with | EConst _0 -> _0
let uu___is_EBoolConst (projectee : expr) : Prims.bool=
  match projectee with | EBoolConst _0 -> true | uu___ -> false
let __proj__EBoolConst__item___0 (projectee : expr) : Prims.bool=
  match projectee with | EBoolConst _0 -> _0
let uu___is_EUop (projectee : expr) : Prims.bool=
  match projectee with | EUop (_0, _1) -> true | uu___ -> false
let __proj__EUop__item___0 (projectee : expr) : uop=
  match projectee with | EUop (_0, _1) -> _0
let __proj__EUop__item___1 (projectee : expr) : expr=
  match projectee with | EUop (_0, _1) -> _1
let uu___is_EBop (projectee : expr) : Prims.bool=
  match projectee with | EBop (_0, _1, _2) -> true | uu___ -> false
let __proj__EBop__item___0 (projectee : expr) : bop=
  match projectee with | EBop (_0, _1, _2) -> _0
let __proj__EBop__item___1 (projectee : expr) : expr=
  match projectee with | EBop (_0, _1, _2) -> _1
let __proj__EBop__item___2 (projectee : expr) : expr=
  match projectee with | EBop (_0, _1, _2) -> _2
let uu___is_EApply (projectee : expr) : Prims.bool=
  match projectee with | EApply (_0, _1) -> true | uu___ -> false
let __proj__EApply__item___0 (projectee : expr) : id=
  match projectee with | EApply (_0, _1) -> _0
let __proj__EApply__item___1 (projectee : expr) : expr Prims.list=
  match projectee with | EApply (_0, _1) -> _1
type spec_kind =
  | SpecRequires of expr 
  | SpecEnsures of expr 
  | SpecModifies of reg_operand Prims.list 
  | SpecBufferPerm of reg_operand * Prims.nat 
let uu___is_SpecRequires (projectee : spec_kind) : Prims.bool=
  match projectee with | SpecRequires _0 -> true | uu___ -> false
let __proj__SpecRequires__item___0 (projectee : spec_kind) : expr=
  match projectee with | SpecRequires _0 -> _0
let uu___is_SpecEnsures (projectee : spec_kind) : Prims.bool=
  match projectee with | SpecEnsures _0 -> true | uu___ -> false
let __proj__SpecEnsures__item___0 (projectee : spec_kind) : expr=
  match projectee with | SpecEnsures _0 -> _0
let uu___is_SpecModifies (projectee : spec_kind) : Prims.bool=
  match projectee with | SpecModifies _0 -> true | uu___ -> false
let __proj__SpecModifies__item___0 (projectee : spec_kind) :
  reg_operand Prims.list= match projectee with | SpecModifies _0 -> _0
let uu___is_SpecBufferPerm (projectee : spec_kind) : Prims.bool=
  match projectee with | SpecBufferPerm (_0, _1) -> true | uu___ -> false
let __proj__SpecBufferPerm__item___0 (projectee : spec_kind) : reg_operand=
  match projectee with | SpecBufferPerm (_0, _1) -> _0
let __proj__SpecBufferPerm__item___1 (projectee : spec_kind) : Prims.nat=
  match projectee with | SpecBufferPerm (_0, _1) -> _1
type pformal_kind =
  | FormalGhost 
  | FormalInline 
  | FormalIn 
  | FormalOut 
  | FormalInOut 
let uu___is_FormalGhost (projectee : pformal_kind) : Prims.bool=
  match projectee with | FormalGhost -> true | uu___ -> false
let uu___is_FormalInline (projectee : pformal_kind) : Prims.bool=
  match projectee with | FormalInline -> true | uu___ -> false
let uu___is_FormalIn (projectee : pformal_kind) : Prims.bool=
  match projectee with | FormalIn -> true | uu___ -> false
let uu___is_FormalOut (projectee : pformal_kind) : Prims.bool=
  match projectee with | FormalOut -> true | uu___ -> false
let uu___is_FormalInOut (projectee : pformal_kind) : Prims.bool=
  match projectee with | FormalInOut -> true | uu___ -> false
type pformal =
  {
  pformal_name: id ;
  pformal_typ: vaf_typ ;
  pformal_kind: pformal_kind }
let __proj__Mkpformal__item__pformal_name (projectee : pformal) : id=
  match projectee with
  | { pformal_name; pformal_typ; pformal_kind = pformal_kind1;_} ->
      pformal_name
let __proj__Mkpformal__item__pformal_typ (projectee : pformal) : vaf_typ=
  match projectee with
  | { pformal_name; pformal_typ; pformal_kind = pformal_kind1;_} ->
      pformal_typ
let __proj__Mkpformal__item__pformal_kind (projectee : pformal) :
  pformal_kind=
  match projectee with
  | { pformal_name; pformal_typ; pformal_kind = pformal_kind1;_} ->
      pformal_kind1
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
let uu___is_OpMov (projectee : ins_opcode) : Prims.bool=
  match projectee with | OpMov -> true | uu___ -> false
let uu___is_OpAdd (projectee : ins_opcode) : Prims.bool=
  match projectee with | OpAdd -> true | uu___ -> false
let uu___is_OpSub (projectee : ins_opcode) : Prims.bool=
  match projectee with | OpSub -> true | uu___ -> false
let uu___is_OpXor (projectee : ins_opcode) : Prims.bool=
  match projectee with | OpXor -> true | uu___ -> false
let uu___is_OpAnd (projectee : ins_opcode) : Prims.bool=
  match projectee with | OpAnd -> true | uu___ -> false
let uu___is_OpOr (projectee : ins_opcode) : Prims.bool=
  match projectee with | OpOr -> true | uu___ -> false
let uu___is_OpRev (projectee : ins_opcode) : Prims.bool=
  match projectee with | OpRev -> true | uu___ -> false
let uu___is_OpLoad (projectee : ins_opcode) : Prims.bool=
  match projectee with | OpLoad -> true | uu___ -> false
let uu___is_OpStore (projectee : ins_opcode) : Prims.bool=
  match projectee with | OpStore -> true | uu___ -> false
let opcode_to_u32 (op : ins_opcode) : FStar_UInt32.t=
  match op with
  | OpMov -> Stdint.Uint32.zero
  | OpAdd -> Stdint.Uint32.one
  | OpSub -> Stdint.Uint32.of_int 2
  | OpXor -> Stdint.Uint32.of_int 3
  | OpAnd -> Stdint.Uint32.of_int 4
  | OpOr -> Stdint.Uint32.of_int 5
  | OpRev -> Stdint.Uint32.of_int 6
  | OpLoad -> Stdint.Uint32.of_int 7
  | OpStore -> Stdint.Uint32.of_int 8
let u32_to_opcode (tag : FStar_UInt32.t) : ins_opcode=
  if tag = Stdint.Uint32.zero
  then OpMov
  else
    if tag = Stdint.Uint32.one
    then OpAdd
    else
      if tag = (Stdint.Uint32.of_int 2)
      then OpSub
      else
        if tag = (Stdint.Uint32.of_int 3)
        then OpXor
        else
          if tag = (Stdint.Uint32.of_int 4)
          then OpAnd
          else
            if tag = (Stdint.Uint32.of_int 5)
            then OpOr
            else
              if tag = (Stdint.Uint32.of_int 6)
              then OpRev
              else if tag = (Stdint.Uint32.of_int 7) then OpLoad else OpStore
type stmt =
  | SIns of ins_opcode * operand Prims.list 
  | SAssign of id * expr 
  | SAssert of expr 
  | SAssume of expr 
  | SBlock of stmt Prims.list 
  | SIfElse of expr * stmt Prims.list * stmt Prims.list 
  | SWhile of expr * expr Prims.list * stmt Prims.list 
  | SCall of id * expr Prims.list 
let uu___is_SIns (projectee : stmt) : Prims.bool=
  match projectee with | SIns (_0, _1) -> true | uu___ -> false
let __proj__SIns__item___0 (projectee : stmt) : ins_opcode=
  match projectee with | SIns (_0, _1) -> _0
let __proj__SIns__item___1 (projectee : stmt) : operand Prims.list=
  match projectee with | SIns (_0, _1) -> _1
let uu___is_SAssign (projectee : stmt) : Prims.bool=
  match projectee with | SAssign (_0, _1) -> true | uu___ -> false
let __proj__SAssign__item___0 (projectee : stmt) : id=
  match projectee with | SAssign (_0, _1) -> _0
let __proj__SAssign__item___1 (projectee : stmt) : expr=
  match projectee with | SAssign (_0, _1) -> _1
let uu___is_SAssert (projectee : stmt) : Prims.bool=
  match projectee with | SAssert _0 -> true | uu___ -> false
let __proj__SAssert__item___0 (projectee : stmt) : expr=
  match projectee with | SAssert _0 -> _0
let uu___is_SAssume (projectee : stmt) : Prims.bool=
  match projectee with | SAssume _0 -> true | uu___ -> false
let __proj__SAssume__item___0 (projectee : stmt) : expr=
  match projectee with | SAssume _0 -> _0
let uu___is_SBlock (projectee : stmt) : Prims.bool=
  match projectee with | SBlock _0 -> true | uu___ -> false
let __proj__SBlock__item___0 (projectee : stmt) : stmt Prims.list=
  match projectee with | SBlock _0 -> _0
let uu___is_SIfElse (projectee : stmt) : Prims.bool=
  match projectee with | SIfElse (_0, _1, _2) -> true | uu___ -> false
let __proj__SIfElse__item___0 (projectee : stmt) : expr=
  match projectee with | SIfElse (_0, _1, _2) -> _0
let __proj__SIfElse__item___1 (projectee : stmt) : stmt Prims.list=
  match projectee with | SIfElse (_0, _1, _2) -> _1
let __proj__SIfElse__item___2 (projectee : stmt) : stmt Prims.list=
  match projectee with | SIfElse (_0, _1, _2) -> _2
let uu___is_SWhile (projectee : stmt) : Prims.bool=
  match projectee with | SWhile (_0, _1, _2) -> true | uu___ -> false
let __proj__SWhile__item___0 (projectee : stmt) : expr=
  match projectee with | SWhile (_0, _1, _2) -> _0
let __proj__SWhile__item___1 (projectee : stmt) : expr Prims.list=
  match projectee with | SWhile (_0, _1, _2) -> _1
let __proj__SWhile__item___2 (projectee : stmt) : stmt Prims.list=
  match projectee with | SWhile (_0, _1, _2) -> _2
let uu___is_SCall (projectee : stmt) : Prims.bool=
  match projectee with | SCall (_0, _1) -> true | uu___ -> false
let __proj__SCall__item___0 (projectee : stmt) : id=
  match projectee with | SCall (_0, _1) -> _0
let __proj__SCall__item___1 (projectee : stmt) : expr Prims.list=
  match projectee with | SCall (_0, _1) -> _1
type proc_decl =
  {
  proc_name: id ;
  proc_target: arch_target ;
  proc_formals: pformal Prims.list ;
  proc_returns: pformal Prims.list ;
  proc_specs: spec_kind Prims.list ;
  proc_body: stmt Prims.list ;
  proc_is_ins: Prims.bool ;
  proc_ins_term: Prims.string ;
  proc_modifies: id Prims.list }
let __proj__Mkproc_decl__item__proc_name (projectee : proc_decl) : id=
  match projectee with
  | { proc_name; proc_target; proc_formals; proc_returns; proc_specs;
      proc_body; proc_is_ins; proc_ins_term; proc_modifies;_} -> proc_name
let __proj__Mkproc_decl__item__proc_target (projectee : proc_decl) :
  arch_target=
  match projectee with
  | { proc_name; proc_target; proc_formals; proc_returns; proc_specs;
      proc_body; proc_is_ins; proc_ins_term; proc_modifies;_} -> proc_target
let __proj__Mkproc_decl__item__proc_formals (projectee : proc_decl) :
  pformal Prims.list=
  match projectee with
  | { proc_name; proc_target; proc_formals; proc_returns; proc_specs;
      proc_body; proc_is_ins; proc_ins_term; proc_modifies;_} -> proc_formals
let __proj__Mkproc_decl__item__proc_returns (projectee : proc_decl) :
  pformal Prims.list=
  match projectee with
  | { proc_name; proc_target; proc_formals; proc_returns; proc_specs;
      proc_body; proc_is_ins; proc_ins_term; proc_modifies;_} -> proc_returns
let __proj__Mkproc_decl__item__proc_specs (projectee : proc_decl) :
  spec_kind Prims.list=
  match projectee with
  | { proc_name; proc_target; proc_formals; proc_returns; proc_specs;
      proc_body; proc_is_ins; proc_ins_term; proc_modifies;_} -> proc_specs
let __proj__Mkproc_decl__item__proc_body (projectee : proc_decl) :
  stmt Prims.list=
  match projectee with
  | { proc_name; proc_target; proc_formals; proc_returns; proc_specs;
      proc_body; proc_is_ins; proc_ins_term; proc_modifies;_} -> proc_body
let __proj__Mkproc_decl__item__proc_is_ins (projectee : proc_decl) :
  Prims.bool=
  match projectee with
  | { proc_name; proc_target; proc_formals; proc_returns; proc_specs;
      proc_body; proc_is_ins; proc_ins_term; proc_modifies;_} -> proc_is_ins
let __proj__Mkproc_decl__item__proc_ins_term (projectee : proc_decl) :
  Prims.string=
  match projectee with
  | { proc_name; proc_target; proc_formals; proc_returns; proc_specs;
      proc_body; proc_is_ins; proc_ins_term; proc_modifies;_} ->
      proc_ins_term
let __proj__Mkproc_decl__item__proc_modifies (projectee : proc_decl) :
  id Prims.list=
  match projectee with
  | { proc_name; proc_target; proc_formals; proc_returns; proc_specs;
      proc_body; proc_is_ins; proc_ins_term; proc_modifies;_} ->
      proc_modifies
type module_decl =
  {
  mod_name: id ;
  mod_includes: id Prims.list ;
  mod_procs: proc_decl Prims.list }
let __proj__Mkmodule_decl__item__mod_name (projectee : module_decl) : 
  id=
  match projectee with | { mod_name; mod_includes; mod_procs;_} -> mod_name
let __proj__Mkmodule_decl__item__mod_includes (projectee : module_decl) :
  id Prims.list=
  match projectee with
  | { mod_name; mod_includes; mod_procs;_} -> mod_includes
let __proj__Mkmodule_decl__item__mod_procs (projectee : module_decl) :
  proc_decl Prims.list=
  match projectee with | { mod_name; mod_includes; mod_procs;_} -> mod_procs
