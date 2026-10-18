module Valerie.Compiler.Validate

open FStar.UInt32
module U32 = FStar.UInt32
open FStar.UInt64
module U64 = FStar.UInt64

open Valerie.Defs
module AST = Valerie.Compiler.AST

/// Return Status Codes for C extraction
type engine_status_t =
  | EngineSuccess       : engine_status_t
  | EngineErrNullInput  : engine_status_t
  | EngineErrLowering   : engine_status_t
  | EngineErrEmit       : engine_status_t

let status_to_u32 (s: engine_status_t) : U32.t =
  match s with
  | EngineSuccess      -> 0ul
  | EngineErrNullInput -> 1ul
  | EngineErrLowering  -> 2ul
  | EngineErrEmit      -> 3ul

/// Buffer Descriptor for C extraction
noeq type buffer_desc_t = {
  bd_reg_id: U32.t;
  bd_len: U64.t;
}

let create_buffer_desc (reg_id: U32.t) (len: U64.t) : buffer_desc_t = {
  bd_reg_id = reg_id;
  bd_len = len;
}

/// Central Engine State for C Extraction
noeq type engine_state_t = {
  es_target: AST.arch_target;
  es_status: engine_status_t;
  es_buffer_count: U32.t;
}

let init_engine (target_tag: U32.t) : engine_state_t = {
  es_target = AST.u32_to_arch_target target_tag;
  es_status = EngineSuccess;
  es_buffer_count = 0ul;
}

let engine_get_status (st: engine_state_t) : U32.t =
  status_to_u32 st.es_status

let engine_get_target (st: engine_state_t) : U32.t =
  AST.arch_target_to_u32 st.es_target

let engine_register_buffer (st: engine_state_t) (desc: buffer_desc_t) : engine_state_t = {
  st with es_buffer_count = U32.add_mod st.es_buffer_count 1ul;
}

let rec validate_stmt (target: AST.arch_target) (s: AST.stmt) : Tot bool (decreases s) =
  let max_reg =
    match target with
    | AST.TargetAArch64 -> 31ul
    | AST.TargetArm32   -> 15ul
    | AST.TargetX64     -> 15ul
    | AST.TargetRiscv64 -> 31ul
  in
  match s with
  | AST.SIns _ operands ->
      let check_op (op: AST.operand) : bool =
        match op with
        | AST.OReg (AST.RegNumbered r) -> r <= max_reg
        | AST.OMemBuffer (AST.RegNumbered r) _ -> r <= max_reg
        | AST.OImm _ -> true
      in
      FStar.List.Tot.for_all check_op operands
  | AST.SBlock stmts ->
      validate_stmts target stmts
  | AST.SIfElse _ s_then s_else ->
      validate_stmts target s_then &&
      validate_stmts target s_else
  | AST.SWhile _ _ body ->
      validate_stmts target body
  | _ -> true

and validate_stmts (target: AST.arch_target) (stmts: list AST.stmt) : Tot bool (decreases stmts) =
  match stmts with
  | [] -> true
  | s :: rest ->
      validate_stmt target s && validate_stmts target rest

let validate_proc (p: AST.proc_decl) : bool =
  let target = p.AST.proc_target in
  let max_reg =
    match target with
    | AST.TargetAArch64 -> 31ul
    | AST.TargetArm32   -> 15ul
    | AST.TargetX64     -> 15ul
    | AST.TargetRiscv64 -> 31ul
  in
  let check_spec (sp: AST.spec_kind) : bool =
    match sp with
    | AST.SpecModifies regs ->
        let check_r (r: AST.reg_operand) : bool =
          match r with
          | AST.RegNumbered n -> n <= max_reg
        in
        FStar.List.Tot.for_all check_r regs
    | AST.SpecBufferPerm (AST.RegNumbered r) _ -> r <= max_reg
    | _ -> true
  in
  FStar.List.Tot.for_all check_spec p.AST.proc_specs &&
  FStar.List.Tot.for_all (validate_stmt target) p.AST.proc_body

let validate_module (m: AST.module_decl) : bool =
  FStar.List.Tot.for_all validate_proc m.AST.mod_procs
