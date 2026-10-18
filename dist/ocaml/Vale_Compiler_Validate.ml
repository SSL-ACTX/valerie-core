open Prims
type engine_status_t =
  | EngineSuccess 
  | EngineErrNullInput 
  | EngineErrLowering 
  | EngineErrEmit 
let uu___is_EngineSuccess (projectee : engine_status_t) : Prims.bool=
  match projectee with | EngineSuccess -> true | uu___ -> false
let uu___is_EngineErrNullInput (projectee : engine_status_t) : Prims.bool=
  match projectee with | EngineErrNullInput -> true | uu___ -> false
let uu___is_EngineErrLowering (projectee : engine_status_t) : Prims.bool=
  match projectee with | EngineErrLowering -> true | uu___ -> false
let uu___is_EngineErrEmit (projectee : engine_status_t) : Prims.bool=
  match projectee with | EngineErrEmit -> true | uu___ -> false
let status_to_u32 (s : engine_status_t) : FStar_UInt32.t=
  match s with
  | EngineSuccess -> Stdint.Uint32.zero
  | EngineErrNullInput -> Stdint.Uint32.one
  | EngineErrLowering -> Stdint.Uint32.of_int 2
  | EngineErrEmit -> Stdint.Uint32.of_int 3
type buffer_desc_t = {
  bd_reg_id: FStar_UInt32.t ;
  bd_len: FStar_UInt64.t }
let __proj__Mkbuffer_desc_t__item__bd_reg_id (projectee : buffer_desc_t) :
  FStar_UInt32.t= match projectee with | { bd_reg_id; bd_len;_} -> bd_reg_id
let __proj__Mkbuffer_desc_t__item__bd_len (projectee : buffer_desc_t) :
  FStar_UInt64.t= match projectee with | { bd_reg_id; bd_len;_} -> bd_len
let create_buffer_desc (reg_id : FStar_UInt32.t) (len : FStar_UInt64.t) :
  buffer_desc_t= { bd_reg_id = reg_id; bd_len = len }
type engine_state_t =
  {
  es_target: Vale_Compiler_AST.arch_target ;
  es_status: engine_status_t ;
  es_buffer_count: FStar_UInt32.t }
let __proj__Mkengine_state_t__item__es_target (projectee : engine_state_t) :
  Vale_Compiler_AST.arch_target=
  match projectee with
  | { es_target; es_status; es_buffer_count;_} -> es_target
let __proj__Mkengine_state_t__item__es_status (projectee : engine_state_t) :
  engine_status_t=
  match projectee with
  | { es_target; es_status; es_buffer_count;_} -> es_status
let __proj__Mkengine_state_t__item__es_buffer_count
  (projectee : engine_state_t) : FStar_UInt32.t=
  match projectee with
  | { es_target; es_status; es_buffer_count;_} -> es_buffer_count
let init_engine (target_tag : FStar_UInt32.t) : engine_state_t=
  {
    es_target = (Vale_Compiler_AST.u32_to_arch_target target_tag);
    es_status = EngineSuccess;
    es_buffer_count = Stdint.Uint32.zero
  }
let engine_get_status (st : engine_state_t) : FStar_UInt32.t=
  status_to_u32 st.es_status
let engine_get_target (st : engine_state_t) : FStar_UInt32.t=
  Vale_Compiler_AST.arch_target_to_u32 st.es_target
let engine_register_buffer (st : engine_state_t) (desc : buffer_desc_t) :
  engine_state_t=
  {
    es_target = (st.es_target);
    es_status = (st.es_status);
    es_buffer_count =
      (FStar_UInt32.add_mod st.es_buffer_count Stdint.Uint32.one)
  }
let rec validate_stmt (target : Vale_Compiler_AST.arch_target)
  (s : Vale_Compiler_AST.stmt) : Prims.bool=
  let max_reg =
    match target with
    | Vale_Compiler_AST.TargetAArch64 -> Stdint.Uint32.of_int 31
    | Vale_Compiler_AST.TargetArm32 -> Stdint.Uint32.of_int 15
    | Vale_Compiler_AST.TargetX64 -> Stdint.Uint32.of_int 15
    | Vale_Compiler_AST.TargetRiscv64 -> Stdint.Uint32.of_int 31 in
  match s with
  | Vale_Compiler_AST.SIns (uu___, operands) ->
      let check_op op =
        match op with
        | Vale_Compiler_AST.OReg (Vale_Compiler_AST.RegNumbered r) ->
            FStar_UInt32.lte r max_reg
        | Vale_Compiler_AST.OMemBuffer
            (Vale_Compiler_AST.RegNumbered r, uu___1) ->
            FStar_UInt32.lte r max_reg
        | Vale_Compiler_AST.OImm uu___1 -> true in
      FStar_List_Tot_Base.for_all check_op operands
  | Vale_Compiler_AST.SBlock stmts -> validate_stmts target stmts
  | Vale_Compiler_AST.SIfElse (uu___, s_then, s_else) ->
      (validate_stmts target s_then) && (validate_stmts target s_else)
  | Vale_Compiler_AST.SWhile (uu___, uu___1, body) ->
      validate_stmts target body
  | uu___ -> true
and validate_stmts (target : Vale_Compiler_AST.arch_target)
  (stmts : Vale_Compiler_AST.stmt Prims.list) : Prims.bool=
  match stmts with
  | [] -> true
  | s::rest -> (validate_stmt target s) && (validate_stmts target rest)
let validate_proc (p : Vale_Compiler_AST.proc_decl) : Prims.bool=
  let target = p.Vale_Compiler_AST.proc_target in
  let max_reg =
    match target with
    | Vale_Compiler_AST.TargetAArch64 -> Stdint.Uint32.of_int 31
    | Vale_Compiler_AST.TargetArm32 -> Stdint.Uint32.of_int 15
    | Vale_Compiler_AST.TargetX64 -> Stdint.Uint32.of_int 15
    | Vale_Compiler_AST.TargetRiscv64 -> Stdint.Uint32.of_int 31 in
  let check_spec sp =
    match sp with
    | Vale_Compiler_AST.SpecModifies regs ->
        let check_r r =
          match r with
          | Vale_Compiler_AST.RegNumbered n -> FStar_UInt32.lte n max_reg in
        FStar_List_Tot_Base.for_all check_r regs
    | Vale_Compiler_AST.SpecBufferPerm
        (Vale_Compiler_AST.RegNumbered r, uu___) ->
        FStar_UInt32.lte r max_reg
    | uu___ -> true in
  (FStar_List_Tot_Base.for_all check_spec p.Vale_Compiler_AST.proc_specs) &&
    (FStar_List_Tot_Base.for_all (validate_stmt target)
       p.Vale_Compiler_AST.proc_body)
let validate_module (m : Vale_Compiler_AST.module_decl) : Prims.bool=
  FStar_List_Tot_Base.for_all validate_proc m.Vale_Compiler_AST.mod_procs
