open Prims
type compile_result =
  {
  cr_mod: Vale_Compiler_AST.module_decl ;
  cr_valid: Prims.bool ;
  cr_vaf: Prims.string ;
  cr_fstar: Prims.string ;
  cr_pulse: Prims.string ;
  cr_ir: Prims.string ;
  cr_asm: Prims.string }
let __proj__Mkcompile_result__item__cr_mod (projectee : compile_result) :
  Vale_Compiler_AST.module_decl=
  match projectee with
  | { cr_mod; cr_valid; cr_vaf; cr_fstar; cr_pulse; cr_ir; cr_asm;_} ->
      cr_mod
let __proj__Mkcompile_result__item__cr_valid (projectee : compile_result) :
  Prims.bool=
  match projectee with
  | { cr_mod; cr_valid; cr_vaf; cr_fstar; cr_pulse; cr_ir; cr_asm;_} ->
      cr_valid
let __proj__Mkcompile_result__item__cr_vaf (projectee : compile_result) :
  Prims.string=
  match projectee with
  | { cr_mod; cr_valid; cr_vaf; cr_fstar; cr_pulse; cr_ir; cr_asm;_} ->
      cr_vaf
let __proj__Mkcompile_result__item__cr_fstar (projectee : compile_result) :
  Prims.string=
  match projectee with
  | { cr_mod; cr_valid; cr_vaf; cr_fstar; cr_pulse; cr_ir; cr_asm;_} ->
      cr_fstar
let __proj__Mkcompile_result__item__cr_pulse (projectee : compile_result) :
  Prims.string=
  match projectee with
  | { cr_mod; cr_valid; cr_vaf; cr_fstar; cr_pulse; cr_ir; cr_asm;_} ->
      cr_pulse
let __proj__Mkcompile_result__item__cr_ir (projectee : compile_result) :
  Prims.string=
  match projectee with
  | { cr_mod; cr_valid; cr_vaf; cr_fstar; cr_pulse; cr_ir; cr_asm;_} -> cr_ir
let __proj__Mkcompile_result__item__cr_asm (projectee : compile_result) :
  Prims.string=
  match projectee with
  | { cr_mod; cr_valid; cr_vaf; cr_fstar; cr_pulse; cr_ir; cr_asm;_} ->
      cr_asm
let compile_vaf_module (m : Vale_Compiler_AST.module_decl)
  (target : Vale_Compiler_AST.arch_target) : compile_result=
  let desugared_m = Vale_Compiler_Desugar.desugar_module m in
  let is_valid = Vale_Compiler_Validate.validate_module desugared_m in
  let vaf_text = Vale_Compiler_EmitVaf.emit_vaf_module desugared_m in
  let fstar_text =
    Vale_Compiler_EmitFStar.emit_fstar_module desugared_m target in
  let pulse_text = Vale_Compiler_EmitPulse.emit_pulse_module desugared_m in
  let ir_text =
    match desugared_m.Vale_Compiler_AST.mod_procs with
    | [] -> "(empty module)\n"
    | p::uu___ -> Vale_Compiler_Print.print_proc p in
  let asm_text =
    match target with
    | Vale_Compiler_AST.TargetAArch64 ->
        Vale_Compiler_EmitAsm_AArch64.emit_module_asm desugared_m true true
    | Vale_Compiler_AST.TargetArm32 ->
        Vale_Compiler_EmitAsm_Arm32.emit_module_asm desugared_m true
    | Vale_Compiler_AST.TargetX64 ->
        Vale_Compiler_EmitAsm_X64.emit_module_asm desugared_m true
    | Vale_Compiler_AST.TargetRiscv64 ->
        Vale_Compiler_EmitAsm_Riscv64.emit_module_asm desugared_m true in
  {
    cr_mod = desugared_m;
    cr_valid = is_valid;
    cr_vaf = vaf_text;
    cr_fstar = fstar_text;
    cr_pulse = pulse_text;
    cr_ir = ir_text;
    cr_asm = asm_text
  }
