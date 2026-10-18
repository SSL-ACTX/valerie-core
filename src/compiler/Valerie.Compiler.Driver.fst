module Valerie.Compiler.Driver

open Valerie.Compiler.AST
open Valerie.Compiler.Desugar
open Valerie.Compiler.EmitVaf
open Valerie.Compiler.EmitFStar
open Valerie.Compiler.Print
open Valerie.Compiler.EmitPulse
open Valerie.Compiler.EmitAsm.AArch64
open Valerie.Compiler.EmitAsm.Arm32
open Valerie.Compiler.EmitAsm.X64
open Valerie.Compiler.EmitAsm.Riscv64

type compile_result = {
  cr_mod: module_decl;
  cr_valid: bool;
  cr_vaf: string;
  cr_fstar: string;
  cr_pulse: string;
  cr_ir: string;
  cr_asm: string;
}

let compile_vaf_module (m:module_decl) (target:arch_target) : compile_result =
  let desugared_m = desugar_module m in
  let is_valid = Valerie.Compiler.Validate.validate_module desugared_m in
  let vaf_text = emit_vaf_module desugared_m in
  let fstar_text = emit_fstar_module desugared_m target in
  let pulse_text = emit_pulse_module desugared_m in
  let ir_text =
    match desugared_m.mod_procs with
    | [] -> "(empty module)\n"
    | p :: _ -> print_proc p
  in
  let asm_text =
    match target with
    | TargetAArch64 -> Valerie.Compiler.EmitAsm.AArch64.emit_module_asm desugared_m true true
    | TargetArm32 -> Valerie.Compiler.EmitAsm.Arm32.emit_module_asm desugared_m true
    | TargetX64 -> Valerie.Compiler.EmitAsm.X64.emit_module_asm desugared_m true
    | TargetRiscv64 -> Valerie.Compiler.EmitAsm.Riscv64.emit_module_asm desugared_m true
  in
  {
    cr_mod = desugared_m;
    cr_valid = is_valid;
    cr_vaf = vaf_text;
    cr_fstar = fstar_text;
    cr_pulse = pulse_text;
    cr_ir = ir_text;
    cr_asm = asm_text;
  }
