module Valerie.Compiler.Lower

open Valerie.Compiler.AST
module AArch64 = Valerie.Compiler.Lower.AArch64
module Arm32 = Valerie.Compiler.Lower.Arm32
module X64 = Valerie.Compiler.Lower.X64
module Riscv64 = Valerie.Compiler.Lower.Riscv64

type lowered_target_code =
  | LoweredAArch64 : Valerie.AArch64.Semantics.code -> lowered_target_code
  | LoweredArm32 : Valerie.Arm32.Semantics.code -> lowered_target_code
  | LoweredX64 : Valerie.X64.Semantics.code -> lowered_target_code
  | LoweredRiscv64 : Valerie.Riscv64.Semantics.code -> lowered_target_code

let lower_proc_to_target (p:proc_decl) : option lowered_target_code =
  match p.proc_target with
  | TargetAArch64 ->
      (match AArch64.lower_stmts p.proc_body with
       | Some c -> Some (LoweredAArch64 c)
       | None -> None)
  | TargetArm32 ->
      (match Arm32.lower_stmts p.proc_body with
       | Some c -> Some (LoweredArm32 c)
       | None -> None)
  | TargetX64 ->
      (match X64.lower_stmts p.proc_body with
       | Some c -> Some (LoweredX64 c)
       | None -> None)
  | TargetRiscv64 ->
      (match Riscv64.lower_stmts p.proc_body with
       | Some c -> Some (LoweredRiscv64 c)
       | None -> None)
