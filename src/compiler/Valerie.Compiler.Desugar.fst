module Valerie.Compiler.Desugar

open Valerie.Compiler.AST

let rec append_stmts (l1:list stmt) (l2:list stmt) : Tot (list stmt) (decreases l1) =
  match l1 with
  | [] -> l2
  | x :: xs -> x :: append_stmts xs l2

let rec desugar_stmt (s:stmt) : Tot (list stmt) (decreases s) =
  match s with
  | SBlock stmts -> desugar_stmts stmts
  | SIfElse cond t_branch f_branch ->
      [SIfElse cond (desugar_stmts t_branch) (desugar_stmts f_branch)]
  | SWhile cond invs body ->
      [SWhile cond invs (desugar_stmts body)]
  | other -> [other]

and desugar_stmts (stmts:list stmt) : Tot (list stmt) (decreases stmts) =
  match stmts with
  | [] -> []
  | s :: rest ->
      append_stmts (desugar_stmt s) (desugar_stmts rest)

let desugar_proc (p:proc_decl) : proc_decl =
  { p with proc_body = desugar_stmts p.proc_body }

let rec desugar_procs (procs:list proc_decl) : Tot (list proc_decl) (decreases procs) =
  match procs with
  | [] -> []
  | p :: rest -> desugar_proc p :: desugar_procs rest

let desugar_module (m:module_decl) : module_decl =
  { m with mod_procs = desugar_procs m.mod_procs }
