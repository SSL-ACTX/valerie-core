open Prims
let rec append_stmts (l1 : Valerie_Compiler_AST.stmt Prims.list)
  (l2 : Valerie_Compiler_AST.stmt Prims.list) :
  Valerie_Compiler_AST.stmt Prims.list=
  match l1 with | [] -> l2 | x::xs -> x :: (append_stmts xs l2)
let rec desugar_stmt (s : Valerie_Compiler_AST.stmt) :
  Valerie_Compiler_AST.stmt Prims.list=
  match s with
  | Valerie_Compiler_AST.SBlock stmts -> desugar_stmts stmts
  | Valerie_Compiler_AST.SIfElse (cond, t_branch, f_branch) ->
      [Valerie_Compiler_AST.SIfElse
         (cond, (desugar_stmts t_branch), (desugar_stmts f_branch))]
  | Valerie_Compiler_AST.SWhile (cond, invs, body) ->
      [Valerie_Compiler_AST.SWhile (cond, invs, (desugar_stmts body))]
  | other -> [other]
and desugar_stmts (stmts : Valerie_Compiler_AST.stmt Prims.list) :
  Valerie_Compiler_AST.stmt Prims.list=
  match stmts with
  | [] -> []
  | s::rest -> append_stmts (desugar_stmt s) (desugar_stmts rest)
let desugar_proc (p : Valerie_Compiler_AST.proc_decl) :
  Valerie_Compiler_AST.proc_decl=
  {
    Valerie_Compiler_AST.proc_name = (p.Valerie_Compiler_AST.proc_name);
    Valerie_Compiler_AST.proc_target = (p.Valerie_Compiler_AST.proc_target);
    Valerie_Compiler_AST.proc_formals = (p.Valerie_Compiler_AST.proc_formals);
    Valerie_Compiler_AST.proc_returns = (p.Valerie_Compiler_AST.proc_returns);
    Valerie_Compiler_AST.proc_specs = (p.Valerie_Compiler_AST.proc_specs);
    Valerie_Compiler_AST.proc_body =
      (desugar_stmts p.Valerie_Compiler_AST.proc_body);
    Valerie_Compiler_AST.proc_is_ins = (p.Valerie_Compiler_AST.proc_is_ins);
    Valerie_Compiler_AST.proc_ins_term =
      (p.Valerie_Compiler_AST.proc_ins_term);
    Valerie_Compiler_AST.proc_modifies =
      (p.Valerie_Compiler_AST.proc_modifies)
  }
let rec desugar_procs (procs : Valerie_Compiler_AST.proc_decl Prims.list) :
  Valerie_Compiler_AST.proc_decl Prims.list=
  match procs with
  | [] -> []
  | p::rest -> (desugar_proc p) :: (desugar_procs rest)
let desugar_module (m : Valerie_Compiler_AST.module_decl) :
  Valerie_Compiler_AST.module_decl=
  {
    Valerie_Compiler_AST.mod_name = (m.Valerie_Compiler_AST.mod_name);
    Valerie_Compiler_AST.mod_includes = (m.Valerie_Compiler_AST.mod_includes);
    Valerie_Compiler_AST.mod_procs =
      (desugar_procs m.Valerie_Compiler_AST.mod_procs)
  }
