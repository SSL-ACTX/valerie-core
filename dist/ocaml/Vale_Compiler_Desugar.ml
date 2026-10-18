open Prims
let rec append_stmts (l1 : Vale_Compiler_AST.stmt Prims.list)
  (l2 : Vale_Compiler_AST.stmt Prims.list) :
  Vale_Compiler_AST.stmt Prims.list=
  match l1 with | [] -> l2 | x::xs -> x :: (append_stmts xs l2)
let rec desugar_stmt (s : Vale_Compiler_AST.stmt) :
  Vale_Compiler_AST.stmt Prims.list=
  match s with
  | Vale_Compiler_AST.SBlock stmts -> desugar_stmts stmts
  | Vale_Compiler_AST.SIfElse (cond, t_branch, f_branch) ->
      [Vale_Compiler_AST.SIfElse
         (cond, (desugar_stmts t_branch), (desugar_stmts f_branch))]
  | Vale_Compiler_AST.SWhile (cond, invs, body) ->
      [Vale_Compiler_AST.SWhile (cond, invs, (desugar_stmts body))]
  | other -> [other]
and desugar_stmts (stmts : Vale_Compiler_AST.stmt Prims.list) :
  Vale_Compiler_AST.stmt Prims.list=
  match stmts with
  | [] -> []
  | s::rest -> append_stmts (desugar_stmt s) (desugar_stmts rest)
let desugar_proc (p : Vale_Compiler_AST.proc_decl) :
  Vale_Compiler_AST.proc_decl=
  {
    Vale_Compiler_AST.proc_name = (p.Vale_Compiler_AST.proc_name);
    Vale_Compiler_AST.proc_target = (p.Vale_Compiler_AST.proc_target);
    Vale_Compiler_AST.proc_formals = (p.Vale_Compiler_AST.proc_formals);
    Vale_Compiler_AST.proc_returns = (p.Vale_Compiler_AST.proc_returns);
    Vale_Compiler_AST.proc_specs = (p.Vale_Compiler_AST.proc_specs);
    Vale_Compiler_AST.proc_body =
      (desugar_stmts p.Vale_Compiler_AST.proc_body);
    Vale_Compiler_AST.proc_is_ins = (p.Vale_Compiler_AST.proc_is_ins);
    Vale_Compiler_AST.proc_ins_term = (p.Vale_Compiler_AST.proc_ins_term);
    Vale_Compiler_AST.proc_modifies = (p.Vale_Compiler_AST.proc_modifies)
  }
let rec desugar_procs (procs : Vale_Compiler_AST.proc_decl Prims.list) :
  Vale_Compiler_AST.proc_decl Prims.list=
  match procs with
  | [] -> []
  | p::rest -> (desugar_proc p) :: (desugar_procs rest)
let desugar_module (m : Vale_Compiler_AST.module_decl) :
  Vale_Compiler_AST.module_decl=
  {
    Vale_Compiler_AST.mod_name = (m.Vale_Compiler_AST.mod_name);
    Vale_Compiler_AST.mod_includes = (m.Vale_Compiler_AST.mod_includes);
    Vale_Compiler_AST.mod_procs =
      (desugar_procs m.Vale_Compiler_AST.mod_procs)
  }
