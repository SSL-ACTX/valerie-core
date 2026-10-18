module Valerie.Compiler.EmitVaf

open FStar.String
open Valerie.Compiler.AST

let rec string_of_typ (t:vaf_typ) : Tot string (decreases t) =
  match t with
  | TVar s -> s
  | TNat -> "nat"
  | TInt -> "int"
  | TBool -> "bool"
  | TU8 -> "u8"
  | TU32 -> "u32"
  | TU64 -> "u64"
  | TSeq sub -> concat "" ["seq("; string_of_typ sub; ")"]
  | TBuffer sub _ -> concat "" ["buffer("; string_of_typ sub; ")"]
  | TTuple ts -> concat "" ["("; concat_typs ts; ")"]

and concat_typs (ts:list vaf_typ) : Tot string (decreases ts) =
  match ts with
  | [] -> ""
  | [t] -> string_of_typ t
  | t :: rest -> concat "" [string_of_typ t; ", "; concat_typs rest]

let string_of_reg (r:reg_operand) : string =
  match r with
  | RegNumbered n -> concat "" ["r"; FStar.UInt32.to_string n]

let string_of_opcode (op:ins_opcode) : string =
  match op with
  | OpMov -> "Mov"
  | OpAdd -> "Add"
  | OpSub -> "Sub"
  | OpXor -> "Xor"
  | OpAnd -> "And"
  | OpOr  -> "Or"
  | OpRev -> "Rev"
  | OpLoad -> "Load"
  | OpStore -> "Store"

let string_of_operand (op:operand) : string =
  match op with
  | OReg r -> string_of_reg r
  | OImm i -> FStar.UInt64.to_string i
  | OMemBuffer base offset -> concat "" ["["; string_of_reg base; " + "; FStar.UInt32.to_string offset; "]"]

let string_of_uop (op:uop) : string =
  match op with
  | UNot -> "!"
  | UNeg -> "-"
  | UOld -> "old"

let string_of_bop (op:bop) : string =
  match op with
  | BAdd -> "+"
  | BSub -> "-"
  | BMul -> "*"
  | BDiv -> "/"
  | BMod -> "%"
  | BAnd -> "&&"
  | BOr -> "||"
  | BEq -> "=="
  | BNe -> "!="
  | BLt -> "<"
  | BGt -> ">"
  | BLe -> "<="
  | BGe -> ">="
  | BImply -> "==>"
  | BEquiv -> "<==>"

let rec string_of_expr (e:expr) : Tot string (decreases e) =
  match e with
  | EVar v -> v
  | EConst c -> string_of_int c
  | EBoolConst b -> if b then "true" else "false"
  | EUop UOld inner ->
      concat "" ["old("; string_of_expr inner; ")"]
  | EUop op inner ->
      concat "" ["("; string_of_uop op; " "; string_of_expr inner; ")"]
  | EBop op e1 e2 ->
      concat "" ["("; string_of_expr e1; " "; string_of_bop op; " "; string_of_expr e2; ")"]
  | EApply f args ->
      concat "" [f; "("; concat_exprs args; ")"]

and concat_exprs (es:list expr) : Tot string (decreases es) =
  match es with
  | [] -> ""
  | [x] -> string_of_expr x
  | x :: xs -> concat "" [string_of_expr x; ", "; concat_exprs xs]

let string_of_pformal_kind (k:pformal_kind) : string =
  match k with
  | FormalGhost -> "ghost "
  | FormalInline -> "inline "
  | FormalIn -> "in "
  | FormalOut -> "out "
  | FormalInOut -> "inout "

let string_of_pformal (f:pformal) : string =
  concat "" [string_of_pformal_kind f.pformal_kind; f.pformal_name; ":"; string_of_typ f.pformal_typ]

let rec concat_pformals (fs:list pformal) : Tot string (decreases fs) =
  match fs with
  | [] -> ""
  | [x] -> string_of_pformal x
  | x :: xs -> concat "" [string_of_pformal x; ", "; concat_pformals xs]

let rec concat_operands (ops:list operand) : Tot string (decreases ops) =
  match ops with
  | [] -> ""
  | [x] -> string_of_operand x
  | x :: xs -> concat "" [string_of_operand x; ", "; concat_operands xs]

let rec concat_regs (rs:list reg_operand) : Tot string (decreases rs) =
  match rs with
  | [] -> ""
  | [x] -> string_of_reg x
  | x :: xs -> concat "" [string_of_reg x; "; "; concat_regs xs]

let string_of_spec (s:spec_kind) : string =
  match s with
  | SpecRequires e -> concat "" ["    requires\n        "; string_of_expr e; ";\n"]
  | SpecEnsures e -> concat "" ["    ensures\n        "; string_of_expr e; ";\n"]
  | SpecModifies rs -> concat "" ["    modifies\n        "; concat_regs rs; ";\n"]
  | SpecBufferPerm r len -> concat "" ["    requires\n        valid_buffer("; string_of_reg r; ", "; string_of_int len; ");\n"]

let rec emit_specs (specs:list spec_kind) : Tot string (decreases specs) =
  match specs with
  | [] -> ""
  | s :: rest -> concat "" [string_of_spec s; emit_specs rest]

let rec emit_invariants (indent:string) (invs:list expr) : Tot string (decreases invs) =
  match invs with
  | [] -> ""
  | inv :: rest ->
      concat "" [indent; "invariant\n"; indent; "    "; string_of_expr inv; ";\n"; emit_invariants indent rest]

let rec emit_stmt (indent:string) (s:stmt) : Tot string (decreases s) =
  match s with
  | SIns op args ->
      concat "" [indent; string_of_opcode op; "("; concat_operands args; ");\n"]
  | SAssign v e ->
      concat "" [indent; v; " := "; string_of_expr e; ";\n"]
  | SAssert e ->
      concat "" [indent; "assert "; string_of_expr e; ";\n"]
  | SAssume e ->
      concat "" [indent; "assume "; string_of_expr e; ";\n"]
  | SBlock stmts ->
      concat "" [
        indent; "{\n";
        emit_stmts (concat "" [indent; "    "]) stmts;
        indent; "}\n"
      ]
  | SIfElse cond t_branch f_branch ->
      concat "" [
        indent; "if ("; string_of_expr cond; ") {\n";
        emit_stmts (concat "" [indent; "    "]) t_branch;
        indent; "} else {\n";
        emit_stmts (concat "" [indent; "    "]) f_branch;
        indent; "}\n"
      ]
  | SWhile cond invs body ->
      concat "" [
        indent; "while ("; string_of_expr cond; ")\n";
        emit_invariants (concat "" [indent; "    "]) invs;
        indent; "{\n";
        emit_stmts (concat "" [indent; "    "]) body;
        indent; "}\n"
      ]
  | SCall name args ->
      concat "" [indent; name; "("; concat_exprs args; ");\n"]

and emit_stmts (indent:string) (stmts:list stmt) : Tot string (decreases stmts) =
  match stmts with
  | [] -> ""
  | s :: rest -> concat "" [emit_stmt indent s; emit_stmts indent rest]

let emit_proc (p:proc_decl) : string =
  let header = concat "" ["procedure "; p.proc_name; "("; concat_pformals p.proc_formals; ")"] in
  let rets =
    match p.proc_returns with
    | [] -> ""
    | _ -> concat "" [" returns ("; concat_pformals p.proc_returns; ")"]
  in
  let specs = emit_specs p.proc_specs in
  let body = emit_stmts "    " p.proc_body in
  concat "" [header; rets; "\n"; specs; "{\n"; body; "}\n\n"]

let rec emit_procs (procs:list proc_decl) : Tot string (decreases procs) =
  match procs with
  | [] -> ""
  | p :: rest -> concat "" [emit_proc p; emit_procs rest]

let emit_vaf_module (m:module_decl) : string =
  concat "" [
    "// Verified Assembly Framework (VAF 2.0)\n";
    "// Emitted by Modern Vale/Pulse Compiler\n\n";
    "module "; m.mod_name; "\n\n";
    emit_procs m.mod_procs
  ]
