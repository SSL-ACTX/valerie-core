module Valerie.Compiler.Print

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
  | TSeq sub -> concat "" ["(seq "; string_of_typ sub; ")"]
  | TBuffer sub _ -> concat "" ["(buffer "; string_of_typ sub; ")"]
  | TTuple ts -> concat "" ["("; concat_typs ts; ")"]

and concat_typs (ts:list vaf_typ) : Tot string (decreases ts) =
  match ts with
  | [] -> ""
  | [t] -> string_of_typ t
  | t :: rest -> concat "" [string_of_typ t; " & "; concat_typs rest]

let string_of_target (t:arch_target) : string =
  match t with
  | TargetAArch64 -> "AArch64"
  | TargetArm32 -> "Arm32"
  | TargetX64 -> "X64"
  | TargetRiscv64 -> "Riscv64"

let string_of_reg (r:reg_operand) : string =
  match r with
  | RegNumbered n -> concat "" ["r"; FStar.UInt32.to_string n]

let string_of_opcode (op:ins_opcode) : string =
  match op with
  | OpMov -> "mov"
  | OpAdd -> "add"
  | OpSub -> "sub"
  | OpXor -> "xor"
  | OpAnd -> "and"
  | OpOr  -> "or"
  | OpRev -> "rev"
  | OpLoad -> "load"
  | OpStore -> "store"

let string_of_operand (op:operand) : string =
  match op with
  | OReg r -> concat "" ["(OReg "; string_of_reg r; ")"]
  | OImm i -> concat "" ["(OImm "; FStar.UInt64.to_string i; ")"]
  | OMemBuffer base offset -> concat "" ["(OMemBuffer "; string_of_reg base; " "; FStar.UInt32.to_string offset; ")"]

let string_of_bop (op:bop) : string =
  match op with
  | BAdd -> "+"
  | BSub -> "-"
  | BMul -> "*"
  | BDiv -> "/"
  | BMod -> "%"
  | BAnd -> "/\\"
  | BOr -> "\\/"
  | BEq -> "=="
  | BNe -> "<>"
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
  | EUop UNot inner -> concat "" ["(~ "; string_of_expr inner; ")"]
  | EUop UNeg inner -> concat "" ["(- "; string_of_expr inner; ")"]
  | EUop UOld inner -> concat "" ["(old "; string_of_expr inner; ")"]
  | EBop op e1 e2 -> concat "" ["("; string_of_expr e1; " "; string_of_bop op; " "; string_of_expr e2; ")"]
  | EApply f args -> concat "" ["("; f; " "; concat_exprs args; ")"]

and concat_exprs (es:list expr) : Tot string (decreases es) =
  match es with
  | [] -> ""
  | [x] -> string_of_expr x
  | x :: xs -> concat "" [string_of_expr x; " "; concat_exprs xs]

let rec concat_operands (ops:list operand) : Tot string (decreases ops) =
  match ops with
  | [] -> ""
  | [x] -> string_of_operand x
  | x :: xs -> concat "" [string_of_operand x; " "; concat_operands xs]

let rec print_stmt (indent:string) (s:stmt) : Tot string (decreases s) =
  match s with
  | SIns op args -> concat "" [indent; "(ins "; string_of_opcode op; " ["; concat_operands args; "])\n"]
  | SAssign v e -> concat "" [indent; "(set "; v; " "; string_of_expr e; ")\n"]
  | SAssert e -> concat "" [indent; "(assert "; string_of_expr e; ")\n"]
  | SAssume e -> concat "" [indent; "(assume "; string_of_expr e; ")\n"]
  | SBlock stmts -> concat "" [indent; "(block\n"; print_stmts (concat "" [indent; "  "]) stmts; indent; ")\n"]
  | SIfElse cond t_branch f_branch ->
      concat "" [
        indent; "(if "; string_of_expr cond; "\n";
        print_stmts (concat "" [indent; "  "]) t_branch;
        indent; "else\n";
        print_stmts (concat "" [indent; "  "]) f_branch;
        indent; ")\n"
      ]
  | SWhile cond invs body ->
      concat "" [
        indent; "(while "; string_of_expr cond; "\n";
        print_stmts (concat "" [indent; "  "]) body;
        indent; ")\n"
      ]
  | SCall name args -> concat "" [indent; "(call "; name; " ["; concat_exprs args; "])\n"]

and print_stmts (indent:string) (stmts:list stmt) : Tot string (decreases stmts) =
  match stmts with
  | [] -> ""
  | s :: rest -> concat "" [print_stmt indent s; print_stmts indent rest]

let print_proc (p:proc_decl) : string =
  concat "" [
    "Procedure "; p.proc_name; " ["; string_of_target p.proc_target; "]:\n";
    print_stmts "  " p.proc_body;
    "\n"
  ]
