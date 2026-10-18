open Prims
let rec string_of_typ (t : Valerie_Compiler_AST.vaf_typ) : Prims.string=
  match t with
  | Valerie_Compiler_AST.TVar s -> s
  | Valerie_Compiler_AST.TNat -> "nat"
  | Valerie_Compiler_AST.TInt -> "int"
  | Valerie_Compiler_AST.TBool -> "bool"
  | Valerie_Compiler_AST.TU8 -> "u8"
  | Valerie_Compiler_AST.TU32 -> "u32"
  | Valerie_Compiler_AST.TU64 -> "u64"
  | Valerie_Compiler_AST.TSeq sub ->
      FStar_String.concat "" ["(seq "; string_of_typ sub; ")"]
  | Valerie_Compiler_AST.TBuffer (sub, uu___) ->
      FStar_String.concat "" ["(buffer "; string_of_typ sub; ")"]
  | Valerie_Compiler_AST.TTuple ts ->
      FStar_String.concat "" ["("; concat_typs ts; ")"]
and concat_typs (ts : Valerie_Compiler_AST.vaf_typ Prims.list) :
  Prims.string=
  match ts with
  | [] -> ""
  | t::[] -> string_of_typ t
  | t::rest ->
      FStar_String.concat "" [string_of_typ t; " & "; concat_typs rest]
let string_of_target (t : Valerie_Compiler_AST.arch_target) : Prims.string=
  match t with
  | Valerie_Compiler_AST.TargetAArch64 -> "AArch64"
  | Valerie_Compiler_AST.TargetArm32 -> "Arm32"
  | Valerie_Compiler_AST.TargetX64 -> "X64"
  | Valerie_Compiler_AST.TargetRiscv64 -> "Riscv64"
let string_of_reg (r : Valerie_Compiler_AST.reg_operand) : Prims.string=
  match r with
  | Valerie_Compiler_AST.RegNumbered n ->
      FStar_String.concat "" ["r"; FStar_UInt32.to_string n]
let string_of_opcode (op : Valerie_Compiler_AST.ins_opcode) : Prims.string=
  match op with
  | Valerie_Compiler_AST.OpMov -> "mov"
  | Valerie_Compiler_AST.OpAdd -> "add"
  | Valerie_Compiler_AST.OpSub -> "sub"
  | Valerie_Compiler_AST.OpXor -> "xor"
  | Valerie_Compiler_AST.OpAnd -> "and"
  | Valerie_Compiler_AST.OpOr -> "or"
  | Valerie_Compiler_AST.OpRev -> "rev"
  | Valerie_Compiler_AST.OpLoad -> "load"
  | Valerie_Compiler_AST.OpStore -> "store"
let string_of_operand (op : Valerie_Compiler_AST.operand) : Prims.string=
  match op with
  | Valerie_Compiler_AST.OReg r ->
      FStar_String.concat "" ["(OReg "; string_of_reg r; ")"]
  | Valerie_Compiler_AST.OImm i ->
      FStar_String.concat "" ["(OImm "; FStar_UInt64.to_string i; ")"]
  | Valerie_Compiler_AST.OMemBuffer (base, offset) ->
      FStar_String.concat ""
        ["(OMemBuffer ";
        string_of_reg base;
        " ";
        FStar_UInt32.to_string offset;
        ")"]
let string_of_bop (op : Valerie_Compiler_AST.bop) : Prims.string=
  match op with
  | Valerie_Compiler_AST.BAdd -> "+"
  | Valerie_Compiler_AST.BSub -> "-"
  | Valerie_Compiler_AST.BMul -> "*"
  | Valerie_Compiler_AST.BDiv -> "/"
  | Valerie_Compiler_AST.BMod -> "%"
  | Valerie_Compiler_AST.BAnd -> "/\\"
  | Valerie_Compiler_AST.BOr -> "\\/"
  | Valerie_Compiler_AST.BEq -> "=="
  | Valerie_Compiler_AST.BNe -> "<>"
  | Valerie_Compiler_AST.BLt -> "<"
  | Valerie_Compiler_AST.BGt -> ">"
  | Valerie_Compiler_AST.BLe -> "<="
  | Valerie_Compiler_AST.BGe -> ">="
  | Valerie_Compiler_AST.BImply -> "==>"
  | Valerie_Compiler_AST.BEquiv -> "<==>"
let rec string_of_expr (e : Valerie_Compiler_AST.expr) : Prims.string=
  match e with
  | Valerie_Compiler_AST.EVar v -> v
  | Valerie_Compiler_AST.EConst c -> Prims.string_of_int c
  | Valerie_Compiler_AST.EBoolConst b -> if b then "true" else "false"
  | Valerie_Compiler_AST.EUop (Valerie_Compiler_AST.UNot, inner) ->
      FStar_String.concat "" ["(~ "; string_of_expr inner; ")"]
  | Valerie_Compiler_AST.EUop (Valerie_Compiler_AST.UNeg, inner) ->
      FStar_String.concat "" ["(- "; string_of_expr inner; ")"]
  | Valerie_Compiler_AST.EUop (Valerie_Compiler_AST.UOld, inner) ->
      FStar_String.concat "" ["(old "; string_of_expr inner; ")"]
  | Valerie_Compiler_AST.EBop (op, e1, e2) ->
      FStar_String.concat ""
        ["(";
        string_of_expr e1;
        " ";
        string_of_bop op;
        " ";
        string_of_expr e2;
        ")"]
  | Valerie_Compiler_AST.EApply (f, args) ->
      FStar_String.concat "" ["("; f; " "; concat_exprs args; ")"]
and concat_exprs (es : Valerie_Compiler_AST.expr Prims.list) : Prims.string=
  match es with
  | [] -> ""
  | x::[] -> string_of_expr x
  | x::xs -> FStar_String.concat "" [string_of_expr x; " "; concat_exprs xs]
let rec concat_operands (ops : Valerie_Compiler_AST.operand Prims.list) :
  Prims.string=
  match ops with
  | [] -> ""
  | x::[] -> string_of_operand x
  | x::xs ->
      FStar_String.concat "" [string_of_operand x; " "; concat_operands xs]
let rec print_stmt (indent : Prims.string) (s : Valerie_Compiler_AST.stmt) :
  Prims.string=
  match s with
  | Valerie_Compiler_AST.SIns (op, args) ->
      FStar_String.concat ""
        [indent;
        "(ins ";
        string_of_opcode op;
        " [";
        concat_operands args;
        "])\n"]
  | Valerie_Compiler_AST.SAssign (v, e) ->
      FStar_String.concat ""
        [indent; "(set "; v; " "; string_of_expr e; ")\n"]
  | Valerie_Compiler_AST.SAssert e ->
      FStar_String.concat "" [indent; "(assert "; string_of_expr e; ")\n"]
  | Valerie_Compiler_AST.SAssume e ->
      FStar_String.concat "" [indent; "(assume "; string_of_expr e; ")\n"]
  | Valerie_Compiler_AST.SBlock stmts ->
      FStar_String.concat ""
        [indent;
        "(block\n";
        print_stmts (FStar_String.concat "" [indent; "  "]) stmts;
        indent;
        ")\n"]
  | Valerie_Compiler_AST.SIfElse (cond, t_branch, f_branch) ->
      FStar_String.concat ""
        [indent;
        "(if ";
        string_of_expr cond;
        "\n";
        print_stmts (FStar_String.concat "" [indent; "  "]) t_branch;
        indent;
        "else\n";
        print_stmts (FStar_String.concat "" [indent; "  "]) f_branch;
        indent;
        ")\n"]
  | Valerie_Compiler_AST.SWhile (cond, invs, body) ->
      FStar_String.concat ""
        [indent;
        "(while ";
        string_of_expr cond;
        "\n";
        print_stmts (FStar_String.concat "" [indent; "  "]) body;
        indent;
        ")\n"]
  | Valerie_Compiler_AST.SCall (name, args) ->
      FStar_String.concat ""
        [indent; "(call "; name; " ["; concat_exprs args; "])\n"]
and print_stmts (indent : Prims.string)
  (stmts : Valerie_Compiler_AST.stmt Prims.list) : Prims.string=
  match stmts with
  | [] -> ""
  | s::rest ->
      FStar_String.concat "" [print_stmt indent s; print_stmts indent rest]
let print_proc (p : Valerie_Compiler_AST.proc_decl) : Prims.string=
  FStar_String.concat ""
    ["Procedure ";
    p.Valerie_Compiler_AST.proc_name;
    " [";
    string_of_target p.Valerie_Compiler_AST.proc_target;
    "]:\n";
    print_stmts "  " p.Valerie_Compiler_AST.proc_body;
    "\n"]
