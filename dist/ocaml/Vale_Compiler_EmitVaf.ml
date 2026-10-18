open Prims
let rec string_of_typ (t : Vale_Compiler_AST.vaf_typ) : Prims.string=
  match t with
  | Vale_Compiler_AST.TVar s -> s
  | Vale_Compiler_AST.TNat -> "nat"
  | Vale_Compiler_AST.TInt -> "int"
  | Vale_Compiler_AST.TBool -> "bool"
  | Vale_Compiler_AST.TU8 -> "u8"
  | Vale_Compiler_AST.TU32 -> "u32"
  | Vale_Compiler_AST.TU64 -> "u64"
  | Vale_Compiler_AST.TSeq sub ->
      FStar_String.concat "" ["seq("; string_of_typ sub; ")"]
  | Vale_Compiler_AST.TBuffer (sub, uu___) ->
      FStar_String.concat "" ["buffer("; string_of_typ sub; ")"]
  | Vale_Compiler_AST.TTuple ts ->
      FStar_String.concat "" ["("; concat_typs ts; ")"]
and concat_typs (ts : Vale_Compiler_AST.vaf_typ Prims.list) : Prims.string=
  match ts with
  | [] -> ""
  | t::[] -> string_of_typ t
  | t::rest ->
      FStar_String.concat "" [string_of_typ t; ", "; concat_typs rest]
let string_of_reg (r : Vale_Compiler_AST.reg_operand) : Prims.string=
  match r with
  | Vale_Compiler_AST.RegNumbered n ->
      FStar_String.concat "" ["r"; FStar_UInt32.to_string n]
let string_of_opcode (op : Vale_Compiler_AST.ins_opcode) : Prims.string=
  match op with
  | Vale_Compiler_AST.OpMov -> "Mov"
  | Vale_Compiler_AST.OpAdd -> "Add"
  | Vale_Compiler_AST.OpSub -> "Sub"
  | Vale_Compiler_AST.OpXor -> "Xor"
  | Vale_Compiler_AST.OpAnd -> "And"
  | Vale_Compiler_AST.OpOr -> "Or"
  | Vale_Compiler_AST.OpRev -> "Rev"
  | Vale_Compiler_AST.OpLoad -> "Load"
  | Vale_Compiler_AST.OpStore -> "Store"
let string_of_operand (op : Vale_Compiler_AST.operand) : Prims.string=
  match op with
  | Vale_Compiler_AST.OReg r -> string_of_reg r
  | Vale_Compiler_AST.OImm i -> FStar_UInt64.to_string i
  | Vale_Compiler_AST.OMemBuffer (base, offset) ->
      FStar_String.concat ""
        ["["; string_of_reg base; " + "; FStar_UInt32.to_string offset; "]"]
let string_of_uop (op : Vale_Compiler_AST.uop) : Prims.string=
  match op with
  | Vale_Compiler_AST.UNot -> "!"
  | Vale_Compiler_AST.UNeg -> "-"
  | Vale_Compiler_AST.UOld -> "old"
let string_of_bop (op : Vale_Compiler_AST.bop) : Prims.string=
  match op with
  | Vale_Compiler_AST.BAdd -> "+"
  | Vale_Compiler_AST.BSub -> "-"
  | Vale_Compiler_AST.BMul -> "*"
  | Vale_Compiler_AST.BDiv -> "/"
  | Vale_Compiler_AST.BMod -> "%"
  | Vale_Compiler_AST.BAnd -> "&&"
  | Vale_Compiler_AST.BOr -> "||"
  | Vale_Compiler_AST.BEq -> "=="
  | Vale_Compiler_AST.BNe -> "!="
  | Vale_Compiler_AST.BLt -> "<"
  | Vale_Compiler_AST.BGt -> ">"
  | Vale_Compiler_AST.BLe -> "<="
  | Vale_Compiler_AST.BGe -> ">="
  | Vale_Compiler_AST.BImply -> "==>"
  | Vale_Compiler_AST.BEquiv -> "<==>"
let rec string_of_expr (e : Vale_Compiler_AST.expr) : Prims.string=
  match e with
  | Vale_Compiler_AST.EVar v -> v
  | Vale_Compiler_AST.EConst c -> Prims.string_of_int c
  | Vale_Compiler_AST.EBoolConst b -> if b then "true" else "false"
  | Vale_Compiler_AST.EUop (Vale_Compiler_AST.UOld, inner) ->
      FStar_String.concat "" ["old("; string_of_expr inner; ")"]
  | Vale_Compiler_AST.EUop (op, inner) ->
      FStar_String.concat ""
        ["("; string_of_uop op; " "; string_of_expr inner; ")"]
  | Vale_Compiler_AST.EBop (op, e1, e2) ->
      FStar_String.concat ""
        ["(";
        string_of_expr e1;
        " ";
        string_of_bop op;
        " ";
        string_of_expr e2;
        ")"]
  | Vale_Compiler_AST.EApply (f, args) ->
      FStar_String.concat "" [f; "("; concat_exprs args; ")"]
and concat_exprs (es : Vale_Compiler_AST.expr Prims.list) : Prims.string=
  match es with
  | [] -> ""
  | x::[] -> string_of_expr x
  | x::xs -> FStar_String.concat "" [string_of_expr x; ", "; concat_exprs xs]
let string_of_pformal_kind (k : Vale_Compiler_AST.pformal_kind) :
  Prims.string=
  match k with
  | Vale_Compiler_AST.FormalGhost -> "ghost "
  | Vale_Compiler_AST.FormalInline -> "inline "
  | Vale_Compiler_AST.FormalIn -> "in "
  | Vale_Compiler_AST.FormalOut -> "out "
  | Vale_Compiler_AST.FormalInOut -> "inout "
let string_of_pformal (f : Vale_Compiler_AST.pformal) : Prims.string=
  FStar_String.concat ""
    [string_of_pformal_kind f.Vale_Compiler_AST.pformal_kind;
    f.Vale_Compiler_AST.pformal_name;
    ":";
    string_of_typ f.Vale_Compiler_AST.pformal_typ]
let rec concat_pformals (fs : Vale_Compiler_AST.pformal Prims.list) :
  Prims.string=
  match fs with
  | [] -> ""
  | x::[] -> string_of_pformal x
  | x::xs ->
      FStar_String.concat "" [string_of_pformal x; ", "; concat_pformals xs]
let rec concat_operands (ops : Vale_Compiler_AST.operand Prims.list) :
  Prims.string=
  match ops with
  | [] -> ""
  | x::[] -> string_of_operand x
  | x::xs ->
      FStar_String.concat "" [string_of_operand x; ", "; concat_operands xs]
let rec concat_regs (rs : Vale_Compiler_AST.reg_operand Prims.list) :
  Prims.string=
  match rs with
  | [] -> ""
  | x::[] -> string_of_reg x
  | x::xs -> FStar_String.concat "" [string_of_reg x; "; "; concat_regs xs]
let string_of_spec (s : Vale_Compiler_AST.spec_kind) : Prims.string=
  match s with
  | Vale_Compiler_AST.SpecRequires e ->
      FStar_String.concat ""
        ["    requires\n        "; string_of_expr e; ";\n"]
  | Vale_Compiler_AST.SpecEnsures e ->
      FStar_String.concat ""
        ["    ensures\n        "; string_of_expr e; ";\n"]
  | Vale_Compiler_AST.SpecModifies rs ->
      FStar_String.concat ""
        ["    modifies\n        "; concat_regs rs; ";\n"]
  | Vale_Compiler_AST.SpecBufferPerm (r, len) ->
      FStar_String.concat ""
        ["    requires\n        valid_buffer(";
        string_of_reg r;
        ", ";
        Prims.string_of_int len;
        ");\n"]
let rec emit_specs (specs : Vale_Compiler_AST.spec_kind Prims.list) :
  Prims.string=
  match specs with
  | [] -> ""
  | s::rest -> FStar_String.concat "" [string_of_spec s; emit_specs rest]
let rec emit_invariants (indent : Prims.string)
  (invs : Vale_Compiler_AST.expr Prims.list) : Prims.string=
  match invs with
  | [] -> ""
  | inv::rest ->
      FStar_String.concat ""
        [indent;
        "invariant\n";
        indent;
        "    ";
        string_of_expr inv;
        ";\n";
        emit_invariants indent rest]
let rec emit_stmt (indent : Prims.string) (s : Vale_Compiler_AST.stmt) :
  Prims.string=
  match s with
  | Vale_Compiler_AST.SIns (op, args) ->
      FStar_String.concat ""
        [indent; string_of_opcode op; "("; concat_operands args; ");\n"]
  | Vale_Compiler_AST.SAssign (v, e) ->
      FStar_String.concat "" [indent; v; " := "; string_of_expr e; ";\n"]
  | Vale_Compiler_AST.SAssert e ->
      FStar_String.concat "" [indent; "assert "; string_of_expr e; ";\n"]
  | Vale_Compiler_AST.SAssume e ->
      FStar_String.concat "" [indent; "assume "; string_of_expr e; ";\n"]
  | Vale_Compiler_AST.SBlock stmts ->
      FStar_String.concat ""
        [indent;
        "{\n";
        emit_stmts (FStar_String.concat "" [indent; "    "]) stmts;
        indent;
        "}\n"]
  | Vale_Compiler_AST.SIfElse (cond, t_branch, f_branch) ->
      FStar_String.concat ""
        [indent;
        "if (";
        string_of_expr cond;
        ") {\n";
        emit_stmts (FStar_String.concat "" [indent; "    "]) t_branch;
        indent;
        "} else {\n";
        emit_stmts (FStar_String.concat "" [indent; "    "]) f_branch;
        indent;
        "}\n"]
  | Vale_Compiler_AST.SWhile (cond, invs, body) ->
      FStar_String.concat ""
        [indent;
        "while (";
        string_of_expr cond;
        ")\n";
        emit_invariants (FStar_String.concat "" [indent; "    "]) invs;
        indent;
        "{\n";
        emit_stmts (FStar_String.concat "" [indent; "    "]) body;
        indent;
        "}\n"]
  | Vale_Compiler_AST.SCall (name, args) ->
      FStar_String.concat "" [indent; name; "("; concat_exprs args; ");\n"]
and emit_stmts (indent : Prims.string)
  (stmts : Vale_Compiler_AST.stmt Prims.list) : Prims.string=
  match stmts with
  | [] -> ""
  | s::rest ->
      FStar_String.concat "" [emit_stmt indent s; emit_stmts indent rest]
let emit_proc (p : Vale_Compiler_AST.proc_decl) : Prims.string=
  let header =
    FStar_String.concat ""
      ["procedure ";
      p.Vale_Compiler_AST.proc_name;
      "(";
      concat_pformals p.Vale_Compiler_AST.proc_formals;
      ")"] in
  let rets =
    match p.Vale_Compiler_AST.proc_returns with
    | [] -> ""
    | uu___ ->
        FStar_String.concat ""
          [" returns (";
          concat_pformals p.Vale_Compiler_AST.proc_returns;
          ")"] in
  let specs = emit_specs p.Vale_Compiler_AST.proc_specs in
  let body = emit_stmts "    " p.Vale_Compiler_AST.proc_body in
  FStar_String.concat "" [header; rets; "\n"; specs; "{\n"; body; "}\n\n"]
let rec emit_procs (procs : Vale_Compiler_AST.proc_decl Prims.list) :
  Prims.string=
  match procs with
  | [] -> ""
  | p::rest -> FStar_String.concat "" [emit_proc p; emit_procs rest]
let emit_vaf_module (m : Vale_Compiler_AST.module_decl) : Prims.string=
  FStar_String.concat ""
    ["// Verified Assembly Framework (VAF 2.0)\n";
    "// Emitted by Modern Vale/Pulse Compiler\n\n";
    "module ";
    m.Vale_Compiler_AST.mod_name;
    "\n\n";
    emit_procs m.Vale_Compiler_AST.mod_procs]
