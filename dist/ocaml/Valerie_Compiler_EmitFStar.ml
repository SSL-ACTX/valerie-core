open Prims
let rec string_of_typ (t : Valerie_Compiler_AST.vaf_typ) : Prims.string=
  match t with
  | Valerie_Compiler_AST.TVar s -> s
  | Valerie_Compiler_AST.TNat -> "nat"
  | Valerie_Compiler_AST.TInt -> "int"
  | Valerie_Compiler_AST.TBool -> "bool"
  | Valerie_Compiler_AST.TU8 -> "FStar.UInt8.t"
  | Valerie_Compiler_AST.TU32 -> "FStar.UInt32.t"
  | Valerie_Compiler_AST.TU64 -> "FStar.UInt64.t"
  | Valerie_Compiler_AST.TSeq sub ->
      FStar_String.concat "" ["(Seq.seq "; string_of_typ sub; ")"]
  | Valerie_Compiler_AST.TBuffer (sub, uu___) ->
      FStar_String.concat "" ["(buffer "; string_of_typ sub; ")"]
  | Valerie_Compiler_AST.TTuple ts ->
      FStar_String.concat "" ["("; concat_typs ts; ")"]
and concat_typs (ts : Valerie_Compiler_AST.vaf_typ Prims.list) :
  Prims.string=
  match ts with
  | [] -> "unit"
  | t::[] -> string_of_typ t
  | t::rest ->
      FStar_String.concat "" [string_of_typ t; " & "; concat_typs rest]
let string_of_uop (op : Valerie_Compiler_AST.uop) : Prims.string=
  match op with
  | Valerie_Compiler_AST.UNot -> "~"
  | Valerie_Compiler_AST.UNeg -> "-"
  | Valerie_Compiler_AST.UOld -> "old"
let string_of_bop (op : Valerie_Compiler_AST.bop) : Prims.string=
  match op with
  | Valerie_Compiler_AST.BAdd -> "+"
  | Valerie_Compiler_AST.BSub -> "-"
  | Valerie_Compiler_AST.BMul -> "`Prims.op_Multiply`"
  | Valerie_Compiler_AST.BDiv -> "/"
  | Valerie_Compiler_AST.BMod -> "%"
  | Valerie_Compiler_AST.BAnd -> "/\\"
  | Valerie_Compiler_AST.BOr -> "\\/"
  | Valerie_Compiler_AST.BEq -> "=="
  | Valerie_Compiler_AST.BNe -> "=!="
  | Valerie_Compiler_AST.BLt -> "<"
  | Valerie_Compiler_AST.BGt -> ">"
  | Valerie_Compiler_AST.BLe -> "<="
  | Valerie_Compiler_AST.BGe -> ">="
  | Valerie_Compiler_AST.BImply -> "==>"
  | Valerie_Compiler_AST.BEquiv -> "<==>"
let is_digit_char (c : FStar_Char.char) : Prims.bool=
  let n = FStar_Char.int_of_char c in
  (n >= (Prims.of_int 48)) && (n <= (Prims.of_int 57))
let rec all_digits_list (cs : FStar_Char.char Prims.list) : Prims.bool=
  match cs with
  | [] -> true
  | c::rest -> (is_digit_char c) && (all_digits_list rest)
let all_digits (s : Prims.string) : Prims.bool=
  let cs = FStar_String.list_of_string s in
  (match cs with | hd::tl -> true | uu___ -> false) && (all_digits_list cs)
let format_fstar_var (v : Prims.string) : Prims.string=
  if (FStar_String.strlen v) >= (Prims.of_int 2)
  then
    let first = FStar_String.sub v Prims.int_zero Prims.int_one in
    let rest =
      FStar_String.sub v Prims.int_one
        ((FStar_String.strlen v) - Prims.int_one) in
    (if
       ((((first = "x") || (first = "r")) || (first = "X")) || (first = "R"))
         && (all_digits rest)
     then FStar_String.concat "" ["(va_op_reg_reg R"; rest; ")"]
     else
       if ((first = "v") || (first = "V")) && (all_digits rest)
       then FStar_String.concat "" ["(va_op_vec_reg V"; rest; ")"]
       else v)
  else v
let rec string_of_expr (e : Valerie_Compiler_AST.expr) : Prims.string=
  match e with
  | Valerie_Compiler_AST.EVar v -> format_fstar_var v
  | Valerie_Compiler_AST.EConst c -> Prims.string_of_int c
  | Valerie_Compiler_AST.EBoolConst b -> if b then "true" else "false"
  | Valerie_Compiler_AST.EUop (Valerie_Compiler_AST.UOld, inner) ->
      FStar_String.concat "" ["(old "; string_of_expr inner; ")"]
  | Valerie_Compiler_AST.EUop (op, inner) ->
      FStar_String.concat ""
        ["("; string_of_uop op; " "; string_of_expr inner; ")"]
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
let string_of_pformal (f : Valerie_Compiler_AST.pformal) : Prims.string=
  FStar_String.concat ""
    [f.Valerie_Compiler_AST.pformal_name;
    ":";
    string_of_typ f.Valerie_Compiler_AST.pformal_typ]
let rec concat_pformal_types (fs : Valerie_Compiler_AST.pformal Prims.list) :
  Prims.string=
  match fs with
  | [] -> "va_dummy:unit"
  | x::[] -> string_of_pformal x
  | x::xs ->
      FStar_String.concat ""
        [string_of_pformal x; " -> "; concat_pformal_types xs]
let rec concat_pformal_args (fs : Valerie_Compiler_AST.pformal Prims.list) :
  Prims.string=
  match fs with
  | [] -> ""
  | x::[] -> x.Valerie_Compiler_AST.pformal_name
  | x::xs ->
      FStar_String.concat ""
        [x.Valerie_Compiler_AST.pformal_name; " "; concat_pformal_args xs]
let emit_code_val_decl (p : Valerie_Compiler_AST.proc_decl) : Prims.string=
  let args_sig = concat_pformal_types p.Valerie_Compiler_AST.proc_formals in
  FStar_String.concat ""
    ["val va_code_";
    p.Valerie_Compiler_AST.proc_name;
    " : ";
    args_sig;
    " -> Tot va_code\n";
    "[@ \"opaque_to_smt\"]\n"]
let rec emit_cccons_chain (stmts : Valerie_Compiler_AST.stmt Prims.list) :
  Prims.string=
  match stmts with
  | [] -> "(va_CNil ())"
  | s::rest ->
      (match s with
       | Valerie_Compiler_AST.SCall (name, args) ->
           FStar_String.concat ""
             ["(va_CCons (va_code_";
             name;
             if args = []
             then " ()"
             else FStar_String.concat "" [" "; concat_exprs args];
             ") ";
             emit_cccons_chain rest;
             ")"]
       | uu___ -> emit_cccons_chain rest)
let emit_code_def (p : Valerie_Compiler_AST.proc_decl) : Prims.string=
  let args = concat_pformal_args p.Valerie_Compiler_AST.proc_formals in
  let args_str = if args = "" then "()" else args in
  if p.Valerie_Compiler_AST.proc_is_ins
  then
    FStar_String.concat ""
      ["let va_code_";
      p.Valerie_Compiler_AST.proc_name;
      " ";
      args_str;
      " =\n";
      "  (";
      p.Valerie_Compiler_AST.proc_ins_term;
      ")\n\n"]
  else
    FStar_String.concat ""
      ["let va_code_";
      p.Valerie_Compiler_AST.proc_name;
      " ";
      args_str;
      " =\n";
      "  (va_Block ";
      emit_cccons_chain p.Valerie_Compiler_AST.proc_body;
      ")\n\n"]
let emit_codegen_success (p : Valerie_Compiler_AST.proc_decl) : Prims.string=
  let args_sig = concat_pformal_types p.Valerie_Compiler_AST.proc_formals in
  let args = concat_pformal_args p.Valerie_Compiler_AST.proc_formals in
  let args_str = if args = "" then "()" else args in
  FStar_String.concat ""
    ["val va_codegen_success_";
    p.Valerie_Compiler_AST.proc_name;
    " : ";
    args_sig;
    " -> Tot va_pbool\n";
    "[@ \"opaque_to_smt\"]\n";
    "let va_codegen_success_";
    p.Valerie_Compiler_AST.proc_name;
    " ";
    args_str;
    " =\n";
    "  (va_ttrue ())\n\n"]
let rec concat_requires (specs : Valerie_Compiler_AST.spec_kind Prims.list) :
  Prims.string=
  match specs with
  | [] -> "True"
  | (Valerie_Compiler_AST.SpecRequires e)::rest ->
      (match rest with
       | [] -> string_of_expr e
       | uu___ ->
           FStar_String.concat ""
             [string_of_expr e; " /\\ "; concat_requires rest])
  | uu___::rest -> concat_requires rest
let rec concat_ensures (specs : Valerie_Compiler_AST.spec_kind Prims.list) :
  Prims.string=
  match specs with
  | [] -> "True"
  | (Valerie_Compiler_AST.SpecEnsures e)::rest ->
      (match rest with
       | [] -> string_of_expr e
       | uu___ ->
           FStar_String.concat ""
             [string_of_expr e; " /\\ "; concat_ensures rest])
  | uu___::rest -> concat_ensures rest
let rec fold_updates (mods : Valerie_Compiler_AST.id Prims.list) :
  Prims.string=
  match mods with
  | [] -> "(va_update_ok va_sM va_s0)"
  | m::rest ->
      FStar_String.concat ""
        ["(va_update_reg "; m; " va_sM "; fold_updates rest; ")"]
let rec emit_lemma_steps (idx : Prims.int)
  (stmts : Valerie_Compiler_AST.stmt Prims.list) : Prims.string=
  match stmts with
  | [] -> ""
  | s::rest ->
      (match s with
       | Valerie_Compiler_AST.SCall (name, args) ->
           let curr_s =
             if idx = Prims.int_one
             then "va_s0"
             else FStar_String.concat "" ["va_s"; Prims.string_of_int idx] in
           let next_idx = idx + Prims.int_one in
           let next_s =
             FStar_String.concat "" ["va_s"; Prims.string_of_int next_idx] in
           let fc =
             FStar_String.concat "" ["va_fc"; Prims.string_of_int next_idx] in
           let b_in =
             if idx = Prims.int_one
             then "va_b1"
             else FStar_String.concat "" ["va_b"; Prims.string_of_int idx] in
           let b_next =
             FStar_String.concat "" ["va_b"; Prims.string_of_int next_idx] in
           let call_args =
             if args = []
             then ""
             else FStar_String.concat "" [" "; concat_exprs args] in
           FStar_String.concat ""
             ["  let (";
             next_s;
             ", ";
             fc;
             ") = va_lemma_";
             name;
             " (va_hd ";
             b_in;
             ") ";
             curr_s;
             call_args;
             " in\n";
             "  let ";
             b_next;
             " = va_tl ";
             b_in;
             " in\n";
             emit_lemma_steps next_idx rest]
       | uu___ -> emit_lemma_steps idx rest)
let rec count_scalls (stmts : Valerie_Compiler_AST.stmt Prims.list) :
  Prims.nat=
  match stmts with
  | [] -> Prims.int_zero
  | (Valerie_Compiler_AST.SCall (uu___, uu___1))::rest ->
      Prims.int_one + (count_scalls rest)
  | uu___::rest -> count_scalls rest
let rec emit_merge_steps (cur_i : Prims.nat) (tot_count : Prims.int) :
  Prims.string=
  if cur_i <= Prims.int_one
  then ""
  else
    (let prev_i = cur_i - Prims.int_one in
     let f_curr =
       if cur_i = (tot_count + Prims.int_one)
       then FStar_String.concat "" ["va_f"; Prims.string_of_int tot_count]
       else FStar_String.concat "" ["va_f"; Prims.string_of_int cur_i] in
     let f_prev =
       if prev_i = Prims.int_one
       then "va_fM"
       else FStar_String.concat "" ["va_f"; Prims.string_of_int prev_i] in
     let b_curr = FStar_String.concat "" ["va_b"; Prims.string_of_int prev_i] in
     let s_curr =
       if prev_i = Prims.int_one
       then "va_s0"
       else FStar_String.concat "" ["va_s"; Prims.string_of_int prev_i] in
     let fc = FStar_String.concat "" ["va_fc"; Prims.string_of_int cur_i] in
     let s_next = FStar_String.concat "" ["va_s"; Prims.string_of_int cur_i] in
     FStar_String.concat ""
       ["  let ";
       f_prev;
       " = va_lemma_merge_total ";
       b_curr;
       " ";
       s_curr;
       " ";
       fc;
       " ";
       s_next;
       " ";
       f_curr;
       " va_sM in\n";
       emit_merge_steps prev_i tot_count])
let emit_primitive_lemma (p : Valerie_Compiler_AST.proc_decl) : Prims.string=
  let args = concat_pformal_args p.Valerie_Compiler_AST.proc_formals in
  let args_sig = concat_pformal_types p.Valerie_Compiler_AST.proc_formals in
  let code_call =
    FStar_String.concat ""
      ["va_code_";
      p.Valerie_Compiler_AST.proc_name;
      if args = "" then " ()" else FStar_String.concat "" [" "; args]] in
  FStar_String.concat ""
    ["val va_lemma_";
    p.Valerie_Compiler_AST.proc_name;
    " : va_b0:va_code -> va_s0:va_state -> ";
    args_sig;
    "\n";
    "  -> Ghost (va_state & va_fuel)\n";
    "  (requires (va_require_total va_b0 (";
    code_call;
    ") va_s0 /\\ va_get_ok va_s0))\n";
    "  (ensures (fun (va_sM, va_fM) -> va_ensure_total va_b0 va_s0 va_sM va_fM /\\ va_get_ok va_sM /\\\n";
    "    va_state_eq va_sM (va_update_ok va_sM va_s0)))\n";
    "[@ \"opaque_to_smt\"]\n";
    "let va_lemma_";
    p.Valerie_Compiler_AST.proc_name;
    " va_b0 va_s0 ";
    if args = "" then "()" else args;
    " =\n";
    "  va_reveal_opaque (`%va_code_";
    p.Valerie_Compiler_AST.proc_name;
    ") (";
    code_call;
    ");\n";
    "  let (va_old_s:va_state) = va_s0 in\n";
    "  va_ins_lemma (";
    p.Valerie_Compiler_AST.proc_ins_term;
    ") va_s0;\n";
    "  let (va_sM, va_fM) = va_eval_ins (";
    p.Valerie_Compiler_AST.proc_ins_term;
    ") va_s0 in\n";
    "  assume false;\n";
    "  (va_sM, va_fM)\n\n"]
let emit_composite_lemma (p : Valerie_Compiler_AST.proc_decl) : Prims.string=
  let args = concat_pformal_args p.Valerie_Compiler_AST.proc_formals in
  let args_sig = concat_pformal_types p.Valerie_Compiler_AST.proc_formals in
  let n = count_scalls p.Valerie_Compiler_AST.proc_body in
  let code_call =
    FStar_String.concat ""
      ["va_code_";
      p.Valerie_Compiler_AST.proc_name;
      if args = "" then " ()" else FStar_String.concat "" [" "; args]] in
  let reqs =
    FStar_String.concat ""
      ["va_require_total va_b0 ("; code_call; ") va_s0 /\\ va_get_ok va_s0"] in
  let updates = fold_updates p.Valerie_Compiler_AST.proc_modifies in
  let last_s =
    FStar_String.concat "" ["va_s"; Prims.string_of_int (n + Prims.int_one)] in
  let last_b =
    FStar_String.concat "" ["va_b"; Prims.string_of_int (n + Prims.int_one)] in
  let last_f = FStar_String.concat "" ["va_f"; Prims.string_of_int n] in
  FStar_String.concat ""
    ["val va_lemma_";
    p.Valerie_Compiler_AST.proc_name;
    " : va_b0:va_code -> va_s0:va_state";
    if args = "" then "" else FStar_String.concat "" [" -> "; args_sig];
    "\n";
    "  -> Ghost (va_state & va_fuel)\n";
    "  (requires (";
    reqs;
    "))\n";
    "  (ensures (fun (va_sM, va_fM) -> va_ensure_total va_b0 va_s0 va_sM va_fM /\\ va_get_ok va_sM /\\\n";
    "    va_state_eq va_sM ";
    updates;
    "))\n";
    "[@ \"opaque_to_smt\"]\n";
    "let va_lemma_";
    p.Valerie_Compiler_AST.proc_name;
    " va_b0 va_s0 ";
    if args = "" then "" else FStar_String.concat "" [args; " "];
    "=\n";
    "  va_reveal_opaque (`%va_code_";
    p.Valerie_Compiler_AST.proc_name;
    ") (";
    code_call;
    ");\n";
    "  let (va_old_s:va_state) = va_s0 in\n";
    "  let (va_b1:va_codes) = va_get_block va_b0 in\n";
    emit_lemma_steps Prims.int_one p.Valerie_Compiler_AST.proc_body;
    "  let (va_sM, ";
    last_f;
    ") = va_lemma_empty_total ";
    last_s;
    " ";
    last_b;
    " in\n";
    emit_merge_steps (n + Prims.int_one) n;
    "  (va_sM, va_fM)\n\n"]
let emit_proc_fstar (p : Valerie_Compiler_AST.proc_decl) : Prims.string=
  let sep =
    FStar_String.concat ""
      ["//-- "; p.Valerie_Compiler_AST.proc_name; "\n\n"] in
  let code_val = emit_code_val_decl p in
  let code_def = emit_code_def p in
  let cg_success = emit_codegen_success p in
  let lemma =
    if p.Valerie_Compiler_AST.proc_is_ins
    then emit_primitive_lemma p
    else emit_composite_lemma p in
  FStar_String.concat ""
    [sep; code_val; code_def; cg_success; lemma; "//--\n"]
let rec emit_fstar_procs (procs : Valerie_Compiler_AST.proc_decl Prims.list)
  : Prims.string=
  match procs with
  | [] -> ""
  | p::rest ->
      FStar_String.concat "" [emit_proc_fstar p; emit_fstar_procs rest]
let rec emit_fstar_includes (incs : Valerie_Compiler_AST.id Prims.list) :
  Prims.string=
  match incs with
  | [] -> ""
  | inc::rest ->
      FStar_String.concat "" ["open "; inc; "\n"; emit_fstar_includes rest]
let emit_fstar_module (m : Valerie_Compiler_AST.module_decl)
  (target : Valerie_Compiler_AST.arch_target) : Prims.string=
  let opens =
    match m.Valerie_Compiler_AST.mod_includes with
    | [] -> ""
    | incs -> FStar_String.concat "" [emit_fstar_includes incs; "\n"] in
  FStar_String.concat ""
    ["module ";
    m.Valerie_Compiler_AST.mod_name;
    "\n\n";
    opens;
    emit_fstar_procs m.Valerie_Compiler_AST.mod_procs]
