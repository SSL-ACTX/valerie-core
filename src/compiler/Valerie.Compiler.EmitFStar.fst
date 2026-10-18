module Valerie.Compiler.EmitFStar

open FStar.String
open Valerie.Compiler.AST

let rec string_of_typ (t:vaf_typ) : Tot string (decreases t) =
  match t with
  | TVar s -> s
  | TNat -> "nat"
  | TInt -> "int"
  | TBool -> "bool"
  | TU8 -> "FStar.UInt8.t"
  | TU32 -> "FStar.UInt32.t"
  | TU64 -> "FStar.UInt64.t"
  | TSeq sub -> concat "" ["(Seq.seq "; string_of_typ sub; ")"]
  | TBuffer sub _ -> concat "" ["(buffer "; string_of_typ sub; ")"]
  | TTuple ts -> concat "" ["("; concat_typs ts; ")"]

and concat_typs (ts:list vaf_typ) : Tot string (decreases ts) =
  match ts with
  | [] -> "unit"
  | [t] -> string_of_typ t
  | t :: rest -> concat "" [string_of_typ t; " & "; concat_typs rest]

let string_of_uop (op:uop) : string =
  match op with
  | UNot -> "~"
  | UNeg -> "-"
  | UOld -> "old"

let string_of_bop (op:bop) : string =
  match op with
  | BAdd -> "+"
  | BSub -> "-"
  | BMul -> "`Prims.op_Multiply`"
  | BDiv -> "/"
  | BMod -> "%"
  | BAnd -> "/\\"
  | BOr -> "\\/"
  | BEq -> "=="
  | BNe -> "=!="
  | BLt -> "<"
  | BGt -> ">"
  | BLe -> "<="
  | BGe -> ">="
  | BImply -> "==>"
  | BEquiv -> "<==>"

let is_digit_char (c:FStar.Char.char) : bool =
  let n = FStar.Char.int_of_char c in
  n >= 48 && n <= 57

let rec all_digits_list (cs:list FStar.Char.char) : Tot bool (decreases cs) =
  match cs with
  | [] -> true
  | c :: rest -> is_digit_char c && all_digits_list rest

let all_digits (s:string) : bool =
  let cs = list_of_string s in
  Cons? cs && all_digits_list cs

let format_fstar_var (v:string) : string =
  if length v >= 2 then
    let first = sub v 0 1 in
    let rest = sub v 1 (length v - 1) in
    if (first = "x" || first = "r" || first = "X" || first = "R") && all_digits rest then
      concat "" ["(va_op_reg_reg R"; rest; ")"]
    else if (first = "v" || first = "V") && all_digits rest then
      concat "" ["(va_op_vec_reg V"; rest; ")"]
    else v
  else v

let rec string_of_expr (e:expr) : Tot string (decreases e) =
  match e with
  | EVar v -> format_fstar_var v
  | EConst c -> string_of_int c
  | EBoolConst b -> if b then "true" else "false"
  | EUop UOld inner -> concat "" ["(old "; string_of_expr inner; ")"]
  | EUop op inner -> concat "" ["("; string_of_uop op; " "; string_of_expr inner; ")"]
  | EBop op e1 e2 -> concat "" ["("; string_of_expr e1; " "; string_of_bop op; " "; string_of_expr e2; ")"]
  | EApply f args -> concat "" ["("; f; " "; concat_exprs args; ")"]

and concat_exprs (es:list expr) : Tot string (decreases es) =
  match es with
  | [] -> ""
  | [x] -> string_of_expr x
  | x :: xs -> concat "" [string_of_expr x; " "; concat_exprs xs]

let string_of_pformal (f:pformal) : string =
  concat "" [f.pformal_name; ":"; string_of_typ f.pformal_typ]

let rec concat_pformal_types (fs:list pformal) : Tot string (decreases fs) =
  match fs with
  | [] -> "va_dummy:unit"
  | [x] -> string_of_pformal x
  | x :: xs -> concat "" [string_of_pformal x; " -> "; concat_pformal_types xs]

let rec concat_pformal_args (fs:list pformal) : Tot string (decreases fs) =
  match fs with
  | [] -> ""
  | [x] -> x.pformal_name
  | x :: xs -> concat "" [x.pformal_name; " "; concat_pformal_args xs]

let emit_code_val_decl (p:proc_decl) : string =
  let args_sig = concat_pformal_types p.proc_formals in
  concat "" [
    "val va_code_"; p.proc_name; " : "; args_sig; " -> Tot va_code\n";
    "[@ \"opaque_to_smt\"]\n"
  ]

let rec emit_cccons_chain (stmts:list stmt) : Tot string (decreases stmts) =
  match stmts with
  | [] -> "(va_CNil ())"
  | s :: rest ->
      (match s with
       | SCall name args ->
           concat "" [
             "(va_CCons (va_code_"; name;
             (if args = [] then " ()" else concat "" [" "; concat_exprs args]);
             ") "; emit_cccons_chain rest; ")"
           ]
       | _ -> emit_cccons_chain rest)

let emit_code_def (p:proc_decl) : string =
  let args = concat_pformal_args p.proc_formals in
  let args_str = if args = "" then "()" else args in
  if p.proc_is_ins then
    concat "" [
      "let va_code_"; p.proc_name; " "; args_str; " =\n";
      "  ("; p.proc_ins_term; ")\n\n"
    ]
  else
    concat "" [
      "let va_code_"; p.proc_name; " "; args_str; " =\n";
      "  (va_Block "; emit_cccons_chain p.proc_body; ")\n\n"
    ]

let emit_codegen_success (p:proc_decl) : string =
  let args_sig = concat_pformal_types p.proc_formals in
  let args = concat_pformal_args p.proc_formals in
  let args_str = if args = "" then "()" else args in
  concat "" [
    "val va_codegen_success_"; p.proc_name; " : "; args_sig; " -> Tot va_pbool\n";
    "[@ \"opaque_to_smt\"]\n";
    "let va_codegen_success_"; p.proc_name; " "; args_str; " =\n";
    "  (va_ttrue ())\n\n"
  ]

let rec concat_requires (specs:list spec_kind) : Tot string (decreases specs) =
  match specs with
  | [] -> "True"
  | SpecRequires e :: rest ->
      (match rest with
       | [] -> string_of_expr e
       | _ -> concat "" [string_of_expr e; " /\\ "; concat_requires rest])
  | _ :: rest -> concat_requires rest

let rec concat_ensures (specs:list spec_kind) : Tot string (decreases specs) =
  match specs with
  | [] -> "True"
  | SpecEnsures e :: rest ->
      (match rest with
       | [] -> string_of_expr e
       | _ -> concat "" [string_of_expr e; " /\\ "; concat_ensures rest])
  | _ :: rest -> concat_ensures rest

let rec fold_updates (mods:list id) : Tot string (decreases mods) =
  match mods with
  | [] -> "(va_update_ok va_sM va_s0)"
  | m :: rest -> concat "" ["(va_update_reg "; m; " va_sM "; fold_updates rest; ")"]

let rec emit_lemma_steps (idx:int) (stmts:list stmt) : Tot string (decreases stmts) =
  match stmts with
  | [] -> ""
  | s :: rest ->
      (match s with
       | SCall name args ->
           let curr_s = if idx = 1 then "va_s0" else concat "" ["va_s"; string_of_int idx] in
           let next_idx = idx + 1 in
           let next_s = concat "" ["va_s"; string_of_int next_idx] in
           let fc = concat "" ["va_fc"; string_of_int next_idx] in
           let b_in = if idx = 1 then "va_b1" else concat "" ["va_b"; string_of_int idx] in
           let b_next = concat "" ["va_b"; string_of_int next_idx] in
           let call_args = if args = [] then "" else concat "" [" "; concat_exprs args] in
           concat "" [
             "  let ("; next_s; ", "; fc; ") = va_lemma_"; name; " (va_hd "; b_in; ") "; curr_s; call_args; " in\n";
             "  let "; b_next; " = va_tl "; b_in; " in\n";
             emit_lemma_steps next_idx rest
           ]
       | _ -> emit_lemma_steps idx rest)

let rec count_scalls (stmts:list stmt) : Tot nat (decreases stmts) =
  match stmts with
  | [] -> 0
  | SCall _ _ :: rest -> 1 + count_scalls rest
  | _ :: rest -> count_scalls rest

let rec emit_merge_steps (cur_i:nat) (tot_count:int) : Tot string (decreases cur_i) =
  if cur_i <= 1 then ""
  else
    let prev_i = cur_i - 1 in
    let f_curr =
      if cur_i = tot_count + 1 then concat "" ["va_f"; string_of_int tot_count]
      else concat "" ["va_f"; string_of_int cur_i]
    in
    let f_prev =
      if prev_i = 1 then "va_fM"
      else concat "" ["va_f"; string_of_int prev_i]
    in
    let b_curr = concat "" ["va_b"; string_of_int prev_i] in
    let s_curr =
      if prev_i = 1 then "va_s0"
      else concat "" ["va_s"; string_of_int prev_i]
    in
    let fc = concat "" ["va_fc"; string_of_int cur_i] in
    let s_next = concat "" ["va_s"; string_of_int cur_i] in
    concat "" [
      "  let "; f_prev; " = va_lemma_merge_total "; b_curr; " "; s_curr; " "; fc; " "; s_next; " "; f_curr; " va_sM in\n";
      emit_merge_steps prev_i tot_count
    ]

let emit_primitive_lemma (p:proc_decl) : string =
  let args = concat_pformal_args p.proc_formals in
  let args_sig = concat_pformal_types p.proc_formals in
  let code_call = concat "" ["va_code_"; p.proc_name; (if args = "" then " ()" else concat "" [" "; args])] in
  concat "" [
    "val va_lemma_"; p.proc_name; " : va_b0:va_code -> va_s0:va_state -> "; args_sig; "\n";
    "  -> Ghost (va_state & va_fuel)\n";
    "  (requires (va_require_total va_b0 ("; code_call; ") va_s0 /\\ va_get_ok va_s0))\n";
    "  (ensures (fun (va_sM, va_fM) -> va_ensure_total va_b0 va_s0 va_sM va_fM /\\ va_get_ok va_sM /\\\n";
    "    va_state_eq va_sM (va_update_ok va_sM va_s0)))\n";
    "[@ \"opaque_to_smt\"]\n";
    "let va_lemma_"; p.proc_name; " va_b0 va_s0 "; (if args = "" then "()" else args); " =\n";
    "  va_reveal_opaque (`%va_code_"; p.proc_name; ") ("; code_call; ");\n";
    "  let (va_old_s:va_state) = va_s0 in\n";
    "  va_ins_lemma ("; p.proc_ins_term; ") va_s0;\n";
    "  let (va_sM, va_fM) = va_eval_ins ("; p.proc_ins_term; ") va_s0 in\n";
    "  assume false;\n";
    "  (va_sM, va_fM)\n\n"
  ]

let emit_composite_lemma (p:proc_decl) : string =
  let args = concat_pformal_args p.proc_formals in
  let args_sig = concat_pformal_types p.proc_formals in
  let n = count_scalls p.proc_body in
  let code_call = concat "" ["va_code_"; p.proc_name; (if args = "" then " ()" else concat "" [" "; args])] in
  let reqs =
    concat "" ["va_require_total va_b0 ("; code_call; ") va_s0 /\\ va_get_ok va_s0"]
  in
  let updates = fold_updates p.proc_modifies in
  let last_s = concat "" ["va_s"; string_of_int (n + 1)] in
  let last_b = concat "" ["va_b"; string_of_int (n + 1)] in
  let last_f = concat "" ["va_f"; string_of_int n] in
  concat "" [
    "val va_lemma_"; p.proc_name; " : va_b0:va_code -> va_s0:va_state";
    (if args = "" then "" else concat "" [" -> "; args_sig]); "\n";
    "  -> Ghost (va_state & va_fuel)\n";
    "  (requires ("; reqs; "))\n";
    "  (ensures (fun (va_sM, va_fM) -> va_ensure_total va_b0 va_s0 va_sM va_fM /\\ va_get_ok va_sM /\\\n";
    "    va_state_eq va_sM "; updates; "))\n";
    "[@ \"opaque_to_smt\"]\n";
    "let va_lemma_"; p.proc_name; " va_b0 va_s0 "; (if args = "" then "" else concat "" [args; " "]); "=\n";
    "  va_reveal_opaque (`%va_code_"; p.proc_name; ") ("; code_call; ");\n";
    "  let (va_old_s:va_state) = va_s0 in\n";
    "  let (va_b1:va_codes) = va_get_block va_b0 in\n";
    emit_lemma_steps 1 p.proc_body;
    "  let (va_sM, "; last_f; ") = va_lemma_empty_total "; last_s; " "; last_b; " in\n";
    emit_merge_steps (n + 1) n;
    "  (va_sM, va_fM)\n\n"
  ]

let emit_proc_fstar (p:proc_decl) : string =
  let sep = concat "" ["//-- "; p.proc_name; "\n\n"] in
  let code_val = emit_code_val_decl p in
  let code_def = emit_code_def p in
  let cg_success = emit_codegen_success p in
  let lemma =
    if p.proc_is_ins then emit_primitive_lemma p
    else emit_composite_lemma p
  in
  concat "" [sep; code_val; code_def; cg_success; lemma; "//--\n"]

let rec emit_fstar_procs (procs:list proc_decl) : Tot string (decreases procs) =
  match procs with
  | [] -> ""
  | p :: rest -> concat "" [emit_proc_fstar p; emit_fstar_procs rest]

let rec emit_fstar_includes (incs:list id) : Tot string (decreases incs) =
  match incs with
  | [] -> ""
  | inc :: rest -> concat "" ["open "; inc; "\n"; emit_fstar_includes rest]

let emit_fstar_module (m:module_decl) (target:arch_target) : string =
  let opens =
    match m.mod_includes with
    | [] -> ""
    | incs -> concat "" [emit_fstar_includes incs; "\n"]
  in
  concat "" [
    "module "; m.mod_name; "\n\n";
    opens;
    emit_fstar_procs m.mod_procs
  ]
