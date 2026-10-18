(* Bridge from parser Ast to verified Valerie_Compiler_AST *)
open Ast

let id_to_string = function
  | Id s -> s
  | Reserved s -> s
  | Operator s -> s

let rec typ_to_vaf_typ (operand_types : string list) (t : typ) : Valerie_Compiler_AST.vaf_typ =
  match t with
  | TBool _ -> Valerie_Compiler_AST.TBool
  | TInt _ -> Valerie_Compiler_AST.TInt
  | TName (Id "nat") -> Valerie_Compiler_AST.TNat
  | TName (Id ("uint8" | "u8" | "U8")) -> Valerie_Compiler_AST.TU8
  | TName (Id ("uint32" | "u32" | "U32")) -> Valerie_Compiler_AST.TU32
  | TName (Id ("uint64" | "u64" | "U64")) -> Valerie_Compiler_AST.TU64
  | TApply (Id "buffer", [elem_t; (TName (Id sz_str) | TApply (Id sz_str, []))]) ->
      let actual_elem = match elem_t with
        | TApply (Id "buffer", [inner]) -> inner
        | _ -> elem_t
      in
      let elem_vaf = typ_to_vaf_typ operand_types actual_elem in
      let sz = try Z.of_string sz_str with _ -> Z.zero in
      Valerie_Compiler_AST.TBuffer (elem_vaf, sz)
  | TApply (Id "buffer", [elem_t; _]) ->
      let elem_vaf = typ_to_vaf_typ operand_types elem_t in
      Valerie_Compiler_AST.TBuffer (elem_vaf, Z.zero)
  | TApply (Id "buffer", [elem_t]) ->
      let elem_vaf = typ_to_vaf_typ operand_types elem_t in
      Valerie_Compiler_AST.TBuffer (elem_vaf, Z.zero)
  | TName (Id s) when List.mem s operand_types -> Valerie_Compiler_AST.TVar ("va_operand_" ^ s)
  | TName x -> Valerie_Compiler_AST.TVar (id_to_string x)
  | TTuple ts -> Valerie_Compiler_AST.TTuple (List.map (typ_to_vaf_typ operand_types) ts)
  | _ -> Valerie_Compiler_AST.TInt

let rec exp_to_expr (e : exp) : Valerie_Compiler_AST.expr =
  match e with
  | ELoc (_, e') -> exp_to_expr e'
  | EVar (x, _) ->
      let s = id_to_string x in
      Valerie_Compiler_AST.EVar s
  | EInt n -> Valerie_Compiler_AST.EConst n
  | EBool b -> Valerie_Compiler_AST.EBoolConst b
  | EOp (Uop (UNot _), [e1], _) -> Valerie_Compiler_AST.EUop (Valerie_Compiler_AST.UNot, exp_to_expr e1)
  | EOp (Uop UNeg, [e1], _) -> Valerie_Compiler_AST.EUop (Valerie_Compiler_AST.UNeg, exp_to_expr e1)
  | EOp (Uop UOld, [e1], _) -> Valerie_Compiler_AST.EUop (Valerie_Compiler_AST.UOld, exp_to_expr e1)
  | EOp (Bop b, [e1; e2], _) ->
      let bop = match b with
        | BAdd -> Valerie_Compiler_AST.BAdd
        | BSub -> Valerie_Compiler_AST.BSub
        | BMul -> Valerie_Compiler_AST.BMul
        | BDiv -> Valerie_Compiler_AST.BDiv
        | BMod -> Valerie_Compiler_AST.BMod
        | BAnd _ -> Valerie_Compiler_AST.BAnd
        | BOr _ -> Valerie_Compiler_AST.BOr
        | BEq _ -> Valerie_Compiler_AST.BEq
        | BNe _ -> Valerie_Compiler_AST.BNe
        | BLt -> Valerie_Compiler_AST.BLt
        | BGt -> Valerie_Compiler_AST.BGt
        | BLe -> Valerie_Compiler_AST.BLe
        | BGe -> Valerie_Compiler_AST.BGe
        | BImply -> Valerie_Compiler_AST.BImply
        | BEquiv -> Valerie_Compiler_AST.BEquiv
        | _ -> Valerie_Compiler_AST.BAdd
      in
      Valerie_Compiler_AST.EBop (bop, exp_to_expr e1, exp_to_expr e2)
  | EApply (e_fn, _, args, _) ->
      let fname = match e_fn with
        | EVar (x, _) -> id_to_string x
        | ELoc (_, EVar (x, _)) -> id_to_string x
        | _ -> "unknown"
      in
      Valerie_Compiler_AST.EApply (fname, List.map exp_to_expr args)
  | _ -> Valerie_Compiler_AST.EBoolConst true

let rec stmt_to_vaf_stmt (s : stmt) : Valerie_Compiler_AST.stmt option =
  match s with
  | SLoc (_, s') -> stmt_to_vaf_stmt s'
  (* Memory load: reg := buf[offset] *)
  | SAssign ([(x, _)], (EOp (Subscript, [EVar (buf, _); EInt offset], _) |
                        EOp (Subscript, [ELoc (_, EVar (buf, _)); ELoc (_, EInt offset)], _) |
                        EOp (Subscript, [EVar (buf, _); ELoc (_, EInt offset)], _) |
                        EOp (Subscript, [ELoc (_, EVar (buf, _)); EInt offset], _) |
                        ELoc (_, EOp (Subscript, [EVar (buf, _); EInt offset], _)) |
                        ELoc (_, EOp (Subscript, [ELoc (_, EVar (buf, _)); ELoc (_, EInt offset)], _)))) ->
      let reg_name = id_to_string x in
      let buf_name = id_to_string buf in
      Some (Valerie_Compiler_AST.SCall ("Load32", [Valerie_Compiler_AST.EVar reg_name; Valerie_Compiler_AST.EVar buf_name; Valerie_Compiler_AST.EConst offset]))
  (* Memory store: buf[offset] := reg *)
  | SAssign ([], (EOp (Update, [EVar (buf, _); EInt offset; EVar (src, _)], _) |
                  EOp (Update, [ELoc (_, EVar (buf, _)); ELoc (_, EInt offset); ELoc (_, EVar (src, _))], _) |
                  EOp (Update, [EVar (buf, _); EInt offset; ELoc (_, EVar (src, _))], _) |
                  ELoc (_, EOp (Update, [EVar (buf, _); EInt offset; EVar (src, _)], _)) |
                  ELoc (_, EOp (Update, [ELoc (_, EVar (buf, _)); ELoc (_, EInt offset); ELoc (_, EVar (src, _))], _)))) ->
      let buf_name = id_to_string buf in
      let src_name = id_to_string src in
      Some (Valerie_Compiler_AST.SCall ("Store32", [Valerie_Compiler_AST.EVar buf_name; Valerie_Compiler_AST.EConst offset; Valerie_Compiler_AST.EVar src_name]))
  | SAssign ([], (EApply (e_fn, _, args, _) | ELoc (_, EApply (e_fn, _, args, _)))) ->
      let fname = match e_fn with
        | EVar (x, _) -> id_to_string x
        | ELoc (_, EVar (x, _)) -> id_to_string x
        | _ -> "unknown"
      in
      Some (Valerie_Compiler_AST.SCall (fname, List.map exp_to_expr args))
  | SAssert (_, e) -> Some (Valerie_Compiler_AST.SAssert (exp_to_expr e))
  | SAssume e -> Some (Valerie_Compiler_AST.SAssume (exp_to_expr e))
  | SBlock stmts ->
      let converted = List.filter_map stmt_to_vaf_stmt stmts in
      Some (Valerie_Compiler_AST.SBlock converted)
  | SIfElse (_, cond, s_then, s_else) ->
      let c = exp_to_expr cond in
      let t = List.filter_map stmt_to_vaf_stmt s_then in
      let e = List.filter_map stmt_to_vaf_stmt s_else in
      Some (Valerie_Compiler_AST.SIfElse (c, t, e))
  | SWhile (cond, invs, _, body) ->
      let c = exp_to_expr cond in
      let inv_exprs = List.map (fun (_, e) -> exp_to_expr e) invs in
      let b = List.filter_map stmt_to_vaf_stmt body in
      Some (Valerie_Compiler_AST.SWhile (c, inv_exprs, b))
  | _ -> None

let spec_to_vaf_spec (s : spec) : Valerie_Compiler_AST.spec_kind list =
  match s with
  | Requires (_, e) -> [Valerie_Compiler_AST.SpecRequires (exp_to_expr e)]
  | Ensures (_, e) -> [Valerie_Compiler_AST.SpecEnsures (exp_to_expr e)]
  | Modifies (_, e) ->
      (match e with
       | EVar (Id s, _) ->
           let clean = if String.length s > 1 && (s.[0] = 'x' || s.[0] = 'r') then String.sub s 1 (String.length s - 1) else s in
           (match int_of_string_opt clean with
            | Some n -> [Valerie_Compiler_AST.SpecModifies [Valerie_Compiler_AST.RegNumbered (Stdint.Uint32.of_int n)]]
            | None -> [])
       | _ -> [])
  | SpecRaw (RawSpec (RRequires _, items)) ->
      List.filter_map (function
        | (_, SpecExp e) -> Some (Valerie_Compiler_AST.SpecRequires (exp_to_expr e))
        | _ -> None
      ) items
  | SpecRaw (RawSpec (REnsures _, items)) ->
      List.filter_map (function
        | (_, SpecExp e) -> Some (Valerie_Compiler_AST.SpecEnsures (exp_to_expr e))
        | _ -> None
      ) items
  | SpecRaw (RawSpec (RModifies _, items)) ->
      let regs = List.filter_map (function
        | (_, SpecExp (EVar (Id s, _))) ->
            let clean = if String.length s > 1 && (s.[0] = 'x' || s.[0] = 'r') then String.sub s 1 (String.length s - 1) else s in
            (match int_of_string_opt clean with
             | Some n -> Some (Valerie_Compiler_AST.RegNumbered (Stdint.Uint32.of_int n))
             | None -> None)
        | _ -> None
      ) items in
      if regs = [] then [] else [Valerie_Compiler_AST.SpecModifies regs]
  | _ -> []

let rec exp_to_ins_term (e : exp) : string =
  match e with
  | ELoc (_, e') -> exp_to_ins_term e'
  | EVar (x, _) -> id_to_string x
  | EInt n -> Z.to_string n
  | EApply (e_fn, _, args, _) ->
      let fname = match e_fn with
        | EVar (x, _) -> id_to_string x
        | ELoc (_, EVar (x, _)) -> id_to_string x
        | _ -> "ins"
      in
      if args = [] then fname
      else "(" ^ fname ^ " " ^ String.concat " " (List.map exp_to_ins_term args) ^ ")"
  | _ -> ""

let proc_to_vaf_proc (operand_types : string list) (p : proc_decl) (target : Valerie_Compiler_AST.arch_target) : Valerie_Compiler_AST.proc_decl =
  let name = id_to_string p.pname in
  let formals = List.map (fun (x, t, _storage, io, _) ->
    let fkind = match io with
      | In -> Valerie_Compiler_AST.FormalIn
      | Out -> Valerie_Compiler_AST.FormalOut
      | InOut -> Valerie_Compiler_AST.FormalInOut
    in
    {
      Valerie_Compiler_AST.pformal_name = id_to_string x;
      Valerie_Compiler_AST.pformal_typ = typ_to_vaf_typ operand_types t;
      Valerie_Compiler_AST.pformal_kind = fkind;
    }
  ) p.pargs in
  let returns = List.map (fun (x, t, _storage, _io, _) ->
    {
      Valerie_Compiler_AST.pformal_name = id_to_string x;
      Valerie_Compiler_AST.pformal_typ = typ_to_vaf_typ operand_types t;
      Valerie_Compiler_AST.pformal_kind = Valerie_Compiler_AST.FormalOut;
    }
  ) p.prets in
  let specs = List.concat_map (fun (_, sp) -> spec_to_vaf_spec sp) p.pspecs in
  let body = match p.pbody with
    | None -> []
    | Some stmts -> List.filter_map stmt_to_vaf_stmt stmts
  in
  let (is_ins, ins_term) =
    match List.assoc_opt (Id "instruction") p.pattrs with
    | Some [e] -> (true, exp_to_ins_term e)
    | _ -> (false, "")
  in
  let modifies_list =
    List.concat_map (fun (_, sp) ->
      match sp with
      | Modifies (_, EVar (Id s, _)) ->
          let upper = if String.length s >= 2 && (s.[0] = 'x' || s.[0] = 'r') then "R" ^ String.sub s 1 (String.length s - 1)
                      else if String.length s >= 2 && s.[0] = 'v' then "V" ^ String.sub s 1 (String.length s - 1)
                      else s in
          [upper]
      | SpecRaw (RawSpec (RModifies _, items)) ->
          List.filter_map (function
            | (_, SpecExp (EVar (Id s, _))) ->
                let upper = if String.length s >= 2 && (s.[0] = 'x' || s.[0] = 'r') then "R" ^ String.sub s 1 (String.length s - 1)
                            else if String.length s >= 2 && s.[0] = 'v' then "V" ^ String.sub s 1 (String.length s - 1)
                            else s in
                Some upper
            | _ -> None
          ) items
      | _ -> []
    ) p.pspecs
  in
  {
    Valerie_Compiler_AST.proc_name = name;
    Valerie_Compiler_AST.proc_target = target;
    Valerie_Compiler_AST.proc_formals = formals;
    Valerie_Compiler_AST.proc_returns = returns;
    Valerie_Compiler_AST.proc_specs = specs;
    Valerie_Compiler_AST.proc_body = body;
    Valerie_Compiler_AST.proc_is_ins = is_ins;
    Valerie_Compiler_AST.proc_ins_term = ins_term;
    Valerie_Compiler_AST.proc_modifies = modifies_list;
  }

let bridge_decls (mname : string) (includes : include_decl list) (decls : decls) (target : Valerie_Compiler_AST.arch_target) : Valerie_Compiler_AST.module_decl =
  let opens = List.filter_map (fun inc ->
    let has_open = List.exists (function
      | (Id "open", _) -> true
      | _ -> false
    ) inc.inc_attrs in
    if has_open then Some inc.inc_path else None
  ) includes in
  let explicit_operand_types = List.filter_map (fun (_, d) ->
    match d with
    | DOperandType (Id s, _, _, _, _) -> Some s
    | _ -> None
  ) decls in
  (* Standard target profile auto-injects standard operand types: reg, reg32, reg64, vec *)
  let operand_types = ["reg"; "reg32"; "reg64"; "vec"; "vec128"] @ explicit_operand_types in
  let procs = List.filter_map (fun (_, d) ->
    match d with
    | DProc p -> Some (proc_to_vaf_proc operand_types p target)
    | _ -> None
  ) decls in
  {
    Valerie_Compiler_AST.mod_name = mname;
    Valerie_Compiler_AST.mod_includes = opens;
    Valerie_Compiler_AST.mod_procs = procs;
  }
