(* Ast utilities in OCaml - port of ast_util.fs *)
open Ast

let tUnit = TName (Id "unit")
let tBool = TBool BpBool
let tProp = TBool BpProp
let tInt = TInt (NegInf, Inf)
let tOperand xo = TName (Reserved xo)
let tState = TName (Reserved "state")
let tCode = TName (Reserved "code")
let tPbool = TName (Reserved "pbool")
let tCodes = TName (Reserved "codes")
let tFuel = TName (Reserved "fuel")
let tTransformationResult = TName (Reserved "transformation_result")
let ktype0 = KType Z.zero
let ktype1 = KType Z.one

let tapply (x:id) (ts:typ list):typ = TApply (x, ts)
let evar (x:id) = EVar (x, None)
let ebind (op:bindOp) (es:exp list) (fs:formal list) (ts:exp list list) (e:exp) = EBind (op, es, fs, ts, e, None)
let eop (op:op) (es:exp list) = EOp (op, es, None)
let eapply (x:id) (es:exp list):exp = EApply (evar x, None, es, None)
let eapply_t (x:id) (es:exp list) (t:typ option):exp = EApply (evar x, None, es, t)
let eapply_opt (x:id) (es:exp list):exp = match es with [] -> EVar (x, None) | _ -> eapply x es
let eapply_opt_t (x:id) (es:exp list) (t:typ option):exp = match es with [] -> EVar (x, t) | _ -> eapply_t x es t

let name_of_id x =
  let s = match x with Id s | Reserved s | Operator s -> s in
  let es = String.split_on_char '.' s in
  let rec aux s l =
    match l with
    | hd :: [] -> (s, hd)
    | hd :: tl -> aux (if s = "" then hd else (s ^ "." ^ hd)) tl
    | _ -> failwith "Empty list." in
  let (mn, sn) = aux "" es in
  match x with
  | Id _ -> (mn, Id sn)
  | Reserved _ -> (mn, Reserved sn)
  | Operator _ -> (mn, Operator sn)

let expAt l e = ELoc (l, e)
let stmtAt l s = [SLoc (l, s)]
let rec locs_of_exp (e:exp):loc list =
  match e with
  | ELoc (l, _) -> [l]
  | EOp (_, es, _) -> List.concat_map locs_of_exp es
  | EApply (e1, _, es, _) -> List.concat_map locs_of_exp (e1 :: es)
  | EBind (_, es, _, _, body, _) -> List.concat_map locs_of_exp (body :: es)
  | _ -> []

let one_loc_of_exp (defaultLoc:loc) (e:exp):loc =
  match locs_of_exp e with [] -> defaultLoc | h::_ -> h
let rec skip_loc (e:exp):exp =
  match e with
  | ELoc (_, e) -> skip_loc e
  | _ -> e

let skip_loc_opt (e:exp option):exp option =
  match e with
  | None -> None
  | Some e -> Some (skip_loc e)
let assert_attrs_default =
  {
    is_inv = false;
    is_split = false;
    is_refined = false;
    is_quicktype = None;
  }
let err_id (x:id):string =
  match x with
  | Id s -> s
  | Reserved s -> s
  | Operator s -> "operator(" ^ s ^ ")"
let rec exp_typ e =
  match e with
  | ELoc (_, e) -> exp_typ e
  | EVar (_, t) -> t
  | EOp (_, _, t) -> t
  | EApply (_, _, _, t) -> t
  | EBind (_, _, _, _, _, t) -> t
  | ECast (_, _, t) -> Some t
  | _ -> None

let binary_op_of_list (b:bop) (empty:exp) (es:exp list) =
  match es with
  | [] -> empty
  | h::t -> List.fold_left (fun accum e -> eop (Bop b) [accum; e]) h t

let and_of_list = binary_op_of_list (BAnd BpProp) (EBool true)
let or_of_list = binary_op_of_list (BOr BpProp) (EBool false)

let rec exps_of_spec_exps (es:(loc * spec_exp) list):(loc * exp) list =
  match es with
  | [] -> []
  | (loc, SpecExp e)::es -> (loc, e)::(exps_of_spec_exps es)
  | (loc, SpecLet (x, t, ex))::es ->
      let es = exps_of_spec_exps es in
      let e = and_of_list (List.map (fun (l, e) -> ELabel (l, e)) es) in
      [(loc, EBind (BindLet, [ex], [(x, t)], [], e, exp_typ e))]
let string_of_id (x:id):string =
  match x with
  | Id s -> s
  | Reserved s -> s
  | Operator s -> "operator(" ^ s ^ ")"
