(* Parse utilities in OCaml - port of parse_util.fs *)
open Ast

exception ParseErr of string
let parse_err s = raise (ParseErr s)

let curLoc (lexbuf : Lexing.lexbuf) : loc =
  let pos = lexbuf.Lexing.lex_curr_p in
  {
    loc_file = pos.Lexing.pos_fname;
    loc_line = pos.Lexing.pos_lnum;
    loc_col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1;
    loc_pos = pos.Lexing.pos_cnum;
  }
let rec exp2id (e:exp) (i:id):id =
  match i with
  | Id s ->
    (
      match e with
      | ELoc(_, EVar (Id r, _)) ->
        Id (r ^ "." ^ s)
      | ELoc(_, EOp (FieldOp (Id r), [ee], _)) ->
        exp2id ee (Id (r ^ "." ^ s))
      | _ -> parse_err "malformed qualified function/method name"
    )
  | _ -> parse_err "malformed qualified function/method name"
