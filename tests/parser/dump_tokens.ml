open Ast

let print_token = function
  | Parse.EOF -> "EOF"
  | Parse.MODULE loc -> "MODULE"
  | Parse.ID (loc, s) -> Printf.sprintf "ID(%s)" s
  | Parse.VERBATIM loc -> "VERBATIM"
  | Parse.VERBATIM_BLOCK lines -> Printf.sprintf "VERBATIM_BLOCK(%d lines)" (List.length lines)
  | Parse.VAR loc -> "VAR"
  | Parse.OPERAND_TYPE loc -> "OPERAND_TYPE"
  | Parse.PROCEDURE loc -> "PROCEDURE"
  | Parse.LBRACECOLON loc -> "{:"
  | Parse.RBRACE loc -> "}"
  | Parse.COLON loc -> ":"
  | Parse.SEMI loc -> ";"
  | _ -> "<other>"

let run () =
  let fn = "tests/fixtures/vale/chacha20_quarter.vaf" in
  let ic = open_in fn in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  let lexbuf = Lexing.from_string s in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = fn };
  let rec loop count =
    if count > 200 then () else
    let tok = Lex.lextoken lexbuf in
    let pos = lexbuf.Lexing.lex_curr_p in
    Printf.printf "Tok %d [%s:%d:%d]: %s\n"
      count pos.Lexing.pos_fname pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
      (print_token tok);
    if tok <> Parse.EOF then loop (count + 1)
  in
  loop 1
;;

run ()
