open Ast

let run () =
  let fn = "tests/fixtures/vale/chacha20_quarter.vaf" in
  let ic = open_in fn in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  let lexbuf = Lexing.from_string s in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = fn };
  let (includes, decls) =
    try Parse.start Lex.lextoken lexbuf
    with exn ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Printf.eprintf "Parsing failed at %s line %d col %d: %s\n"
        pos.Lexing.pos_fname pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
        (Printexc.to_string exn);
      raise exn
  in
  Printf.printf "SUCCESS: parsed %d includes and %d decls from %s!\n"
    (List.length includes) (List.length decls) fn;
  List.iter (fun (_loc, d) -> match d with
    | DProc p -> Printf.printf "  Found Procedure: %s with %d stmts\n"
        (match p.pname with Id s -> s | _ -> "?")
        (match p.pbody with None -> 0 | Some stmts -> List.length stmts)
    | DVar (x, _, _, _) -> Printf.printf "  Found Var: %s\n" (match x with Id s -> s | _ -> "?")
    | DOperandType (x, _, _, _, _) -> Printf.printf "  Found OperandType: %s\n" (match x with Id s -> s | _ -> "?")
    | DVerbatim (attrs, lines) -> Printf.printf "  Found Verbatim block (%d lines)\n" (List.length lines)
    | DPragma (ModuleName m) -> Printf.printf "  Found Module: %s\n" m
    | _ -> Printf.printf "  Found other decl\n"
  ) decls
;;

run ()
