{
open Ast
open Parse
open Parse_util

type lex_mode =
  | LexNormal
  | LexVerbatim

let mode = ref LexNormal
let init_lex () = mode := LexNormal

module StringMap = Map.Make(String)
let macros : (token list) StringMap.t ref = ref StringMap.empty
let custom_tokens : (loc -> token) StringMap.t ref = ref StringMap.empty

let next_line (lexbuf : Lexing.lexbuf) =
  Lexing.new_line lexbuf

let operatorToken (tok : string) (isCustom : bool) (s : string) : (loc -> token) option =
  match tok with
  | "!" -> Some (fun loc -> BANG (loc, (isCustom, s)))
  | "*" -> Some (fun loc -> STAR (loc, (isCustom, s)))
  | "/" -> Some (fun loc -> SLASH (loc, (isCustom, s)))
  | "%" -> Some (fun loc -> PERCENT (loc, (isCustom, s)))
  | "+" -> Some (fun loc -> PLUS (loc, (isCustom, s)))
  | "-" -> Some (fun loc -> MINUS (loc, (isCustom, s)))
  | "<" -> Some (fun loc -> LT (loc, (isCustom, s)))
  | ">" -> Some (fun loc -> GT (loc, (isCustom, s)))
  | "=" -> Some (fun loc -> EQ (loc, (isCustom, s)))
  | "|" -> Some (fun loc -> BAR (loc, (isCustom, s)))
  | ":=" -> Some (fun loc -> COLONEQ (loc, (isCustom, s)))
  | "@=" -> Some (fun loc -> ATEQ (loc, (isCustom, s)))
  | "<=" -> Some (fun loc -> LE (loc, (isCustom, s)))
  | ">=" -> Some (fun loc -> GE (loc, (isCustom, s)))
  | "==" -> Some (fun loc -> EQEQ (loc, (isCustom, s)))
  | "!=" -> Some (fun loc -> NE (loc, (isCustom, s)))
  | "&&" | "/\\" -> Some (fun loc -> AMPAMP (loc, (isCustom, s)))
  | "||" | "\\/" -> Some (fun loc -> BARBAR (loc, (isCustom, s)))
  | "<==" -> Some (fun loc -> LTEQEQ (loc, (isCustom, s)))
  | "==>" -> Some (fun loc -> EQEQGT (loc, (isCustom, s)))
  | "<==>" -> Some (fun loc -> LTEQEQGT (loc, (isCustom, s)))
  | "~" -> Some (fun loc -> BANG (loc, (isCustom, s)))
  | "<>" -> Some (fun loc -> NE (loc, (isCustom, s)))
  | _ -> None

let strip_underscore s =
  let b = Buffer.create (String.length s) in
  String.iter (fun c -> if c <> '_' then Buffer.add_char b c) s;
  Buffer.contents b

let ends_with suffix s =
  let slen = String.length s in
  let suflen = String.length suffix in
  slen >= suflen && String.sub s (slen - suflen) suflen = suffix

let trim_string s =
  let is_space = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false in
  let len = String.length s in
  let rec left i = if i < len && is_space s.[i] then left (i + 1) else i in
  let rec right i = if i >= 0 && is_space s.[i] then right (i - 1) else i in
  let l = left 0 in
  let r = right (len - 1) in
  if l > r then "" else String.sub s l (r - l + 1)
}

let op_char = ['!' '%' '+' '-' '&' '^' '|' '<' '>' '=' '.' '#' ':' '$' '?' '`' '~' '@' '\\']
let op = (op_char | '*' | '/' op_char)+ ('/' | "//" | "/*")?

rule comment = parse
  | "*/" { () }
  | "/*" { comment lexbuf; comment lexbuf }
  | "\n\r" | "\r\n" | ['\n' '\r'] { next_line lexbuf; comment lexbuf }
  | _ { comment lexbuf }

and preprocess_skip deep = parse
  | "#else" { if deep then preprocess_skip deep lexbuf else () }
  | "#endif" { () }
  | "#ifdef" { preprocess_skip true lexbuf; preprocess_skip deep lexbuf }
  | "#ifndef" { preprocess_skip true lexbuf; preprocess_skip deep lexbuf }
  | "\n\r" | "\r\n" | ['\n' '\r'] { next_line lexbuf; preprocess_skip deep lexbuf }
  | _ { preprocess_skip deep lexbuf }

and skip_line = parse
  | [^ '\n' '\r']* { () }

and file_name = parse
  | [' ']* [^ ' ' '\n' '\r']+ [^ '\n' '\r']* { lextoken lexbuf }
  | [^ '\n' '\r']* { lextoken lexbuf }

and line_number = parse
  | ['0'-'9']+ { file_name lexbuf }

and verbatim lines = parse
  | "\n\r" | "\r\n" | ['\n' '\r'] { next_line lexbuf; verbatim lines lexbuf }
  | [' ' '\t']* "#endverbatim" [^ '\n' '\r']* ("\n\r" | "\r\n" | ['\n' '\r'] | eof)
    {
      next_line lexbuf;
      mode := LexNormal;
      VERBATIM_BLOCK (List.rev lines)
    }
  | [^ '\n' '\r']+
    {
      let s = Lexing.lexeme lexbuf in
      verbatim (s :: lines) lexbuf
    }
  | eof { mode := LexNormal; VERBATIM_BLOCK (List.rev lines) }

and end_line = parse
  | "\n\r" | "\r\n" | ['\n' '\r'] { next_line lexbuf; lextoken lexbuf }

and lextoken = parse
  | "\n\r" | "\r\n" | ['\n' '\r']
    {
      next_line lexbuf;
      match !mode with
      | LexNormal -> lextoken lexbuf
      | LexVerbatim -> verbatim [] lexbuf
    }
  | [' ' '\t']+ { lextoken lexbuf }
  | "//" [^ '\n' '\r']* { lextoken lexbuf }
  | "#line" [' ']+ { line_number lexbuf }
  | "/*" { comment lexbuf; lextoken lexbuf }

  | "#ifdef" [' ']+ ['A'-'Z' 'a'-'z' '0'-'9' '_' '$' '?']+
    {
      let s = Lexing.lexeme lexbuf in
      let x = trim_string (String.sub s 6 (String.length s - 6)) in
      if StringMap.mem x !macros then lextoken lexbuf
      else (preprocess_skip false lexbuf; lextoken lexbuf)
    }
  | "#ifndef" [' ']+ ['A'-'Z' 'a'-'z' '0'-'9' '_' '$' '?']+
    {
      let s = Lexing.lexeme lexbuf in
      let x = trim_string (String.sub s 7 (String.length s - 7)) in
      if not (StringMap.mem x !macros) then lextoken lexbuf
      else (preprocess_skip false lexbuf; lextoken lexbuf)
    }
  | "#else" { preprocess_skip false lexbuf; lextoken lexbuf }
  | "#endif" { lextoken lexbuf }

  | "#token" [' ']+ [^ ' ' '\n' '\r']+ [' ']+ "precedence" [' ']+ [^ ' ' '\n' '\r']+ [' ']*
    {
      let s = Lexing.lexeme lexbuf in
      let s_nospace = strip_underscore (String.concat "" (String.split_on_char ' ' s)) in
      (* Parse tokens manually *)
      let i_tok = 6 in (* len of #token *)
      let sp = "precedence" in
      let sp_idx = try (
        let re = Str.regexp_string sp in
        Str.search_forward re s 0
      ) with _ -> -1 in
      if sp_idx >= 0 then begin
        let name = trim_string (String.sub s i_tok (sp_idx - i_tok)) in
        let prec = trim_string (String.sub s (sp_idx + 10) (String.length s - sp_idx - 10)) in
        (match operatorToken prec true name with
         | None -> parse_err ("unknown precedence: '" ^ prec ^ "'")
         | Some f -> custom_tokens := StringMap.add name f !custom_tokens)
      end;
      end_line lexbuf
    }

  | "#verbatim" { mode := LexVerbatim; VERBATIM (curLoc lexbuf) }
  | ";" { SEMI (curLoc lexbuf) }
  | "(" { LPAREN (curLoc lexbuf) }
  | ")" { RPAREN (curLoc lexbuf) }
  | "?[" { QLBRACKET (curLoc lexbuf) }
  | "#[" { HASHLBRACKET (curLoc lexbuf) }
  | "[" { LBRACKET (curLoc lexbuf) }
  | "]" { RBRACKET (curLoc lexbuf) }
  | "{:" { LBRACECOLON (curLoc lexbuf) }
  | "{" { LBRACE (curLoc lexbuf) }
  | "}" { RBRACE (curLoc lexbuf) }
  | "," { COMMA (curLoc lexbuf) }
  | "/" { SLASH (curLoc lexbuf, (false, "/")) }

  | op
    {
      let s = Lexing.lexeme lexbuf in
      let loc = curLoc lexbuf in
      let op_tok s =
        match (s, operatorToken s false s, StringMap.find_opt s !custom_tokens) with
        | ("@", _, _) -> AT loc
        | ("#", _, _) -> HASH loc
        | (".", _, _) -> DOT loc
        | (":", _, _) -> COLON loc
        | ("->", _, _) -> MINUSGT loc
        | (_, None, None) -> parse_err ("unknown token '" ^ s ^ "'")
        | (_, None, Some f) -> f loc
        | (_, Some f, _) -> f loc
      in
      if ends_with "/*" s then
        let s2 = String.sub s 0 (String.length s - 2) in
        comment lexbuf; op_tok s2
      else if ends_with "//" s then
        let s2 = String.sub s 0 (String.length s - 2) in
        skip_line lexbuf; op_tok s2
      else
        op_tok s
    }

  | "Type" { TTYPE (curLoc lexbuf) }
  | "type" { TYPE (curLoc lexbuf) }
  | "Dependent" { DDEPENDENT (curLoc lexbuf) }
  | "dependent" { DEPENDENT (curLoc lexbuf) }
  | "const" { CONST (curLoc lexbuf) }
  | "readonly" { READONLY (curLoc lexbuf) }
  | "function" { FUNCTION (curLoc lexbuf) }
  | "returns" { RETURNS (curLoc lexbuf) }
  | "axiom" { AXIOM (curLoc lexbuf) }
  | "extern" { EXTERN (curLoc lexbuf) }
  | "procedure" { PROCEDURE (curLoc lexbuf) }
  | "lets" { LETS (curLoc lexbuf) }
  | "requires" { REQUIRES (curLoc lexbuf) }
  | "ensures" { ENSURES (curLoc lexbuf) }
  | "reads" { READS (curLoc lexbuf) }
  | "modifies" { MODIFIES (curLoc lexbuf) }
  | "preserves" { PRESERVES (curLoc lexbuf) }
  | "decreases" { DECREASES (curLoc lexbuf) }
  | "invariant" { INVARIANT (curLoc lexbuf) }
  | "assert" { ASSERT (curLoc lexbuf) }
  | "implies" { IMPLIES (curLoc lexbuf) }
  | "by" { BY (curLoc lexbuf) }
  | "assume" { ASSUME (curLoc lexbuf) }
  | "calc" { CALC (curLoc lexbuf) }
  | "havoc" { HAVOC (curLoc lexbuf) }
  | "goto" { GOTO (curLoc lexbuf) }
  | "lemma" { LEMMA (curLoc lexbuf) }
  | "call" { CALL (curLoc lexbuf) }
  | "forall" { FORALL (curLoc lexbuf) }
  | "exists" { EXISTS (curLoc lexbuf) }
  | "fun" { FUN (curLoc lexbuf) }
  | "old" { OLD (curLoc lexbuf) }
  | "this" { THIS (curLoc lexbuf) }
  | "true" { LITBOOL ((curLoc lexbuf), true) }
  | "false" { LITBOOL ((curLoc lexbuf), false) }
  | "is" { IS (curLoc lexbuf) }
  | "let" { LET (curLoc lexbuf) }
  | "in" { IN (curLoc lexbuf) }
  | "out" { OUT (curLoc lexbuf) }
  | "inout" { INOUT (curLoc lexbuf) }
  | "mut" { MUT (curLoc lexbuf) }
  | "var" { VAR (curLoc lexbuf) }
  | "if" { IF (curLoc lexbuf) }
  | "then" { THEN (curLoc lexbuf) }
  | "else" { ELSE (curLoc lexbuf) }
  | "while" { WHILE (curLoc lexbuf) }
  | "for" { FOR (curLoc lexbuf) }
  | "return" { RETURN (curLoc lexbuf) }
  | "reveal" { REVEAL (curLoc lexbuf) }
  | "static" { STATIC (curLoc lexbuf) }
  | "module" { MODULE (curLoc lexbuf) }
  | "target" { TARGET (curLoc lexbuf) }
  | "feature" { FEATURE (curLoc lexbuf) }
  | "import" { IMPORT (curLoc lexbuf) }
  | "ghost" { GHOST (curLoc lexbuf) }
  | "inline" { INLINE (curLoc lexbuf) }
  | "operator" { OPERATOR (curLoc lexbuf) }
  | "include" { INCLUDE (curLoc lexbuf) }
  | "operand_type" { OPERAND_TYPE (curLoc lexbuf) }
  | "tuple" { TUPLE (curLoc lexbuf) }
  | "bool" { BOOL (curLoc lexbuf) }
  | "prop" { PROP (curLoc lexbuf) }
  | "int" { INT (curLoc lexbuf) }
  | "int_range" { INT_RANGE (curLoc lexbuf) }

  | '"' [^ '"' '\n' '\r' '\t' '\\']* '"'
    {
      let s = Lexing.lexeme lexbuf in
      STRING (curLoc lexbuf, String.sub s 1 (String.length s - 2))
    }

  | "0x" ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F' '_']*
    {
      let s = Lexing.lexeme lexbuf in
      let clean = strip_underscore s in
      let hex_str = String.sub clean 2 (String.length clean - 2) in
      LITINT (curLoc lexbuf, Z.of_string_base 16 hex_str)
    }

  | ['0'-'9'] ['0'-'9' '_']*
    {
      let s = Lexing.lexeme lexbuf in
      let clean = strip_underscore s in
      LITINT (curLoc lexbuf, Z.of_string clean)
    }

  | ['0'-'9']+ '.' ['0'-'9']+
    {
      LITREAL (curLoc lexbuf, Lexing.lexeme lexbuf)
    }

  | '_' '"' [^ '"' '\n' '\r' '\t' '\\']* '"'
    {
      let s = Lexing.lexeme lexbuf in
      ID (curLoc lexbuf, String.sub s 2 (String.length s - 3))
    }

  | ['_' 'a'-'z' 'A'-'Z' '\'']+ ['_' 'a'-'z' 'A'-'Z' '0'-'9' '\'']*
    {
      ID (curLoc lexbuf, Lexing.lexeme lexbuf)
    }

  | eof { EOF }
  | '\000' { EOF }
  | _
    {
      let s = Lexing.lexeme lexbuf in
      parse_err ("cannot parse character: \"" ^ s ^ "\"\n(ascii code " ^ string_of_int (Char.code s.[0]) ^ ")")
    }
