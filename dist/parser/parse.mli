type token =
  | ID of (
# 47 "parse.mly"
        Ast.loc * string
# 6 "parse.mli"
)
  | LITINT of (
# 48 "parse.mly"
        Ast.loc * Z.t
# 11 "parse.mli"
)
  | LITREAL of (
# 49 "parse.mly"
        Ast.loc * string
# 16 "parse.mli"
)
  | LITBOOL of (
# 50 "parse.mly"
        Ast.loc * bool
# 21 "parse.mli"
)
  | BV of (
# 51 "parse.mly"
        Ast.loc * int
# 26 "parse.mli"
)
  | INT of (
# 52 "parse.mly"
        Ast.loc
# 31 "parse.mli"
)
  | INT_RANGE of (
# 52 "parse.mly"
        Ast.loc
# 36 "parse.mli"
)
  | BOOL of (
# 52 "parse.mly"
        Ast.loc
# 41 "parse.mli"
)
  | PROP of (
# 52 "parse.mly"
        Ast.loc
# 46 "parse.mli"
)
  | TUPLE of (
# 52 "parse.mly"
        Ast.loc
# 51 "parse.mli"
)
  | QLBRACKET of (
# 53 "parse.mly"
        Ast.loc
# 56 "parse.mli"
)
  | HASHLBRACKET of (
# 53 "parse.mly"
        Ast.loc
# 61 "parse.mli"
)
  | LBRACKET of (
# 53 "parse.mly"
        Ast.loc
# 66 "parse.mli"
)
  | RBRACKET of (
# 53 "parse.mly"
        Ast.loc
# 71 "parse.mli"
)
  | LBRACE of (
# 53 "parse.mly"
        Ast.loc
# 76 "parse.mli"
)
  | RBRACE of (
# 53 "parse.mly"
        Ast.loc
# 81 "parse.mli"
)
  | LPAREN of (
# 53 "parse.mly"
        Ast.loc
# 86 "parse.mli"
)
  | RPAREN of (
# 53 "parse.mly"
        Ast.loc
# 91 "parse.mli"
)
  | LBRACECOLON of (
# 53 "parse.mly"
        Ast.loc
# 96 "parse.mli"
)
  | AT of (
# 54 "parse.mly"
        Ast.loc
# 101 "parse.mli"
)
  | COMMA of (
# 54 "parse.mly"
        Ast.loc
# 106 "parse.mli"
)
  | DOT of (
# 54 "parse.mly"
        Ast.loc
# 111 "parse.mli"
)
  | HASH of (
# 54 "parse.mly"
        Ast.loc
# 116 "parse.mli"
)
  | MINUSGT of (
# 54 "parse.mly"
        Ast.loc
# 121 "parse.mli"
)
  | BANG of (
# 55 "parse.mly"
        Ast.loc * (bool * string)
# 126 "parse.mli"
)
  | BAR of (
# 55 "parse.mly"
        Ast.loc * (bool * string)
# 131 "parse.mli"
)
  | LT of (
# 56 "parse.mly"
        Ast.loc * (bool * string)
# 136 "parse.mli"
)
  | GT of (
# 56 "parse.mly"
        Ast.loc * (bool * string)
# 141 "parse.mli"
)
  | EQ of (
# 56 "parse.mly"
        Ast.loc * (bool * string)
# 146 "parse.mli"
)
  | LE of (
# 56 "parse.mly"
        Ast.loc * (bool * string)
# 151 "parse.mli"
)
  | GE of (
# 56 "parse.mly"
        Ast.loc * (bool * string)
# 156 "parse.mli"
)
  | EQEQ of (
# 56 "parse.mly"
        Ast.loc * (bool * string)
# 161 "parse.mli"
)
  | NE of (
# 56 "parse.mly"
        Ast.loc * (bool * string)
# 166 "parse.mli"
)
  | PLUS of (
# 57 "parse.mly"
        Ast.loc * (bool * string)
# 171 "parse.mli"
)
  | MINUS of (
# 57 "parse.mly"
        Ast.loc * (bool * string)
# 176 "parse.mli"
)
  | STAR of (
# 57 "parse.mly"
        Ast.loc * (bool * string)
# 181 "parse.mli"
)
  | SLASH of (
# 57 "parse.mly"
        Ast.loc * (bool * string)
# 186 "parse.mli"
)
  | PERCENT of (
# 57 "parse.mly"
        Ast.loc * (bool * string)
# 191 "parse.mli"
)
  | AMPAMP of (
# 58 "parse.mly"
        Ast.loc * (bool * string)
# 196 "parse.mli"
)
  | BARBAR of (
# 58 "parse.mly"
        Ast.loc * (bool * string)
# 201 "parse.mli"
)
  | LTEQEQ of (
# 58 "parse.mly"
        Ast.loc * (bool * string)
# 206 "parse.mli"
)
  | EQEQGT of (
# 58 "parse.mly"
        Ast.loc * (bool * string)
# 211 "parse.mli"
)
  | LTEQEQGT of (
# 58 "parse.mly"
        Ast.loc * (bool * string)
# 216 "parse.mli"
)
  | ATEQ of (
# 59 "parse.mly"
        Ast.loc * (bool * string)
# 221 "parse.mli"
)
  | COLONEQ of (
# 59 "parse.mly"
        Ast.loc * (bool * string)
# 226 "parse.mli"
)
  | HAVOC of (
# 60 "parse.mly"
        Ast.loc
# 231 "parse.mli"
)
  | OLD of (
# 60 "parse.mly"
        Ast.loc
# 236 "parse.mli"
)
  | THIS of (
# 60 "parse.mly"
        Ast.loc
# 241 "parse.mli"
)
  | IS of (
# 61 "parse.mly"
        Ast.loc
# 246 "parse.mli"
)
  | THEN of (
# 61 "parse.mly"
        Ast.loc
# 251 "parse.mli"
)
  | ELSE of (
# 61 "parse.mly"
        Ast.loc
# 256 "parse.mli"
)
  | LET of (
# 61 "parse.mly"
        Ast.loc
# 261 "parse.mli"
)
  | IN of (
# 61 "parse.mly"
        Ast.loc
# 266 "parse.mli"
)
  | OUT of (
# 61 "parse.mly"
        Ast.loc
# 271 "parse.mli"
)
  | INOUT of (
# 61 "parse.mly"
        Ast.loc
# 276 "parse.mli"
)
  | MUT of (
# 61 "parse.mly"
        Ast.loc
# 281 "parse.mli"
)
  | REVEAL of (
# 61 "parse.mly"
        Ast.loc
# 286 "parse.mli"
)
  | GHOST of (
# 61 "parse.mly"
        Ast.loc
# 291 "parse.mli"
)
  | INLINE of (
# 61 "parse.mly"
        Ast.loc
# 296 "parse.mli"
)
  | TTYPE of (
# 62 "parse.mly"
        Ast.loc
# 301 "parse.mli"
)
  | TYPE of (
# 62 "parse.mly"
        Ast.loc
# 306 "parse.mli"
)
  | OPERAND_TYPE of (
# 62 "parse.mly"
        Ast.loc
# 311 "parse.mli"
)
  | VAR of (
# 62 "parse.mly"
        Ast.loc
# 316 "parse.mli"
)
  | CONST of (
# 62 "parse.mly"
        Ast.loc
# 321 "parse.mli"
)
  | READONLY of (
# 62 "parse.mly"
        Ast.loc
# 326 "parse.mli"
)
  | FUNCTION of (
# 62 "parse.mly"
        Ast.loc
# 331 "parse.mli"
)
  | AXIOM of (
# 62 "parse.mly"
        Ast.loc
# 336 "parse.mli"
)
  | EXTERN of (
# 62 "parse.mly"
        Ast.loc
# 341 "parse.mli"
)
  | PROCEDURE of (
# 62 "parse.mly"
        Ast.loc
# 346 "parse.mli"
)
  | STATIC of (
# 62 "parse.mly"
        Ast.loc
# 351 "parse.mli"
)
  | MODULE of (
# 62 "parse.mly"
        Ast.loc
# 356 "parse.mli"
)
  | IMPORT of (
# 62 "parse.mly"
        Ast.loc
# 361 "parse.mli"
)
  | OPERATOR of (
# 62 "parse.mly"
        Ast.loc
# 366 "parse.mli"
)
  | TARGET of (
# 62 "parse.mly"
        Ast.loc
# 371 "parse.mli"
)
  | FEATURE of (
# 62 "parse.mly"
        Ast.loc
# 376 "parse.mli"
)
  | DDEPENDENT of (
# 63 "parse.mly"
        Ast.loc
# 381 "parse.mli"
)
  | DEPENDENT of (
# 63 "parse.mly"
        Ast.loc
# 386 "parse.mli"
)
  | LEMMA of (
# 64 "parse.mly"
        Ast.loc
# 391 "parse.mli"
)
  | CALL of (
# 64 "parse.mly"
        Ast.loc
# 396 "parse.mli"
)
  | RETURN of (
# 64 "parse.mly"
        Ast.loc
# 401 "parse.mli"
)
  | IF of (
# 64 "parse.mly"
        Ast.loc
# 406 "parse.mli"
)
  | WHILE of (
# 64 "parse.mly"
        Ast.loc
# 411 "parse.mli"
)
  | FOR of (
# 64 "parse.mly"
        Ast.loc
# 416 "parse.mli"
)
  | GOTO of (
# 64 "parse.mly"
        Ast.loc
# 421 "parse.mli"
)
  | ASSERT of (
# 64 "parse.mly"
        Ast.loc
# 426 "parse.mli"
)
  | IMPLIES of (
# 64 "parse.mly"
        Ast.loc
# 431 "parse.mli"
)
  | BY of (
# 64 "parse.mly"
        Ast.loc
# 436 "parse.mli"
)
  | ASSUME of (
# 64 "parse.mly"
        Ast.loc
# 441 "parse.mli"
)
  | CALC of (
# 64 "parse.mly"
        Ast.loc
# 446 "parse.mli"
)
  | DECREASES of (
# 64 "parse.mly"
        Ast.loc
# 451 "parse.mli"
)
  | INVARIANT of (
# 64 "parse.mly"
        Ast.loc
# 456 "parse.mli"
)
  | COLON of (
# 64 "parse.mly"
        Ast.loc
# 461 "parse.mli"
)
  | YIELD of (
# 64 "parse.mly"
        Ast.loc
# 466 "parse.mli"
)
  | SEMI of (
# 64 "parse.mly"
        Ast.loc
# 471 "parse.mli"
)
  | RETURNS of (
# 65 "parse.mly"
        Ast.loc
# 476 "parse.mli"
)
  | LETS of (
# 65 "parse.mly"
        Ast.loc
# 481 "parse.mli"
)
  | REQUIRES of (
# 65 "parse.mly"
        Ast.loc
# 486 "parse.mli"
)
  | ENSURES of (
# 65 "parse.mly"
        Ast.loc
# 491 "parse.mli"
)
  | READS of (
# 65 "parse.mly"
        Ast.loc
# 496 "parse.mli"
)
  | MODIFIES of (
# 65 "parse.mly"
        Ast.loc
# 501 "parse.mli"
)
  | PRESERVES of (
# 65 "parse.mly"
        Ast.loc
# 506 "parse.mli"
)
  | FORALL of (
# 66 "parse.mly"
        Ast.loc
# 511 "parse.mli"
)
  | EXISTS of (
# 66 "parse.mly"
        Ast.loc
# 516 "parse.mli"
)
  | FUN of (
# 66 "parse.mly"
        Ast.loc
# 521 "parse.mli"
)
  | INCLUDE of (
# 67 "parse.mly"
        Ast.loc
# 526 "parse.mli"
)
  | STRING of (
# 68 "parse.mly"
        Ast.loc * string
# 531 "parse.mli"
)
  | VERBATIM of (
# 69 "parse.mly"
        Ast.loc
# 536 "parse.mli"
)
  | VERBATIM_BLOCK of (
# 70 "parse.mly"
        string list
# 541 "parse.mli"
)
  | EOF

val start :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.include_decl list * Ast.decls
