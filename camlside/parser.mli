type token =
  | EOF
  | NUM of (string)
  | ID of (string)
  | LPAREN
  | RPAREN
  | PLUS
  | MINUS
  | TIMES
  | POWER
  | ASSERT
  | AND
  | OR
  | IC
  | IN
  | EQ
  | GEQ
  | LEQ
  | GR
  | LE

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.formula
