type token =
  | NUMERAL of (string)
  | DECIMAL of (string)
  | HEXADECIMAL of (string)
  | EOF
  | NUM of (string)
  | ID of (string)
  | SUBVAR of (string)
  | LPAREN
  | RPAREN
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | POWER
  | LET
  | ASSERT
  | AND
  | CONJ
  | OR
  | NOT
  | ITE
  | EQ
  | GEQ
  | LEQ
  | GR
  | LE

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Exp.ass_expr
