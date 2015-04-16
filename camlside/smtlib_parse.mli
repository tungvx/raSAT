type token =
  | EOF
  | AS
  | ASSERT
  | CHECKSAT
  | DECLAREFUN
  | DECLARESORT
  | DEFINEFUN
  | DEFINESORT
  | EXCLIMATIONPT
  | EXISTS
  | EXIT
  | FORALL
  | GETASSERT
  | GETASSIGN
  | GETINFO
  | GETOPTION
  | GETPROOF
  | GETUNSATCORE
  | GETVALUE
  | LET
  | LPAREN
  | POP
  | PUSH
  | RPAREN
  | SETINFO
  | SETLOGIC
  | SETOPTION
  | UNDERSCORE
  | ASCIIWOR of (string)
  | BINARY of (string)
  | DECIMAL of (string)
  | HEXADECIMAL of (string)
  | KEYWORD of (string)
  | NUMERAL of (string)
  | STRINGLIT of (string)
  | SYMBOL of (string)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Smtlib_syntax.commands option
