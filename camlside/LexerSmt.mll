{
open Lexing
open ParserSmt
}

let alpha = ['a'-'z' 'A'-'Z' '_']
let numeral = ('0' | ['1'-'9'](['0'-'9']*))

rule lex = parse
  | numeral as s {NUMERAL s}
  | numeral '.' ('0'*) numeral as s {DECIMAL s}
  | "#x" (['0'-'9' 'a'-'f' 'A'-'F'])+ as s {HEXADECIMAL s}
  | "#b" (['0'-'1'])+ as s {BINARY s}
  | '\"' ("\\\"" | "\\\\" | [' '-'~'])* '\"' as s {STRING s}
  | (['a'-'z''A'-'Z''~''!''@''$''%''^''&''*''_''-''+''=''<''>''.''?''/'] ['0'-'9''a'-'z''A'-'Z''~''!''@''$''%''^''&''*''_''-''+''=''<''>''.''?''/']*) | ('|' ([' '-'~''\t''\n'] # ['\\''|'])* '|') as s {SYMBOL s}
  | ':' ['0'-'9''a'-'z''A'-'Z''~''!''@''$''%''^''&''*''_''-''+''=''<''>''.''?''/']+ as s {KEYWORD s}
  
  | [' ' '\r' '\t'] {  lex lexbuf }
  | '\n'   { new_line lexbuf; lex lexbuf }
  | ('-'? num+) | ("inf") | ("-inf") as s { NUM s }
  | "and"  { AND }  
  | "&"	{ CONJ}   
  | "or"   { OR }
  | "not"  { NOT }
  | "let"  { LET }
  | "assert" { ASSERT }  
  | alpha (alpha|num)* as s { ID s }
  | '?'(alpha|num)* as s {SUBVAR s}
  | "+"    { PLUS }
  | "-"    { MINUS }
  | "*"    { TIMES }
  | "/"    { DIV }
  | "^"    { POWER }
  | "="    { EQ }
  | ">="   { GEQ }
  | "<="   { LEQ }
  | ">"    { GR  }
  | "<"    { LE  }
  | "("    { LPAREN }
  | ")"    { RPAREN }  
  | "ite"  { ITE } 
  | eof    { EOF }
