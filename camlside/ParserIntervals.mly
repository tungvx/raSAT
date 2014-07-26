%{
open Parsing
open IA
%}

%token IC
%token IN
%token <string> ID
%token <string> NUM
%token LPAREN RPAREN
%token EOF

%left IC
%left IN

%type <(string * IA.interval) list> main
%start main

%%

main:
  | intervals EOF { $1 }
  
intervals:
  | ID IN NUM NUM  { [($1, new IA.interval (float_of_string $3) (float_of_string $4))]}
  | IC intervals intervals       { List.append $2 $3 }
  | LPAREN intervals RPAREN      { $2 }
