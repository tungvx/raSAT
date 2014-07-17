%{
open Parsing
open Ast
open IA
%}

%token EOF
%token <string> NUM
%token <string> ID
%token LPAREN RPAREN 
%token PLUS MINUS TIMES POWER
%token ASSERT
%token AND OR

%token IC
%token IN

%token EQ GEQ LEQ GR LE

%left PLUS
%left MINUS
%left TIMES
%left POWER

%left ASSERT

%left AND
%left OR

%left IC
%left IN

%left EQ
%left GEQ
%left LEQ
%left GR
%left LE

%type <Ast.formula> main
%start main

%%

main:
  | f_expr EOF { $1 }

f_expr:
  | ASSERT bool_expr             { Ass $2 }    
  | intv_expr                    { Intv $1 }
  | LPAREN f_expr RPAREN         { $2 }

bool_expr:
  | EQ poly_expr NUM             { 
                                   let number = float_of_string $3 in
                                   if number = 0. then Eq  $2 
                                   else Eq (Sub($2, Real number))
                                 }
  | GEQ poly_expr NUM            { 
                                   let number = float_of_string $3 in
                                   if number = 0. then Geq  $2 
                                   else Geq (Sub($2, Real number))
                                 }
  | LEQ poly_expr NUM            { 
                                   let number = float_of_string $3 in
                                   if number = 0. then Leq  $2 
                                   else Leq (Sub($2, Real number))
                                 }
  | GR poly_expr  NUM            { 
                                   let number = float_of_string $3 in
                                   if number = 0. then Gr  $2 
                                   else Gr (Sub($2, Real number))
                                 }
  | LE poly_expr  NUM            { 
                                   let number = float_of_string $3 in
                                   if number = 0. then Le  $2 
                                   else Le (Sub($2, Real number))
                                 }
  | LPAREN bool_expr RPAREN      { $2 }
  | AND bool_expr bool_expr      { And ($2, $3) }
  | OR bool_expr bool_expr       { BOr ($2, $3) }

poly_expr:
  | PLUS poly_expr poly_expr     { Add ($2, $3) }
  | MINUS poly_expr poly_expr    { Sub ($2, $3) }
  | TIMES poly_expr poly_expr    { Mul ($2, $3) }
  | POWER poly_expr NUM          { Pow ($2, int_of_string $3) }
  | NUM                          { Real (float_of_string $1) }
  | ID                           { Var $1 }
  | LPAREN poly_expr RPAREN      { $2 }

intv_expr:
  | ID IN NUM NUM  { [($1, new IA.interval (float_of_string $3) (float_of_string $4))]}
  | IC intv_expr intv_expr       { List.append $2 $3 }
  | LPAREN intv_expr RPAREN      { $2 }
