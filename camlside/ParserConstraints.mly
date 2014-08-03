%{
open Parsing
open Ast
open PolynomialConstraint
%}

%token EOF
%token <string> NUM
%token <string> ID
%token LPAREN RPAREN 
%token PLUS MINUS TIMES POWER
%token ASSERT
%token AND OR

%token EQ GEQ LEQ GR LE

%left PLUS
%left MINUS
%left TIMES
%left POWER

%left ASSERT

%left AND
%left OR


%left EQ
%left GEQ
%left LEQ
%left GR
%left LE

%type <PolynomialConstraint.constraints> main
%start main

%%

main:
  | f_expr EOF { $1 }

f_expr:
  | ASSERT polynomialConstraints { $2 }   
  | LPAREN f_expr RPAREN         { $2 }

polynomialConstraints:
  | EQ poly_expr NUM             { 
                                   let number = float_of_string $3 in
                                   if number = 0. then Single (new polynomialConstraint (Eq $2))
                                   else Single (new polynomialConstraint (Eq (Sub($2, Real number))))
                                 }
  | GEQ poly_expr NUM            { 
                                   let number = float_of_string $3 in
                                   if number = 0. then Single (new polynomialConstraint (Geq  $2))
                                   else Single (new polynomialConstraint (Geq (Sub($2, Real number))))
                                 }
  | LEQ poly_expr NUM            { 
                                   let number = float_of_string $3 in
                                   if number = 0. then Single (new polynomialConstraint(Leq  $2))
                                   else Single (new polynomialConstraint(Leq (Sub($2, Real number))))
                                 }
  | GR poly_expr  NUM            { 
                                   let number = float_of_string $3 in
                                   if number = 0. then Single (new polynomialConstraint(Gr  $2))
                                   else Single (new polynomialConstraint(Gr (Sub($2, Real number))))
                                 }
  | LE poly_expr  NUM            { 
                                   let number = float_of_string $3 in
                                   if number = 0. then Single (new polynomialConstraint(Le  $2))
                                   else Single (new polynomialConstraint(Le (Sub($2, Real number))))
                                 }
  | LPAREN polynomialConstraints RPAREN      { $2 }
  | AND polynomialConstraints polynomialConstraints      { And ($2, $3) }
  | OR polynomialConstraints polynomialConstraints       { BOr ($2, $3) }

poly_expr:
  | PLUS poly_expr poly_expr     { Add ($2, $3) }
  | MINUS poly_expr poly_expr    { Sub ($2, $3) }
  | TIMES poly_expr poly_expr    { Mul ($2, $3) }
  | POWER poly_expr NUM          { Pow ($2, int_of_string $3) }
  | NUM                          { Real (float_of_string $1) }
  | ID                           { Var $1 }
  | LPAREN poly_expr RPAREN      { $2 }
