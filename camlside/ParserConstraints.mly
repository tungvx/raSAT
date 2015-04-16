%{
open Parsing
open Ast
open PolynomialConstraint
open Variable
%}

%token EOF
%token <string> NUM
%token <string> ID
%token LPAREN RPAREN 
%token PLUS MINUS TIMES
%token ASSERT
%token AND OR

%token EQ GEQ LEQ GR LE NEQ

%left PLUS
%left MINUS
%left TIMES

%left ASSERT

%left AND
%left OR


%left EQ
%left NEQ
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
                                   let (polyCons, variablesSet) = $2 in
                                   if number = 0. then Single (new polynomialConstraint (Eq polyCons) variablesSet)
                                   else Single (new polynomialConstraint (Eq (Sub($2, (Real number, VariablesSet.empty)))) variablesSet)
                                 }
  | NEQ poly_expr NUM             { 
                                   let number = float_of_string $3 in
                                   let (polyCons, variablesSet) = $2 in
                                   if number = 0. then Single (new polynomialConstraint (Neq polyCons) variablesSet)
                                   else Single (new polynomialConstraint (Neq (Sub($2, (Real number, variablesSet)))) variablesSet)
                                 }                                 
  | GEQ poly_expr NUM            { 
                                   let number = float_of_string $3 in
                                   let (polyCons, variablesSet) = $2 in
                                   if number = 0. then Single (new polynomialConstraint (Geq  polyCons) variablesSet)
                                   else Single (new polynomialConstraint (Geq (Sub($2, (Real number, variablesSet)))) variablesSet)
                                 }
  | LEQ poly_expr NUM            { 
                                   let number = float_of_string $3 in
                                   let (polyCons, variablesSet) = $2 in
                                   if number = 0. then Single (new polynomialConstraint(Leq polyCons) variablesSet)
                                   else Single (new polynomialConstraint(Leq (Sub($2, (Real number, variablesSet)))) variablesSet)
                                 }
  | GR poly_expr  NUM            { 
                                   let number = float_of_string $3 in
                                   let (polyCons, variablesSet) = $2 in
                                   if number = 0. then Single (new polynomialConstraint(Gr  polyCons) variablesSet)
                                   else Single (new polynomialConstraint(Gr (Sub($2, (Real number, variablesSet)))) variablesSet)
                                 }
  | LE poly_expr  NUM            { 
                                   let number = float_of_string $3 in
                                   let (polyCons, variablesSet) = $2 in
                                   if number = 0. then Single (new polynomialConstraint(Le  polyCons) variablesSet)
                                   else Single (new polynomialConstraint(Le (Sub($2, (Real number, variablesSet)))) variablesSet)
                                 }
  | LPAREN polynomialConstraints RPAREN      { $2 }
  | AND polynomialConstraints polynomialConstraints      { And ($2, $3) }
  | OR polynomialConstraints polynomialConstraints       { BOr ($2, $3) }

poly_expr:
  | PLUS poly_expr poly_expr     { let (polyExpr2, variablesSet2) = $2 in let (polyExpr3, variablesSet3) = $3 in 
                                   (Add ($2, $3), VariablesSet.union variablesSet2 variablesSet3) }
  | MINUS poly_expr poly_expr    { let (polyExpr2, variablesSet2) = $2 in let (polyExpr3, variablesSet3) = $3 in 
                                   (Sub ($2, $3), VariablesSet.union variablesSet2 variablesSet3) }
  | TIMES poly_expr poly_expr    { let (polyExpr2, variablesSet2) = $2 in let (polyExpr3, variablesSet3) = $3 in 
                                   (Mul ($2, $3), VariablesSet.union variablesSet2 variablesSet3) }
  | NUM                          { (Real (float_of_string $1), VariablesSet.empty) }
  | ID                           { (Var $1, VariablesSet.singleton $1) }
  | LPAREN poly_expr RPAREN      { $2 }
