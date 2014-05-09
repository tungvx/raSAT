%{
open Parsing
open Ast
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
  | EQ poly_expr poly_expr       { Eq  ($2, $3) }
  | GEQ poly_expr poly_expr      { Geq ($2, $3) }
  | LEQ poly_expr poly_expr      { Leq ($2, $3) }
  | GR poly_expr poly_expr       { Gr  ($2, $3) }
  | LE poly_expr poly_expr       { Le  ($2, $3) }
  | LPAREN bool_expr RPAREN      { $2 }
  | AND bool_expr bool_expr      { And ($2, $3) }

poly_expr:
  | PLUS poly_expr poly_expr     { Add ($2, $3) }
  | MINUS poly_expr poly_expr    { Sub ($2, $3) }
  | TIMES poly_expr poly_expr    { Mul ($2, $3) }
  | POWER poly_expr NUM          { Pow ($2, int_of_string $3) }
  | NUM                          { Real (float_of_string $1) }
  | ID                           { Var $1 }
  | LPAREN poly_expr RPAREN      { $2 }

intv_expr:
  | intv_clause                  { Cl $1 }
  | IC intv_expr intv_expr       { Ic ($2, $3) }
  | LPAREN intv_expr RPAREN      { $2 }

intv_clause: 
  | ID IN NUM NUM                { In ($1, float_of_string $3, float_of_string $4) }
  | OR intv_clause intv_clause   { Or ($2, $3) }
  | LPAREN intv_clause RPAREN    { $2 }
