%{
open Parsing
open Exp
%}

%token EOF
%token <string> NUM
%token <string> ID
%token <string> SUBVAR
%token LPAREN RPAREN 
%token PLUS MINUS TIMES DIV POWER
%token LET
%token ASSERT
%token AND CONJ OR NOT ITE
%token EQ GEQ LEQ GR LE

%left PLUS
%left MINUS
%left TIMES
%left DIV
%left POWER

%left LET
%left ASSERT

%left AND
%left CONJ
%left OR
%left NOT
%left ITE

%left EQ
%left GEQ
%left LEQ
%left GR
%left LE

%type <Exp.ass_expr> main
%start main

%%

main:
  | fass_expr EOF { $1 }

fass_expr:
  | ASSERT ass_expr              { $2 }    
  | LPAREN fass_expr RPAREN      { $2 }

ass_expr:
  | smt_bool_expr                    { Ch $1 }
  | LET let_expr ass_expr        { As ($2, $3) }
  | CONJ ass_expr ass_expr		{ Conj ($2, $3)}
  | LPAREN ass_expr RPAREN       { $2 }

let_expr:
  | SUBVAR smt_poly_expr             { PEq ($1, $2) }
  | SUBVAR smt_bool_expr             { BEq ($1, $2) }
  | LPAREN let_expr RPAREN       { $2 }
  | let_expr let_expr            { Let ($1, $2) }

smt_bool_expr:
  | EQ smt_poly_expr smt_poly_expr       { Eq  ($2, $3) }
  | GEQ smt_poly_expr smt_poly_expr      { (*print_endline (bool_toString 0 (Geq ($2, $3))); flush stdout;*)Geq ($2, $3) }
  | LEQ smt_poly_expr smt_poly_expr      { Leq ($2, $3) }
  | GR smt_poly_expr smt_poly_expr       { Gr  ($2, $3) }
  | LE smt_poly_expr smt_poly_expr       { Le  ($2, $3) }
  | LPAREN smt_bool_expr RPAREN      { $2 }
  | AND smt_bool_expr smt_bool_exprs      { And ($2, $3) }
  | OR smt_bool_expr smt_bool_exprs      { Or ($2, $3) }
  | NOT smt_bool_expr                { Not $2 }
  | SUBVAR                       { BVar $1 }

smt_bool_exprs:
  | smt_bool_expr                 {$1}
  | smt_bool_expr smt_bool_exprs  {Multiple($1, $2)}

smt_poly_expr:
  | PLUS smt_poly_expr smt_poly_exprs     { Add ($2, $3) }
  | MINUS smt_poly_expr smt_poly_expr    { Sub ($2, $3) }
  | MINUS smt_poly_expr              { Sub (Real (0.0), $2) }
  | TIMES smt_poly_expr smt_poly_exprs    { (*print_endline (poly_toString "" 0 (Mul ($2, $3))); flush stdout;*) Mul ($2, $3) }
  | DIV smt_poly_expr smt_poly_expr      { Div ($2, $3) }
  | POWER smt_poly_expr NUM          { Pow ($2, int_of_string $3) }
  | NUM                          { (*print_endline $1; flush stdout;*) Real (float_of_string $1) }
  | ID                           { (*print_endline $1; flush stdout;*) Var $1 }
  | SUBVAR                       { SubVar $1 }
  | LPAREN smt_poly_expr RPAREN      {(*print_endline (poly_toString "" 0 $2); flush stdout;*) $2 }
  
smt_poly_exprs:
  | smt_poly_expr {$1}
  | smt_poly_expr smt_poly_exprs {MultiplePoly ($1, $2)}  
