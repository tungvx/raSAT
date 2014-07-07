(*SMT expression*)
type smt_poly_expr = 
  | Add of smt_poly_expr * smt_poly_expr
  | Sub of smt_poly_expr * smt_poly_expr
  | Mul of smt_poly_expr * smt_poly_expr
  | Div of smt_poly_expr * smt_poly_expr
  | Pow of smt_poly_expr * int
  | Real of float
  | Var of string
  | SubVar of string

type smt_bool_expr = 
  | Eq of smt_poly_expr * smt_poly_expr
  | Geq of smt_poly_expr * smt_poly_expr
  | Leq of smt_poly_expr * smt_poly_expr
  | Gr of smt_poly_expr * smt_poly_expr
  | Le of smt_poly_expr * smt_poly_expr
  | And of smt_bool_expr * smt_bool_expr
  | Or of smt_bool_expr * smt_bool_expr
  | BVar of string
  | Not of smt_bool_expr

type let_expr = 
  | PEq of string * smt_poly_expr
  | BEq of string * smt_bool_expr
  | Let of let_expr * let_expr

type ass_expr = 
  | Ch of smt_bool_expr
  | As of let_expr * ass_expr
  | Conj of ass_expr * ass_expr


(*=====================================================
(*raSAT expressin*)
type poly_expr = 
  | Add of poly_expr * poly_expr 
  | Sub of poly_expr * poly_expr
  | Mul of poly_expr * poly_expr
  | Pow of poly_expr * int
  | Real of float
  | Var of string

type bool_expr = 
  | Eq of poly_expr * poly_expr
  | Geq of poly_expr * poly_expr
  | Leq of poly_expr * poly_expr
  | Gr of poly_expr * poly_expr
  | Le of poly_expr * poly_expr
  | And of bool_expr * bool_expr

type intv_clause =                         (*interval constraint for each variable*)
  | In of string * float * float
  | Or of intv_clause * intv_clause                

type intv_expr = 
  | Cl of intv_clause
  | Ic of intv_expr * intv_expr            (*Ic: Interval constraints *)

type formula =
  | Ass of bool_expr
  | Intv of intv_expr  

*)
