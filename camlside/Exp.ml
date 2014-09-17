(*SMT expression*)
type smt_poly_expr = 
  | Add of smt_poly_expr * smt_poly_expr
  | Sub of smt_poly_expr * smt_poly_expr
  | Mul of smt_poly_expr * smt_poly_expr
  | Div of smt_poly_expr * smt_poly_expr
  | MultiplePoly of smt_poly_expr * smt_poly_expr
  | Pow of smt_poly_expr * int
  | Real of float
  | Var of string
  | SubVar of string

type smt_bool_expr = 
  | Eq of smt_poly_expr * smt_poly_expr
  | Neq of smt_poly_expr * smt_poly_expr
  | Geq of smt_poly_expr * smt_poly_expr
  | Leq of smt_poly_expr * smt_poly_expr
  | Gr of smt_poly_expr * smt_poly_expr
  | Le of smt_poly_expr * smt_poly_expr
  | BVar of string
  | Not of smt_bool_expr
  | Multiple of smt_bool_expr * smt_bool_expr
  | And of smt_bool_expr * smt_bool_expr
  | Or of smt_bool_expr * smt_bool_expr

type let_expr = 
  | PEq of string * smt_poly_expr
  | BEq of string * smt_bool_expr
  | Let of let_expr * let_expr

type ass_expr = 
  | Ch of smt_bool_expr
  | As of let_expr * ass_expr
  | Conj of ass_expr * ass_expr


let isVar = function
  | Real c -> true 
  | Var x -> true
  | SubVar u -> true
  | Mul (e1, e2) -> true	
  | _ -> false

let rev = function
  | "+" -> "-"
  | "-" -> "+"
  | _ -> "-"

  
let sign_simp sign (num: float) = match sign with
| "" -> string_of_float num
| _ ->
  if  (num < 0.) then rev sign ^ string_of_float (abs_float num)
  else sign ^ string_of_float num 
  

(*Represente a polynomial expression by a string*)
let rec poly_toString sign isAddOrMul  = function
  | Real c -> sign_simp sign c
  | Var x -> sign ^ x
  | SubVar u -> sign ^ u
  | Mul (Real 1., Var x) -> sign ^ x
  | Mul (Var x, Real 1.) -> sign ^ x
  | Mul (Real -1., Var x) -> rev sign ^ x
  | Mul (Var x, Real -1.) -> rev sign ^ x
  | Add (e1, e2) -> (poly_toString sign 1 e1) ^ (poly_toString "+" 1 e2)
  | MultiplePoly (e1, e2) -> 
    if isAddOrMul = 1 then 
      (poly_toString sign 1 e1) ^ (poly_toString "+" 1 e2)
    else if isAddOrMul = 2 then 
      (poly_toString sign 2 e1) ^ (poly_toString "*" 2 e2)
    else ""
  | Sub (e1, e2) -> (poly_toString sign 0 e1) ^
      (if (isVar e2) then                    
	      (poly_toString "-" 0 e2)
      else
	      "-("^(poly_toString "" 0 e2)^")")
  | Mul (e1, e2) -> 
      (if (isVar e1) then 
         (poly_toString sign 2 e1)
      else
         sign ^"("^(poly_toString "" 2 e1)^")" )
         ^ "*" ^ 
         (if (isVar e2) then                    
	         (poly_toString "" 2 e2)
         else
	         "("^(poly_toString "" 2 e2)^")")
  | Div (e1, e2) -> (poly_toString sign 0 e1) ^"/"^ (poly_toString "+" 0 e2)
  | Pow (e1, n)  -> sign ^ "("^(poly_toString "" 0 e1)^")" ^ "^" ^ (string_of_int n)

(*Represent a bool expression by a string*)
let rec bool_toString isAndOr = function
  | BVar bVar -> bVar
  | Eq (e1, e2) -> (poly_toString "" 0 e1)^" = " ^ (poly_toString "" 0 e2)
  | Neq (e1, e2) -> (poly_toString "" 0 e1)^" != " ^ (poly_toString "" 0 e2)
  | Le (e1, e2) -> (poly_toString "" 0 e1)^" < " ^ (poly_toString "" 0 e2)
  | Leq(e1, e2) -> (poly_toString "" 0 e1)^" <= " ^(poly_toString "" 0 e2)
  | Gr (e1, e2) -> (poly_toString "" 0 e1)^" > " ^ (poly_toString "" 0 e2)
  | Geq(e1, e2) -> (poly_toString "" 0 e1)^" >= " ^(poly_toString "" 0 e2)
  | And(e1, e2) -> (bool_toString 1 e1)^"\nand "^(bool_toString 1 e2)
  | Or(e1, e2) -> (bool_toString (-1) e1)^"\nor "^(bool_toString (-1) e2)
  | Multiple (b1, b2) -> 
    if isAndOr = -1 then
      (bool_toString (-1) b1)^"\nor "^(bool_toString (-1) b2)
    else (* default will be And *)
      (bool_toString 1 b1)^"\nor "^(bool_toString 1 b2)
  | Not (e1) ->  "Not " ^ (bool_toString 0 e1)  

(* ============================== START let_expr_to_infix_string =======================================*)
(* This function converts the let expression into the string in infix form *)
let rec let_expr_to_infix_string = function 
  | PEq (var, smtPolyExpr) -> var ^ " = " ^ (poly_toString "" 0 smtPolyExpr)
  | BEq (var, smtBoolExpr) -> var ^ " = " ^ (bool_toString 0 smtBoolExpr)
  | Let (letExpr1, letExpr2) -> (let_expr_to_infix_string letExpr1) ^ "\n" ^ (let_expr_to_infix_string letExpr2)
(* ============================== END let_expr_to_infix_string =======================================*)


(* ============================== START ass_expr_to_infix_string =======================================*)
(* This function converts the assertion expression into the string in infix form *)
let rec ass_expr_to_infix_string = function 
  | Ch smtBoolExpr -> bool_toString 0 smtBoolExpr
  | As(letExpr, assExpr) ->  (let_expr_to_infix_string letExpr) ^ "\n" ^ (ass_expr_to_infix_string assExpr)
  | Conj(assExpr1, assExpr2) -> (ass_expr_to_infix_string assExpr1) ^ "\n" ^ (ass_expr_to_infix_string assExpr2)  
(* ============================== END ass_expr_to_infix_string =======================================*)

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
