open Exp           (*type: literal, clause, formula... declaration*)   
open Expr          (*Simplify expression module*)
open Variable
open Util
open IA


(*This module will generate form for Solver from SMT benchmark files*)
module Caml = struct

type nil_expr = 
  | BC of smt_bool_expr
  | Nil
  | AND of nil_expr * nil_expr 

(* Read: get an SMT expression from a string *)
let read s = ParserSmt.main LexerSmt.lex (Lexing.from_string s)


(* ---------------------------
   Reduce and simplify assert expressions     
   --------------------------- *)

(*Get CheckSAT expression*)
let rec getAss_expr = function
  | Ch (e) -> e
  | As (e1, e2) -> getAss_expr e2
  | Conj (e1, e2) -> And (getAss_expr e1, getAss_expr e2)

(*List pair of based sub variables and associated expression from Let expression*)
let rec lstSubVar = function
  | As (e1, e2) -> 
      let rec getSubVar = function
	      | PEq (svar, e) -> [(svar, e)]
	      | Let (e1, e2) -> List.append (getSubVar e1) (getSubVar e2)
	      | _ -> [] 
	    in
      List.append (getSubVar e1) (lstSubVar e2)
  | Conj (e1, e2) -> List.append (lstSubVar e1) (lstSubVar e2)
  | _ -> []

(*List set of boolean variable from expression*)
let rec lstBVar = function
  | As (e1, e2) -> 
      let rec getBVar = function
	| BEq (svar, e) -> [(svar, e)]
	| Let (e1, e2) -> List.append (getBVar e1) (getBVar e2)
	| _ -> [] in
      List.append (getBVar e1) (lstBVar e2)
  | Conj (e1, e2) -> List.append (lstBVar e1) (lstBVar e2)
  | _ -> []

(*Substitute sub variables for based variables*)
let rec subst_poly ass = function
  | Real c -> Real c
  | Var x -> Var x
  | SubVar u -> subst_poly ass (List.assoc u ass)
  | Add (e1, e2) -> Add (subst_poly ass e1, subst_poly ass e2)
  | Sub (e1, e2) -> Sub (subst_poly ass e1, subst_poly ass e2)
  | Mul (e1, e2) -> Mul (subst_poly ass e1, subst_poly ass e2)
  | Div (e1, e2) -> Div (subst_poly ass e1, subst_poly ass e2)
  | Pow (e1, n)  -> Pow (subst_poly ass e1, n)

(*Substitute  sub bool variables for based variables*)
let rec subst_bool bass pass = function
  | BVar b -> subst_bool bass pass (List.assoc b bass)
  | Eq  (e1, e2) -> Eq  (subst_poly pass e1, subst_poly pass e2)
  | Neq  (e1, e2) -> Neq  (subst_poly pass e1, subst_poly pass e2)
  | Le  (e1, e2) -> Le  (subst_poly pass e1, subst_poly pass e2)
  | Leq (e1, e2) -> Leq (subst_poly pass e1, subst_poly pass e2)
  | Gr  (e1, e2) -> Gr  (subst_poly pass e1, subst_poly pass e2)
  | Geq (e1, e2) -> Geq (subst_poly pass e1, subst_poly pass e2)
  | And (b1, b2) -> And (subst_bool bass pass b1, subst_bool bass pass b2)
  | Or (b1, b2) -> Or (subst_bool bass pass b1, subst_bool bass pass b2)
  | Multiple (b1, b2) -> Multiple (subst_bool bass pass b1, subst_bool bass pass b2)
  | Not e -> Not (subst_bool bass pass e)

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

(*Compute abs of float number*)
let abs_float (num: float) =
  if (num < 0.) then -.num
  else num
  
  
let sign_simp sign (num: float) = match sign with
| "" -> string_of_float num
| _ ->
  if  (num < 0.) then rev sign ^ string_of_float (abs_float num)
  else sign ^ string_of_float num  

(*Represente a polynomial expression by a string*)
let rec poly_toString sign  = function
  | Real c -> sign_simp sign c
  | Var x -> sign ^ x
  | SubVar u -> sign ^ u
  | Mul (Real 1., Var x) -> sign ^ x
  | Mul (Var x, Real 1.) -> sign ^ x
  | Mul (Real -1., Var x) -> rev sign ^ x
  | Mul (Var x, Real -1.) -> rev sign ^ x
  | Add (e1, e2) -> (poly_toString sign e1) ^ (poly_toString "+" e2)
  | Sub (e1, e2) -> (poly_toString sign e1) ^
      (if (isVar e2) then                    
	(poly_toString "-" e2)
      else
	"-("^(poly_toString "" e2)^")")
  | Mul (e1, e2) -> 
      (if (isVar e1) then 
         (poly_toString sign e1)
      else
         sign^"("^(poly_toString "" e1)^")" )
      ^ "*" ^ 
      (if (isVar e2) then                    
	(poly_toString "" e2)
      else
	"("^(poly_toString "" e2)^")")
  | Div (e1, e2) -> (poly_toString sign e1) ^"/"^ (poly_toString "+" e2)
  | Pow (e1, n)  -> sign ^ "("^(poly_toString "" e1)^")" ^ "^" ^ (string_of_int n)

(*Represent a bool expression by a string*)
let rec bool_toString isAndOr = function
  | BVar bVar -> bVar
  | Eq (e1, e2) -> (poly_toString "" e1)^" = " ^ (poly_toString "" e2)
  | Neq (e1, e2) -> (poly_toString "" e1)^" != " ^ (poly_toString "" e2)
  | Le (e1, e2) -> (poly_toString "" e1)^" < " ^ (poly_toString "" e2)
  | Leq(e1, e2) -> (poly_toString "" e1)^" <= " ^(poly_toString "" e2)
  | Gr (e1, e2) -> (poly_toString "" e1)^" > " ^ (poly_toString "" e2)
  | Geq(e1, e2) -> (poly_toString "" e1)^" >= " ^(poly_toString "" e2)
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
  | PEq (var, smtPolyExpr) -> var ^ " = " ^ (poly_toString "" smtPolyExpr)
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
  

(*Remove 0. constant in an expression*)
let rec remove_zero e = match e with
  |Var x -> Var x
  |Real c -> Real c
  |Add (e1, Real 0.) -> e1
  |Add (Real 0., e1) -> e1
  |Add (e1, e2) -> Add (remove_zero e1, remove_zero e2)
  |Sub (e1, Real 0.) -> e1
  |Sub (Real 0., e1) -> Mul (Real (-1.), e1)
  |Sub (e1, e2) -> Sub (remove_zero e1, remove_zero e2)
  |Mul (e1, e2) -> Mul (remove_zero e1, remove_zero e2)
  |Pow (e1, n) -> Pow (remove_zero e1, n)
  | _ -> e

(*Reduce or simplify an bool expression*)
let rec bool_reduce = function
  | BVar bVar -> BVar bVar
  | Eq (e1, e2) -> Eq (remove_zero (Expr.reduce e1), remove_zero (Expr.reduce e2))
  | Neq (e1, e2) -> Neq (remove_zero (Expr.reduce e1), remove_zero (Expr.reduce e2))
  | Le (e1, e2) -> Le (remove_zero (Expr.reduce e1), remove_zero (Expr.reduce e2))
  | Leq(e1, e2) -> Leq(remove_zero (Expr.reduce e1), remove_zero (Expr.reduce e2))
  | Gr (e1, e2) -> Gr (remove_zero (Expr.reduce e1), remove_zero (Expr.reduce e2))
  | Geq(e1, e2) -> Geq(remove_zero (Expr.reduce e1), remove_zero (Expr.reduce e2))
  | And(e1, e2) -> And(bool_reduce e1, bool_reduce e2)
  | Multiple(b1, b2) -> Multiple(bool_reduce b1, bool_reduce b2)
  | Or(e1, e2) -> Or(bool_reduce e1, bool_reduce e2)
  | Not (e1) -> Not (bool_reduce e1)


(*Represent a poly expression to a string by prefix order*)
let rec poly_toPrefix = function
  | Real c -> string_of_float c
  | Var x -> x
  | SubVar u -> u
  | Add (e1, e2) -> "(+ "^ poly_toPrefix e1 ^ " " ^ poly_toPrefix e2 ^ ")"
  | Sub (e1, e2) -> "(- "^ poly_toPrefix e1 ^ " " ^ poly_toPrefix e2 ^ ")"
  | Mul (e1, e2) -> "(* "^ poly_toPrefix e1 ^ " " ^ poly_toPrefix e2 ^ ")"
  | Div (e1, e2) -> "(/ "^ poly_toPrefix e1 ^ " " ^ poly_toPrefix e2 ^ ")"
  | Pow (e1, n)  -> "(^ "^ poly_toPrefix e1 ^ " " ^ (string_of_int n)^ ")"

(*Represent a bool expression to a string by prefix order*)
let rec bool_toPrefix isAndOr = function
  | BVar bVar -> bVar
  | Eq (e1, e2) -> "(= " ^ poly_toPrefix e1 ^ " " ^ poly_toPrefix e2 ^ ")"
  | Neq (e1, e2) -> "(!= " ^ poly_toPrefix e1 ^ " " ^ poly_toPrefix e2 ^ ")"
  | Le (e1, e2) -> "(< " ^ poly_toPrefix e1 ^ " " ^ poly_toPrefix e2 ^ ")"
  | Leq(e1, e2) -> "(<= "^ poly_toPrefix e1 ^ " " ^ poly_toPrefix e2 ^ ")"
  | Gr (e1, e2) -> "(> " ^ poly_toPrefix e1 ^ " " ^ poly_toPrefix e2 ^ ")"
  | Geq(e1, e2) -> "(>= " ^ poly_toPrefix e1 ^ " " ^ poly_toPrefix e2 ^ ")"
  | And(e1, e2) ->"(and "^ bool_toPrefix 1 e1 ^ " " ^ bool_toPrefix 1 e2^ ")"
  | Or(e1, e2) ->"(or "^ bool_toPrefix (-1) e1 ^ " " ^ bool_toPrefix (-1) e2^ ")"
  | Multiple(e1, e2) ->
    if isAndOr = -1 then 
      "(or "^ bool_toPrefix (-1) e1 ^ " " ^ bool_toPrefix (-1) e2^ ")"
    else 
      "(and "^ bool_toPrefix 1 e1 ^ " " ^ bool_toPrefix 1 e2^ ")"
  | Not (e1)    ->"(not "^bool_toPrefix 0 e1^ ")"

(*Represent a nil expression to a string by prefix order*)
let rec bound_toPrefix = function
  | Nil -> ""
  | BC (b) -> bool_toPrefix 0 b
  | AND (b1, b2) -> "(and "^ bound_toPrefix b1 ^ " " ^ bound_toPrefix b2^ ")"

let rec div_constr e = match e with
  | And(e1, e2) -> div_constr e1 ^ "\n" ^ div_constr e2
  | _ -> bool_toPrefix 0 e

let poly_isCons = function
  | Real 0. -> true
  | _ -> false

(*Simplify a boolean expression: a constant placed in right hand side*)
let rec bool_simp = function
  | BVar bVar -> BVar bVar
  | Eq (e1, e2) -> 
      if poly_isCons e2 then Eq (e1, e2)
      else Eq (Sub(e1, e2), Real 0.)
  | Neq (e1, e2) -> 
      if poly_isCons e2 then Neq (e1, e2)
      else Neq (Sub(e1, e2), Real 0.)
  | Le (e1, e2) -> 
      if poly_isCons e2 then Le (e1, e2)
      else Le (Sub(e1, e2), Real 0.)
  | Leq(e1, e2) -> 
      if poly_isCons e2 then Leq (e1, e2)
      else Leq (Sub(e1, e2), Real 0.)
  | Gr (e1, e2) -> 
      if poly_isCons e2 then Gr (e1, e2)
      else Gr (Sub(e1, e2), Real 0.)
  | Geq(e1, e2) -> 
      if poly_isCons e2 then Geq (e1, e2)
      else Geq (Sub(e1, e2), Real 0.)
  | And(e1, e2) -> And (bool_simp e1, bool_simp e2)
  | Or(e1, e2) -> Or (bool_simp e1, bool_simp e2)
  | Multiple(e1, e2) -> Multiple (bool_simp e1, bool_simp e2)
  | Not (e1) -> Not (bool_simp e1)


(*------ REFINEMENT STEP -------
  ------ Get an upper bound or lower a bound of based variables from SMT Benchmark constraints*)

(*get the list of variables from a polynomial expression*)
  let rec get_vars = function
    | Var x -> [x]
    | Add(e1, e2) -> List.append (get_vars e1) (get_vars e2)
    | Sub(e1, e2) -> List.append (get_vars e1) (get_vars e2)
    | Mul(e1, e2) -> List.append (get_vars e1) (get_vars e2)
    | Div(e1, e2) -> List.append (get_vars e1) (get_vars e2)
    | Pow(e1, n)  -> get_vars e1
    | _ -> []

  (*Reduce list to set*)
  let rec set_list l1 l2 = match l2 with
      |[] -> l1
      |h::t -> 
	  if (List.mem h l1) then 
	    set_list l1 t
	  else
	    set_list (h::l1) t

  let red_list l = set_list [] l


(*Assume all variables >= 0, get all the bounds for variables
 
A naive implementation

*)

(*Remove an element from a list*)
let rec rem_list l a = match l with
  | h::t -> 
      if (h=a) then t
      else h::(rem_list t a)
  | [] -> []

(*Get the common elements in two lists*)
let rec com_list l1 l2 = match l1 with
  | h::t ->
      if (List.mem h l2) then h::(com_list t (rem_list l2 h))
      else com_list t l2
  | [] -> []

let merge_list (l1, a) (l2, b) = (List.append l1 l2, a*.b)

let rec _lstVar = function
  | Var x  -> ([x], 1.)
  | Real c -> ([] , c)
  | Mul (e1, e2) -> merge_list (_lstVar e1) (_lstVar e2)
  | Div (e1, e2) -> merge_list (_lstVar e1) (_lstVar e2)
  | Add (e1, e2) -> let (l1, a1) = _lstVar e1 in
                    let (l2, a2) = _lstVar e2 in
		    ((com_list l1 l2), 1.)
  | Sub (e1, e2) -> let (l1, a1) = _lstVar e1 in
                    let (l2, a2) = _lstVar e2 in
		    ((com_list l1 l2), 1.)
  | _ -> ([], 1.)


(*Check an element existing in a list*)
let rec in_list l a = match l with
  | h::t ->
      if (h=a) then true
      else in_list t a
  | [] -> false


(*Check a variable in multiplication of expression*)
let rec in_expr e v = match e with
  | Var x -> 
      if (x=v) then true
      else false
  | Mul (e1, e2) ->
      if (in_expr e1 v) then true
      else (in_expr e2 v)
  | Div (e1, e2) ->
      if (in_expr e1 v) then true
      else (in_expr e2 v)
  | Add (e1, e2) -> 
      if (in_expr e1 v) && (in_expr e2 v) then true
      else false
  | Sub (e1, e2) -> 
      if (in_expr e1 v) && (in_expr e2 v) then true
      else false
(*  | Pow (e1, n) ->
      if (in_expr e1 v) then true
      else false
*)
  | _ -> false
 
(*Div a polynomial function by a variable*)
let rec div_var exp v = match exp with
  | Var x -> 
      if (x=v) then Real 1.
      else Var x
  | Mul (Var x, Var y) -> 
      if (x=v) then (Var y)
      else if (y=v) then (Var x)
      else Mul (Var x, Var y)
  | Mul (Var x, e) -> 
      if (x=v) then e
      else Mul (Var x, (div_var e v))
  | Mul (e, Var x) -> 
      if (x=v) then e
      else Mul ((div_var e v), Var x)
  | Mul (Real c, e) -> Mul (Real c, (div_var e v))
  | Mul (e1, e2) -> 
      if (in_expr e1 v) then Mul ((div_var e1 v), e2)
      else Mul (e1, (div_var e2 v))
  | Add (e1, e2) -> 
      if (in_expr e1 v) && (in_expr e2 v) then Add ((div_var e1 v), (div_var e2 v))
      else exp
  | Sub (e1, e2) -> 
      if (in_expr e1 v) && (in_expr e2 v) then Sub ((div_var e1 v), (div_var e2 v))
      else exp
(*  | Pow (e1, n) ->
      if (in_expr e1 v) then Pow (x, n-1)
      else exp
*)
  | _ -> exp

(*Div a polynomial function by a coeff*)
let rec div_coef exp c = match exp with
  | Mul (Real a, e) -> Mul (Real (a/.c), e)
  | Mul (Var x, e) -> Mul (Var x, (div_coef e c))
  | Mul (e1, e2) -> Mul (e1, (div_coef e2 c))
  | _ -> exp

(*Div a polynomial function by a list of variables*)
let rec div_func exp l = match l with
  | h::t -> div_func (div_var exp h) t
  | [] -> exp

let simp_expr e1 e2 = 
  let (l1, a1) = _lstVar e1 in
  let (l2, a2) = _lstVar e2 in
  let l = com_list l1 l2 in
  let m = min (abs_float a1) (abs_float a2) in
  (div_coef (div_func e1 l) m, div_coef (div_func e2 l) m)

(*Reduce an polynomial function with assume that all variables >=0*)
let rem_var e = match e with 
  | Real c -> Real c
  | Var x -> Real 1.
  | Add (e1, e2) ->
      let (l1,a1) = _lstVar e1 in
      let (l2,a2) = _lstVar e2 in
      let l = com_list l1 l2 in
      let m = min (abs_float a1) (abs_float a2) in
      Add (div_coef (div_func e1 l) m, div_coef (div_func e2 l) m)
  | Sub (e1, e2) ->
      let (l1,a1) = _lstVar e1 in
      let (l2,a2) = _lstVar e2 in
      let l = com_list l1 l2 in
      let m = min (abs_float a1) (abs_float a2) in
      Sub (div_coef (div_func e1 l) m, div_coef (div_func e2 l) m)	
  | Mul (e1, e2) ->
      let (l,a) = _lstVar (Mul(e1, e2)) in
      div_coef (div_func (Mul(e1, e2)) l) (abs_float a)
  | _ -> e

(*Simplify constraint in boolean expression
  with assume that all based variables >=0
*)

let rem_var1 e =
  let lstVars = red_list ( get_vars e) in
  div_func e lstVars

(*
let rec simp_cons e = match e with
  | BVar b -> BVar b
  | Eq (e, Real 0.)  -> Eq  (rem_var1 e, Real 0.)
  | Eq (Real 0., e)  -> Eq  (rem_var1 e, Real 0.)
  | Geq(e, Real 0.)  -> Geq (rem_var1 e, Real 0.)
  | Geq(Real 0., e)  -> Leq (rem_var1 e, Real 0.)
  | Leq(e, Real 0.)  -> Leq (rem_var1 e, Real 0.)
  | Leq(Real 0., e)  -> Geq (rem_var1 e, Real 0.)
  | Gr (e, Real 0.)  -> Gr  (rem_var1 e, Real 0.)
  | Gr (Real 0., e)  -> Le  (rem_var1 e, Real 0.)
  | Le (e, Real 0.)  -> Le  (rem_var1 e, Real 0.)
  | Le (Real 0., e)  -> Gr  (rem_var1 e, Real 0.)

  | Eq (e1, e2)  -> 
      let (c1, c2) = (simp_expr e1 e2) in
      Eq (c1, c2)
  | Leq (e1, e2)  -> 
      let (c1, c2) = (simp_expr e1 e2) in
      Leq (c1, c2)
  | Geq (e1, e2)  -> 
      let (c1, c2) = (simp_expr e1 e2) in
      Geq (c1, c2)
  | Le (e1, e2)  -> 
      let (c1, c2) = (simp_expr e1 e2) in
      Le (c1, c2)
  | Gr (e1, e2)  -> 
      let (c1, c2) = (simp_expr e1 e2) in
      Gr (c1, c2)
  | And (e1, e2) -> And (simp_cons e1, simp_cons e2)

*)

(*Simplify constraint in boolean expression
  with assume that all based variables >=0 *)

let rec simp_cons  exp = match exp with
  | BVar b -> BVar b
  | Gr (e, Real 0.)  -> Gr  (rem_var1 e, Real 0.)
  | Gr (Real 0., e)  -> Le  (rem_var1 e, Real 0.)
  | Le (e, Real 0.)  -> Le  (rem_var1 e, Real 0.)
  | Le (Real 0., e)  -> Gr  (rem_var1 e, Real 0.)

  | Le (e1, e2)  -> 
      let (c1, c2) = (simp_expr e1 e2) in
      Le (c1, c2)
  | Gr (e1, e2)  -> 
      let (c1, c2) = (simp_expr e1 e2) in
      Gr (c1, c2)
  | And (e1, e2) -> And (simp_cons e1, simp_cons e2)
  | _ -> exp

(*Reformulate of relation between two expresions: x + b > c <=> x > c-b*)
let rec refor_expr e (c: float) = match e with
  | Mul (Var x, Real a) -> 
      if (a>0.) then (Var x, Real (c/.a), '+')
      else if (a<0.) then (Var x, Real (c/.a), '-')
      else (Real 0., Real c, '+')
  | Mul (Real a, Var x) -> 
      if (a>0.) then (Var x, Real (c/.a), '+')
      else if (a<0.) then (Var x, Real (c/.a), '-')
      else (Real 0., Real c, '+')
  | Add(e1, Real a) -> refor_expr e1 (c-.a)
  | Add(Real a, e1) -> refor_expr e1 (c-.a)
  | Sub(e1, Real a) -> refor_expr e1 (c+.a)
  | Sub(Real a, e1) -> refor_expr (Mul (Real (-1.), e1)) (c-.a)
  | _ -> (e, Real c, '.')

(*Get the lower bound or upper bound from simplify constraints called bound constraints*)
let rec getBound (e: smt_bool_expr) = match e with 
  | Eq (Var x, Real c)  -> BC (Eq  (Var x, Real c))
  | Eq (Real c, Var x)  -> BC (Eq  (Var x, Real c))
  | Geq(Var x, Real c)  -> BC (Geq (Var x, Real c))
  | Geq(Real c, Var x)  -> BC (Leq (Var x, Real c))
  | Gr (Var x, Real c)  -> BC (Gr  (Var x, Real c))
  | Gr (Real c, Var x)  -> BC (Le  (Var x, Real c))
  | Le (Var x, Real c)  -> BC (Le  (Var x, Real c))
  | Le (Real c, Var x)  -> BC (Gr  (Var x, Real c))
  | Leq(Var x, Real c)  -> BC (Leq (Var x, Real c))
  | Leq(Real c, Var x)  -> BC (Geq (Var x, Real c))

  | Eq (e1, Real c) -> 
      let (e3, e4, s) = refor_expr e1 c in
      if (s='.') then Nil 
      else getBound (Eq (e3, e4))      
  | Eq (Real c, e1) -> 
      let (e3, e4, s) = refor_expr e1 c in
      if (s='.') then Nil 
      else getBound (Eq (e3, e4))      
  | Geq (Real c, e1) -> 
      let (e3, e4, s) = refor_expr e1 c in
      if (s='+') then getBound (Leq (e3, e4))
      else if (s='-') then getBound (Geq (e3, e4))
      else Nil
  | Geq (e1, Real c) -> 
      let (e3, e4, s) = refor_expr e1 c in
      if (s='+') then getBound (Geq (e3, e4))
      else if (s='-') then getBound (Leq (e3, e4))
      else Nil
  | Leq (Real c, e1) -> 
      let (e3, e4, s) = refor_expr e1 c in
      if (s='+') then getBound (Geq (e3, e4))	  
      else if (s='-') then getBound (Leq (e3, e4))
      else Nil
  | Leq (e1, Real c) -> 
      let (e3, e4, s) = refor_expr e1 c in
      if (s='+') then getBound (Leq (e3, e4))
      else if (s='-') then getBound (Geq (e3, e4))
      else Nil
  | Gr (Real c, e1) -> 
      let (e3, e4, s) = refor_expr e1 c in
      if (s='+') then getBound (Le (e3, e4))
      else getBound (Gr (e3, e4))
  | Gr (e1, Real c) -> 
      let (e3, e4, s) = refor_expr e1 c in
      if (s='+') then getBound (Gr (e3, e4))
      else if (s='-') then getBound (Le (e3, e4))
      else Nil
  | Le (Real c, e1) -> 
      let (e3, e4, s) = refor_expr e1 c in
      if (s='+') then getBound (Gr (e3, e4))
      else if (s='-') then getBound (Le (e3, e4))
      else Nil
  | Le (e1, Real c) -> 
      let (e3, e4, s) = refor_expr e1 c in
      if (s='+') then getBound (Le (e3, e4))
      else if (s='-') then getBound (Gr (e3, e4))
      else Nil
  | And (e1, e2) -> AND (getBound e1, getBound e2)
  | _ -> Nil

(*Remove Nil constraints*)
let rec remov_nil expr = match expr with
  | Nil -> Nil
  | AND (Nil, e) -> remov_nil e
  | AND (e, Nil) -> remov_nil e
  | AND (e1, e2) -> 
      let n1 = remov_nil e1 in
      let n2 = remov_nil e2 in
      if (n1 <> Nil) && (n2<>Nil) then AND (n1, n2)
      else if (n1 <> Nil) then n1
      else if (n2 <> Nil) then n2
      else Nil
  | _ -> expr


(*This part will decompose interval constraints to disjoint of small part: 
  [a,b] = [a, a1] or [a1, a2] or ... [an, b]
*)

(* Notice: should add a function checking whether exists any inconsistent constraints?*)

(*
  - Update bound of interval constraints from SMT Benchmark constraints 
  - When lower bound > upper bound which means UNSAT
*)

(*Update bound of variable x from bCons constraint*)
let rec update_bound (x, lo, up, bCons) = match bCons with
  | BC (Leq (Var y, Real a))
  | BC (Le  (Var y, Real a)) -> 
      if (x=y)&&(a<up) then (x, lo, a)
      else (x, lo, up)      
  | BC (Geq (Var y, Real a))
  | BC (Gr  (Var y, Real a)) -> 
      if (x=y)&&(a>lo) then (x, a, up)
      else (x, lo, up)
  | AND (e1, e2) -> 
      let (x1, lo1, up1) = update_bound (x, lo, up, e1) in
      update_bound (x1, lo1, up1, e2)
  | _ -> (x, lo, up)


(*Generate a string of constraints*)
let gen_string (v, l1, u1, bst, bCons) = 
  let (x, lo, up) = update_bound (v, l1, u1, bCons) in
  let s = ref "" in
  let l = ref lo in 
  let n = int_of_float ((up-.lo)/.bst) in
  (*addition part*)
  (*if (l1 >= 0.0) && (u1 >= 1.) then *)
  (*  s := "(" ^ x ^ " in " ^ (string_of_float !l) ^ " " ^ (string_of_float !l) ^")"; *)
    (*s := "(or " ^ !s ^ " (" ^ x ^ " in " ^ (string_of_float 1.) ^ " " ^ (string_of_float 1.) ^"))");*)
  
  for i=1 to n do
    if (!s="") then
     s := "(" ^ x ^ " in " ^ (string_of_float !l) ^ " " ^ (string_of_float (!l+.bst)) ^")"
    else
     (s := "(or " ^ !s ^ " (" ^ x ^ " in " ^ (string_of_float !l) ^ " " ^ (string_of_float (!l+.bst)) ^"))");
    l := !l +.bst;
    (*addition part*)
    (*s := "(or " ^ !s ^ " (" ^ x ^ " in " ^ (string_of_float !l) ^ " " ^ (string_of_float (!l)) ^"))";*)
  done;
  
  let ch = (float_of_int n) *.bst in
  if (ch < up) then (
  if (!s="") then
    (s := "(" ^ x ^ " in " ^ (string_of_float !l) ^ " " ^ (string_of_float up) ^")")
  else
    (s := "(or " ^ !s ^ " (" ^ x ^ " in " ^ (string_of_float !l) ^ " " ^ (string_of_float up)^"))");
  );

  s := !s ^ "\n";
  !s

(*
let gen_string (v, l, u, bst, bCons) = 
  let (x, lo, up) = update_bound (v, l, u, bCons) in
  let s = ref "" in
  let l = ref lo in
  while (!l+.bst <=up) do
    if (!s="") then
     (s := !s ^ "(" ^ x ^ " in " ^ (string_of_float !l) ^ " " ^ (string_of_float (!l+.bst)) ^")")
    else
      (s := "or (" ^ !s ^ " (" ^ x ^ " in " ^ (string_of_float !l) ^ " " ^ (string_of_float (!l+.bst)) ^"))");
    l := !l +.bst;
  done;
  if (!l < up) then (
  if (!s="") then
    (s := !s ^ "(" ^ x ^ " in " ^ (string_of_float !l) ^ " " ^ (string_of_float up) ^")")
  else
    (s := "or (" ^ !s ^ " (" ^ x ^ " in " ^ (string_of_float !l) ^ " " ^ (string_of_float up)^"))");
  );
  s := "(assert (" ^ !s ^ ")\n";
  !s *)

let rec gen_intv e mb bst bCons= match e with
  | Geq (Var x, Real a) 
  | Gr  (Var x, Real a) -> gen_string (x, a, (a +.mb), bst, bCons)
  | Leq (Var x, Real a) 
  | Le  (Var x, Real a) -> gen_string (x, (a -.mb), a, bst, bCons)	
  | And (e1, e2) -> (gen_intv e1 mb bst bCons) ^ (gen_intv e2 mb bst bCons)
  | _ -> ""

(*get boolean expression from assert expression*)  
let rec get_bExpr e = match e with
  | Ch (b) -> b
  | As (l, a) -> get_bExpr a
  | Conj (e1, e2) -> And (get_bExpr e1, get_bExpr e2) (*seems neve occurs*)

  (*Reduce list to set*)
  let red_list l = 
    let rec set_list l1 l2 = match l2 with
      |[] -> l1
      |h::t -> 
	  if (List.mem h l1) then 
	    set_list l1 t
	  else
	    set_list (h::l1) t in
    set_list [] l

  (*Decompose a boolean constraints to list of bool constraints without and*)
  let rec bool_toList e = match e with
    | And (e1, e2) -> List.append (bool_toList e2) (bool_toList e1)
    | _ -> [e]

  (*Construct a boolean constraint from a list of boolean constraints*)
  let rec list_toBool lst = match lst with
    |[] -> Eq (Real 0., Real 0.) (*This case will never happen*)
    |[a] -> a
    |h::t -> And (h, list_toBool t)

  let list_remove element lst = List.filter (fun x-> x <> element) lst

  (*remove trivial constraints such as 1. > 0.*)
  let is_trivial = function
    |Geq (Real c1, Real c2) ->
        if (c1 >= c2) then 
           true
        else 
           false
    |Leq (Real c1, Real c2) ->
        if (c1 <= c2) then 
           true
        else 
           false
    |Gr (Real c1, Real c2) ->
        if (c1 > c2) then 
           true
        else 
           false
    |Le (Real c1, Real c2) ->
        if (c1 < c2) then 
           true
        else 
           false
    | _ -> false

  let rec remove_trivial = function
    |[] -> []
    |h::t -> if (is_trivial h) then 
	        remove_trivial t
             else 
                h:: remove_trivial t  
  
  let rec red_cons lst = match lst with
    | [] -> []        
    |Gr(e, Real c)::t ->	 
	if (List.mem (Geq(e, Real c)) t) then
	  Gr(e, Real c):: red_cons (list_remove (Geq (e, Real c)) t)
	else
  	  Gr(e, Real c):: red_cons t
    |Geq(e, Real c)::t ->	 
	if (List.mem (Gr(e, Real c)) t) then
	  red_cons t
	else
  	  Geq(e, Real c):: red_cons t
    |Le(e, Real c)::t ->	 
	if (List.mem (Leq(e, Real c)) t) then
	  Le(e, Real c):: red_cons (list_remove (Leq (e, Real c)) t)
	else
  	  Le(e, Real c):: red_cons t
    |Leq(e, Real c)::t ->	 
	if (List.mem (Le(e, Real c)) t) then
	  red_cons t
	else
  	  Leq(e, Real c):: red_cons t
    |h::t -> h:: red_cons t

      

(*--------------------------------------------------------------------------
  This function will generate interval constraints from SMT benchmark files
  --------------------------------------------------------------------------*)

(*re-formula expression for not*)
  let rec neg e = match e with
    |Eq (e1, e2) -> Neq (e1, e2)    
    |Neq (e1, e2) -> Eq (e1, e2)
    |Le (e1, e2) -> Geq (e1, e2)
    |Leq(e1, e2) -> Gr (e1, e2)
    |Gr (e1, e2) -> Leq (e1, e2)
    |Geq(e1, e2) -> Le (e1, e2)
    |Not(e1) -> e1
    |And (boolExp1, boolExp2) -> Or (neg boolExp1, neg boolExp2)
    |Or (boolExp1, boolExp2) -> And (neg boolExp1, neg boolExp2)
    |Multiple (boolExp1, boolExp2) -> Multiple (neg boolExp1, neg boolExp2)
    |BVar var -> BVar var

(*reduce expression for "not" and "/" *)
  let rec remove_div e = match e with
    | Div (Real a1, Real a2) -> Real (a1 /. a2)
    | Add (e1, e2) -> Add (remove_div e1, remove_div e2)
    | Sub (e1, e2) -> Sub (remove_div e1, remove_div e2)
    | Mul (e1, e2) -> Mul (remove_div e1, remove_div e2)
    | _ -> e 

  let rec reduce_expr e = match e with 
    | Eq (e1, e2) -> Eq (remove_div e1, remove_div e2)
    | Geq (e1, e2) -> Geq (remove_div e1, remove_div e2)
    | Gr (e1, e2) -> Gr (remove_div e1, remove_div e2)
    | Leq (e1, e2) -> Leq (remove_div e1, remove_div e2)
    | Le (e1, e2) -> Le (remove_div e1, remove_div e2)
    | Not (a) -> reduce_expr (neg (a))
    | And (e1, e2) -> And (reduce_expr e1, reduce_expr e2)
    | Or (e1, e2) -> Or (reduce_expr e1, reduce_expr e2)
    | _ -> e

 (*remove "not" from expression*)
 let rec remove_not e = match e with 
    | Not (a) -> remove_not (neg (a))
    | And (e1, e2) -> And (remove_not e1, remove_not e2)
    | Or (e1, e2) -> Or (remove_not e1, remove_not e2)
    | _ -> e

  let rec make_lstIntv eIntv upperBound = match eIntv with
    | Geq (Var x, Real a) -> [(x, a, upperBound)]
    | And (e1, e2) -> List.append (make_lstIntv e1 upperBound) (make_lstIntv e2 upperBound)
    | _ -> [] (*This case is never happen*)

  let rec update_var (x, lb, ub) l = match l with
    | [] -> (x, lb, ub)
    | (y, a1, a2) :: t-> 
        if (x=y) then
          (*update_var (x, (max lb a1), (min ub a2)) t*)
          update_var (x, (max lb a1), (min ub a2)) t
        else
          update_var (x, lb, ub) t

  let rec update_list l l1 l2 = match l1 with
    | [] -> l
    | h:: t -> 
        let lst = (update_var h l2)::l in
          update_list lst t l2
  
  let rec tolist_bc l = match l with
    | Nil -> []
    | BC (e) -> (match e with
        |Geq (Var x, Real c) -> [(x, c, infinity)]
        |Geq (Real c, Var x) -> [(x, neg_infinity, c)]
        |Gr (Var x, Real c) -> [(x, c, infinity)]
        |Gr (Real c, Var x) -> [(x, neg_infinity, c)]
        |Leq (Var x, Real c) -> [(x, neg_infinity, c)]
        |Leq (Real c, Var x) -> [(x, c, infinity)]
        |Le (Var x, Real c) -> [(x, neg_infinity, c)]
        |Le (Real c, Var x) -> [(x, c, infinity)]
        |_ -> [])
    |AND (e1, e2) -> List.append (tolist_bc e1) (tolist_bc e2) 

  let rec toString_lstIntv l = match l with
    | [] -> ""
    | (x, lb, ub) :: t -> 
        "(" ^ x ^ " in " ^ (string_of_float lb) ^ " "^ (string_of_float ub) ^ ")\n"
          ^ (toString_lstIntv t)

(*Simplify expressions: operations on constants*)  
let rec simplify_expr e = match e with
  |Var x -> Var x
  |Real c -> Real c
  |Add (Real a, Real b) -> Real (a+.b)
  |Sub (Real a, Real b) -> Real (a-.b)
  |Mul (Real a, Real b) -> Real (a*.b)
  |Mul (Real 1., Var x) -> Var x
  |Mul (Var x, Real 1.) -> Var x
  |Div (Var x, Real 1.) -> Var x
  |Div (Real a, Real b) -> Real (a/.b)
  |Div (e, Real c) -> Mul (e, Real (1./.c))
  |Add (e1, e2) -> Add (simplify_expr e1, simplify_expr e2)
  |Sub (e1, e2) -> Sub (simplify_expr e1, simplify_expr e2)
  |Mul (e1, e2) -> Mul (simplify_expr e1, simplify_expr e2)
  |Div (e1, e2) -> Div (simplify_expr e1, simplify_expr e2)
  | _ -> e

(*simplify bool expressions*)
let rec simplify_bool e = match e with
  | BVar b -> BVar b
  | Eq  (e1, e2) -> Eq  (simplify_expr e1, simplify_expr e2)
  | Neq  (e1, e2) -> Neq  (simplify_expr e1, simplify_expr e2)
  | Le  (e1, e2) -> Le  (simplify_expr e1, simplify_expr e2)
  | Leq (e1, e2) -> Leq (simplify_expr e1, simplify_expr e2)
  | Gr  (e1, e2) -> Gr  (simplify_expr e1, simplify_expr e2)
  | Geq (e1, e2) -> Geq (simplify_expr e1, simplify_expr e2)
  | And (b1, b2) -> And (simplify_bool b1, simplify_bool b2)
  | Or (b1, b2) -> Or (simplify_bool b1, simplify_bool b2)
  | Multiple (b1, b2) -> Multiple (simplify_bool b1, simplify_bool b2)
  | Not (e1) -> Not (simplify_bool e1)


(* This function returns the set of all variables of a polynomial expression *)
let rec get_varsSet_polyExpr polyExpr = match polyExpr with
  | Var v  -> VariablesSet.singleton v
  | Add(e1, e2) -> VariablesSet.union (get_varsSet_polyExpr e1) (get_varsSet_polyExpr e2)
  | Sub(e1, e2) -> VariablesSet.union (get_varsSet_polyExpr e1) (get_varsSet_polyExpr e2) 
  | Mul(e1, e2) -> VariablesSet.union (get_varsSet_polyExpr e1) (get_varsSet_polyExpr e2)
  | _ -> VariablesSet.empty


(* This function returns the set of all variables of a single boolean expression *)
let rec get_varsSet_boolExpr smtBoolExpr = match smtBoolExpr with
  | BVar b -> VariablesSet.singleton b
  | Eq  (e1, e2) -> VariablesSet.union (get_varsSet_polyExpr e1) (get_varsSet_polyExpr e2)
  | Neq  (e1, e2) -> VariablesSet.union (get_varsSet_polyExpr e1) (get_varsSet_polyExpr e2)
  | Le  (e1, e2) -> VariablesSet.union (get_varsSet_polyExpr e1) (get_varsSet_polyExpr e2)
  | Leq (e1, e2) -> VariablesSet.union (get_varsSet_polyExpr e1) (get_varsSet_polyExpr e2)
  | Gr  (e1, e2) -> VariablesSet.union (get_varsSet_polyExpr e1) (get_varsSet_polyExpr e2)
  | Geq (e1, e2) -> VariablesSet.union (get_varsSet_polyExpr e1) (get_varsSet_polyExpr e2)
  | And (b1, b2) -> VariablesSet.union (get_varsSet_boolExpr b1) (get_varsSet_boolExpr b2)
  | Or (b1, b2) -> VariablesSet.union (get_varsSet_boolExpr b1) (get_varsSet_boolExpr b2)
  | Multiple (b1, b2) -> VariablesSet.union (get_varsSet_boolExpr b1) (get_varsSet_boolExpr b2)
  | Not (e1) -> get_varsSet_boolExpr e1


let string_of_bounds bounds = match bounds with
  | [] -> ""
  | (var, lb, ub)::t -> var ^ " " ^ (string_of_float lb) ^ " " ^ (string_of_float ub) ^ " "


let genSmtForm sIntv sAssert ub =       
  (*get Assert expression from sAssert *)  
  let eAss = read sAssert in
  (*print_endline (ass_expr_to_infix_string eAss);
  flush stdout;*)
 
  let pass = lstSubVar eAss in
  let bass = lstBVar eAss in   

  let ori_expr = getAss_expr eAss in
  (*print_endline (bool_toPrefix ori_expr);
  flush stdout;*)

  (*Remove temporary variables and substitute them by based variables*)
  let sub_expr = subst_bool bass pass ori_expr in
  (*print_endline (bool_toPrefix sub_expr);
  flush stdout;*)

  (*simplify expression: operations on constants, i.e., constant +-*/ constant..., Div (a, b) = a/b*)
  let simp_expr = simplify_bool sub_expr in
  (*print_endline (bool_toPrefix simp_expr);
  flush stdout;*)

  (*remove not in expressions, "not" is allowed in quite restricted format, i.e., not (a > b)*)
  (*let red_expr = reduce_expr simp_expr in*)
  let red_expr = remove_not simp_expr in
  (*print_endline (bool_toPrefix red_expr);
  flush stdout;*)

  (*Placed constants to a left/right of equations, i.e., e1 > e2 <=> e1 - e2 > 0 *)
  let final_expr = bool_simp red_expr in
  (*print_endline (bool_toPrefix final_expr);
  flush stdout;*)

 (*simplify expression after substitute all temporary variables*)
  let expr = bool_reduce final_expr in
  (*print_endline (bool_toPrefix expr);
  flush stdout;*)

  (*remove redundant expression, i.e., remove a >= 0 when a >0 occurs*)
  let eList = bool_toList expr in
  let simp_list = red_cons (red_list eList) in
  
  let new_expr = list_toBool simp_list in

  (*Get bound constraints of variables from assert expression eAss*)
  let bound_cons = remov_nil (getBound new_expr) in

  (*generate interval constraints *)  
  let eIntv = get_bExpr (read sIntv) in
  let upperBound = float_of_string ub in
  let lstIntv = make_lstIntv eIntv upperBound in

  (*get a list of bound constraints*)
  let lst_bounds = tolist_bc bound_cons in

  (*update bounds for interval constraints from bound_cons*)  
  let new_lstIntv = update_list [] lstIntv lst_bounds in

  let strIntv = toString_lstIntv new_lstIntv in  

  let strAss = "(assert "^(bool_toPrefix 0 new_expr)^")" in
  (*print_endline strAss;
  flush stdout;*)
  (strIntv ^ strAss, 1)

  
end

(* Export this function to C/C++ *)
let _ = Callback.register "caml_genSmtForm" Caml.genSmtForm;;
