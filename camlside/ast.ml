open List 
open Util
open IA
open Assignments
open Variable

(*raSAT expression*)
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

(* compute derivative of a polynomial by a variable *)
let rec getDerivative smtPolynomial var =
	match  smtPolynomial with
	| Real c -> Real 0.
	| Var x -> if x = var then Real 1. else Real 0.
	| Add(e1, e2) -> Add(getDerivative e1 var, getDerivative e2 var)
	| Sub(e1, e2) -> Sub(getDerivative e1 var, getDerivative e2 var)
	| Mul(e1, e2) -> Add(Mul(getDerivative e1 var, e2), Mul(e1, getDerivative e2 var))
	| Pow(e, n)  -> Mul(getDerivative e var, Mul(Real (float n), Pow(e, n - 1)))


(* get the left expression of comparision operators*)
let leftExp = function
  | Eq (e1, e2)  -> e1
  | Leq (e1, e2) -> e1
  | Le (e1, e2)  -> e1
  | Geq (e1, e2) -> e1
  | Gr (e1, e2)  -> e1
  | _ -> Real 0.         (*This case never happen*)

(* get the right expression of comparision operators*)
let rightExp = function  
  | Eq (e1, e2)  -> e2
  | Leq (e1, e2) -> e2
  | Le (e1, e2)  -> e2
  | Geq (e1, e2) -> e2
  | Gr (e1, e2)  -> e2
  | _ -> Real 0.         (*This case never happen*)
  

(* This function return the left and right expression of a boolean expresion *)  
let get_left_right = function 
  | Eq (e1, e2)  -> (e1, e2)
  | Leq (e1, e2) -> (e1, e2)
  | Le (e1, e2)  -> (e1, e2)
  | Geq (e1, e2) -> (e1, e2)
  | Gr (e1, e2)  -> (e1, e2)
  | _ -> (Real 0., Real 0.)         (*This case never happen*)


(* evalFloat compute the value of a polynomial function from an assignment*)
let rec evalFloat ass = function
	| Real c -> c
	| Var v -> List.assoc v ass
	| Pow (u,1) -> evalFloat ass u 
	| Add (u,v) -> evalFloat ass u +. evalFloat ass v
	| Sub (u,v) -> evalFloat ass u -. evalFloat ass v
	| Mul (u,v) -> evalFloat ass u *. evalFloat ass v
	| Pow (u,c) -> evalFloat ass u *. evalFloat ass (Pow (u, c-1))

(* Check whether a boolean expression is SAT or not, provided the assignments of variables. *)
(* The values of sides also returned *)
let checkSAT_computeValues boolExp assignments = 
	let leftExpression = leftExp boolExp in
	let rightExpression = rightExp boolExp in
	let leftValue = evalFloat assignments leftExpression in
	let rightValue = evalFloat assignments rightExpression in 
	match boolExp with
	|Eq (e1, e2) -> 
		if (leftValue = rightValue) then (true, leftValue, rightValue)
		else (false, leftValue, rightValue)
	|Leq(e1, e2) -> 
		if (leftValue <= rightValue) then (true, leftValue, rightValue)
		else (false, leftValue, rightValue)
	|Le (e1, e2) -> 
		if (leftValue < rightValue) then (true, leftValue, rightValue)
		else (false, leftValue, rightValue)
	|Geq(e1, e2) -> 
		if (leftValue >= rightValue) then (true, leftValue, rightValue)
		else (false, leftValue, rightValue)
	|Gr (e1, e2) -> 
		if (leftValue > rightValue) then (true, leftValue, rightValue)
		else (false, leftValue, rightValue)
	| _ -> (true, leftValue, rightValue)     (*This case never happen*)



(*get the list of variables from a polynomial expression*)
let rec get_vars = function
	| Var x -> [x]
	| Add(e1, e2) -> List.append (get_vars e1) (get_vars e2)
	| Sub(e1, e2) -> List.append (get_vars e1) (get_vars e2)
	| Mul(e1, e2) -> List.append (get_vars e1) (get_vars e2)
	| Pow(e1, n)  -> get_vars e1
	| _ -> []


(*bool_vars gets the list of variables from a boolean constraint*)
let rec bool_vars e = 
	let left = leftExp e in
	let right = rightExp e in
	List.append (get_vars left) (get_vars right)
	

(*==================== START poly_expr_to_infix_string ==============================*)	
(* This function converts a polynomial expression into infix string form *)
let rec poly_expr_to_infix_string = function
  | Var x -> x
	| Add(e1, e2) -> (poly_expr_to_infix_string e1) ^ "+" ^ (poly_expr_to_infix_string e2)
	| Sub(e1, e2) -> (poly_expr_to_infix_string e1) ^ "-" ^ (poly_expr_to_infix_string e2)
	| Mul(e1, e2) -> (poly_expr_to_infix_string e1) ^ "*" ^ (poly_expr_to_infix_string e2)
	| Pow(e1, n)  -> (poly_expr_to_infix_string e1) ^ "^" ^ (string_of_int n)
	| Real r -> string_of_float r
(*==================== END poly_expr_to_infix_string ==============================*)	

(*==================== START poly_expr_list_to_infix_string ==============================*)	
(* This function converts a polynomial expression into infix string form *)
let rec poly_expr_list_to_infix_string = function
  | [] -> ""
  | [h] -> poly_expr_to_infix_string h
	| h::t -> (poly_expr_to_infix_string h) ^ ", " ^ (poly_expr_list_to_infix_string t)
(*==================== END poly_expr_list_to_infix_string ==============================*)	

	
(*==================== START bool_expr_to_infix_string ==============================*)	
(* This function converts a bool expression into the string of infix form *)
let rec bool_expr_to_infix_string boolExpr =
  match boolExpr with
  |Eq (e1, e2) -> 
		(poly_expr_to_infix_string e1) ^ " = " ^ (poly_expr_to_infix_string e2)
	|Leq(e1, e2) -> 
		(poly_expr_to_infix_string e1) ^ " <= " ^ (poly_expr_to_infix_string e2)
	|Le (e1, e2) -> 
    (poly_expr_to_infix_string e1) ^ " < " ^ (poly_expr_to_infix_string e2)
	|Geq(e1, e2) -> 
		(poly_expr_to_infix_string e1) ^ " >= " ^ (poly_expr_to_infix_string e2)
	|Gr (e1, e2) -> 
		(poly_expr_to_infix_string e1) ^ " > " ^ (poly_expr_to_infix_string e2)
	| _ -> ""     (*This case never happen*)
(*==================== END bool_expr_to_infix_string ==============================*)			
	
	
(*==================== START bool_expr_list_to_infix_string ==============================*)		
(* This function converts a list of bool expressions into the string of infix form *)
let rec bool_expr_list_to_infix_string boolExprs = 
  match boolExprs with
  | [] -> ""
  | h::t ->
    let t_infix_string = bool_expr_list_to_infix_string t in 
    let h_infix_string = bool_expr_to_infix_string h in 
    if (t_infix_string = "") then h_infix_string
    else h_infix_string ^ "\n" ^ t_infix_string
(*==================== END bool_expr_list_to_infix_string ==============================*)


(*==================== START is_equality ==============================*)		
(* This function checks if a list of boolean expressions are all equalities *)
let rec is_equation boolExpr = 
  match boolExpr with
  | Eq (e1, e2) -> true
  | _ -> false
(*==================== END is_equality ==============================*)
    

(*==================== START is_all_equalities ==============================*)		
(* This function checks if a list of boolean expressions are all equalities *)
let rec is_all_equations boolExprs = 
  match boolExprs with
  | [] -> true
  | h::t -> (is_equation h) && (is_all_equations t)
(*==================== END is_all_equalities ==============================*)


(*==================== START first_uk_cl ==============================*)		
(* This function tries to find a first inequality expression *)
let rec first_inequation boolExprs = 
  match boolExprs with
  | [] -> []
  | h::t -> (
    match h with 
    | Eq(e1, e2) -> first_inequation t
    | _ -> [h]
  )
(*==================== END first_uk_cl ==============================*)


(* e_toAf1 compute the AF1 assignment from interval constraints  *)
let rec e_toAf1 ass id numVar = match ass with
  |[] -> []
  |a::b -> match a with
  |(x, ix) -> (x, Util.toAf1 ix id numVar):: e_toAf1 b (id+1) numVar


(* e_toAf2 compute the AF2 assignment from interval constraints*)
let rec e_toAf2 ass id numVar = match ass with
  |[] -> []
  |a::b -> match a with
  |(x, ix) -> (x, Util.toAf2 ix id numVar):: e_toAf2 b (id+1) numVar

(* e_toCai1 compute the CAI1 assignment from interval constraints*)
let rec e_toCai1 ass id numVar = match ass with
  |[] -> []
  |a::b -> match a with
  |(x, ix) -> (x, Util.toCai1 ix id numVar):: e_toCai1 b (id+1) numVar

(* e_toCai2 compute the CAI2 assignment from interval constraints*)
let rec e_toCai2 ass id numVar = match ass with
  |[] -> []
  |a::b -> match a with
  |(x, ix) -> (x, Util.toCai2 ix id numVar):: e_toCai2 b (id+1) numVar

(* e_toCai3 compute the CAI3 assignment from interval constraints*)
let rec e_toCai3 ass id numVar = match ass with
  |[] -> []
  |a::b -> match a with
  |(x, ix) -> (x, Util.toCai3 ix id numVar):: e_toCai3 b (id+1) numVar
  
(* evalCI compute the bound of a polynomial function from an assignment ass by CI form*)
let rec evalCI ass = function
  | Real c -> new IA.interval c c
  | Var v -> List.assoc v ass
  | Add (u,v) -> IA.CI.(evalCI ass u + evalCI ass v)
  | Sub (u,v) -> IA.CI.(evalCI ass u - evalCI ass v)
  | Mul (u,v) -> IA.CI.(evalCI ass u * evalCI ass v)
  | Pow (u,c) -> IA.CI.(evalCI ass u ^ c)

(* evalICI compute the bound of a polynomial function from an assignment ass by ICI form*)
let rec evalICI ass = function
  | Real c -> new IA.inf_interval IA.(Float c) IA.(Float c)
  | Var v -> List.assoc v ass
  | Add (u,v) -> IA.ICI.(evalICI ass u + evalICI ass v)
  | Sub (u,v) -> IA.ICI.(evalICI ass u - evalICI ass v)
  | Mul (u,v) -> IA.ICI.(evalICI ass u * evalICI ass v)
  | Pow (u,c) -> IA.ICI.(evalICI ass u ^ c)

(* evalAf1 compute the bound of a polynomial function from an assignment ass by AF1 form*)
let rec evalAf1 ass n = function
  | Real c -> Util.toAf1 (new IA.interval c c) 0 n
  | Var v -> List.assoc v ass
  | Add (u,v) -> IA.AF1.(evalAf1 ass n u + evalAf1 ass n v)
  | Sub (u,v) -> IA.AF1.(evalAf1 ass n u - evalAf1 ass n v)
  | Mul (u,v) -> IA.AF1.(evalAf1 ass n u * evalAf1 ass n v)
  | Pow (u,c) -> IA.AF1.(evalAf1 ass n u ^ c)
 
(* evalAf2 compute the bound of a polynomial function from an assignment ass by AF2 form*)
let rec evalAf2 ass n= function
  | Real c -> Util.toAf2 (new IA.interval c c) 0 n
  | Var v -> List.assoc v ass
  | Add (u,v) -> IA.AF2.(evalAf2 ass n u + evalAf2 ass n v)
  | Sub (u,v) -> IA.AF2.(evalAf2 ass n u - evalAf2 ass n v)
  | Mul (u,v) -> IA.AF2.(evalAf2 ass n u * evalAf2 ass n v)
  | Pow (u,c) -> IA.AF2.(evalAf2 ass n u ^ c)

(* evalCai1 compute the bound of a polynomial function from an assignment ass by CAI1 form*)
let rec evalCai1 ass n= function
  | Real c -> Util.toCai1 (new IA.interval c c) 0 n
  | Var v -> List.assoc v ass
  | Add (u,v) -> IA.CAI1.(evalCai1 ass n u + evalCai1 ass n v)
  | Sub (u,v) -> IA.CAI1.(evalCai1 ass n u - evalCai1 ass n v)
  | Mul (u,v) -> IA.CAI1.(evalCai1 ass n u * evalCai1 ass n v)
  | Pow (u,c) -> IA.CAI1.(evalCai1 ass n u ^ c)

(* evalCai2 compute the bound of a polynomial function from an assignment ass by CAI2 form*)
let rec evalCai2 ass n= function
  | Real c -> Util.toCai2 (new IA.interval c c) 0 n
  | Var v -> List.assoc v ass
  | Add (u,v) -> IA.CAI2.(evalCai2 ass n u + evalCai2 ass n v)
  | Sub (u,v) -> IA.CAI2.(evalCai2 ass n u - evalCai2 ass n v)
  | Mul (u,v) -> IA.CAI2.(evalCai2 ass n u * evalCai2 ass n v)
  | Pow (u,c) -> IA.CAI2.(evalCai2 ass n u ^ c)

(* evalCai3 compute the bound of a polynomial function from an assignment ass by CAI3 form*)
let rec evalCai3 ass n= function
  | Real c -> Util.toCai3 (new IA.interval c c) 0 n
  | Var v -> List.assoc v ass
  | Add (u,v) -> IA.CAI3.(evalCai3 ass n u + evalCai3 ass n v)
  | Sub (u,v) -> IA.CAI3.(evalCai3 ass n u - evalCai3 ass n v)
  | Mul (u,v) -> IA.CAI3.(evalCai3 ass n u * evalCai3 ass n v)
  | Pow (u,c) -> IA.CAI3.(evalCai3 ass n u ^ c)
  

(*evaluate the bound of poly expression by type of interval arithmetic*)						     
let poly_eval e ia assIntv = 
  let iIntvVar = List.length assIntv in
  if (ia=1) then (
    let assAf1 = e_toAf1 assIntv 1 iIntvVar in
    let res = evalAf1 assAf1 iIntvVar e in
    res#evaluate;
  )  
  else if (ia=2) then (
    let assAf2 = e_toAf2 assIntv 1 iIntvVar in
    let res = evalAf2 assAf2 iIntvVar e in
    res#evaluate;
  )  
  else if (ia=3) then (
    let assCai1 = e_toCai1 assIntv 1 iIntvVar in
    let res = evalCai1 assCai1 iIntvVar e in
    res#evaluate;
  )  
  else if (ia=4) then (
    let assCai2 = e_toCai2 assIntv 1 iIntvVar in
    let res = evalCai2 assCai2 iIntvVar e in
    res#evaluate;
  )  
  else if (ia=5) then (
    let assCai3 = e_toCai3 assIntv 1 iIntvVar in
    let res = evalCai3 assCai3 iIntvVar e in
    res#evaluate;
  )  
  else (
    let res = evalCI assIntv e in
    res;
  )  
    

(* Check whether an expression e is satisfiable or not provided the over-approximation of sides *)
let check_sat_providedBounds boolExp leftBound rightBound = 
  match boolExp with
  |Eq (e1, e2) -> 
    if (leftBound#l = leftBound#h && leftBound#h = rightBound#l  && rightBound#l = rightBound#h) then 1
    else if (leftBound#h < rightBound#l || leftBound#l > rightBound#h) then -1
    else 0
  |Leq(e1, e2) -> 
    if (leftBound#h <= rightBound#l) then 1
    else if (leftBound#l > rightBound#h) then -1
    else 0
  |Le (e1, e2) -> 
    if (leftBound#h < rightBound#l) then 1
    else if (leftBound#l >= rightBound#h) then -1
    else 0
  |Geq(e1, e2) -> 
    if (leftBound#l >= rightBound#h) then 1
    else if (leftBound#h < rightBound#l) then -1
    else 0
  |Gr (e1, e2) -> 
    if (leftBound#l > rightBound#h) then 1
    else if (leftBound#h <= rightBound#l) then -1
    else 0
  | _ -> 1     (*This case never happen*)
    

(*check whether an expression e is satisfiable*)
let checkSat e ia assIntv =
  let left = leftExp e in
  let right = rightExp e in
  let leftBound  = poly_eval left ia assIntv in
  let rightBound = poly_eval right ia assIntv in
  (*print_endline ("Approximating: " ^ bool_expr_to_infix_string e ^ " = [" ^ string_of_float leftBound#l ^ ", " ^ string_of_float leftBound#h ^ "] with " ^ intervals_to_string assIntv);
  flush stdout;*)
  check_sat_providedBounds e leftBound rightBound
  
    
(* Check sat with combination of AF2 and CI *)      
let check_sat_af_two_ci boolExp intv = 
  let left = leftExp boolExp in (* get the left expression *)
  let right = rightExp boolExp in (* get the right expression *)
  (* evaluate the expression using AF2 *)
  let afTwoLeftBound  = poly_eval left 2 intv in
  let afTwoRightBound = poly_eval right 2 intv in
  let sat = check_sat_providedBounds boolExp afTwoLeftBound afTwoRightBound in
  if sat = 0 then (* AF2 fails to conclude the expression *)
    
    (* Compute bouds of polynomial using *)
    let ciLeftBound  = poly_eval left 0 intv in
    let ciRightBound = poly_eval right 0 intv in
    
    let newLeftLowerBound = max afTwoLeftBound#l ciLeftBound#l in
    let newLeftHigherBound = min afTwoLeftBound#h ciLeftBound#h in
    let newLeftBound = new IA.interval newLeftLowerBound newLeftHigherBound in
    let newRightLowerBound = max afTwoRightBound#l ciRightBound#l in
    let newRightHigherBound = min afTwoRightBound#h ciRightBound#h in
    let newRightBound = new IA.interval newRightLowerBound newRightHigherBound in
    check_sat_providedBounds boolExp newLeftBound newRightBound
  else sat
  
	
(* This function returns a set of all variables of a polynomial expression *)
let rec get_vars_set_polyExp = function
  | Var x -> VariablesSet.singleton x
	| Add(e1, e2) -> VariablesSet.union (get_vars_set_polyExp e1) (get_vars_set_polyExp e2)
	| Sub(e1, e2) -> VariablesSet.union (get_vars_set_polyExp e1) (get_vars_set_polyExp e2)
	| Mul(e1, e2) -> VariablesSet.union (get_vars_set_polyExp e1) (get_vars_set_polyExp e2)
	| Pow(e1, n)  -> get_vars_set_polyExp e1
	| _ -> VariablesSet.empty
	

(* This function returns a set of 
variables of a boolean expression. The sort critia is
using alphabet ordering *)	
let get_vars_set_boolExp boolExp = 
  let (left, right) = get_left_right boolExp in
  let leftVarsSet = get_vars_set_polyExp left in
  let rightVarsSet = get_vars_set_polyExp right in
  VariablesSet.union leftVarsSet rightVarsSet  

(* This function add information of variables set and 
number of variables into boolean expression *)
let rec add_info boolExps = match boolExps with
  | [] -> []
  | h::t ->
    let variablesSet = get_vars_set_boolExp h in
    let variablesNum = VariablesSet.cardinal variablesSet in
    (h, variablesSet, variablesNum) :: (add_info t)

(* This function sort the list of the apis using variables dependency *)
let sort_boolExp_dependency boolExps = 
  let expressiveBoolExps = add_info boolExps in
  let sortedEBoolExps = List.fast_sort Util.compare_dependency expressiveBoolExps in
  Util.extract_boolExps sortedEBoolExps
