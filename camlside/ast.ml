open List 
open Util
open IA
open Assignments
open Variable

(* miniSAT expressions *)
type miniSAT_expr =
  | Lit of int
  | MAnd of miniSAT_expr * miniSAT_expr
  | MOr of miniSAT_expr * miniSAT_expr

(*raSAT expression*)
type poly_expr = 
  | Add of poly_expr * poly_expr 
  | Sub of poly_expr * poly_expr
  | Mul of poly_expr * poly_expr
  | Pow of poly_expr * int
  | Real of float
  | Var of string

type bool_expr = 
  | Eq of poly_expr
  | Geq of poly_expr
  | Leq of poly_expr
  | Gr of poly_expr
  | Le of poly_expr
  | And of bool_expr * bool_expr
  | BOr of bool_expr * bool_expr

type intv_clause =                         (*interval constraint for each variable*)
  | In of string * float * float
  | Or of intv_clause * intv_clause                

type intv_expr = 
  | Cl of intv_clause
  | Ic of intv_expr * intv_expr            (*Ic: Interval constraints *)

type formula =
  | Ass of bool_expr
  | Intv of intv_expr  
  

(* This function convert the cnf miniSAT expression into string under the format of miniSAT input *)
let rec string_of_cnf_miniSATExpr cnfMiniSATExpr isFinal = match cnfMiniSATExpr with
  | Lit i -> 
    let (ending, clauses) = 
      if isFinal then (" 0\n", 1)
      else (" ", 0)
    in
    (string_of_int i ^ ending, clauses)
  | MAnd (e1, e2) -> 
    let (string1, clauses1) = string_of_cnf_miniSATExpr e1 true in
    let (string2, clauses2) = string_of_cnf_miniSATExpr e2 true in
    (string1 ^ string2, clauses1 + clauses2)
  | MOr (e1, e2) -> 
    let (string1, clauses1) = string_of_cnf_miniSATExpr e1 false in
    let (string2, clauses2) = string_of_cnf_miniSATExpr e2 true in
    (string1 ^ string2, clauses1 + clauses2)


let rec distributeLeft m1 m2 = match m2 with
  | MAnd(e,f) -> MAnd(distributeLeft m1 e,distributeLeft m1 f)
  | _ -> MOr(m1, m2)
 
let rec distributeOr m1 m2 = match m1 with 
  | MAnd(e,f) -> MAnd(distributeOr e m2 ,distributeOr f m2)
  | _ -> distributeLeft m1 m2

(* convert a miniSAT expression into conjunctive normal form *)  
let rec cnf_of_miniSATExpr miniSATExpr = match miniSATExpr with 
  | MAnd (m1, m2) -> MAnd (cnf_of_miniSATExpr m1, cnf_of_miniSATExpr m2)
  | MOr (m1, m2) -> 
    let cnfM1 = cnf_of_miniSATExpr m1 in
    let cnfM2 = cnf_of_miniSATExpr m2 in
    distributeOr cnfM1 cnfM2    
  | _ -> miniSATExpr
  

(* encode the boolexpression into the form of miniSAT lit *)  
let rec miniSATExpr_of_boolExpr boolExpr index = match boolExpr with
  | And (b1, b2) -> 
    let (mB1, index1) = miniSATExpr_of_boolExpr b1 index in
    let (mB2, index2) = miniSATExpr_of_boolExpr b2 index1 in
    (MAnd (mB1, mB2), index2)
  | BOr (b1, b2) -> 
    let (mB1, index1) = miniSATExpr_of_boolExpr b1 index in
    let (mB2, index2) = miniSATExpr_of_boolExpr b2 index1 in
    (MOr (mB1, mB2), index2)
  | _ -> (Lit index, index + 1)
  

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
let get_exp = function
  | Eq e  -> e
  | Leq e -> e
  | Le e  -> e
  | Geq e -> e
  | Gr e  -> e
  | _ -> Real 0.         (*This case never happen*)


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
	let expression = get_exp boolExp in
	let value = evalFloat assignments expression in
	match boolExp with
	|Eq e -> 
		if value = 0. then (true, value)
		else (false, value)
	|Leq e -> 
		if value <= 0. then (true, value)
		else (false, value)
	|Le e -> 
		if value < 0. then (true, value)
		else (false, value)
	|Geq e -> 
		if value >= 0. then (true, value)
		else (false, value)
	|Gr e -> 
		if value > 0. then (true, value)
		else (false, value)
	| _ -> (true, value)     (*This case never happen*)



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
	let exp = get_exp e in
	get_vars exp
	

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
  |Eq e -> 
		(poly_expr_to_infix_string e) ^ " = 0"
	|Leq e -> 
		(poly_expr_to_infix_string e) ^ " <= 0"
	|Le e -> 
    (poly_expr_to_infix_string e) ^ " < 0"
	|Geq e -> 
		(poly_expr_to_infix_string e) ^ " >= 0"
	|Gr e -> 
		(poly_expr_to_infix_string e) ^ " > 0"
	|And(b1, b2)  -> 
	  bool_expr_to_infix_string b1 ^ " And\n" ^ bool_expr_to_infix_string b2
	|BOr(b1, b2) -> 
	  bool_expr_to_infix_string b1 ^ " Or\n" ^ bool_expr_to_infix_string b2
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
  | Eq e -> true
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
    | Eq e -> first_inequation t
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
    |(x, ix) -> ((x, Util.toAf2 ix id numVar), (x, id-1)):: e_toAf2 b (id+1) numVar

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


(* This function convert intervals of float into intervals of inf *)
let rec infIntervals_of_intervals intervals = match intervals with
  | [] -> []
  | (x, interval)::t -> 
    let lowerBound = IA.bound_of_float interval#l in
    let upperBound = IA.bound_of_float interval#h in
    (x, new IA.inf_interval lowerBound upperBound) :: (infIntervals_of_intervals t)

  
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
  | Real c -> let newC = IA.bound_of_float c in new IA.inf_interval newC newC
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
let rec evalAf2 ass n = function
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
  let polyExp = get_exp boolExp in
  get_vars_set_polyExp polyExp
  

(* Check if a variables is in the set *)
let check_mem varsSet (var, intv) = 
  VariablesSet.mem var varsSet


(*evaluate the bound of poly expression by type of interval arithmetic*)						     
let poly_eval e ia assIntv = 
  let varsSet = get_vars_set_polyExp e in
  let assIntv = List.filter (check_mem varsSet) assIntv in
  let iIntvVar = List.length assIntv in
  if (ia=1) then (
    let assAf1 = e_toAf1 assIntv 1 iIntvVar in
    let res = evalAf1 assAf1 iIntvVar e in
    (res#evaluate, []);
  )  
  else if (ia=2) then (
    let assAf2VarPos = e_toAf2 assIntv 1 iIntvVar in
    let (assAf2, varPos) = List.split assAf2VarPos in
    let res = evalAf2 assAf2 iIntvVar e in
    let varsSensitivity = res#extract_varsSen varPos in
    (res#evaluate, varsSensitivity);
  )  
  else if (ia=3) then (
    let assCai1 = e_toCai1 assIntv 1 iIntvVar in
    let res = evalCai1 assCai1 iIntvVar e in
    (res#evaluate, []);
  )  
  else if (ia=4) then (
    let assCai2 = e_toCai2 assIntv 1 iIntvVar in
    let res = evalCai2 assCai2 iIntvVar e in
    (res#evaluate, []);
  )  
  else if (ia=5) then (
    let assCai3 = e_toCai3 assIntv 1 iIntvVar in
    let res = evalCai3 assCai3 iIntvVar e in
    (res#evaluate, []);
  )
  else if (ia = -1) then (
    let infiniteIntervals = infIntervals_of_intervals assIntv in
    let res = evalICI infiniteIntervals e in
    (res#to_interval, []);
  )
  else (
    let res = evalCI assIntv e in
    (res, []);
  )  
    

(* Check whether an expression e is satisfiable or not provided the over-approximation of sides *)
let check_sat_providedBounds boolExp bound = 
  match boolExp with
  |Eq e -> 
    if (bound#l = bound#h && bound#h = 0.) then 1
    else if (bound#h < 0. || bound#l > 0.) then -1
    else 0
  |Leq e -> 
    if (bound#h <= 0.) then 1
    else if (bound#l > 0.) then -1
    else 0
  |Le e -> 
    if (bound#h < 0.) then 1
    else if (bound#l >= 0.) then -1
    else 0
  |Geq e -> 
    if (bound#l >= 0.) then 1
    else if (bound#h < 0.) then -1
    else 0
  |Gr e -> 
    if (bound#l > 0.) then 1
    else if (bound#h <= 0.) then -1
    else 0
  | _ -> 1     (*This case never happen*)
    

(*check whether an expression e is satisfiable*)
let checkSat e ia assIntv =
  let polyExp = get_exp e in
  let (bound, _)  = poly_eval polyExp ia assIntv in
  (*print_endline ("Approximating: " ^ bool_expr_to_infix_string e ^ " = [" ^ string_of_float leftBound#l ^ ", " ^ string_of_float leftBound#h ^ "] with " ^ intervals_to_string assIntv);

  flush stdout;*)
  check_sat_providedBounds e bound
  
  
(* Substract variables sensitivity from the right hand side to the left handside of the expression *)  
let rec substract_varSen leftVarsSen (var, varSen) = match leftVarsSen with
  | [] -> [(var, ~-. varSen)]
  | (currentVar, currentVarSen)::remaining -> 
    if var = currentVar then (currentVar, currentVarSen -. varSen)::remaining
    else (currentVar, currentVarSen)::(substract_varSen leftVarsSen (var, varSen))
  
(* This function merge the sensitivity of vars in left handside with right handside of an expression*)  
let rec merge_varsSen leftVarsSen = function 
  | [] -> leftVarsSen
  | h::t -> 
    let newLeftVarsSen = substract_varSen leftVarsSen h in
    merge_varsSen newLeftVarsSen t
  
    
(* Check sat with combination of AF2 and CI *)      
let check_sat_af_two_ci boolExp intv = 
  let polyExp = get_exp boolExp in
  (* evaluate the expression using AF2 *)
  let (afTwoBound, afTwoVarsSen)  = poly_eval polyExp 2 intv in
  (*print_endline (assignments_toString afTwoVarsSen);
  flush stdout;*)
  let sat = check_sat_providedBounds boolExp afTwoBound in
  if sat = 0 then (* AF2 fails to conclude the expression *)
    (* Compute bouds of polynomial using *)
    let (ciBound, _)  = poly_eval polyExp 0 intv in
    
    let newLowerBound = max afTwoBound#l ciBound#l in
    let newHigherBound = min afTwoBound#h ciBound#h in
    let newBound = new IA.interval newLowerBound newHigherBound in
    let sat = check_sat_providedBounds boolExp newBound in
    (sat, afTwoVarsSen)
  else (sat, afTwoVarsSen)
    

(* This function check sat with CI only *)
let check_sat_inf_ci boolExp intv = 
  let polyExp = get_exp boolExp in (* get the expression *)
  let (iciBound, _)  = poly_eval polyExp (-1) intv in (* -1 is ICI *)
  (*print_endline ("Bound: " ^ (string_of_float iciLeftBound#l) ^ " " ^ (string_of_float iciLeftBound#h));
  flush stdout;*)
  let sat = check_sat_providedBounds boolExp iciBound in
  (sat, [])
    
(* This function compare two variables by their sensitivity *)
let compare_sensitivity (firstVar, firstSen) (secondVar, secondSen) =
  compare firstSen secondSen    
    
    
(* This function change the value of sensitivities into positive values *)
let rec change_toPosSen varsSen = match varsSen with
  | [] -> []
  | (var, sen) :: t -> 
    let posSen = abs_float sen in
    (var, posSen)::(change_toPosSen t)    
    

(* This function add information of variables set and 
number of variables into boolean expression *)
let rec add_info boolExps = match boolExps with
  | [] -> []
  | (boolExp, varsSen)::t ->
    (* Change all the sensitivity values into positive ones *)
    let posVarsSen = change_toPosSen varsSen in
    (* sort the sensitivities of variables *)
    let sortedVarsSen = List.fast_sort compare_sensitivity posVarsSen in
    (*print_endline (assignments_toString sortedVarsSen);*)
    let variablesSet = get_vars_set_boolExp boolExp in
    let variablesNum = VariablesSet.cardinal variablesSet in
    (boolExp, sortedVarsSen, variablesSet, variablesNum) :: (add_info t)

(* This function sort the list of the apis using variables dependency *)
let sort_boolExp_dependency boolExps = 
  let expressiveBoolExps = add_info boolExps in
  let sortedEBoolExps = List.fast_sort Util.compare_dependency expressiveBoolExps in
  Util.extract_boolExps sortedEBoolExps
