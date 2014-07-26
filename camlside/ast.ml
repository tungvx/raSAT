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


(* evalFloat compute the value of a polynomial function from an assignment*)
let rec evalFloat varsTCMap = function
	| Real c -> c
	| Var v -> StringMap.find v varsTCMap
	| Pow (u,1) -> evalFloat varsTCMap u 
	| Add (u,v) -> evalFloat varsTCMap u +. evalFloat varsTCMap v
	| Sub (u,v) -> evalFloat varsTCMap u -. evalFloat varsTCMap v
	| Mul (u,v) -> evalFloat varsTCMap u *. evalFloat varsTCMap v
	| Pow (u,c) -> evalFloat varsTCMap u *. evalFloat varsTCMap (Pow (u, c-1))

(* Check whether a boolean expression is SAT or not, provided the assignments of variables. *)
(* The values of sides also returned *)
let checkSAT_computeValues boolExp varsTCsMap = 
	let expression = get_exp boolExp in
	let value = evalFloat varsTCsMap expression in
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



(*get the list of variables from a polynomial expression*)
let rec get_vars = function
	| Var x -> [x]
	| Add(e1, e2) -> List.append (get_vars e1) (get_vars e2)
	| Sub(e1, e2) -> List.append (get_vars e1) (get_vars e2)
	| Mul(e1, e2) -> List.append (get_vars e1) (get_vars e2)
	| Pow(e1, n)  -> get_vars e1
	| _ -> []
	

(*==================== START string_infix_of_polyExpr ==============================*)	
(* This function converts a polynomial expression into infix string form *)
let rec string_infix_of_polyExpr = function
  | Var x -> x
	| Add(e1, e2) -> (string_infix_of_polyExpr e1) ^ "+" ^ (string_infix_of_polyExpr e2)
	| Sub(e1, e2) -> (string_infix_of_polyExpr e1) ^ "-" ^ (string_infix_of_polyExpr e2)
	| Mul(e1, e2) -> (string_infix_of_polyExpr e1) ^ "*" ^ (string_infix_of_polyExpr e2)
	| Pow(e1, n)  -> (string_infix_of_polyExpr e1) ^ "^" ^ (string_of_int n)
	| Real r -> string_of_float r
(*==================== END string_infix_of_polyExpr ==============================*)	

(*==================== START string_infix_of_polyExprs ==============================*)	
(* This function converts a polynomial expression into infix string form *)
let rec string_infix_of_polyExprs = function
  | [] -> ""
  | [h] -> string_infix_of_polyExpr h
	| h::t -> (string_infix_of_polyExpr h) ^ ", " ^ (string_infix_of_polyExprs t)
(*==================== END string_infix_of_polyExprs ==============================*)	

	
(*==================== START bool_expr_to_infix_string ==============================*)	
(* This function converts a bool expression into the string of infix form *)
let rec string_infix_of_boolExp boolExpr =
  match boolExpr with
  |Eq e -> 
		(string_infix_of_polyExpr e) ^ " = 0"
	|Leq e -> 
		(string_infix_of_polyExpr e) ^ " <= 0"
	|Le e -> 
    (string_infix_of_polyExpr e) ^ " < 0"
	|Geq e -> 
		(string_infix_of_polyExpr e) ^ " >= 0"
	|Gr e -> 
		(string_infix_of_polyExpr e) ^ " > 0"
(*==================== END bool_expr_to_infix_string ==============================*)			


(*==================== START string_prefix_of_polyExp ==============================*)
(*polynomial functions to prefix representation*)
let rec string_prefix_of_polyExp = function
  | Real c -> string_of_float c
  | Var x -> x
  | Add (e1, e2) -> "(+ " ^ (string_prefix_of_polyExp e1) ^ " " ^ (string_prefix_of_polyExp e2) ^ ")"
  | Sub (e1, e2) -> "(- " ^ (string_prefix_of_polyExp e1) ^ " " ^ (string_prefix_of_polyExp e2) ^ ")"
  | Mul (e1, e2) -> "(* " ^ (string_prefix_of_polyExp e1) ^ " " ^ (string_prefix_of_polyExp e2) ^ ")"
  | Pow (e1, n)  -> "(^ " ^ (string_prefix_of_polyExp e1) ^ " " ^ (string_of_int n)  ^ ")"
(*==================== END string_prefix_of_polyExp ==============================*)


(*==================== START string_prefix_of_boolExpr ==============================*)
(* prefix string format of a boolean expression *)
let rec string_prefix_of_boolExpr  = function
  | Eq e -> "(= " ^ (string_prefix_of_polyExp e)^" 0)"
  | Le e -> "(< " ^ (string_prefix_of_polyExp e)^" 0)"
  | Leq e -> "(<= "^ (string_prefix_of_polyExp e)^" 0)"
  | Gr e -> "(> " ^ (string_prefix_of_polyExp e)^" 0)"
  | Geq e -> "(>= "^ (string_prefix_of_polyExp e)^" 0)"
(*==================== END string_prefix_of_boolExpr ==============================*)  


(*==================== START string_postfix_of_polyExpr ==============================*)
(* postfix string format of a polynomial expression *)
let rec string_postfix_of_polyExpr = function
  | Real c -> " real " ^ string_of_float c ^ " "
  | Var x -> " var " ^ x ^ " "
  | Add (e1, e2) -> (string_postfix_of_polyExpr e1) ^ (string_postfix_of_polyExpr e2) ^ "+ " 
  | Sub (e1, e2) -> (string_postfix_of_polyExpr e1) ^ (string_postfix_of_polyExpr e2) ^ "- "
  | Mul (e1, e2) -> (string_postfix_of_polyExpr e1) ^ (string_postfix_of_polyExpr e2) ^ "* "
  | Pow (e1, n)  -> (string_postfix_of_polyExpr e1) ^ (string_of_int n) ^ "^ "
(*==================== END string_postfix_of_polyExpr ==============================*)


(*==================== START postfix_string_of_boolExpr ==============================*)
(* postfix string format of a boolean expression *)      
let rec string_postfix_of_boolExpr  = function
  | Eq e -> (string_postfix_of_polyExpr e) ^ "real 0 = "
  | Le e -> (string_postfix_of_polyExpr e) ^ "real 0 < " 
  | Leq e -> (string_postfix_of_polyExpr e) ^ "real 0 <= "
  | Gr e -> (string_postfix_of_polyExpr e) ^ "real 0 > " 
  | Geq e -> (string_postfix_of_polyExpr e) ^ "real 0 >= " 
(*==================== END postfix_string_of_boolExpr ==============================*)


(*==================== START is_equality ==============================*)		
(* This function checks if a list of boolean expressions are all equalities *)
let rec is_boolExpr_equation boolExpr = 
  match boolExpr with
  | Eq e -> true
  | _ -> false
(*==================== END is_equality ==============================*)


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
let get_vars_set_boolExpr boolExp = 
  let polyExp = get_exp boolExp in
  get_vars_set_polyExp polyExp


let rec get_vars_set_boolExprs boolExps = match boolExps with
  | [] -> VariablesSet.empty
  | h::t -> VariablesSet.union (get_vars_set_boolExpr h) (get_vars_set_boolExprs t) 


let rec get_interval var (intvMap, intvList) =
  let (intv,(miniSATCode:int)) = StringMap.find var intvMap in
  (intvMap, (var, intv)::intvList)


(*evaluate the bound of poly expression by type of interval arithmetic*)						     
let poly_eval e ia intv = 
  let varsSet = get_vars_set_polyExp e in
  let (_, assIntv) = VariablesSet.fold get_interval varsSet (intv, []) in
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
let check_sat_af_two_ci_boolExpr boolExpr intv = 
  let polyExpr = get_exp boolExpr in
  (* evaluate the expression using AF2 *)
  let (afTwoBound, afTwoVarsSen)  = poly_eval polyExpr 2 intv in
  (*print_endline (assignments_toString afTwoVarsSen);
  flush stdout;*)
  let sat = check_sat_providedBounds boolExpr afTwoBound in
  if sat = 0 then (* AF2 fails to conclude the expression *)
    (* Compute bouds of polynomial using *)
    let (ciBound, _)  = poly_eval polyExpr 0 intv in
    
    let newLowerBound = max afTwoBound#l ciBound#l in
    let newHigherBound = min afTwoBound#h ciBound#h in
    let newBound = new IA.interval newLowerBound newHigherBound in
    let sat = check_sat_providedBounds boolExpr newBound in
    (sat, afTwoVarsSen)
  else (sat, afTwoVarsSen)
    

(* This function check sat with CI only *)
let check_sat_inf_ci_boolExpr boolExpr intv = 
  let polyExpr = get_exp boolExpr in (* get the expression *)
  let (iciBound, _)  = poly_eval polyExpr (-1) intv in (* -1 is ICI *)
  (*print_endline ("Bound: " ^ (string_of_float iciLeftBound#l) ^ " " ^ (string_of_float iciLeftBound#h));
  flush stdout;*)
  let sat = check_sat_providedBounds boolExpr iciBound in
  (sat, ([]: (Variable.StringMap.key * float) list))
