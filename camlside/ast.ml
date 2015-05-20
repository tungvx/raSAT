open List 
open Util
open IA
open Assignments
open Variable
open Interval

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
  | Div of poly_expr * poly_expr
  | Real of float
  | Var of string

type poly_constraint = 
  | Eq of poly_expr
  | Neq of poly_expr
  | Geq of poly_expr
  | Leq of poly_expr
  | Gr of poly_expr
  | Le of poly_expr


(* get varsSet of a polynomial constraint*)
let rec get_varsSet_polyCons = function 
  | Eq polyExpr -> get_varsSet_polyExpr polyExpr
  | Neq polyExpr -> get_varsSet_polyExpr polyExpr
  | Geq polyExpr -> get_varsSet_polyExpr polyExpr
  | Leq polyExpr -> get_varsSet_polyExpr polyExpr
  | Gr polyExpr -> get_varsSet_polyExpr polyExpr
  | Le polyExpr -> get_varsSet_polyExpr polyExpr
and get_varsSet_polyExpr = function
  | Add (polyExpr1, polyExpr2) -> VariablesSet.union (get_varsSet_polyExpr polyExpr1) (get_varsSet_polyExpr polyExpr2)
  | Sub (polyExpr1, polyExpr2) -> VariablesSet.union (get_varsSet_polyExpr polyExpr1) (get_varsSet_polyExpr polyExpr2)
  | Mul (polyExpr1, polyExpr2) -> VariablesSet.union (get_varsSet_polyExpr polyExpr1) (get_varsSet_polyExpr polyExpr2)
  | Div (polyExpr1, polyExpr2) -> VariablesSet.union (get_varsSet_polyExpr polyExpr1) (get_varsSet_polyExpr polyExpr2)
  | Real f -> VariablesSet.empty
  | Var var -> VariablesSet.singleton var

and not_of_polyConstraint = function 
  | Eq polyExpr -> Neq polyExpr
  | Neq polyExpr -> Eq polyExpr
  | Geq polyExpr -> Le polyExpr
  | Leq polyExpr -> Gr polyExpr
  | Gr polyExpr -> Leq polyExpr
  | Le polyExpr -> Geq polyExpr

let rec get_clauses cnfMiniSATExpr isFinal = match cnfMiniSATExpr with
  | Lit i -> 
    (*print_endline (string_of_int i);
    flush stdout;*)
    if isFinal then
      1  
    else 
      0
  | MAnd (e1, e2) -> 
    let clauses1 = get_clauses e1 true in
    let clauses2 = get_clauses e2 true in
    clauses1 + clauses2
  | MOr (e1, e2) -> 
    let clauses1 = get_clauses e1 false in
    let clauses2 = get_clauses e2 isFinal in
    clauses1 + clauses2

(* This function convert the cnf miniSAT expression into string under the format of miniSAT input *)
let rec write_cnf_toFile cnfMiniSATExpr isFinal oc = match cnfMiniSATExpr with
  | Lit i -> 
    (*print_endline (string_of_int i);
    flush stdout;*)
    if isFinal then
      Printf.fprintf oc "%d 0\n" i  
    else 
      Printf.fprintf oc "%d " i
  | MAnd (e1, e2) -> 
    write_cnf_toFile e1 true oc;
    write_cnf_toFile e2 true oc
  | MOr (e1, e2) -> 
    write_cnf_toFile e1 false oc;
    write_cnf_toFile e2 isFinal oc


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
(*let rec getDerivative smtPolynomial var =
	match  smtPolynomial with
	| Real c -> Real 0.
	| Var x -> if x = var then Real 1. else Real 0.
	| Add(e1, e2) -> Add(getDerivative e1 var, getDerivative e2 var)
	| Sub(e1, e2) -> Sub(getDerivative e1 var, getDerivative e2 var)
	| Mul(e1, e2) -> Add(Mul(getDerivative e1 var, e2), Mul(e1, getDerivative e2 var))
*)

(* get the left expression of comparision operators*)
let get_exp = function
  | Eq e  -> e
  | Neq e  -> e
  | Leq e -> e
  | Le e  -> e
  | Geq e -> e
  | Gr e  -> e


(* evalFloat compute the value of a polynomial function from an assignment*)
let rec evalFloat varsTCMap = function
	| Real c -> c
	| Var v -> StringMap.find v varsTCMap
	| Add (u,v) -> evalFloat varsTCMap u +. evalFloat varsTCMap v
	| Sub (u,v) -> evalFloat varsTCMap u -. evalFloat varsTCMap v
	| Mul (u,v) -> evalFloat varsTCMap u *. evalFloat varsTCMap v
	| Div (u, v) -> evalFloat varsTCMap u /. evalFloat varsTCMap v

(* Check whether a boolean expression is SAT or not, provided the assignments of variables. *)
(* The values of sides also returned *)
let checkSAT_computeValues boolExp varsTCsMap = 
	let expression = get_exp boolExp in
	let value = evalFloat varsTCsMap expression in
	match boolExp with
	|Eq e -> 
		if value = 0. then (true, value)
		else (false, value)
	|Neq e -> 
		if value <> 0. then (true, value)
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
	

(*==================== START string_infix_of_polyExpr ==============================*)	
(* This function converts a polynomial expression into infix string form *)
let rec string_infix_of_polyExpr = function
  | Var x -> x
	| Add(e1, e2) -> "(" ^ (string_infix_of_polyExpr e1) ^ ") + (" ^ (string_infix_of_polyExpr e2) ^ ")" 
	| Sub(e1, e2) -> "(" ^ (string_infix_of_polyExpr e1) ^ ") - (" ^ (string_infix_of_polyExpr e2) ^ ")"
	| Mul(e1, e2) -> "(" ^ (string_infix_of_polyExpr e1) ^ ") * (" ^ (string_infix_of_polyExpr e2) ^ ")"
	| Div(e1, e2) -> "(" ^ (string_infix_of_polyExpr e1) ^ ") / (" ^ (string_infix_of_polyExpr e2) ^ ")"
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
	|Neq e -> 
		(string_infix_of_polyExpr e) ^ " != 0"
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
  | Div (e1, e2) -> "(/ " ^ (string_prefix_of_polyExp e1) ^ " " ^ (string_prefix_of_polyExp e2) ^ ")"
(*==================== END string_prefix_of_polyExp ==============================*)


(*==================== START string_prefix_of_boolExpr ==============================*)
(* prefix string format of a boolean expression *)
let rec string_prefix_of_boolExpr  = function
  | Eq e -> "(= " ^ (string_prefix_of_polyExp e)^" 0)"
  | Neq e -> "(!= " ^ (string_prefix_of_polyExp e)^" 0)"
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
  | Div (e1, e2) -> (string_postfix_of_polyExpr e1) ^ (string_postfix_of_polyExpr e2) ^ "/ "
(*==================== END string_postfix_of_polyExpr ==============================*)


(*==================== START postfix_string_of_boolExpr ==============================*)
(* postfix string format of a boolean expression *)      
let rec string_postfix_of_boolExpr  = function
  | Eq e -> (string_postfix_of_polyExpr e) ^ "real 0 = "
  | Neq e -> (string_postfix_of_polyExpr e) ^ "real 0 != "
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
(*let rec evalCI varsIntvsMap = function
  | Real c -> new IA.interval c c
  | Var var -> StringMap.find var varsIntvsMap
  | Add (u, v) -> IA.CI.(evalCI varsIntvsMap u + evalCI varsIntvsMap v)
  | Sub (u, v) -> IA.CI.(evalCI varsIntvsMap u - evalCI varsIntvsMap v)
  | Mul (u, v) -> IA.CI.(evalCI varsIntvsMap u * evalCI varsIntvsMap v)
*)  
let rec evalCI_extra varsIntvsMap = function
  | Real c -> {low = c; high = c}
  | Var var -> 
    let intv = StringMap.find var varsIntvsMap in
    {low = intv#l; high = intv#h}
  | Add (u, v) -> evalCI_extra varsIntvsMap u +$ evalCI_extra varsIntvsMap v
  | Sub (u, v) -> evalCI_extra varsIntvsMap u -$ evalCI_extra varsIntvsMap v
  | Mul (u, v) -> evalCI_extra varsIntvsMap u *$ evalCI_extra varsIntvsMap v
  | Div (u, v) -> evalCI_extra varsIntvsMap u /$ evalCI_extra varsIntvsMap v
  
let rec evalCI varsIntvsMap polyExpr =
  let computedIntv = evalCI_extra varsIntvsMap polyExpr in
  new IA.interval (computedIntv.low) (computedIntv.high)
  

(* evalICI compute the bound of a polynomial function from an assignment ass by ICI form*)
let rec evalICI varsIntvsMap = function
  | Real c -> let newC = IA.bound_of_float c in new IA.inf_interval newC newC
  | Var var -> 
    let intv = StringMap.find var varsIntvsMap in
    let lowerBound = IA.bound_of_float intv#l in
    let upperBound = IA.bound_of_float intv#h in
    new IA.inf_interval lowerBound upperBound
  | Add (u, v) -> IA.ICI.(evalICI varsIntvsMap u + evalICI varsIntvsMap v)
  | Sub (u, v) -> IA.ICI.(evalICI varsIntvsMap u - evalICI varsIntvsMap v)
  | Mul (u, v) -> IA.ICI.(evalICI varsIntvsMap u * evalICI varsIntvsMap v)

(* evalAf1 compute the bound of a polynomial function from an assignment ass by AF1 form*)
let rec evalAf1 ass n = function
  | Real c -> Util.toAf1 (new IA.interval c c) 0 n
  | Var v -> List.assoc v ass
  | Add (u, v) -> IA.AF1.(evalAf1 ass n u + evalAf1 ass n v)
  | Sub (u, v) -> IA.AF1.(evalAf1 ass n u - evalAf1 ass n v)
  | Mul (u, v) -> IA.AF1.(evalAf1 ass n u * evalAf1 ass n v)
 
(* evalAf2 compute the bound of a polynomial function from an assignment ass by AF2 form*)
let rec evalAf2 varsAf2sMap varsNum = function
  | Real c -> Util.toAf2 (new IA.interval c c) 0 varsNum
  | Var var -> StringMap.find var varsAf2sMap
  | Add (u, v) -> IA.AF2.(evalAf2 varsAf2sMap varsNum u + evalAf2 varsAf2sMap varsNum v)
  | Sub (u, v) -> IA.AF2.(evalAf2 varsAf2sMap varsNum u - evalAf2 varsAf2sMap varsNum v)
  | Mul (u, v) -> IA.AF2.(evalAf2 varsAf2sMap varsNum u * evalAf2 varsAf2sMap varsNum v)
  | Div (u, Real f) -> IA.AF2.(evalAf2 varsAf2sMap varsNum u / f)
  | _ -> raise (Failure "Unsupported operation of af2")

(* evalCai1 compute the bound of a polynomial function from an assignment ass by CAI1 form*)
let rec evalCai1 ass n= function
  | Real c -> Util.toCai1 (new IA.interval c c) 0 n
  | Var v -> List.assoc v ass
  | Add (u, v) -> IA.CAI1.(evalCai1 ass n u + evalCai1 ass n v)
  | Sub (u, v) -> IA.CAI1.(evalCai1 ass n u - evalCai1 ass n v)
  | Mul (u, v) -> IA.CAI1.(evalCai1 ass n u * evalCai1 ass n v)

(* evalCai2 compute the bound of a polynomial function from an assignment ass by CAI2 form*)
let rec evalCai2 ass n= function
  | Real c -> Util.toCai2 (new IA.interval c c) 0 n
  | Var v -> List.assoc v ass
  | Add (u, v) -> IA.CAI2.(evalCai2 ass n u + evalCai2 ass n v)
  | Sub (u, v) -> IA.CAI2.(evalCai2 ass n u - evalCai2 ass n v)
  | Mul (u, v) -> IA.CAI2.(evalCai2 ass n u * evalCai2 ass n v)

(* evalCai3 compute the bound of a polynomial function from an assignment ass by CAI3 form*)
let rec evalCai3 ass n= function
  | Real c -> Util.toCai3 (new IA.interval c c) 0 n
  | Var v -> List.assoc v ass
  | Add (u, v) -> IA.CAI3.(evalCai3 ass n u + evalCai3 ass n v)
  | Sub (u, v) -> IA.CAI3.(evalCai3 ass n u - evalCai3 ass n v)
  | Mul (u, v) -> IA.CAI3.(evalCai3 ass n u * evalCai3 ass n v)
  

(* This function returns a set of all variables of a polynomial expression *)
let rec get_vars_set_polyExp = function
  | Var x -> VariablesSet.singleton x
	| Add(e1, e2) -> VariablesSet.union (get_vars_set_polyExp e1) (get_vars_set_polyExp e2)
	| Sub(e1, e2) -> VariablesSet.union (get_vars_set_polyExp e1) (get_vars_set_polyExp e2)
	| Mul(e1, e2) -> VariablesSet.union (get_vars_set_polyExp e1) (get_vars_set_polyExp e2)
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


(* Evaluate a polynomial using AF2 *)
let poly_eval_af2 polyExpr varsSet varsNum varsIntvsMap =
  let add_varAf2 var (varsAf2sMap, nextIndex) =
    let intv = StringMap.find var varsIntvsMap in
    let af2 = Util.toAf2 intv nextIndex varsNum in
    (StringMap.add var af2 varsAf2sMap, nextIndex + 1)
  in 
  let (varsAf2Map, _) = VariablesSet.fold add_varAf2 varsSet (StringMap.empty, 1) in
  try 
    let res = evalAf2 varsAf2Map varsNum polyExpr in
    res#evaluate
  with Failure f -> new IA.interval neg_infinity infinity


(* Evaluate the polynomial using AF2, varsSensitivity is also returned *)
let poly_eval_af2_varsSens polyExpr varsSet varsNum varsIntvsMap =
  let add_varAf2Index var (varsAf2sMap, varsIndicesList, nextIndex) =
    let intv = StringMap.find var varsIntvsMap in
    let af2 = Util.toAf2 intv nextIndex varsNum in
    (*print_string ("Af2 of " ^ var ^ " in " ^ intv#to_string ^ "is ");
    af2#print_form;
    print_string "\n";
    flush stdout;*)
    (StringMap.add var af2 varsAf2sMap, (var, nextIndex-1)::varsIndicesList, nextIndex + 1)
  in
  let (varsAf2Map, varsIndicesList, _) = VariablesSet.fold add_varAf2Index varsSet (StringMap.empty, [], 1) in
  (*let print_var_index (var, index) = 
    print_endline (var ^ ": " ^ string_of_int index ^ " ");
    flush stdout;
  in
  List.iter print_var_index varsIndicesList;*)
  try 
    let res = evalAf2 varsAf2Map varsNum polyExpr in
    (*res#print_form;*)
    let varsSensitivity = res#extract_sortedVarsSens varsIndicesList in
    (res#evaluate, varsSensitivity)
  with Failure f -> 
    let gen_emptyVarsSen var currentSens = (var, 0., false) :: currentSens in
    let varsSensitivity = VariablesSet.fold gen_emptyVarsSen varsSet [] in
    (new IA.interval neg_infinity infinity, varsSensitivity)

(* Evaluate a polynomial using CI *)
let poly_eval_ci polyExpr varsIntvsMap = 
  evalCI varsIntvsMap polyExpr
  
(* Evaluate a polynomial using ICI *)
let poly_eval_ici polyExpr varsIntvsMap = 
  let res = evalICI varsIntvsMap polyExpr in
  res#to_interval

(*evaluate the bound of poly expression by type of interval arithmetic*)						     
let poly_eval e varsSet ia varsIntvsMap = 
  let rec get_interval var intvList =
    let intv = StringMap.find var varsIntvsMap in
    (var, intv)::intvList
  in
  let assIntv = VariablesSet.fold get_interval varsSet [] in
  let iIntvVar = List.length assIntv in
  if (ia=1) then (
    let assAf1 = e_toAf1 assIntv 1 iIntvVar in
    let res = evalAf1 assAf1 iIntvVar e in
    (res#evaluate, []);
  )  
  else if (ia=2) then (
    let estimatedIntv = poly_eval_af2 e varsSet iIntvVar varsIntvsMap in
    (estimatedIntv, ([]:float list));
    (*let assAf2VarPos = e_toAf2 assIntv 1 iIntvVar in
    let (assAf2, varPos) = List.split assAf2VarPos in
    let res = evalAf2 assAf2 iIntvVar e in
    let varsSensitivity = res#extract_varsSen varPos in
    (res#evaluate, varsSensitivity);*)
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
    let res = poly_eval_ici e varsIntvsMap in
    (res, []);
  )
  else (
    let res = poly_eval_ci e varsIntvsMap in
    (res, []);
  )  


(* Check whether an expression e is satisfiable or not provided the over-approximation of sides *)
let check_sat_providedBounds boolExp bound = 
  (*print_endline ("Bounds: " ^ string_of_float bound#l ^ " " ^ string_of_float bound#h);
  flush stdout;*)
  match boolExp with
  |Eq e -> 
    if (bound#l = bound#h && bound#h = 0.) then 1
    else if (bound#h < 0. || bound#l > 0.) then -1
    else 0
  |Neq e -> 
    if (bound#l = bound#h && bound#h = 0.) then -1
    else if (bound#h < 0. || bound#l > 0.) then 1
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


(* Check whether an expression e is satisfiable or not provided the over-approximation of sides, length of SAT area also be returned *)
let check_sat_get_satLength_providedBounds boolExp bound = 
  let lowerBound = bound#l in
  let upperBound = bound#h in
  (*print_endline ("Bounds: " ^ string_of_float lowerBound ^ " " ^ string_of_float upperBound);
  flush stdout;*)
  match boolExp with
  |Eq e -> 
    if (lowerBound = upperBound && upperBound = 0.) then (1, 0.) 
    else if (upperBound < 0. || lowerBound > 0.) then (-1, 0.)
    else (0, 0.)
  |Neq e -> 
    if (lowerBound = upperBound && upperBound = 0.) then (-1, 0.) 
    else if (upperBound < 0. || lowerBound > 0.) then (1, infinity)
    else (0, upperBound -. lowerBound)
  |Leq e -> 
    if (upperBound <= 0.) then (1, upperBound -. lowerBound)
    else if (lowerBound > 0.) then (-1, 0.)
    else (0, 0. -. lowerBound)
  |Le e -> 
    if (upperBound < 0.) then (1, upperBound -. lowerBound)
    else if (lowerBound >= 0.) then (-1, 0.)
    else (0, 0. -. lowerBound)
  |Geq e -> 
    if (lowerBound >= 0.) then (1, upperBound -. lowerBound)
    else if (upperBound < 0.) then (-1, 0.)
    else (0, upperBound)
  |Gr e -> 
    if (lowerBound > 0.) then (1, upperBound -. lowerBound)
    else if (upperBound <= 0.) then (-1, 0.)
    else (0, upperBound)


(* check sat of boolean expression using ICI *)
let check_sat_getBound_getSATLength_ici_boolExpr boolExpr varsIntvsMap =
  let polyExpr = get_exp boolExpr in
  let iciBound  =  poly_eval_ici polyExpr varsIntvsMap in
  let (sat, satLength) = check_sat_get_satLength_providedBounds boolExpr iciBound in
  (sat, iciBound, satLength)

let check_sat_ici_get_satLength_boolExpr boolExpr varsIntvsMap =
  let polyExpr = get_exp boolExpr in
  let iciBound  =  poly_eval_ici polyExpr varsIntvsMap in
  check_sat_get_satLength_providedBounds boolExpr iciBound

(* Check sat with combination of AF2 and CI *)
let check_sat_af_two_ci_boolExpr boolExpr varsSet varsNum varsIntvsMap = 
  let polyExpr = get_exp boolExpr in
  (* evaluate the expression using AF2 *)
  let afTwoBound  = poly_eval_af2 polyExpr varsSet varsNum varsIntvsMap in
  (*print_endline (assignments_toString afTwoVarsSen);
  flush stdout;*)
  let sat = check_sat_providedBounds boolExpr afTwoBound in
  if sat = 0 then (* AF2 fails to conclude the expression *)
    (* Compute bouds of polynomial using *)
    let ciBound  = poly_eval_ci polyExpr varsIntvsMap in
    
    let newLowerBound = max afTwoBound#l ciBound#l in
    let newHigherBound = min afTwoBound#h ciBound#h in
    let newBound = new IA.interval newLowerBound newHigherBound in
    let sat = check_sat_providedBounds boolExpr newBound in
    sat
  else sat
  
    
(* Check sat with combination of AF2 and CI, return the sorted variables by their sensitivities *)      
let check_sat_getBound_af_two_ci_boolExpr_varsSens boolExpr varsSet varsNum varsIntvsMap = 
  let polyExpr = get_exp boolExpr in
  (* evaluate the expression using AF2 *)
  (*print_endline ("Start checking using af2\n");
  flush stdout;*)
  let (afTwoBound, varsSens)  = poly_eval_af2_varsSens polyExpr varsSet varsNum varsIntvsMap in
  (*let print_var_sen (var, sen, isPositiveSen) = 
    print_endline (var ^ ": " ^ string_of_float sen ^ " ");
    flush stdout;
  in
  List.iter print_var_sen varsSens;
  flush stdout;*)
  let (sat, satLength) = check_sat_get_satLength_providedBounds boolExpr afTwoBound in
  if sat = 0 then (* AF2 fails to conclude the expression *)
    (* Compute bouds of polynomial using *)
    let ciBound  = poly_eval_ci polyExpr varsIntvsMap in
    
    let newLowerBound = max afTwoBound#l ciBound#l in
    let newHigherBound = min afTwoBound#h ciBound#h in
    let newBound = new IA.interval newLowerBound newHigherBound in
    let (sat, satLength) = check_sat_get_satLength_providedBounds boolExpr newBound in
    (sat, satLength, varsSens, newBound)
  else (sat, satLength, varsSens, afTwoBound)
  
let check_sat_af_two_ci_get_satLength_boolExpr boolExpr varsSet varsNum varsIntvsMap =
  let polyExpr = get_exp boolExpr in
  (* evaluate the expression using AF2 *)
  let afTwoBound  = poly_eval_af2 polyExpr varsSet varsNum varsIntvsMap in
  (*print_endline (assignments_toString afTwoVarsSen);
  flush stdout;*)
  let (sat, satLength) = check_sat_get_satLength_providedBounds boolExpr afTwoBound in
  if sat = 0 then (* AF2 fails to conclude the expression *)
    (* Compute bouds of polynomial using *)
    let ciBound  = poly_eval_ci polyExpr varsIntvsMap in
    
    let newLowerBound = max afTwoBound#l ciBound#l in
    let newHigherBound = min afTwoBound#h ciBound#h in
    let newBound = new IA.interval newLowerBound newHigherBound in
    check_sat_get_satLength_providedBounds boolExpr newBound
  else (sat, satLength)

let check_constracted oldBound newBound = 
  oldBound#l < newBound#l || oldBound#h > newBound#h

let get_intersection bound1 bound2 =
  let newLowerBound = max bound1#l bound2#l in
  let newHigherBound = min bound1#h bound2#h in
  let newBound = new IA.interval newLowerBound newHigherBound in
  newBound

let get_bound polyExpr varsSet varsIntvsMap =
  let varsNum = VariablesSet.cardinal varsSet in
  let afTwoBound  = poly_eval_af2 polyExpr varsSet varsNum varsIntvsMap in
  let ciBound  = poly_eval_ci polyExpr varsIntvsMap in
  get_intersection  afTwoBound ciBound

(*
let rec backward_propagate_boolExpr var intv varsIntvsMap polyExpr polyExprInterval result =
  if polyExprInterval#h < polyExprInterval#l then (result, new IA.interval 1. (-1.))
  else
  match polyExpr with
  | Var x -> 
    if x = var then 
      if polyExprInterval#l > intv#l || polyExprInterval#h < intv#h then
        (true, get_intersection polyExprInterval intv)
      else (result, intv)
    else (result, intv)
	| Add((e1, varsSet1), (e2, varsSet2)) -> 
	  let (tmpResult1, tmpIntv1) =
	    if VariablesSet.mem var varsSet1 then
	      let bound1 = get_bound e1 varsSet1 varsIntvsMap in
	      let bound2 = get_bound e2 varsSet2 varsIntvsMap in
	      let newBound1 = IA.CI.(polyExprInterval - bound2) in
	      if check_constracted bound1 newBound1 then 
	        backward_propagate_boolExpr var intv varsIntvsMap e1 (get_intersection bound1 newBound1) result
	      else 
	        (result, intv)
	    else (result, intv)
	  in
	  let varsIntvsMap =
	    if tmpResult1 then StringMap.add var tmpIntv1 varsIntvsMap
	    else varsIntvsMap
	  in
	  let (tmpResult2, tmpIntv2) =
	    if VariablesSet.mem var varsSet2 then
	      let bound1 = get_bound e1 varsSet1 varsIntvsMap in
	      let bound2 = get_bound e2 varsSet2 varsIntvsMap in
	      let newBound2 = IA.CI.(polyExprInterval - bound1) in 
	      if check_constracted bound2 newBound2 then
	        backward_propagate_boolExpr var tmpIntv1 varsIntvsMap e1 (get_intersection bound2 newBound2) tmpResult1
	      else (tmpResult1, tmpIntv1)
	    else (tmpResult1, tmpIntv1)
	  in (tmpResult2, tmpIntv2)
	| Sub((e1, varsSet1), (e2, varsSet2)) -> 
	  let (tmpResult1, tmpIntv1) =
	    if VariablesSet.mem var varsSet1 then
	      let bound1 = get_bound e1 varsSet1 varsIntvsMap in
	      let bound2 = get_bound e2 varsSet2 varsIntvsMap in
	      let newBound1 = IA.CI.(polyExprInterval + bound2) in
	      if check_constracted bound1 newBound1 then 
	        backward_propagate_boolExpr var intv varsIntvsMap e1 (get_intersection bound1 newBound1) result
	      else 
	        (result, intv)
	    else (result, intv)
	  in
	  let varsIntvsMap =
	    if tmpResult1 then StringMap.add var tmpIntv1 varsIntvsMap
	    else varsIntvsMap
	  in
	  let (tmpResult2, tmpIntv2) =
	    if VariablesSet.mem var varsSet2 then
	      let bound1 = get_bound e1 varsSet1 varsIntvsMap in
	      let bound2 = get_bound e2 varsSet2 varsIntvsMap in
	      let newBound2 = IA.CI.(bound1 - polyExprInterval) in 
	      if check_constracted bound2 newBound2 then
	        backward_propagate_boolExpr var tmpIntv1 varsIntvsMap e1 (get_intersection bound2 newBound2) tmpResult1
	      else (tmpResult1, tmpIntv1)
	    else (tmpResult1, tmpIntv1)
	  in (tmpResult2, tmpIntv2)
	| Mul((e1, varsSet1), (e2, varsSet2)) ->
	  let (tmpResult1, tmpIntv1) =
	    if VariablesSet.mem var varsSet1 then
	      let bound1 = get_bound e1 varsSet1 varsIntvsMap in
	      let bound2 = get_bound e2 varsSet2 varsIntvsMap in
	      let newBound1 =
	        if bound2#l < 0. && bound2#h > 0. then
	          new IA.interval neg_infinity infinity (* later we need special processing here *)
	        else
	          IA.CI.(polyExprInterval * (new IA.interval (1. /. bound2#h) (1. /. bound2#l)))
	      in
	      if check_constracted bound1 newBound1 then 
	        backward_propagate_boolExpr var intv varsIntvsMap e1 (get_intersection bound1 newBound1) result
	      else 
	        (result, intv)
	    else (result, intv)
	  in
	  let varsIntvsMap =
	    if tmpResult1 then StringMap.add var tmpIntv1 varsIntvsMap
	    else varsIntvsMap
	  in
	  let (tmpResult2, tmpIntv2) =
	    if VariablesSet.mem var varsSet2 then
	      let bound1 = get_bound e1 varsSet1 varsIntvsMap in
	      let bound2 = get_bound e2 varsSet2 varsIntvsMap in
	      let newBound2 = 
	        if bound1#l < 0. && bound1#h > 0. then
	          new IA.interval neg_infinity infinity (* later we need special processing here *)
	        else
	          IA.CI.(polyExprInterval * (new IA.interval (1. /. bound1#h) (1. /. bound1#l)))
	      in 
	      if check_constracted bound2 newBound2 then
	        backward_propagate_boolExpr var tmpIntv1 varsIntvsMap e1 (get_intersection bound2 newBound2) tmpResult1
	      else (tmpResult1, tmpIntv1)
	    else (tmpResult1, tmpIntv1)
	  in (tmpResult2, tmpIntv2)
	| Real r -> (result, intv)
*)	
