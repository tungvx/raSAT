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
  | Add of poly_expr * poly_expr * Interval.interval * IA.af2
  | Sub of poly_expr * poly_expr * Interval.interval * IA.af2
  | Mul of poly_expr * poly_expr * Interval.interval * IA.af2
  | Div of poly_expr * poly_expr * Interval.interval * IA.af2
  | Real of float * bool * Interval.interval * IA.af2
  | Var of string * Interval.interval * IA.af2 * bool
  | Pow of string * int * Interval.interval * bool * Interval.interval  * IA.af2

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
  | Add (polyExpr1, polyExpr2, _, _) -> VariablesSet.union (get_varsSet_polyExpr polyExpr1) (get_varsSet_polyExpr polyExpr2)
  | Sub (polyExpr1, polyExpr2, _, _) -> VariablesSet.union (get_varsSet_polyExpr polyExpr1) (get_varsSet_polyExpr polyExpr2)
  | Mul (polyExpr1, polyExpr2, _, _) -> VariablesSet.union (get_varsSet_polyExpr polyExpr1) (get_varsSet_polyExpr polyExpr2)
  | Div (polyExpr1, polyExpr2, _, _) -> VariablesSet.union (get_varsSet_polyExpr polyExpr1) (get_varsSet_polyExpr polyExpr2)
  | Real (f, _, _, _) -> VariablesSet.empty
  | Var (var, _, _, _) -> VariablesSet.singleton var
  | Pow(var, _, _, _, _, _) -> VariablesSet.singleton var

let not_of_polyConstraint = function 
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
	| Real (c, _, _, _) -> Real 0.
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
	| Real (c, _, _, _) -> c
	| Var (v, _, _, _) -> StringMap.find v varsTCMap
	| Add (u,v, _, _) -> evalFloat varsTCMap u +. evalFloat varsTCMap v
	| Sub (u,v, _, _) -> evalFloat varsTCMap u -. evalFloat varsTCMap v
	| Mul (u,v, _, _) -> evalFloat varsTCMap u *. evalFloat varsTCMap v
	| Div (u, v, _, _) -> evalFloat varsTCMap u /. evalFloat varsTCMap v
  | Pow (var, multiplicity, _, _, _, _) -> StringMap.find var varsTCMap ** (float_of_int multiplicity)

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
  | Var (x, _, _, changed) -> x (* "(" ^ x ^ ", " ^ string_of_bool changed ^ ")" *)
	| Add(e1, e2, _, _) -> "(" ^ (string_infix_of_polyExpr e1) ^ ") + (" ^ (string_infix_of_polyExpr e2) ^ ")" 
	| Sub(e1, e2, _, _) -> "(" ^ (string_infix_of_polyExpr e1) ^ ") - (" ^ (string_infix_of_polyExpr e2) ^ ")"
	| Mul(e1, e2, _, _) -> "(" ^ (string_infix_of_polyExpr e1) ^ ") * (" ^ (string_infix_of_polyExpr e2) ^ ")"
	| Div(e1, e2, _, _) -> "(" ^ (string_infix_of_polyExpr e1) ^ ") / (" ^ (string_infix_of_polyExpr e2) ^ ")"
	| Real (c, _, _, _) -> string_of_float c
  | Pow (var, multiplicity, _, _, _, _) -> var ^ " ^ " ^ string_of_int multiplicity
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
  | Real (c, _, _, _) -> string_of_float c
  | Var (x, _, _, _) -> x
  | Add (e1, e2, _, _) -> "(+ " ^ (string_prefix_of_polyExp e1) ^ " " ^ (string_prefix_of_polyExp e2) ^ ")"
  | Sub (e1, e2, _, _) -> "(- " ^ (string_prefix_of_polyExp e1) ^ " " ^ (string_prefix_of_polyExp e2) ^ ")"
  | Mul (e1, e2, _, _) -> "(* " ^ (string_prefix_of_polyExp e1) ^ " " ^ (string_prefix_of_polyExp e2) ^ ")"
  | Div (e1, e2, _, _) -> "(/ " ^ (string_prefix_of_polyExp e1) ^ " " ^ (string_prefix_of_polyExp e2) ^ ")"
  | Pow (var, multiplicity, _, _, _, _) -> "( " ^ "** " ^ var ^ " " ^ string_of_int multiplicity ^ ")"
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
  | Real (c, _, _, _) -> " real " ^ string_of_float c ^ " "
  | Var (x, _, _, _) -> " var " ^ x ^ " "
  | Add (e1, e2, _, _) -> (string_postfix_of_polyExpr e1) ^ (string_postfix_of_polyExpr e2) ^ "+ " 
  | Sub (e1, e2, _, _) -> (string_postfix_of_polyExpr e1) ^ (string_postfix_of_polyExpr e2) ^ "- "
  | Mul (e1, e2, _, _) -> (string_postfix_of_polyExpr e1) ^ (string_postfix_of_polyExpr e2) ^ "* "
  | Div (e1, e2, _, _) -> (string_postfix_of_polyExpr e1) ^ (string_postfix_of_polyExpr e2) ^ "/ "
  | Pow (var, multiplicity, _, _, _, _) -> var ^ " " ^ string_of_int multiplicity ^ " ** "
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
  | Real (c, _, _, _) -> new IA.interval c c
  | Var (var, _) -> StringMap.find Var (var, _)sIntvsMap
  | Add (u, v, _) -> IA.CI.(evalCI varsIntvsMap u + evalCI varsIntvsMap v)
  | Sub (u, v, _) -> IA.CI.(evalCI varsIntvsMap u - evalCI varsIntvsMap v)
  | Mul (u, v, _) -> IA.CI.(evalCI varsIntvsMap u * evalCI varsIntvsMap v)
*) 

let rec compare_intv intv1 intv2 = 
  intv1.low = intv2.low && intv1.high = intv2.high

let rec check_infinity intv = intv.low = neg_infinity || intv.high = infinity  

let rec evalCI_extra varsIntvsMap = function
  | Real (c, initialized, oldIntv, af2Form) -> 
    (* print_string "\nReal: ";
    print_float c;
    print_I oldIntv;
    flush stdout; *)
    (false, (Real (c, initialized, oldIntv, af2Form)), oldIntv, VariablesSet.empty)
  | Var (var, oldIntv, af2Form, _) -> 
    let intv = StringMap.find var varsIntvsMap in
    (* print_string (var ^ ", oldIntv ");
    print_I oldIntv;
    print_string (", newIntv ");
    print_I intv;
    print_endline "";
    flush stdout; *)
    if compare_intv oldIntv intv then
      (* let testString = " no changed " in
      print_endline testString;
      flush stdout; *)
      (false, Var (var, oldIntv, af2Form, false), oldIntv, VariablesSet.empty)
    else (
      (* let testString = " changed " in
      print_endline testString;
      flush stdout; *)
      if check_infinity intv then 
        (true, Var (var, intv, af2Form, false), intv, VariablesSet.empty)
      else 
        (true, Var (var, intv, af2Form, true), intv, VariablesSet.singleton var)
    )
  | Add (u, v, oldIntv, af2Form) -> 
    (* let testString = "Checking " ^ string_infix_of_polyExpr (Add (u, v, oldIntv, af2Form)) in 
    print_endline testString;
    flush stdout; *)
    let (uChanged, u', uIntv, changedVarsU) = evalCI_extra varsIntvsMap u in 
    let (vChanged, v', vIntv, changedVarsV) = evalCI_extra varsIntvsMap v in
    (* let testString = "After Checking " ^ string_infix_of_polyExpr (Add (u', v', oldIntv, af2Form)) in 
    print_endline testString;
    flush stdout; *)
    let changedVars = VariablesSet.union changedVarsU changedVarsV in
    (* print_string ("\nInterval of " ^ string_infix_of_polyExpr (Add (u, v, oldIntv, af2Form)) ^ "is "); *)
    if uChanged || vChanged then 
      let intv = uIntv +$ vIntv in
      let newIntv = (* inter_I_I oldIntv *) intv in
      (* print_I newIntv;
      flush stdout; *)
      (* print_endline ("Estimating " ^ string_infix_of_polyExpr (Add(u, v, newIntv)) ^ " as " ^ sprintf_I "%f" newIntv);
      flush stdout; *)
      if (compare_intv newIntv oldIntv) then 
        (false, Add(u', v', newIntv, af2Form), newIntv, changedVars)
      else
        (true, Add(u', v', newIntv, af2Form), newIntv, changedVars)
    else
      (false, Add(u', v', oldIntv, af2Form), oldIntv, changedVars)
  | Sub (u, v, oldIntv, af2Form) -> 
    let (uChanged, u', uIntv, changedVarsU) = evalCI_extra varsIntvsMap u in 
    let (vChanged, v', vIntv, changedVarsV) = evalCI_extra varsIntvsMap v in
    let changedVars = VariablesSet.union changedVarsU changedVarsV in
    if uChanged || vChanged then 
      let intv = uIntv -$ vIntv in
      let newIntv = (* inter_I_I oldIntv *) intv in
      if (compare_intv newIntv oldIntv) then
        (false, Sub(u', v', newIntv, af2Form), newIntv, changedVars)
      else
        (true, Sub(u', v', newIntv, af2Form), newIntv, changedVars)
    else
      ((* print_I oldIntv;
            flush stdout; *)
            (false, Sub(u', v', oldIntv, af2Form), oldIntv, changedVars))
  | Mul (u, v, oldIntv, af2Form) -> 
    (* let testString = "Checking " ^ string_infix_of_polyExpr (Mul (u, v, oldIntv, af2Form)) in 
    print_endline testString;
    flush stdout ;*)
    let (uChanged, u', uIntv, changedVarsU) = evalCI_extra varsIntvsMap u in 
    let (vChanged, v', vIntv, changedVarsV) = evalCI_extra varsIntvsMap v in
    (* let testString = "After Checking " ^ string_infix_of_polyExpr (Mul (u', v', oldIntv, af2Form)) in 
    print_endline testString;
    flush stdout; *)
    let changedVars = VariablesSet.union changedVarsU changedVarsV in
    (* print_string ("\nInterval of " ^ string_infix_of_polyExpr (Mul (u, v, oldIntv, af2Form)) ^ "is "); *)
    (* print_string ("\nChanged vars of " ^ string_infix_of_polyExpr (Mul (u, v, oldIntv, af2Form)) ^ "are ");
    print_varsSet changedVars; *)
    if uChanged || vChanged then 
      let intv = uIntv *$ vIntv in
      let newIntv = (* inter_I_I oldIntv *) intv in
      (* print_I newIntv;
      flush stdout; *)
      if (compare_intv newIntv oldIntv) then 
        (false, Mul(u', v', newIntv, af2Form), newIntv, changedVars)
      else 
        (true, Mul(u', v', newIntv, af2Form), newIntv, changedVars)
    else 
      ((* print_I oldIntv;
      flush stdout; *)
            (false, Mul(u', v', oldIntv, af2Form), oldIntv, changedVars))
  | Div (u, v, oldIntv, af2Form) -> 
    let (uChanged, u', uIntv, changedVarsU) = evalCI_extra varsIntvsMap u in 
    let (vChanged, v', vIntv, changedVarsV) = evalCI_extra varsIntvsMap v in
    let changedVars = VariablesSet.union changedVarsU changedVarsV in
    if uChanged || vChanged then 
      let intv = uIntv /$ vIntv in
      let newIntv = (* inter_I_I oldIntv *) intv in
      if (compare_intv newIntv oldIntv) then 
        (false, Div(u', v', newIntv, af2Form), newIntv, changedVars)
      else 
        (true, Div(u', v', newIntv, af2Form), newIntv, changedVars)
    else 
      (false, Div(u', v', oldIntv, af2Form), oldIntv, changedVars)
  | Pow (var, multiplicity, oldVarIntv, _, oldIntv, af2Form) -> 
    let varIntv = StringMap.find var varsIntvsMap in
    if compare_intv varIntv oldVarIntv then
      (false, Pow(var, multiplicity, varIntv, false, oldIntv, af2Form), oldIntv, VariablesSet.empty)
    else
      let intv = pow_I_i varIntv multiplicity in
      let newIntv = (* inter_I_I oldIntv *) intv in
      if check_infinity newIntv then 
        (true, Pow(var, multiplicity, varIntv, false, newIntv, af2Form), newIntv, VariablesSet.empty)
      else 
        (true, Pow(var, multiplicity, varIntv, true, newIntv, af2Form), newIntv, VariablesSet.singleton var)
  
let rec evalCI varsIntvsMap polyExpr =
  evalCI_extra varsIntvsMap polyExpr
  
 
(* evalAf2 compute the bound of a polynomial function from an assignment ass by AF2 form*)
let rec evalAf2 varsIntvsMap varsAf2sMap varsNum = function
  | Real (c, initialized, oldIntv, oldAf2Form) -> 
    (* let testString = "Checking " ^ string_infix_of_polyExpr (Real (c, initialized, oldIntv, oldAf2Form)) in 
    print_endline testString;
    flush stdout; *)
    if initialized then 
      (false, false, true, Real(c, true, oldIntv, oldAf2Form), oldIntv, oldAf2Form)
    else 
      let af2Form = Util.toAf2 {low=c;high=c} 0 varsNum in
      (true, true, true, Real(c, true, oldIntv, af2Form), {low=c;high=c}, af2Form)
  | Var (var, oldIntv, oldAf2Form, changed) -> 
    (* let testString = "Checking " ^ string_infix_of_polyExpr (Var (var, oldIntv, oldAf2Form, changed)) ^ " Changed?: " ^ string_of_bool changed in 
    print_endline testString;
    flush stdout; *)
    if changed then 
      (* let intv = StringMap.find var varsIntvsMap in 
      let index = StringMap.find var varsIndicesMap in
      let af2Form = Util.toAf2 intv index varsNum in
      let af2Intv = af2Form#evaluate in
      let newIntv = inter_I_I af2Intv intv in *)
      let af2Form = StringMap.find var varsAf2sMap in
      (false, true, true, Var(var, oldIntv, af2Form, true), oldIntv, af2Form)
    else
      let usingAF2 = not (check_infinity oldIntv) in
      (false, false, usingAF2, Var (var, oldIntv, oldAf2Form, false), oldIntv, oldAf2Form)
  | Add (u, v, oldIntv, oldAf2Form) -> 
    (* let testString = "Checking " ^ string_infix_of_polyExpr (Add (u, v, oldIntv, oldAf2Form)) ^ " oldIntv " in 
    print_string testString;
    print_I oldIntv;
    print_endline "";
    flush stdout ;*)
    let (uIntvChanged, uAF2Changed, usingAF2U, u', uIntv, af2FormU) = evalAf2 varsIntvsMap varsAf2sMap varsNum u in
    let (vIntvChanged, vAF2changed, usingAF2V, v', vIntv, af2FormV) = evalAf2 varsIntvsMap varsAf2sMap varsNum v in
    (* let testString = "AF2 result of " ^ string_infix_of_polyExpr u ^ " is " in 
    print_string testString;
    flush stdout; *)
    let newIntv =
      if uIntvChanged || vIntvChanged then 
        let tmpIntv = uIntv +$ vIntv in
        inter_I_I oldIntv tmpIntv
      else 
        oldIntv
    in
    if (uAF2Changed || vAF2changed) && usingAF2U && usingAF2V then
      let af2Form = IA.AF2.(af2FormU + af2FormV) in
      (* print_endline "Computing AF2";
      flush stdout; *)
      let af2Intv = af2Form#evaluate in
      let newIntv = inter_I_I af2Intv newIntv in
      let intvChanged = not (compare_intv newIntv oldIntv)
      in
      (intvChanged, true, true, Add(u', v', newIntv, af2Form), newIntv, af2Form)
    else
      let intvChanged = not (compare_intv newIntv oldIntv) in
      (intvChanged, false, usingAF2U && usingAF2V, Add (u', v', oldIntv, oldAf2Form), 
          oldIntv, oldAf2Form)
      
  | Sub (u, v, oldIntv, oldAf2Form) -> 
    let (uIntvChanged, uAF2Changed, usingAF2U, u', uIntv, af2FormU) = evalAf2 varsIntvsMap varsAf2sMap varsNum u in
    let (vIntvChanged, vAF2changed, usingAF2V, v', vIntv, af2FormV) = evalAf2 varsIntvsMap varsAf2sMap varsNum v in
    (* let testString = "AF2 result of " ^ string_infix_of_polyExpr u ^ " is " in 
    print_string testString;
    flush stdout; *)
    let newIntv =
      if uIntvChanged || vIntvChanged then 
        let tmpIntv = uIntv -$ vIntv in
        inter_I_I oldIntv tmpIntv
      else 
        oldIntv
    in
    if (uAF2Changed || vAF2changed) && usingAF2U && usingAF2V then
      let af2Form = IA.AF2.(af2FormU - af2FormV) in
      (* print_endline "Computing AF2";
      flush stdout; *)
      let af2Intv = af2Form#evaluate in
      let newIntv = inter_I_I af2Intv newIntv in
      let intvChanged = not (compare_intv newIntv oldIntv)
      in
      (intvChanged, true, true, Sub(u', v', newIntv, af2Form), newIntv, af2Form)
    else
      let intvChanged = not (compare_intv newIntv oldIntv) in
      (intvChanged, false, usingAF2U && usingAF2V, Sub (u', v', oldIntv, oldAf2Form), 
          oldIntv, oldAf2Form)
  | Mul (u, v, oldIntv, oldAf2Form) -> 
    (* let testString = "Checking " ^ string_infix_of_polyExpr (Mul (u, v, oldIntv, oldAf2Form)) in 
    print_endline testString;
    flush stdout; *)
    let (uIntvChanged, uAF2Changed, usingAF2U, u', uIntv, af2FormU) = evalAf2 varsIntvsMap varsAf2sMap varsNum u in
    let (vIntvChanged, vAF2changed, usingAF2V, v', vIntv, af2FormV) = evalAf2 varsIntvsMap varsAf2sMap varsNum v in
    (* let testString = "AF2 result of " ^ string_infix_of_polyExpr u ^ " is " in 
    print_string testString;
    flush stdout; *)
    let newIntv =
      if uIntvChanged || vIntvChanged then 
        let tmpIntv = uIntv *$ vIntv in
        inter_I_I oldIntv tmpIntv
      else 
        oldIntv
    in
    if (uAF2Changed || vAF2changed) && usingAF2U && usingAF2V then
      let af2Form = IA.AF2.(af2FormU * af2FormV) in
      (* print_endline "Computing AF2";
      flush stdout; *)
      let af2Intv = af2Form#evaluate in
      let newIntv = inter_I_I af2Intv newIntv in
      let intvChanged = not (compare_intv newIntv oldIntv)
      in
      (intvChanged, true, true, Mul(u', v', newIntv, af2Form), newIntv, af2Form)
    else
      let intvChanged = not (compare_intv newIntv oldIntv) in
      (intvChanged, false, usingAF2U && usingAF2V, Mul (u', v', oldIntv, oldAf2Form), 
          oldIntv, oldAf2Form)
  | Div (u, Real (c, initialized, fBound, fAF2Form), oldIntv, oldAf2Form) -> 
    let (uIntvChanged, uAF2Changed, usingAF2U, u', uIntv, af2FormU) = evalAf2 varsIntvsMap varsAf2sMap varsNum u in
    (* let testString = "AF2 result of " ^ string_infix_of_polyExpr u ^ " is " in 
    print_string testString;
    flush stdout; *)
    let newIntv =
      if uIntvChanged then 
        let tmpIntv = uIntv /$. c in
        inter_I_I oldIntv tmpIntv
      else 
        oldIntv
    in
    if uAF2Changed then
      let af2Form = IA.AF2.(af2FormU / c) in
      (* print_endline "Computing AF2";
      flush stdout; *)
      let af2Intv = af2Form#evaluate in
      let newIntv = inter_I_I af2Intv newIntv in
      let intvChanged = not (compare_intv newIntv oldIntv)
      in
      (intvChanged, true, true, Div(u', Real (c, initialized, fBound, fAF2Form), newIntv, af2Form), newIntv, af2Form)
    else
      let intvChanged = not (compare_intv newIntv oldIntv) in
      (intvChanged, false, usingAF2U, Div (u', Real (c, initialized, fBound, fAF2Form), 
          oldIntv, oldAf2Form), oldIntv, oldAf2Form)

  | Div(u, v, oldIntv, oldAf2Form) ->
    let (uIntvChanged, _, _, u', uIntv, _) = evalAf2 varsIntvsMap varsAf2sMap varsNum u in
    let (vIntvChanged, _, _, v', vIntv, _) = evalAf2 varsIntvsMap varsAf2sMap varsNum v in
    (* let testString = "AF2 result of " ^ string_infix_of_polyExpr u ^ " is " in 
    print_string testString;
    flush stdout; *)
    let newIntv =
      if uIntvChanged || vIntvChanged then 
        let tmpIntv = uIntv /$ vIntv in
        inter_I_I oldIntv tmpIntv
      else 
        oldIntv
    in
    let intvChanged = not (compare_intv newIntv oldIntv) in
    (intvChanged, false, false, Div(u', v', newIntv, oldAf2Form), newIntv, oldAf2Form)
    
  | Pow (var, multiplicity, varIntv, varChanged, oldIntv, oldAf2Form) -> 

    if varChanged then
      let varAF2Form = StringMap.find var varsAf2sMap in
      if multiplicity = 1 then  
        (false, true, true, Pow(var, multiplicity, varIntv, true, varIntv, varAF2Form), varIntv, varAF2Form)
      else if multiplicity mod 2 = 0 then 
        let newMultiplicity = multiplicity / 2 in 
        let newPowExpr = Pow (var, newMultiplicity, varIntv, true, {low=neg_infinity;high=infinity}, oldAf2Form) in
        let (_, _, _, _, _, tmpAf2Result) = evalAf2 varsIntvsMap varsAf2sMap varsNum newPowExpr in
        let af2Form = IA.AF2.(tmpAf2Result * tmpAf2Result) in
        let af2Intv = af2Form#evaluate in
        let newIntv = inter_I_I af2Intv oldIntv in
        let intvChanged = not (compare_intv newIntv oldIntv) in
        (intvChanged, true, true, Pow(var, multiplicity, varIntv, true, newIntv, af2Form), newIntv, af2Form)
      else 
        let newPowExpr = Pow (var, multiplicity - 1, varIntv, true, {low=neg_infinity;high=infinity}, oldAf2Form) in
        let (_, _, _, _, _ ,powExprAf2) = evalAf2 varsIntvsMap varsAf2sMap varsNum newPowExpr in
        let af2Form = IA.AF2.(varAF2Form * powExprAf2) in
        let af2Intv = af2Form#evaluate in
        let newIntv = inter_I_I af2Intv oldIntv in
        let intvChanged = not (compare_intv newIntv oldIntv) in 
        (intvChanged, true, true, Pow(var, multiplicity, varIntv, true, newIntv, af2Form), newIntv, af2Form)
    else 
      let usingAF2 = not (check_infinity varIntv) in
      (false, false, usingAF2, Pow (var, multiplicity, varIntv, varChanged, oldIntv, oldAf2Form), oldIntv, oldAf2Form)
  | _ -> raise (Failure "Unsupported operation of af2")  

(* (* evalICI compute the bound of a polynomial function from an assignment ass by ICI form*)
let rec evalICI varsIntvsMap = function
  | Real (c, _, _, _) -> let newC = IA.bound_of_float c in new IA.inf_interval newC newC
  | Var (var, _) -> 
    let intv = StringMap.find var varsIntvsMap in
    let lowerBound = IA.bound_of_float intv#l in
    let upperBound = IA.bound_of_float intv#h in
    new IA.inf_interval lowerBound upperBound
  | Add (u, v, _) -> IA.ICI.(evalICI varsIntvsMap u + evalICI varsIntvsMap v)
  | Sub (u, v, _) -> IA.ICI.(evalICI varsIntvsMap u - evalICI varsIntvsMap v)
  | Mul (u, v, _) -> IA.ICI.(evalICI varsIntvsMap u * evalICI varsIntvsMap v)

(* evalAf1 compute the bound of a polynomial function from an assignment ass by AF1 form*)
let rec evalAf1 ass n = function
  | Real (c, _, _, _) -> Util.toAf1 (new IA.interval c c) 0 n
  | Var (v, _) -> List.assoc v ass
  | Add (u, v, _) -> IA.AF1.(evalAf1 ass n u + evalAf1 ass n v)
  | Sub (u, v, _) -> IA.AF1.(evalAf1 ass n u - evalAf1 ass n v)
  | Mul (u, v, _) -> IA.AF1.(evalAf1 ass n u * evalAf1 ass n v) *)

(* (* evalCai1 compute the bound of a polynomial function from an assignment ass by CAI1 form*)
let rec evalCai1 ass n= function
  | Real (c, _, _, _) -> Util.toCai1 (new IA.interval c c) 0 n
  | Var (v, _) -> List.assoc v ass
  | Add (u, v, _) -> IA.CAI1.(evalCai1 ass n u + evalCai1 ass n v)
  | Sub (u, v, _) -> IA.CAI1.(evalCai1 ass n u - evalCai1 ass n v)
  | Mul (u, v, _) -> IA.CAI1.(evalCai1 ass n u * evalCai1 ass n v)

(* evalCai2 compute the bound of a polynomial function from an assignment ass by CAI2 form*)
let rec evalCai2 ass n= function
  | Real (c, _, _, _) -> Util.toCai2 (new IA.interval c c) 0 n
  | Var (v, _) -> List.assoc v ass
  | Add (u, v, _) -> IA.CAI2.(evalCai2 ass n u + evalCai2 ass n v)
  | Sub (u, v, _) -> IA.CAI2.(evalCai2 ass n u - evalCai2 ass n v)
  | Mul (u, v, _) -> IA.CAI2.(evalCai2 ass n u * evalCai2 ass n v)

(* evalCai3 compute the bound of a polynomial function from an assignment ass by CAI3 form*)
let rec evalCai3 ass n= function
  | Real (c, _, _, _) -> Util.toCai3 (new IA.interval c c) 0 n
  | Var (v, _) -> List.assoc v ass
  | Add (u, v, _) -> IA.CAI3.(evalCai3 ass n u + evalCai3 ass n v)
  | Sub (u, v, _) -> IA.CAI3.(evalCai3 ass n u - evalCai3 ass n v)
  | Mul (u, v, _) -> IA.CAI3.(evalCai3 ass n u * evalCai3 ass n v) *)
  

(* This function returns a set of all variables of a polynomial expression *)
(* let rec get_vars_set_polyExp = function
  | Var (x, _, _) -> VariablesSet.singleton x
	| Add(e1, e2, _, _) -> VariablesSet.union (get_vars_set_polyExp e1) (get_vars_set_polyExp e2)
	| Sub(e1, e2, _, _) -> VariablesSet.union (get_vars_set_polyExp e1) (get_vars_set_polyExp e2)
	| Mul(e1, e2, _, _) -> VariablesSet.union (get_vars_set_polyExp e1) (get_vars_set_polyExp e2)
	| _ -> VariablesSet.empty *)
	

(* This function returns a set of 
variables of a boolean expression. The sort critia is
using alphabet ordering *)	
(* let get_vars_set_boolExpr boolExp = 
  let polyExp = get_exp boolExp in
  get_vars_set_polyExp polyExp
 *)

(* let rec get_vars_set_boolExprs boolExps = match boolExps with
  | [] -> VariablesSet.empty
  | h::t -> VariablesSet.union (get_vars_set_boolExpr h) (get_vars_set_boolExprs t)  *)


(* Evaluate a polynomial using AF2 *)
(* let poly_eval_af2 polyExpr varsSet varsNum varsIntvsMap =
  let add_varAf2 var (varsAf2sMap, nextIndex) =
    let intv = StringMap.find var varsIntvsMap in
    let af2 = Util.toAf2 intv nextIndex varsNum in
    (StringMap.add var af2 varsAf2sMap, nextIndex + 1)
  in 
  let (varsAf2Map, _) = VariablesSet.fold add_varAf2 varsSet (StringMap.empty, 1) in
  try 
    let (newPolyExpr, res) = evalAf2 varsAf2Map varsNum polyExpr in
    res#evaluate
  with Failure f -> {low=neg_infinity;high=infinity} *)

(* let rec reset_intv = function 
  | Real (c, _, _, _) -> Real(c, {low=neg_infinity;high=infinity})
  | Var (v, _) -> Var (v, {low=neg_infinity;high=infinity})
  | Add (u, v, _) -> Add (reset_intv u, reset_intv v, {low=neg_infinity;high=infinity})
  | Sub (u, v, _) -> Sub (reset_intv u, reset_intv v, {low=neg_infinity;high=infinity})
  | Mul (u, v, _) -> Mul (reset_intv u, reset_intv v, {low=neg_infinity;high=infinity})
  | Div (u, v, _) -> Div (reset_intv u, reset_intv v, {low=neg_infinity;high=infinity})
  | Pow (var, multiplicity, _, _, _) -> Pow (var, multiplicity, {low=neg_infinity;high=infinity}, {low=neg_infinity;high=infinity}) *)

(* Evaluate the polynomial using AF2, varsSensitivity is also returned *)
let poly_eval_af2_varsSens polyExpr varsSet varsNum varsIntvsMap varsIndicesMap changedVars =
  let add_varAf2Index var varsAf2sMap =
    let intv = StringMap.find var varsIntvsMap in
    let index = StringMap.find var varsIndicesMap in
    (* print_string ("Af2 of " ^ var ^ " with index " ^ string_of_int index ^ " among " ^ string_of_int varsNum ^ " in ");
    print_I intv;
    flush stdout; *)
    let af2Form = Util.toAf2 intv index varsNum in
    (* print_string "is ";
    af2Form#print_form;
    print_string "\n";
    flush stdout; *)
    StringMap.add var af2Form varsAf2sMap
  in
  (* print_string "Vars Changed: ";
  print_varsSet changedVars;
  print_endline ""; *)
  let varsAf2sMap = VariablesSet.fold add_varAf2Index changedVars StringMap.empty in
  (*let print_var_index (var, index) = 
    print_endline (var ^ ": " ^ string_of_int index ^ " ");
    flush stdout;
  in
  List.iter print_var_index varsIndicesList;*)
  (* print_endline "Checking";
  flush stdout; *)
  let (_, _, usingAF2, newPolyExpr, intv, res) = evalAf2 varsIntvsMap varsAf2sMap varsNum polyExpr in
  (* print_endline "Checked";
  flush stdout; *)
  (*res#print_form;*)
  let varsSensitivity = 
    if usingAF2 then res#extract_sortedVarsSens varsIndicesMap 
    else
      let gen_emptyVarsSen var currentSens = (var, 0., false) :: currentSens in
      VariablesSet.fold gen_emptyVarsSen varsSet []
  in
  (newPolyExpr, intv , varsSensitivity)

(* Evaluate a polynomial using CI *)
let poly_eval_ci polyExpr varsIntvsMap = 
  evalCI varsIntvsMap polyExpr
  
(* (* Evaluate a polynomial using ICI *)
let poly_eval_ici polyExpr varsIntvsMap = 
  let res = evalICI varsIntvsMap polyExpr in
  res#to_interval *)
(* 
(*evaluate the bound of poly expression by type of interval arithmetic*)						     
let poly_eval e varsSet ia varsIntvsMap = 
  (* let rec get_interval var intvList =
    let intv = StringMap.find Var (var, _)sIntvsMap in
    (var, intv)::intvList
  in
  let assIntv = VariablesSet.fold get_interval varsSet [] in
  let iIntvVar = List.length assIntv in
  if (ia=1) then (
    let assAf1 = e_toAf1 assIntv 1 iIntvVar in
    let res = evalAf1 assAf1 iIntvVar e in
    (res#evaluate, []);
  )  
  else *) if (ia=2) then (
    let iIntvVar = VariablesSet.cardinal varsSet in
    let estimatedIntv = poly_eval_af2 e varsSet iIntvVar varsIntvsMap in
    (estimatedIntv, ([]:float list));
    let assAf2VarPos = e_toAf2 assIntv 1 iIntvVar in
    let (assAf2, varPos) = List.split assAf2VarPos in
    let res = evalAf2 assAf2 iIntvVar e in
    let varsSensitivity = res#extract_varsSen varPos in
    (res#evaluate, varsSensitivity);
  )  
  (* else if (ia=3) then (
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
  ) *)
  else (
    let (_,_, res) = poly_eval_ci e varsIntvsMap in
    (res, []);
  )   *)


(* Check whether an expression e is satisfiable or not provided the over-approximation of sides *)
let check_sat_providedBounds boolExp bound = 
  (*print_endline ("Bounds: " ^ string_of_float bound#l ^ " " ^ string_of_float bound#h);
  flush stdout;*)
  match boolExp with
  |Eq e -> 
    if (bound.low = bound.high && bound.high = 0.) then 1
    else if (bound.high < 0. || bound.low > 0.) then -1
    else 0
  |Neq e -> 
    if (bound.low = bound.high && bound.high = 0.) then -1
    else if (bound.high < 0. || bound.low > 0.) then 1
    else 0
  |Leq e -> 
    if (bound.high <= 0.) then 1
    else if (bound.low > 0.) then -1
    else 0
  |Le e -> 
    if (bound.high < 0.) then 1
    else if (bound.low >= 0.) then -1
    else 0
  |Geq e -> 
    if (bound.low >= 0.) then 1
    else if (bound.high < 0.) then -1
    else 0
  |Gr e -> 
    if (bound.low > 0.) then 1
    else if (bound.high <= 0.) then -1
    else 0


(* Check whether an expression e is satisfiable or not provided the over-approximation of sides, length of SAT area also be returned *)
let check_sat_get_satLength_providedBounds boolExp bound = 
  let lowerBound = bound.low in
  let upperBound = bound.high in
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


(* (* Check sat with combination of AF2 and CI *)
let check_sat_af_two_ci_boolExpr boolExpr varsSet varsNum varsIntvsMap = 
  let polyExpr = get_exp boolExpr in
  (* evaluate the expression using AF2 *)
  let afTwoBound  = poly_eval_af2 polyExpr varsSet varsNum varsIntvsMap in
  (*print_endline (assignments_toString afTwoVarsSen);
  flush stdout;*)
  let sat = check_sat_providedBounds boolExpr afTwoBound in
  if sat = 0 then (* AF2 fails to conclude the expression *)
    (* Compute bouds of polynomial using *)
    let (_,_, ciBound)  = poly_eval_ci polyExpr varsIntvsMap in
    let newBound = inter_I_I afTwoBound ciBound in
    let sat = check_sat_providedBounds boolExpr newBound in
    sat
  else sat *)
  
    
(* Check sat with combination of AF2 and CI, return the sorted variables by their sensitivities *)      
let check_sat_getBound_af_two_ci_boolExpr_varsSens boolExpr varsSet varsNum varsIntvsMap varsIndicesMap = 
  let polyExpr = get_exp boolExpr in

  (* Compute bouds of polynomial using CI *)
  let (_, newPolyExpr, ciBound, changedVars)  = poly_eval_ci polyExpr varsIntvsMap in
  (* print_string "\nCI result: ";
  print_I ciBound; 
  flush stdout;  *)
  let (sat, satLength) = check_sat_get_satLength_providedBounds boolExpr ciBound in

  if sat = 0 then (* AF2 fails to conclude the expression *)
    (* evaluate the expression using AF2 *)
    (* let testString = "Checking " ^ string_infix_of_polyExpr newPolyExpr in 
    print_endline testString;
    print_endline ("Start checking using af2\n");
    flush stdout; *)
    let (newPolyExpr, afTwoBound, varsSens)  = poly_eval_af2_varsSens newPolyExpr varsSet varsNum varsIntvsMap varsIndicesMap changedVars in
  (*   let print_var_sen (var, sen, isPositiveSen) = 
      print_endline (var ^ ": " ^ string_of_float sen ^ " ");
      flush stdout;
    in
    List.iter print_var_sen varsSens;
    flush stdout; *)
    (* print_string "\nAF2 result: ";
    print_I afTwoBound;
    flush stdout; *)
    let (sat, satLength) = check_sat_get_satLength_providedBounds boolExpr afTwoBound in    
    (newPolyExpr, sat, satLength, varsSens, afTwoBound)
  else 
    let gen_emptyVarsSen var currentSens = (var, 0., false) :: currentSens in
    let varsSensitivity = VariablesSet.fold gen_emptyVarsSen varsSet [] in
    (newPolyExpr, sat, satLength, varsSensitivity, ciBound)
  
(* let check_sat_af_two_ci_get_satLength_boolExpr boolExpr varsSet varsNum varsIntvsMap =
  let polyExpr = get_exp boolExpr in
  (* evaluate the expression using AF2 *)
  let afTwoBound  = poly_eval_af2 polyExpr varsSet varsNum varsIntvsMap in
  (*print_endline (assignments_toString afTwoVarsSen);
  flush stdout;*)
  let (sat, satLength) = check_sat_get_satLength_providedBounds boolExpr afTwoBound in
  if sat = 0 then (* AF2 fails to conclude the expression *)
    (* Compute bouds of polynomial using *)
    let (_,_, ciBound)  = poly_eval_ci polyExpr varsIntvsMap in
    
    let newBound = inter_I_I afTwoBound ciBound in
    check_sat_get_satLength_providedBounds boolExpr newBound
  else (sat, satLength) *)

let check_contract oldIntv intv esl = 
  oldIntv.low +. esl < intv.low || oldIntv.high > intv.high +. esl

let contain_zero intv = intv.low <= 0. && intv.high >= 0.

let get_intv_ofPolyExpr varsIntvsMap = function
  | Real (_, _, intv, _) -> intv
  | Var (var, _, _, _) -> StringMap.find var varsIntvsMap
  | Add (_, _, intv, _) -> intv
  | Sub (_, _, intv, _) -> intv
  | Mul (_, _, intv, _) -> intv
  | Div (_, _, intv, _) -> intv
  | Pow (_, _, _, _, intv, _) -> intv

let rec contract_polyExpr polyExpr intv varsIntvsMap esl =
  let oldIntv = get_intv_ofPolyExpr varsIntvsMap polyExpr in
  let contracted = check_contract oldIntv intv esl in
  if contracted then 
    let newIntv = inter_I_I oldIntv intv in 
    (* print_endline ("Contracted " ^ string_infix_of_polyExpr polyExpr ^ " from " ^ sprintf_I "%f" oldIntv
      ^ " to " ^ sprintf_I "%f" newIntv);
    flush stdout; *)
    if newIntv.low <= newIntv.high then match polyExpr with 
      | Var (var, _,_,_) -> (contracted, StringMap.add var newIntv varsIntvsMap)
      | Add (u, v, _,_) -> 
        let uIntv = get_intv_ofPolyExpr varsIntvsMap u in
        let vIntv = get_intv_ofPolyExpr varsIntvsMap v in
        (* print_endline ("Contracting " ^ string_infix_of_polyExpr u ^ " from " ^ sprintf_I "%f" uIntv
          ^ " to " ^ sprintf_I "%f" (newIntv -$ vIntv));
        flush stdout;   *)
        (* try to contract u *)
        let (uContracted, varsIntvsMap) = contract_polyExpr u (newIntv -$ vIntv) varsIntvsMap esl in
        if uContracted && StringMap.is_empty varsIntvsMap then 
          (uContracted, StringMap.empty)
        else 
          let (vContracted, varsIntvsMap) = contract_polyExpr v (newIntv -$ uIntv) varsIntvsMap esl in
          (uContracted || vContracted, varsIntvsMap)
      | Sub (u, v, _, _) -> 
        let uIntv = get_intv_ofPolyExpr varsIntvsMap u in
        let vIntv = get_intv_ofPolyExpr varsIntvsMap v in

        (* try to contract u *)
        let (uContracted, varsIntvsMap) = contract_polyExpr u (newIntv +$ vIntv) varsIntvsMap esl in 
        if uContracted && StringMap.is_empty varsIntvsMap then 
          (uContracted, StringMap.empty)
        else
          let (vContracted, varsIntvsMap) = contract_polyExpr v (uIntv -$ newIntv) varsIntvsMap esl in
          (uContracted || vContracted, varsIntvsMap)
      | Mul (u, v, _, _) -> 
        let uIntv = get_intv_ofPolyExpr varsIntvsMap u in
        let vIntv = get_intv_ofPolyExpr varsIntvsMap v in

        (* try to contract u *)
        let (uContracted, varsIntvsMap) = 
          if contain_zero vIntv then (false, varsIntvsMap)
          else contract_polyExpr u (newIntv /$ vIntv) varsIntvsMap esl 
        in
        if uContracted && StringMap.is_empty varsIntvsMap then 
          (uContracted, StringMap.empty)
        else
          let (vContracted, varsIntvsMap) = 
            if contain_zero uIntv then (false, varsIntvsMap) 
            else contract_polyExpr v (newIntv /$ uIntv) varsIntvsMap esl 
          in
          (uContracted || vContracted, varsIntvsMap)
      | Div (u, v, _, _) ->
        let uIntv = get_intv_ofPolyExpr varsIntvsMap u in
        let vIntv = get_intv_ofPolyExpr varsIntvsMap v in

        (* try to contract u *)
        let (uContracted, varsIntvsMap) = contract_polyExpr u (newIntv *$ vIntv) varsIntvsMap esl in
        if uContracted && StringMap.is_empty varsIntvsMap then 
          (uContracted, StringMap.empty)
        else
          let (vContracted, varsIntvsMap) = contract_polyExpr v (uIntv /$ newIntv) varsIntvsMap esl in
          (uContracted || vContracted, varsIntvsMap)
      | Pow (var, multiplicity, _, _, _, _) -> 
        (* print_endline ("Start constracting " ^ var ^ " in " ^ string_infix_of_polyExpr polyExpr);
        flush stdout;
        print_endline ("Finished constracting " ^ var ^ " in " ^ string_infix_of_polyExpr polyExpr); *)
        let varIntv = 
          if multiplicity mod 2 = 0 then 
            let tmpIntv = newIntv **$ ({low=1.;high=1.} /$. float_of_int multiplicity) in
            {low = -.(tmpIntv.high);high=tmpIntv.high}
          else 
            if newIntv.low >= 0. then newIntv **$ ({low=1.;high=1.} /$. float_of_int multiplicity)
            else
              let lowerIntv = {low=abs_float newIntv.low; high=abs_float newIntv.low} **$ ({low=1.;high=1.} /$. float_of_int multiplicity) in
              if newIntv.high >= 0. then 
                let upperIntv = {low=newIntv.high; high=newIntv.high} **$ ({low=1.;high=1.} /$. float_of_int multiplicity) in
                {low= -.(lowerIntv.high); high=upperIntv.high}
              else 
                let upperIntv = {low=abs_float newIntv.high; high=abs_float newIntv.high} **$ ({low=1.;high=1.} /$. float_of_int multiplicity) in
                {low= -.(lowerIntv.high); high= -.(upperIntv.low)}
        in
        (* print_endline ("Contracted " ^ var ^ " from " ^ sprintf_I "%f" newIntv
      ^ " to " ^ sprintf_I "%f" varIntv); *)
        let oldVarIntv = StringMap.find var varsIntvsMap in
        if check_contract oldVarIntv varIntv esl then (contracted, StringMap.add var (inter_I_I oldVarIntv varIntv) varsIntvsMap)
        else (false, varsIntvsMap)
      | _ -> (
        (* print_endline "Error";
        flush stdout; *)
        raise (Failure "Wrong implementation in contraction operation")
      )
    else (contracted, StringMap.empty)  
  else (false, varsIntvsMap)