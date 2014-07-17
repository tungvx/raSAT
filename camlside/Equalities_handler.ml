(* /\/\/\/\/\/\/\/\/\/\ MODULE for equalities handling /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\*)

open Ast
open Assignments
open Util

let check_vars_ofEquation_notYetConsidered equation vars = 
  let equationVarsSet = 
    let equationVarsList = bool_vars equation in
    Util.red_list equationVarsList  
  in 
    (Util.has_common_element equationVarsSet vars, equationVarsSet @ vars)

let check_equality equation assIntv = 
  let polyExp = get_exp equation in
  let lowerBound = getLowerBound assIntv in (* getLowerBound is defined in Assignments.ml *)
  (* checkSAT_computeValues is defined in ast.ml *)
  let lowerBoundValue = evalFloat lowerBound polyExp in
  let upperBound = getUpperBound assIntv in (* getUpperBound is defined in Assignments.ml *)
  let upperBoundValue = evalFloat upperBound polyExp in
  (* Intermediate theorem f(a) . f(b) < 0 ==> f(x) has a root between a and b *)
  lowerBoundValue *. upperBoundValue < 0. 


(*let rec check_equalities_extra candidates =
  match candidates with
  | Nil -> false
  | Cons(((currentEquation, varsCandidates), nextEquations, vars), t) -> 
    match varsCandidates with
    | Nil -> check_equalities_extra (t())
    | Cons()*)


let rec check_equalities equations assIntv vars = 
  match equations with 
  | [] -> true 
  | h::t -> (
    (*let varsSet = get_vars_set_boolExp h in*)
    
  
    let (someVarsConsidered, newVars) = check_vars_ofEquation_notYetConsidered h vars in
    if someVarsConsidered then
      false
    else
      (check_equality h assIntv) && (check_equalities t assIntv newVars)
  )

(* /\/\/\/\/\/\/\/\/\/\ MODULE for equalities handling /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\*)

