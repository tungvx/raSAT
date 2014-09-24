(* /\/\/\/\/\/\/\/\/\/\ MODULE for equalities handling /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\*)

open Ast
open Assignments
open Util
open Variable

let check_equality polyCons varsIntvsMiniSATCodesMap = 
  let polyExp = get_exp polyCons#get_constraint in
  let lowerBound = getLowerBound varsIntvsMiniSATCodesMap in (* getLowerBound is defined in Assignments.ml *)
  (* checkSAT_computeValues is defined in ast.ml *)
  let lowerBoundValue = evalFloat lowerBound polyExp in
  let upperBound = getUpperBound varsIntvsMiniSATCodesMap in (* getUpperBound is defined in Assignments.ml *)
  let upperBoundValue = evalFloat upperBound polyExp in
  (* Intermediate theorem f(a) . f(b) < 0 ==> f(x) has a root between a and b *)
  lowerBoundValue *. upperBoundValue <= 0. 


(* polyConstraints is a list of polynomial constraints, each constraints must be an equation *)
let rec check_equalities polyConstraints varsIntvsMiniSATCodesMap consideredVarsSet = 
  match polyConstraints with 
  | [] -> (true, [])
  | h::t -> (
    let commonVarsSet = VariablesSet.inter consideredVarsSet h#get_varsSet in
    if VariablesSet.is_empty commonVarsSet then
      if check_equality h varsIntvsMiniSATCodesMap then check_equalities t varsIntvsMiniSATCodesMap (VariablesSet.union consideredVarsSet h#get_varsSet)
      else (false, polyConstraints)
    else
      (false, polyConstraints)
  )

(* /\/\/\/\/\/\/\/\/\/\ MODULE for equalities handling /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\*)

