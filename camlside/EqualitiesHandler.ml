(* /\/\/\/\/\/\/\/\/\/\ MODULE for equalities handling /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\*)

open Ast
open Assignments
open Util
open Variable
open InfiniteList

let check_equality polyCons varsSet varsIntvsMiniSATCodesMap = 
  let getFirstPoint var varsIntvsMiniSATCodesMap = 
    let (intv, miniSATCode) = StringMap.find var varsIntvsMiniSATCodesMap in
    StringMap.add var (new IA.interval intv#l intv#l, miniSATCode) varsIntvsMiniSATCodesMap
  in
  let firstPoint = VariablesSet.fold getFirstPoint varsSet varsIntvsMiniSATCodesMap in
  let firstBound = polyCons#get_bound firstPoint in
  
  let getSecondPoint var varsIntvsMiniSATCodesMap = 
    let (intv, miniSATCode) = StringMap.find var varsIntvsMiniSATCodesMap in
    StringMap.add var (new IA.interval intv#h intv#h, miniSATCode) varsIntvsMiniSATCodesMap
  in
  let secondPoint = VariablesSet.fold getSecondPoint varsSet varsIntvsMiniSATCodesMap in
  let secondBound = polyCons#get_bound secondPoint in
 
  (* Intermediate theorem f(a) . f(b) < 0 ==> f(x) has a root between a and b *)
  (firstBound#h < 0. && secondBound#l > 0.) || (firstBound#l > 0. && secondBound#h < 0.)


let rec check_equalities_extra polyConstraints varsSetCandidates varsIntvsMiniSATCodesMap consideredVarsSet = 
  match polyConstraints with 
  | [] -> (true, [])
  | h::t -> 
    match varsSetCandidates with
    | Nil -> (false, polyConstraints) 
    | Cons(varsList, tail) ->
    let varsSet = List.fold_right VariablesSet.add varsList VariablesSet.empty in
    let commonVarsSet = VariablesSet.inter consideredVarsSet varsSet in
    if VariablesSet.is_empty commonVarsSet then
      if check_equality h varsSet varsIntvsMiniSATCodesMap then
        match t with
        | [] -> (true, [])
        | l::m ->
          let varsList = l#get_varsList in
          let newVarsSetCandidates = power_set varsList in
          let (result, _) = 
            check_equalities_extra t newVarsSetCandidates varsIntvsMiniSATCodesMap (VariablesSet.union consideredVarsSet varsSet)
          in
          if result then
            (true, [])
          else
            check_equalities_extra polyConstraints (tail()) varsIntvsMiniSATCodesMap consideredVarsSet
      else check_equalities_extra polyConstraints (tail()) varsIntvsMiniSATCodesMap consideredVarsSet
    else
      check_equalities_extra polyConstraints (tail()) varsIntvsMiniSATCodesMap consideredVarsSet
  

(* polyConstraints is a list of polynomial constraints, each constraints must be an equation *)
let rec check_equalities polyConstraints varsIntvsMiniSATCodesMap consideredVarsSet = 
  match polyConstraints with 
  | [] -> (true, [])
  | h::t -> (
    let varsList = h#get_varsList in
    let varsSetCandidates = power_set varsList in
    check_equalities_extra polyConstraints varsSetCandidates varsIntvsMiniSATCodesMap consideredVarsSet
  )
  

(* /\/\/\/\/\/\/\/\/\/\ MODULE for equalities handling /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\*)

