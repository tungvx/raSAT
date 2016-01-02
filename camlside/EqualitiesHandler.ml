(* /\/\/\/\/\/\/\/\/\/\ MODULE for equalities handling /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\*)

open Ast
open Assignments
open Util
open Variable
open InfiniteList
open IA
open Interval

let check_equality polyCons varsSet varsIntvsMap = 
  let getTwoPoints var (currentFirstPoint, currentSecondPoint) = 
    let intv = StringMap.find var varsIntvsMap in
    let varSATDirection =polyCons#get_varSATDirection var in
    if varSATDirection = 1 || varSATDirection = 0 then
      (StringMap.add var {low=intv.low; high=intv.low} currentFirstPoint,
       StringMap.add var {low=intv.high; high=intv.high} currentSecondPoint)
    else
      (StringMap.add var {low=intv.high; high=intv.high} currentFirstPoint,
       StringMap.add var {low=intv.low; high=intv.low} currentSecondPoint)
  in
  let (firstPoint, secondPoint) = VariablesSet.fold getTwoPoints varsSet (varsIntvsMap, varsIntvsMap) in
  
  let firstBound = polyCons#get_bound firstPoint in
  (* print_endline ("First Bound: " ^ log_intervals firstPoint ^ sprintf_I "%f" firstBound);
  flush stdout; *)

  let secondBound = polyCons#get_bound secondPoint in
  (* print_endline ("Second Bound: "^ log_intervals secondPoint  ^ sprintf_I "%f" secondBound);
  flush stdout; *)
 
  (* Intermediate theorem f(a) . f(b) < 0 ==> f(x) has a root between a and b *)
  if (firstBound.high <= 0. && secondBound.low >= 0.) || (firstBound.low >= 0. && secondBound.high <= 0.) then (
    let get_log var intv currentString =
      if VariablesSet.mem var polyCons#get_varsSet then
        if VariablesSet.mem var varsSet then 
          var ^ " = " ^ (string_of_float_app intv.low) ^ " " ^ currentString
        else
          currentString ^ " " ^ var ^ " = " ^ sprintf_I "%f" intv
      else currentString
    in
    let firstLog = StringMap.fold get_log firstPoint "" in
    let secondLog = StringMap.fold get_log secondPoint "" in
    let log = polyCons#to_string_infix ^ ":\n" ^ firstLog ^ ": " ^ sprintf_I "%f" firstBound ^ "\n" ^ secondLog ^ ": " ^ sprintf_I "%f"  secondBound ^ "\n" in
    polyCons#set_log log;
    true
  )
  else false


let rec check_equalities_extra polyConstraints varsSetCandidates varsIntvsMap consideredVarsSet = 
  match polyConstraints with 
  | [] -> (true, [], consideredVarsSet)
  | h::t -> 
    match varsSetCandidates with
    | Nil -> (false, polyConstraints, consideredVarsSet) 
    | Cons(varsList, tail) ->
    let varsSet = List.fold_right VariablesSet.add varsList VariablesSet.empty in
    let commonVarsSet = VariablesSet.inter consideredVarsSet varsSet in
    if VariablesSet.is_empty commonVarsSet then
      if check_equality h varsSet varsIntvsMap then
        match t with
        | [] -> (true, [], VariablesSet.union consideredVarsSet varsSet)
        | l::m ->
          let varsList = l#get_varsList in
          let newVarsSetCandidates = power_set varsList in
          let (result, _, newConsideredVarsSet) = 
            check_equalities_extra t newVarsSetCandidates varsIntvsMap (VariablesSet.union consideredVarsSet varsSet)
          in
          if result then
            (true, [], newConsideredVarsSet)
          else
            check_equalities_extra polyConstraints (tail()) varsIntvsMap consideredVarsSet
      else check_equalities_extra polyConstraints (tail()) varsIntvsMap consideredVarsSet
    else
      check_equalities_extra polyConstraints (tail()) varsIntvsMap consideredVarsSet
  

(* polyConstraints is a list of polynomial constraints, each constraints must be an equation *)
let rec check_equalities polyConstraints varsIntvsMap consideredVarsSet = 
  match polyConstraints with 
  | [] -> (true, [], consideredVarsSet)
  | h::t -> (
    (* print_endline "Checking Equations";
    flush stdout; *)
    let varsList = h#get_varsList in
    let varsSetCandidates = power_set varsList in
    check_equalities_extra polyConstraints varsSetCandidates varsIntvsMap consideredVarsSet 
  )
  

(* /\/\/\/\/\/\/\/\/\/\ MODULE for equalities handling /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\*)

