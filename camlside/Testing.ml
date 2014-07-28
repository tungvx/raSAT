open Ast
open Variable
open InfiniteList
open Assignments
open PolynomialConstraint  
open Util

(* This is the helping function for the test function*)
let rec test_extra abstractTCInfList varsIntvsMiniSATCodesMap unsatPolyCons remainingTime = 
  match abstractTCInfList with
  | Nil -> ([],-1, [unsatPolyCons],StringMap.empty)
  | Cons((varsTCsMap, testCases, assignedVarsSet, polyConstraints, priorityNum), tail) ->
    if remainingTime <= 0. then ([],-1, [unsatPolyCons],StringMap.empty)
    else 
      let startTime = Sys.time() in
      let firstPolyCons = List.hd polyConstraints in
      let firstPolyConsVarsSet = firstPolyCons#get_varsSet in
      let remainingPolyConstraints = List.tl polyConstraints in
      if VariablesSet.subset firstPolyConsVarsSet assignedVarsSet then (
        (*print_endline (string_of_assignment varsTCsMap);
        flush stdout;*)
        let sat = firstPolyCons#check_SAT varsTCsMap in
        if sat then
          if remainingPolyConstraints = [] then ([], 1, [], varsTCsMap)
          else
            let newAbstractTCInfList = Cons((varsTCsMap, testCases, assignedVarsSet, remainingPolyConstraints, priorityNum), tail) in
            test_extra newAbstractTCInfList varsIntvsMiniSATCodesMap unsatPolyCons (remainingTime -. Sys.time() +. startTime)
        else
          test_extra (tail()) varsIntvsMiniSATCodesMap firstPolyCons (remainingTime -. Sys.time() +. startTime)
      )
      else match testCases with 
        | [] ->
          let firstPolyCons = List.hd polyConstraints in
          let (generatedTCs, newPriorityNum) = firstPolyCons#generateTCs assignedVarsSet varsIntvsMiniSATCodesMap priorityNum in
          let newAbstractTCInfList = Cons((varsTCsMap, generatedTCs, assignedVarsSet, polyConstraints, newPriorityNum), tail) in
          test_extra newAbstractTCInfList varsIntvsMiniSATCodesMap unsatPolyCons (remainingTime -. Sys.time() +. startTime)
        | (var, [])::remainingTCs -> 
          test_extra (tail()) varsIntvsMiniSATCodesMap unsatPolyCons (remainingTime -. Sys.time() +. startTime)
        | (var, nextTC::remainingTC)::remainingTCs -> 
          let newFirstAss = StringMap.add var nextTC varsTCsMap in
          let newAssignedVarsSet = VariablesSet.add var assignedVarsSet in
          let newAbstractTCInfList = 
            Cons((newFirstAss, remainingTCs, newAssignedVarsSet, polyConstraints, priorityNum), 
                  fun() -> Cons((varsTCsMap, (var, remainingTC)::remainingTCs, assignedVarsSet, polyConstraints, priorityNum), tail))
          in
          test_extra newAbstractTCInfList varsIntvsMiniSATCodesMap unsatPolyCons (remainingTime -. Sys.time() +. startTime)


(* This function test the list of unknow clauses, trying to find an SAT instance *)
let test polyConstraints varsIntvsMiniSATCodesMap strTestUS remainingTime =
  (*print_endline "Start Test";*)
  let startTime = Sys.time() in
  
  (*let add_miniSATCodePolyCons currentMiniSATCodesPolyConstraintsMap polyCons =
    IntMap.add polyCons#get_miniSATCode polyCons currentMiniSATCodesPolyConstraintsMap
  in
  let miniSATCodesPolyConstraintsMap = List.fold_left add_miniSATCodePolyCons IntMap.empty polyConstraints in*)
  
  (* sort the polynomial constraings using dependency, which make the additional test data generation minimal *)
  let rec find_min_additionalTCGen_polyCons checkedVarsSet checkedPolyConstraints remainingPolyConstraints currentResult currentAdditionalTCs = match remainingPolyConstraints with
    | [] -> (currentResult, checkedPolyConstraints)
    | h::t -> 
      let varsDiffNum = h#get_varsDiffNum checkedVarsSet in
      if varsDiffNum < currentAdditionalTCs then find_min_additionalTCGen_polyCons checkedVarsSet (currentResult::checkedPolyConstraints) t h varsDiffNum
      else find_min_additionalTCGen_polyCons checkedVarsSet (h::checkedPolyConstraints) t currentResult currentAdditionalTCs
  in
  let rec sort_dependency polyConstraints resultPolyConstraints checkedVarsSet = match polyConstraints with
    | [] -> resultPolyConstraints
    | h :: t -> 
       let (nextBestPolyCons, remainingPolyConstraints) = find_min_additionalTCGen_polyCons checkedVarsSet [] t h (h#get_varsDiffNum checkedVarsSet) in
       sort_dependency remainingPolyConstraints (nextBestPolyCons::resultPolyConstraints) (VariablesSet.union checkedVarsSet nextBestPolyCons#get_varsSet)
  in
  let sortedPolyConstraints = List.rev (sort_dependency polyConstraints [] VariablesSet.empty) in
  (*let print_vars polyCons = print_endline (Util.vars_to_string polyCons#get_varsList); in
  List.iter print_vars sortedPolyConstraints;
  flush stdout;*)
  (* Recursively generate test cases for each boolean expression *)
  let firstPolyCons = List.hd sortedPolyConstraints in
  let priorityNum = 10 in (* only the first $priorityNum variables are allowed to generate 2 test cases, other ones are 1 *)
  let abstractTCInfList = Cons((StringMap.empty, [], VariablesSet.empty, polyConstraints, priorityNum), fun() -> Nil) in 
  test_extra abstractTCInfList varsIntvsMiniSATCodesMap firstPolyCons (remainingTime -. Sys.time() +. startTime)
