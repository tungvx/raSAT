open Ast
open Variable
open InfiniteList
open Assignments
  

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
        (*print_endline (assignments_toString assignments);*)
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
  
  (* sort the polynomial constraings using dependency, which make the additional test data generation minimal *)
  let rec insert_sort_polyConstraints dependencySortedPolyConstraints polyCons = match dependencySortedPolyConstraints with
    | [] -> [polyCons]
    | h :: t -> 
      let varsDiffNum = polyCons#get_varsDiffNum h in
      if varsDiffNum = 0 then (
        h#set_varsDiffNumWithPreviousInTesting 0;
        polyCons :: dependencySortedPolyConstraints
      )
      else (* polyCons and h have the same number of variables *) (
        polyCons#set_varsDiffNumWithPreviousInTesting varsDiffNum;
        match t with
        | [] -> h :: [polyCons]
        | h1 :: t1 -> 
          let varsDiffNum1 = h1#get_varsDiffNumWithPreviousInTesting in
          if varsDiffNum < varsDiffNum1 then (
            (
              let newVarsDiffNum1 = h1#get_varsDiffNum polyCons in
              h1#set_varsDiffNumWithPreviousInTesting newVarsDiffNum1
            );
            h :: (polyCons::t)
          )
          else
            h :: (insert_sort_polyConstraints t polyCons)
      )
  in
  let sortedPolyConstraints = List.fold_left insert_sort_polyConstraints [] polyConstraints in
  
  (* Recursively generate test cases for each boolean expression *)
  let firstPolyCons = List.hd sortedPolyConstraints in
  let priorityNum = 20 in (* only the first $priorityNum variables are allowed to generate 2 test cases, other ones are 1 *)
  let abstractTCInfList = Cons((StringMap.empty, [], VariablesSet.empty, polyConstraints, priorityNum), fun() -> Nil) in 
  test_extra abstractTCInfList varsIntvsMiniSATCodesMap firstPolyCons (remainingTime -. Sys.time() +. startTime)
