open Ast
open Variable
open InfiniteList
open Assignments
open PolynomialConstraint  
open Util


(* This function extract the next polynomials in which the number of additional generated test case will be minimal *)
(* a tupple of best polynomial constraints and the list of remaining polynomial constraints will be returned *)
let rec find_min_additionalTCGen_polyCons checkedVarsSet checkedPolyConstraints remainingPolyConstraints currentResult currentAdditionalTCs = match remainingPolyConstraints with
    | [] -> (currentResult, checkedPolyConstraints)
    | h::t -> 
      let varsDiffNum = h#get_varsDiffNum checkedVarsSet in
      if varsDiffNum < currentAdditionalTCs then find_min_additionalTCGen_polyCons checkedVarsSet (currentResult::checkedPolyConstraints) t h varsDiffNum
      else find_min_additionalTCGen_polyCons checkedVarsSet (h::checkedPolyConstraints) t currentResult currentAdditionalTCs


let rec remove_polyCons_byMiniSATCode miniSATCode polyConstraints = match polyConstraints with
  | [] -> []
  | h :: t ->
    if h#get_miniSATCode = miniSATCode then t
    else h :: (remove_polyCons_byMiniSATCode miniSATCode t)


(* This is the helping function for the test function*)
let rec test_extra abstractTCInfList varsIntvsMiniSATCodesMap unsatPolyCons indicesSortedPolyConstraintsMap polyConstraintsNum sortedPolyConstraintsMapLength remainingTime = 
  match abstractTCInfList with
  | Nil -> ([],-1, [unsatPolyCons],StringMap.empty)
  | Cons((varsTCsMap, testCases, assignedVarsSet, testedPolyCons, currentIndex, remainingPolyConstraints, priorityNum), tail) ->
    if remainingTime <= 0. then ([],-1, [unsatPolyCons],StringMap.empty)
    else 
      let startTime = Sys.time() in
      match testCases with
      | [] -> (* Testing for some apis are implemented here, or testcases will be generated *)
        let testedPolyConsVarsSet = testedPolyCons#get_varsSet in
        if VariablesSet.subset testedPolyConsVarsSet assignedVarsSet then (
          (*print_endline (string_of_assignment varsTCsMap);
          flush stdout;*)
          let sat = testedPolyCons#check_SAT varsTCsMap in
          if sat then
            if currentIndex >= polyConstraintsNum then ([], 1, [], varsTCsMap)
            else
              let nextIndex = currentIndex + 1 in
              let (nextBestPolyCons, remainingPolyConstraints, indicesSortedPolyConstraintsMap, newSortedPolyConstraintsMapLength) = 
                try 
                  let alreadySortedBestPolyCons = IntMap.find nextIndex indicesSortedPolyConstraintsMap in
                  (*print_endline ("Got: " ^ string_of_int nextIndex ^ " with " ^ string_of_int sortedPolyConstraintsMapLength ^ " sorted over " ^ string_of_int polyConstraintsNum);
                  flush stdout;*)
                  let nextRemainingPolyConstraints = 
                    if polyConstraintsNum = sortedPolyConstraintsMapLength then []
                    else remove_polyCons_byMiniSATCode alreadySortedBestPolyCons#get_miniSATCode remainingPolyConstraints 
                  in
                  (alreadySortedBestPolyCons, nextRemainingPolyConstraints, indicesSortedPolyConstraintsMap, sortedPolyConstraintsMapLength)
                with Not_found -> (* here, remainingPolyConstraints must not be empty *)
                  (*print_endline ("Getting new tested constraints: " ^ string_of_int nextIndex);
                  flush stdout;*)
                  let h = List.hd remainingPolyConstraints in
                  let t = List.tl remainingPolyConstraints in
                  (*print_endline "End getting new tested constraints";
                  flush stdout;*)
                  let (tmpBestPolyCons, tmpRemainingPolyConstraints) = find_min_additionalTCGen_polyCons assignedVarsSet [] t h (h#get_varsDiffNum assignedVarsSet) in
                  let newIndicesSortedPolyConstraintsMap = IntMap.add nextIndex tmpBestPolyCons indicesSortedPolyConstraintsMap in
                  (tmpBestPolyCons, tmpRemainingPolyConstraints, newIndicesSortedPolyConstraintsMap, sortedPolyConstraintsMapLength + 1)
              in
              let newAbstractTCInfList = Cons((varsTCsMap, testCases, assignedVarsSet, nextBestPolyCons, nextIndex, remainingPolyConstraints, priorityNum), tail) in
              test_extra newAbstractTCInfList varsIntvsMiniSATCodesMap unsatPolyCons indicesSortedPolyConstraintsMap polyConstraintsNum newSortedPolyConstraintsMapLength (remainingTime -. Sys.time() +. startTime)
          else
            test_extra (tail()) varsIntvsMiniSATCodesMap testedPolyCons indicesSortedPolyConstraintsMap polyConstraintsNum sortedPolyConstraintsMapLength (remainingTime -. Sys.time() +. startTime)
        )
        else 
          let (generatedTCs, newPriorityNum) = testedPolyCons#generateTCs assignedVarsSet varsIntvsMiniSATCodesMap priorityNum in
          let newAbstractTCInfList = Cons((varsTCsMap, generatedTCs, assignedVarsSet, testedPolyCons, currentIndex, remainingPolyConstraints, newPriorityNum), tail) in
          test_extra newAbstractTCInfList varsIntvsMiniSATCodesMap unsatPolyCons indicesSortedPolyConstraintsMap polyConstraintsNum sortedPolyConstraintsMapLength (remainingTime -. Sys.time() +. startTime)  
      | (var, [])::remainingTCs -> 
          test_extra (tail()) varsIntvsMiniSATCodesMap unsatPolyCons indicesSortedPolyConstraintsMap polyConstraintsNum sortedPolyConstraintsMapLength (remainingTime -. Sys.time() +. startTime)
      | (var, nextTC::remainingTC)::remainingTCs -> 
          let newFirstAss = StringMap.add var nextTC varsTCsMap in
          let newAssignedVarsSet = VariablesSet.add var assignedVarsSet in
          let newAbstractTCInfList = 
            Cons((newFirstAss, remainingTCs, newAssignedVarsSet, testedPolyCons, currentIndex, remainingPolyConstraints, priorityNum), 
                  fun() -> Cons((varsTCsMap, (var, remainingTC)::remainingTCs, assignedVarsSet, testedPolyCons, currentIndex, remainingPolyConstraints, priorityNum), tail))
          in
          test_extra newAbstractTCInfList varsIntvsMiniSATCodesMap unsatPolyCons indicesSortedPolyConstraintsMap polyConstraintsNum sortedPolyConstraintsMapLength (remainingTime -. Sys.time() +. startTime)
      

(* This function test the list of unknow clauses, trying to find an SAT instance *)
let test polyConstraints varsIntvsMiniSATCodesMap strTestUS remainingTime =
  (*print_endline "Start Test";*)
  let startTime = Sys.time() in
  
  (* sort the polynomial constraings using dependency, which make the additional test data generation minimal *)
  (*let rec find_min_additionalTCGen_polyCons checkedVarsSet checkedPolyConstraints remainingPolyConstraints currentResult currentAdditionalTCs = match remainingPolyConstraints with
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
  let sortedPolyConstraints = List.rev (sort_dependency polyConstraints [] VariablesSet.empty) in*)
  (*let print_vars polyCons = print_endline (Util.vars_to_string polyCons#get_varsList); in
  List.iter print_vars sortedPolyConstraints;
  flush stdout;*)
  (* Recursively generate test cases for each boolean expression *)
  let polyConstraintsNum = List.length polyConstraints in
  (*print_endline ("Number of Tested Constraints: " ^ string_of_int polyConstraintsNum);
  flush stdout;*)
  let firstPolyCons = List.hd polyConstraints in
  let remainingPolyConstraints = List.tl polyConstraints in
  let indicesSortedPolyConstraintsMap = IntMap.add 1 firstPolyCons IntMap.empty in
  let priorityNum = 10 in (* only the first $priorityNum variables are allowed to generate 2 test cases, other ones are 1 *)
  let abstractTCInfList = Cons((StringMap.empty, [], VariablesSet.empty, firstPolyCons, 1, remainingPolyConstraints, priorityNum), fun() -> Nil) in 
  test_extra abstractTCInfList varsIntvsMiniSATCodesMap firstPolyCons indicesSortedPolyConstraintsMap polyConstraintsNum 1 (remainingTime -. Sys.time() +. startTime)
