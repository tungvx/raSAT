open Ast
open Variable
open InfiniteList
open Assignments
open PolynomialConstraint  
open Util


(* This is the helping function for the test function*)
let rec test_extra abstractTCInfList varsIntvsMiniSATCodesMap unsatPolyCons indicesSortedPolyConstraintsMap polyConstraintsNum sortedPolyConstraintsMapLength varsSATDirectionMap remainingTime = 
  match abstractTCInfList with
  | Nil -> ([],-1, [unsatPolyCons],StringMap.empty)
  | Cons((varsTCsMap, testCases, assignedVarsSet, testedPolyCons, currentIndex, remainingMiniSATCodePolyConstraintsMap, priorityNum), tail) ->
    if remainingTime <= 0. then ([],-1, [unsatPolyCons],StringMap.empty)
    else 
      let startTime = Sys.time() in
      match testCases with
      | [] -> (* Testing for some apis are implemented here, or testcases will be generated *)
        let testedPolyConsVarsSet = testedPolyCons#get_varsSet in
        if VariablesSet.subset testedPolyConsVarsSet assignedVarsSet then (
          (*print_endline (string_of_assignment varsTCsMap);
          flush stdout;*)
          (*print_varsSet testedPolyConsVarsSet;*)
          (*print_varsSet assignedVarsSet;*)
          (*print_endline ("Needed: " ^ varsSet_to_string testedPolyConsVarsSet);
          print_endline ("Generated: " ^ varsSet_to_string assignedVarsSet);
          flush stdout;*)
          let sat = testedPolyCons#check_SAT varsTCsMap in
          if sat then
            if currentIndex >= polyConstraintsNum then ([], 1, [], varsTCsMap)
            else
              let nextIndex = currentIndex + 1 in
              let (nextBestPolyCons, remainingMiniSATCodePolyConstraintsMap, indicesSortedPolyConstraintsMap, newSortedPolyConstraintsMapLength) = 
                try 
                  let alreadySortedBestPolyCons = IntMap.find nextIndex indicesSortedPolyConstraintsMap in
                  (*print_endline ("Got: " ^ string_of_int nextIndex ^ " with " ^ string_of_int sortedPolyConstraintsMapLength ^ " sorted over " ^ string_of_int polyConstraintsNum);
                  flush stdout;*)
                  let nextRemainingMiniSATCodesPolyConstraintsMap = 
                    if polyConstraintsNum <= sortedPolyConstraintsMapLength then IntMap.empty
                    else IntMap.remove alreadySortedBestPolyCons#get_miniSATCode remainingMiniSATCodePolyConstraintsMap 
                  in
                  (alreadySortedBestPolyCons, nextRemainingMiniSATCodesPolyConstraintsMap, indicesSortedPolyConstraintsMap, sortedPolyConstraintsMapLength)
                with Not_found -> (* here, remainingMiniSATCodePolyConstraintsMap must not be empty *)
                  (* add a polynomial constraint as either the best choice or add it to the map of remainings *)
                  let process_miniSATCode_polyCons miniSATCode polyCons (currentBestPolyCons, currentAdditionalTCs, remainingMiniSATCodesPolyConstraintsMap) =
                    let varsDiffNum = polyCons#get_varsDiffNum assignedVarsSet in
                    (*print_endline ("VarsDiffNum: " ^ string_of_int varsDiffNum);
                    flush stdout;*)
                    if varsDiffNum < currentAdditionalTCs then (
                      let currentMiniSATCode = currentBestPolyCons#get_miniSATCode in
                      (*print_endline ("CurrentMiniSATCode: " ^ string_of_int currentMiniSATCode);*)
                      let newRemainingMiniSATCodesPolyConstraintsMap = IntMap.add currentMiniSATCode currentBestPolyCons remainingMiniSATCodesPolyConstraintsMap in
                      (*print_endline "new solution";
                      flush stdout;*)
                      (polyCons, varsDiffNum, newRemainingMiniSATCodesPolyConstraintsMap)
                    )
                    else (
                      (*print_endline "old solution";
                      flush stdout;*)
                      (currentBestPolyCons, currentAdditionalTCs, IntMap.add miniSATCode polyCons remainingMiniSATCodesPolyConstraintsMap)
                    )
                  in
                  (*print_endline ("Remaining Num of consraints: " ^ string_of_int (IntMap.cardinal remainingMiniSATCodePolyConstraintsMap));
                  flush stdout;*)
                  let (_, randomPolyCons) = IntMap.choose remainingMiniSATCodePolyConstraintsMap in
                  (*print_endline "Finish random taking";
                  flush stdout;*)
                  let (tmpBestPolyCons, _, tmpRemainingMiniSATCodesPolyConstraintsMap) = IntMap.fold process_miniSATCode_polyCons (IntMap.remove randomPolyCons#get_miniSATCode remainingMiniSATCodePolyConstraintsMap)
                              (randomPolyCons, randomPolyCons#get_varsDiffNum assignedVarsSet ,IntMap.empty) in
                  (*print_endline "Finish finding optimal solution";
                  flush stdout;*)
                  (*let (_, tmpBestPolyCons) = IntMap.min_binding remainingMiniSATCodePolyConstraintsMap in
                  let tmpRemainingMiniSATCodesPolyConstraintsMap = IntMap.remove tmpBestPolyCons#get_miniSATCode remainingMiniSATCodePolyConstraintsMap in*)
                  let newIndicesSortedPolyConstraintsMap = IntMap.add nextIndex tmpBestPolyCons indicesSortedPolyConstraintsMap in
                  (tmpBestPolyCons, tmpRemainingMiniSATCodesPolyConstraintsMap, newIndicesSortedPolyConstraintsMap, sortedPolyConstraintsMapLength + 1)
              in
              (*print_endline ("\n\nSelecting api: " ^ nextBestPolyCons#to_string_infix);
              print_endline ("Variables sensitivity: " ^ nextBestPolyCons#string_of_varsSen);
              print_string ("Selecting variables for multiple test cases: ");
              flush stdout;*)
              let newAbstractTCInfList = Cons((varsTCsMap, testCases, assignedVarsSet, nextBestPolyCons, nextIndex, remainingMiniSATCodePolyConstraintsMap, priorityNum), tail) in
              test_extra newAbstractTCInfList varsIntvsMiniSATCodesMap unsatPolyCons indicesSortedPolyConstraintsMap polyConstraintsNum newSortedPolyConstraintsMapLength varsSATDirectionMap (remainingTime -. Sys.time() +. startTime)
          else
            test_extra (tail()) varsIntvsMiniSATCodesMap testedPolyCons indicesSortedPolyConstraintsMap polyConstraintsNum sortedPolyConstraintsMapLength varsSATDirectionMap (remainingTime -. Sys.time() +. startTime)
        )
        else 
          let (generatedTCs, newPriorityNum) = testedPolyCons#generateTCs assignedVarsSet varsIntvsMiniSATCodesMap priorityNum varsSATDirectionMap in
          let newAbstractTCInfList = Cons((varsTCsMap, generatedTCs, assignedVarsSet, testedPolyCons, currentIndex, remainingMiniSATCodePolyConstraintsMap, newPriorityNum), tail) in
          test_extra newAbstractTCInfList varsIntvsMiniSATCodesMap unsatPolyCons indicesSortedPolyConstraintsMap polyConstraintsNum sortedPolyConstraintsMapLength varsSATDirectionMap (remainingTime -. Sys.time() +. startTime)  
      | (var, [])::remainingTCs -> 
          test_extra (tail()) varsIntvsMiniSATCodesMap unsatPolyCons indicesSortedPolyConstraintsMap polyConstraintsNum sortedPolyConstraintsMapLength varsSATDirectionMap (remainingTime -. Sys.time() +. startTime)
      | (var, nextTC::remainingTC)::remainingTCs -> 
          let newFirstAss = StringMap.add var nextTC varsTCsMap in
          let newAssignedVarsSet = VariablesSet.add var assignedVarsSet in
          let newAbstractTCInfList = 
            Cons((newFirstAss, remainingTCs, newAssignedVarsSet, testedPolyCons, currentIndex, remainingMiniSATCodePolyConstraintsMap, priorityNum), 
                  fun() -> Cons((varsTCsMap, (var, remainingTC)::remainingTCs, assignedVarsSet, testedPolyCons, currentIndex, remainingMiniSATCodePolyConstraintsMap, priorityNum), tail))
          in
          test_extra newAbstractTCInfList varsIntvsMiniSATCodesMap unsatPolyCons indicesSortedPolyConstraintsMap polyConstraintsNum sortedPolyConstraintsMapLength varsSATDirectionMap (remainingTime -. Sys.time() +. startTime)
      

(* This function test the list of unknow clauses, trying to find an SAT instance *)
let test polyConstraints varsIntvsMiniSATCodesMap strTestUS remainingTime =
  (*print_endline "Start Test";*)
  let startTime = Sys.time() in
  
  (* Get information about SAT direction of variables *)
  let add_sat_direction currentMap polyCons =
    (*print_endline ("Testing: " ^ polyCons#to_string_infix);
    flush stdout;*)
    polyCons#add_sat_direction currentMap
  in
  let varsSATDirectionMap = List.fold_left add_sat_direction StringMap.empty polyConstraints in
  (*let print_varSATDirection var isPositiveDirected =
    print_endline (var ^ ": " ^ string_of_int isPositiveDirected);
    flush stdout;
  in
  StringMap.iter print_varSATDirection varsSATDirectionMap;*)
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
  (*print_endline ("\n\nSelecting api: " ^ firstPolyCons#to_string_infix);
  print_endline ("Variables sensitivity: " ^ firstPolyCons#string_of_varsSen);
  print_string ("Selecting variables for multiple test cases: ");
  flush stdout;*)
  let remainingPolyConstraints = List.tl polyConstraints in
  let add_miniSATCodePolyCons miniSATCodesPolyConstraintsMap polyCons =
    (*print_endline ("MiniSATCode: " ^ string_of_int polyCons#get_miniSATCode);
    flush stdout;*)
    IntMap.add polyCons#get_miniSATCode polyCons miniSATCodesPolyConstraintsMap
  in
  let remainingMiniSATCodesPolyConstraintsMap = List.fold_left add_miniSATCodePolyCons IntMap.empty remainingPolyConstraints in
  (*print_endline ("Number of remaining Constraints: " ^ string_of_int (IntMap.cardinal remainingMiniSATCodesPolyConstraintsMap));
  flush stdout;*)
  let indicesSortedPolyConstraintsMap = IntMap.add 1 firstPolyCons IntMap.empty in
  let priorityNum = 5 in (* only the first $priorityNum variables are allowed to generate 2 test cases, other ones are 1 *)
  let abstractTCInfList = Cons((StringMap.empty, [], VariablesSet.empty, firstPolyCons, 1, remainingMiniSATCodesPolyConstraintsMap, priorityNum), fun() -> Nil) in 
  test_extra abstractTCInfList varsIntvsMiniSATCodesMap firstPolyCons indicesSortedPolyConstraintsMap polyConstraintsNum 1 varsSATDirectionMap (remainingTime -. Sys.time() +. startTime)
