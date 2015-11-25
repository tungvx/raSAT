open Ast
open Variable
open InfiniteList
open Assignments
open PolynomialConstraint  
open Util
open Interval
open Icp

let rec get_first polyConstraints excludeds = match polyConstraints with
  | [] -> raise (Failure "Not found")
  | polyCons :: remainings -> 
    if polyCons#get_logic = "QF_NRA" && (polyCons#isEquation = 1) && List.length remainings > 0 then 
      get_first remainings (polyCons :: excludeds)
    else
      (polyCons, remainings @ excludeds)

let rec remove aList index checkedList = 
  if index = 0 then
    match aList with
    | [] -> raise (Failure "Not found")
    | h::t1 -> (h, checkedList@t1)
  else if index > 0 then
    match aList with
    | [] -> raise (Failure "Not found")
    | h::t1 -> remove t1 (index - 1) (h::checkedList)
  else raise (Failure "Not found")


(* This function get a random element from a list, return it and the remaining list *)
let get_element inputList =
  Random.self_init();
  
  (* (1) (2) need to change (10) *)
  get_first inputList []
  
  (* (* (10) need to change line (1) and (2)*)
  let randomIndex = Random.int (List.length inputList) in
  remove inputList randomIndex [] *)

(*(* This is the helping function for the test function*)
let rec test_extra abstractTCInfList varsIntvsMiniSATCodesMap unsatPolyCons indicesSortedPolyConstraintsMap polyConstraintsNum sortedPolyConstraintsMapLength varsSATDirectionMap miniSATCodesSATPolyConstraintsMap remainingTime = 
  match abstractTCInfList with
  | Nil -> ([],-1, [unsatPolyCons],StringMap.empty, polyConstraintsNum - IntMap.cardinal miniSATCodesSATPolyConstraintsMap)
  | Cons((varsTCsMap, testCases, assignedVarsSet, testedPolyCons, currentIndex, remainingMiniSATCodePolyConstraintsMap, priorityNum), tail) ->
    if remainingTime <= 0. then ([],-1, [unsatPolyCons],StringMap.empty, polyConstraintsNum - IntMap.cardinal miniSATCodesSATPolyConstraintsMap)
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
          if sat then (
            (*print_endline ("SAT constraint: " ^ testedPolyCons#to_string_infix ^ " - miniSATCode: " ^ string_of_int testedPolyCons#get_miniSATCode);
            flush stdout;*)
            if currentIndex >= polyConstraintsNum then ([], 1, [], varsTCsMap, 0)
            else
              let miniSATCodesSATPolyConstraintsMap = IntMap.add (testedPolyCons#get_miniSATCode) testedPolyCons miniSATCodesSATPolyConstraintsMap in
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
              test_extra newAbstractTCInfList varsIntvsMiniSATCodesMap unsatPolyCons indicesSortedPolyConstraintsMap polyConstraintsNum newSortedPolyConstraintsMapLength varsSATDirectionMap miniSATCodesSATPolyConstraintsMap (remainingTime -. Sys.time() +. startTime)
          )
          else
            test_extra (tail()) varsIntvsMiniSATCodesMap testedPolyCons indicesSortedPolyConstraintsMap polyConstraintsNum sortedPolyConstraintsMapLength varsSATDirectionMap miniSATCodesSATPolyConstraintsMap (remainingTime -. Sys.time() +. startTime)
        )
        else 
          let (generatedTCs, newPriorityNum) = testedPolyCons#generateTCs assignedVarsSet varsIntvsMiniSATCodesMap priorityNum varsSATDirectionMap in
          let newAbstractTCInfList = Cons((varsTCsMap, generatedTCs, assignedVarsSet, testedPolyCons, currentIndex, remainingMiniSATCodePolyConstraintsMap, newPriorityNum), tail) in
          test_extra newAbstractTCInfList varsIntvsMiniSATCodesMap unsatPolyCons indicesSortedPolyConstraintsMap polyConstraintsNum sortedPolyConstraintsMapLength varsSATDirectionMap miniSATCodesSATPolyConstraintsMap (remainingTime -. Sys.time() +. startTime)  
      | (var, [])::remainingTCs -> 
          test_extra (tail()) varsIntvsMiniSATCodesMap unsatPolyCons indicesSortedPolyConstraintsMap polyConstraintsNum sortedPolyConstraintsMapLength varsSATDirectionMap miniSATCodesSATPolyConstraintsMap (remainingTime -. Sys.time() +. startTime)
      | (var, nextTC::remainingTC)::remainingTCs -> 
          let newFirstAss = StringMap.add var nextTC varsTCsMap in
          let newAssignedVarsSet = VariablesSet.add var assignedVarsSet in
          let newAbstractTCInfList = 
            Cons((newFirstAss, remainingTCs, newAssignedVarsSet, testedPolyCons, currentIndex, remainingMiniSATCodePolyConstraintsMap, priorityNum), 
                  fun() -> Cons((varsTCsMap, (var, remainingTC)::remainingTCs, assignedVarsSet, testedPolyCons, currentIndex, remainingMiniSATCodePolyConstraintsMap, priorityNum), tail))
          in
          test_extra newAbstractTCInfList varsIntvsMiniSATCodesMap unsatPolyCons indicesSortedPolyConstraintsMap polyConstraintsNum sortedPolyConstraintsMapLength varsSATDirectionMap miniSATCodesSATPolyConstraintsMap (remainingTime -. Sys.time() +. startTime)*)
      
      
      
(* (* This is the helping function for the test function*)
let rec test_extra abstractTCInfList varsIntvsMiniSATCodesMap unsatPolyCons polyConstraintsNum varsSATDirectionMap miniSATCodesSATPolyConstraintsMap remainingTime = 
  match abstractTCInfList with
  | Nil -> ([],-1, [unsatPolyCons],StringMap.empty, polyConstraintsNum - IntMap.cardinal miniSATCodesSATPolyConstraintsMap)
  | Cons((varsTCsMap, testCases, assignedVarsSet, testedPolyCons, remainingPolyConstraints, priorityNum), tail) ->
    if remainingTime <= 0. (* || testedPolyCons#isInfinite *) then ([],-1, [unsatPolyCons],StringMap.empty, polyConstraintsNum - IntMap.cardinal miniSATCodesSATPolyConstraintsMap)
    else 
      let startTime = Sys.time() in
      match testCases with
      | [] -> (* Testing for some apis are implemented here, or testcases will be generated *)
        let sat = testedPolyCons#check_SAT varsTCsMap in
        (* print_endline ("\nTesting: " ^ testedPolyCons#to_string_infix ^ " - miniSATCode: " ^ string_of_int testedPolyCons#get_miniSATCode ^ " - easiness: " ^ string_of_float testedPolyCons#get_easiness);
        flush stdout; *)
        if sat then (
          (*print_endline ("SAT constraint: " ^ testedPolyCons#to_string_infix ^ " - miniSATCode: " ^ string_of_int testedPolyCons#get_miniSATCode ^ " - easiness: " ^ string_of_float testedPolyCons#get_easiness);
          flush stdout;*)
          match remainingPolyConstraints with
          | [] -> ([], 1, [], varsTCsMap, 0)
          | h::t ->
            let miniSATCodesSATPolyConstraintsMap = IntMap.add (testedPolyCons#get_miniSATCode) testedPolyCons miniSATCodesSATPolyConstraintsMap in
            let (selectedPolyCons, newRemainingPolyConstraints) = get_element remainingPolyConstraints in
            let (generatedTCs, newPriorityNum) = selectedPolyCons#generateTCs assignedVarsSet varsIntvsMiniSATCodesMap priorityNum varsSATDirectionMap in
            let newAbstractTCInfList = Cons((varsTCsMap, generatedTCs, assignedVarsSet, selectedPolyCons, newRemainingPolyConstraints, newPriorityNum), tail) in
            test_extra newAbstractTCInfList varsIntvsMiniSATCodesMap unsatPolyCons polyConstraintsNum varsSATDirectionMap miniSATCodesSATPolyConstraintsMap (remainingTime -. Sys.time() +. startTime)
        )
        else (
          (*print_endline ("UNSAT constraint: " ^ testedPolyCons#to_string_infix ^ " - miniSATCode: " ^ string_of_int testedPolyCons#get_miniSATCode ^ " - easiness: " ^ string_of_float testedPolyCons#get_easiness);
          flush stdout;*)
          let newUnsatPolyCons =
            if IntMap.mem testedPolyCons#get_miniSATCode miniSATCodesSATPolyConstraintsMap then unsatPolyCons
            else testedPolyCons
          in
          test_extra (tail()) varsIntvsMiniSATCodesMap newUnsatPolyCons polyConstraintsNum varsSATDirectionMap miniSATCodesSATPolyConstraintsMap (remainingTime -. Sys.time() +. startTime)
        )
      | (var, [])::remainingTCs -> 
          test_extra (tail()) varsIntvsMiniSATCodesMap unsatPolyCons polyConstraintsNum varsSATDirectionMap miniSATCodesSATPolyConstraintsMap (remainingTime -. Sys.time() +. startTime)
      | (var, nextTC::remainingTC)::remainingTCs -> 
          let newFirstAss = StringMap.add var nextTC varsTCsMap in
          let newAssignedVarsSet = VariablesSet.add var assignedVarsSet in
          let newAbstractTCInfList = 
            Cons((newFirstAss, remainingTCs, newAssignedVarsSet, testedPolyCons, remainingPolyConstraints, priorityNum), 
                  fun() -> Cons((varsTCsMap, (var, remainingTC)::remainingTCs, assignedVarsSet, testedPolyCons, remainingPolyConstraints, priorityNum), tail))
          in
          test_extra newAbstractTCInfList varsIntvsMiniSATCodesMap unsatPolyCons polyConstraintsNum varsSATDirectionMap miniSATCodesSATPolyConstraintsMap (remainingTime -. Sys.time() +. startTime)
 *)

(* This is the helping function for the incremental test function*)
let rec test_extra_incremental abstractTCInfList varsIntvsMiniSATCodesMap testSATPolyConstraints testUNSATPolyConstraints satVarsTCsMap generatedVarsSet polyConstraintsNum varsSATDirectionMap = 
  match abstractTCInfList with
  | Nil -> (-1, testSATPolyConstraints, testUNSATPolyConstraints,satVarsTCsMap, generatedVarsSet)
  | Cons((varsTCsMap, testCases, assignedVarsSet, testedPolyCons, remainingPolyConstraints, priorityNum), tail) ->    
    let startTime = Sys.time() in
    match testCases with
    | [] -> (* Testing for some apis are implemented here, or testcases will be generated *)
      let sat = testedPolyCons#check_SAT varsTCsMap in
      (* print_endline ("\nTesting: " ^ testedPolyCons#to_string_infix ^ " - miniSATCode: " ^ string_of_int testedPolyCons#get_miniSATCode ^ " - easiness: " ^ string_of_float testedPolyCons#get_easiness);
      flush stdout; *)
      if sat then (
        (* print_endline ("SAT constraint: " ^ testedPolyCons#to_string_infix ^ " - miniSATCode: " ^ string_of_int testedPolyCons#get_miniSATCode ^ " - easiness: " ^ string_of_float testedPolyCons#get_easiness);
        flush stdout; *)
        match remainingPolyConstraints with
        | [] -> (1, testedPolyCons :: testSATPolyConstraints, [], varsTCsMap, generatedVarsSet)
        | _ ->
          let (selectedPolyCons, newRemainingPolyConstraints) = get_element remainingPolyConstraints in
          let (generatedTCs, newPriorityNum) = selectedPolyCons#generateTCs assignedVarsSet varsIntvsMiniSATCodesMap priorityNum varsSATDirectionMap in
          let newAbstractTCInfList = Cons((varsTCsMap, generatedTCs, assignedVarsSet, selectedPolyCons, newRemainingPolyConstraints, newPriorityNum), fun() -> Nil) in
          test_extra_incremental newAbstractTCInfList varsIntvsMiniSATCodesMap (testedPolyCons :: testSATPolyConstraints) remainingPolyConstraints varsTCsMap assignedVarsSet polyConstraintsNum varsSATDirectionMap
      )
      else (
        (* print_endline ("UNSAT constraint: " ^ testedPolyCons#to_string_infix ^ " - miniSATCode: " ^ string_of_int testedPolyCons#get_miniSATCode ^ " - easiness: " ^ string_of_float testedPolyCons#get_easiness);
        flush stdout; *)
        test_extra_incremental (tail()) varsIntvsMiniSATCodesMap testSATPolyConstraints testUNSATPolyConstraints satVarsTCsMap generatedVarsSet polyConstraintsNum varsSATDirectionMap 
      )
    | (var, [])::remainingTCs -> 
        test_extra_incremental (tail()) varsIntvsMiniSATCodesMap testSATPolyConstraints (testedPolyCons :: remainingPolyConstraints) satVarsTCsMap generatedVarsSet polyConstraintsNum varsSATDirectionMap 
    | (var, nextTC::remainingTC)::remainingTCs -> 
        let newFirstAss = StringMap.add var nextTC varsTCsMap in
        let newAssignedVarsSet = VariablesSet.add var assignedVarsSet in
        let newAbstractTCInfList = 
          Cons((newFirstAss, remainingTCs, newAssignedVarsSet, testedPolyCons, remainingPolyConstraints, priorityNum), 
                fun() -> Cons((varsTCsMap, (var, remainingTC)::remainingTCs, assignedVarsSet, testedPolyCons, remainingPolyConstraints, priorityNum), tail))
        in
        test_extra_incremental newAbstractTCInfList varsIntvsMiniSATCodesMap testSATPolyConstraints remainingPolyConstraints satVarsTCsMap generatedVarsSet polyConstraintsNum varsSATDirectionMap 
      

(* let rec test_icp polyConstraints varsIntvsMap varsTCsMap testedVars esl remainingTime =
  match polyConstraints with 
  | [] -> ([], 1, [], varsTCsMap, 0)
  | _ -> 
    (* Get information about SAT direction of variables *)
    let startTime = Sys.time() in
    (* let add_sat_direction currentMap polyCons =
      (*print_endline ("Testing: " ^ polyCons#to_string_infix);
      flush stdout;*)
      polyCons#add_sat_direction currentMap
    in
    let varsSATDirectionMap = List.fold_left add_sat_direction StringMap.empty polyConstraints in *)
    let (firstPolyCons, remainingPolyConstraints) = get_element polyConstraints in
    let varDiff = firstPolyCons#get_varsDiff testedVars in
    if VariablesSet.is_empty varDiff then ([],-1, [firstPolyCons],StringMap.empty, 0)
    else 
      let [(var, _, isVarPositiveDirected)] = firstPolyCons#get_n_varsSen_fromSet 1 varDiff in
      let intv = StringMap.find var varsIntvsMap in
      (* let isVarPositiveDirected = StringMap.find var varsSATDirectionMap in *)
      let tc = firstPolyCons#generate_tc_var intv isVarPositiveDirected in
      if intv.low <= tc && tc <= intv.high then 
        let varsTCsMap = StringMap.add var tc varsTCsMap in
        let varsIntvsMap = StringMap.add var {low=tc;high=tc} varsIntvsMap in
        let (res, _, uk_cl, _, varsIntvsMap, _, _) = icp 1 IntSet.empty [] [] polyConstraints varsIntvsMap esl 0. 0. (remainingTime -. Sys.time() +. startTime) in
        if res = 1 then ([], 1, [], varsTCsMap, 0)
        else if res = -1 then ([],-1, [firstPolyCons], StringMap.empty, 0)
        else test_icp uk_cl varsIntvsMap varsTCsMap (VariablesSet.add var testedVars) esl (remainingTime -. Sys.time() +. startTime)
      else ([],-1, [firstPolyCons],StringMap.empty, 0) *)

(* This function test the list of unknow clauses, trying to find an SAT instance *)
let rec test polyConstraints varsIntvsMaps =
  (* print_endline "\n\nStart Testing"; *)
  let startTime = Sys.time() in

  (* print_endline ("\n\n\nStart testing: " ^ string_infix_of_polynomialConstraints polyConstraints);
  flush stdout; *)
  
  (* Get information about SAT direction of variables *)
  let add_sat_direction currentMap polyCons =
    (*print_endline ("Testing: " ^ polyCons#to_string_infix);
    flush stdout;*)
    polyCons#add_sat_direction currentMap
  in
  let varsSATDirectionMap = List.fold_left add_sat_direction StringMap.empty polyConstraints in
  (* let print_varSATDirection var isPositiveDirected =
    print_endline (var ^ ": " ^ string_of_int isPositiveDirected);
    flush stdout;
  in
  StringMap.iter print_varSATDirection varsSATDirectionMap; *)
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
  (* print_endline ("Number of Tested Constraints: " ^ string_of_int polyConstraintsNum);
  flush stdout; *)
  let (firstPolyCons, remainingPolyConstraints) = get_element polyConstraints in
  (* print_endline ("\n\nSelecting api: " ^ firstPolyCons#to_string_infix);
  print_endline ("Variables sensitivity: " ^ firstPolyCons#string_of_varsSen);
  print_string ("Selecting variables for multiple test cases: ");
  flush stdout; *)
  (*let add_miniSATCodePolyCons miniSATCodesPolyConstraintsMap polyCons =
    (*print_endline ("MiniSATCode: " ^ string_of_int polyCons#get_miniSATCode);
    flush stdout;*)
    IntMap.add polyCons#get_miniSATCode polyCons miniSATCodesPolyConstraintsMap
  in
  let remainingMiniSATCodesPolyConstraintsMap = List.fold_left add_miniSATCodePolyCons IntMap.empty remainingPolyConstraints in*)
  (*print_endline ("Number of remaining Constraints: " ^ string_of_int (IntMap.cardinal remainingMiniSATCodesPolyConstraintsMap));
  flush stdout;*)
  (*let indicesSortedPolyConstraintsMap = IntMap.add 1 firstPolyCons IntMap.empty in*)
  let priorityNum = 10 in (* only the first $priorityNum variables are allowed to generate 2 test cases, other ones are 1 *)
  let (generatedTCs, newPriorityNum) = firstPolyCons#generateTCs VariablesSet.empty varsIntvsMaps priorityNum varsSATDirectionMap in
  let abstractTCInfList = Cons((StringMap.empty, generatedTCs, VariablesSet.empty, firstPolyCons, (*1, remainingMiniSATCodesPolyConstraintsMap*) remainingPolyConstraints, newPriorityNum), fun() -> Nil) in 
  (*let (tc, sTest, clTest_US, varsTCsMap, b) = test_extra abstractTCInfList varsIntvsMaps firstPolyCons (*indicesSortedPolyConstraintsMap*) polyConstraintsNum (*1*) varsSATDirectionMap IntMap.empty (remainingTime -. Sys.time() +. startTime) in*)
  let (sTest, testSATPolyConstraints, testUNSATPolyConstraints, satVarsTCsMap, generatedVarsSet) = test_extra_incremental abstractTCInfList varsIntvsMaps [] [] StringMap.empty VariablesSet.empty (*indicesSortedPolyConstraintsMap*) polyConstraintsNum (*1*) varsSATDirectionMap in
  if sTest = 1 || (is_all_equations testUNSATPolyConstraints && can_apply_imvt testUNSATPolyConstraints ) then
   (*  let printString = "Test Result: " ^ string_of_int sTest in
    print_endline printString;
    print_endline ("test-SAT: " ^ string_infix_of_polynomialConstraints testSATPolyConstraints);
    flush stdout;
    print_endline ("test-UNSAT: " ^ string_infix_of_polynomialConstraints testUNSATPolyConstraints);
    flush stdout; *)
    verify_SAT testSATPolyConstraints sTest [] testUNSATPolyConstraints satVarsTCsMap VariablesSet.empty
  else (sTest, testSATPolyConstraints, testUNSATPolyConstraints, satVarsTCsMap, generatedVarsSet)
  
and verify_SAT polyConstraints result testSATPolyConstraints testUNSATPolyConstraints satVarsTCsMap generatedVarsSet = match polyConstraints with
  | [] -> (result, testSATPolyConstraints, testUNSATPolyConstraints, satVarsTCsMap, generatedVarsSet)
  | polyCons :: remainingPolyConstraints -> 
    let sat = polyCons#verify_SAT satVarsTCsMap in
    if sat = 1 then 
      let generatedVarsSet = VariablesSet.union generatedVarsSet polyCons#get_varsSet in
      verify_SAT remainingPolyConstraints result (polyCons :: testSATPolyConstraints) testUNSATPolyConstraints satVarsTCsMap generatedVarsSet
    else verify_SAT remainingPolyConstraints (-1) testSATPolyConstraints (polyCons :: testUNSATPolyConstraints) satVarsTCsMap generatedVarsSet
  
(*  
(* incremental test *)  
let rec test_incrementally polyConstraints varsIntvsMiniSATCodesMap remainingTime = match polyConstraints with
  | [] -> ([], 1, [], varsTCsMap, 0)
  | h::t -> *)
