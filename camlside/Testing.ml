open Ast
open Variable
open InfiniteList
open Assignments
open PolynomialConstraint  
open Util
open Interval

(* ----------Start function generate parewise testcase-----------*) 
let pareWiseTesting (passVariables:(float Variable.StringMap.t))
(varsList:string list)
(intervalVarMap:(Interval.interval Variable.StringMap.t))=
  let finalTestCase  = ref [] in 
          let testCaseTemp =[[ "L";"L";"L";"L";"L";"L";"L";"L";"L";"L"];
                              [ "L";"U";"U";"U";"U";"U";"U";"U";"U";"U"];
                              [ "U";"L";"U";"L";"U";"L";"U";"L";"U";"L"];
                              [ "U";"U";"L";"U";"L";"U";"L";"U";"L";"U"];
                              [ "L";"L";"L";"U";"U";"L";"L";"U";"U";"L"];
                              [ "L";"U";"U";"L";"L";"U";"U";"L";"L";"U"];
                              [ "U";"L";"U";"U";"L";"U";"L";"L";"U";"L"];
                              [ "U";"U";"L";"L";"U";"L";"U";"U";"L";"U"];
                              [ "L";"L";"L";"L";"L";"U";"U";"U";"U";"U"];
                              [ "L";"U";"U";"U";"U";"L";"L";"L";"L";"L"]] in
          (* Start function loop column in testCaseTemp*)
         let countLoop : int ref =ref 0 in
         let refRowTC = ref StringMap.empty in
         refRowTC := passVariables;
         let rec loopColumnTestTemp listTestTemp varsList intervalVarMap passVariables=
          let resultTestcase = passVariables in
          match listTestTemp with
            | [] -> resultTestcase
            | head::tail -> 
            begin
             let currentIndex : int = !countLoop in
             let variableName = List.nth varsList currentIndex in
             let intervalOfVariable = StringMap.find variableName intervalVarMap in
            let tc =
                 let lowerBound = 
                       if head = "L" then intervalOfVariable.low else (intervalOfVariable.high +. intervalOfVariable.low)/.2.0
                     in
                 let upperBound =
                      if head = "U" then intervalOfVariable.high else (intervalOfVariable.high +. intervalOfVariable.low)/.2.0 
                     in
                 let bound =  upperBound -. lowerBound in
                      let bound = 
                      if bound = infinity then max_float
                       else bound
                  in
                Random.self_init();
                let randomNum = Random.float bound in (* random number from 0 to bound *)
                     if Random.bool() then 
                        let testcase = ceil(lowerBound +. randomNum) in
                        if testcase > upperBound then floor(lowerBound +. randomNum)
                        else testcase
                      else 
                        let testcase = floor(lowerBound +. randomNum) in
                        if testcase < lowerBound then ceil(lowerBound +. randomNum)
                        else testcase
              in 
             let resultTestcase = StringMap.add variableName tc resultTestcase in
             (*Increase countLoop*)
             incr countLoop;
             refRowTC := StringMap.add variableName tc !refRowTC;
             (*Call menthod loopColumnTestTemp again*)
             loopColumnTestTemp tail varsList intervalVarMap passVariables;
            end 
          in
          (* End function loop column in testCaseTemp*)

          (* Start function loop row in testCaseTemp*)
          let rec loopRowTestTemp testCaseTemp varsList intervalVarMap passVariables= match testCaseTemp with
            | [] ->  !finalTestCase
            | head::body -> 
              begin
                  (*Call loop colum test case templare*)
                 let finalRowTestcase = loopColumnTestTemp head varsList intervalVarMap passVariables in
                  (*Reset countLoop*)
                 countLoop := 0;
                  (*add maperTestcase to list finalTestcase*)
                 finalTestCase := !refRowTC :: !finalTestCase; 
                 (*Call loop again*)
                 loopRowTestTemp body varsList intervalVarMap passVariables ;
              end in 
              loopRowTestTemp testCaseTemp varsList intervalVarMap passVariables

          (* End function loop row in testCaseTemp*)
(* ----------End generate parewise testcase-----------------------*)

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

(* This is the helping function for the incremental test function*)
let rec test_extra_incremental abstractTCInfList varsIntvsMap testSATPolyConstraints testUNSATPolyConstraints satVarsTCsMap generatedVarsSet polyConstraintsNum varsSATDirectionMap varsSet = 
  match abstractTCInfList with
  | Nil -> (-1, testSATPolyConstraints, testUNSATPolyConstraints,satVarsTCsMap, generatedVarsSet)
  | Cons((varsTCsMap, testCases, assignedVarsSet, testedPolyCons, remainingPolyConstraints, priorityNum), tail) ->    
    let startTime = Sys.time() in
    match testCases with
    | [] -> (* Testing for some apis are implemented here, or testcases will be generated *)
      
      (* check if all the variables of testedPolyCons have a test value *)
      if VariablesSet.subset testedPolyCons#get_varsSet assignedVarsSet then 
      (

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
            let neededVarsNum = (VariablesSet.cardinal varsSet) - (VariablesSet.cardinal assignedVarsSet) - 10 in
            
            let (generatedTCs, newPriorityNum) = 
              if neededVarsNum <> 0  then
                let neededVarsNum = 
                  if neededVarsNum < 0 then 10 + neededVarsNum
                  else neededVarsNum
                in
                selectedPolyCons#generateTCs assignedVarsSet varsIntvsMap priorityNum varsSATDirectionMap neededVarsNum 
              else
                ([], priorityNum)
            in
            let newAbstractTCInfList = Cons((varsTCsMap, generatedTCs, assignedVarsSet, selectedPolyCons, newRemainingPolyConstraints, newPriorityNum), fun() -> Nil) in
            test_extra_incremental newAbstractTCInfList varsIntvsMap (testedPolyCons :: testSATPolyConstraints) remainingPolyConstraints varsTCsMap assignedVarsSet polyConstraintsNum varsSATDirectionMap varsSet              
        )
        else (
          (* print_endline ("UNSAT constraint: " ^ testedPolyCons#to_string_infix ^ " - miniSATCode: " ^ string_of_int testedPolyCons#get_miniSATCode ^ " - easiness: " ^ string_of_float testedPolyCons#get_easiness);
          flush stdout; *)
          test_extra_incremental (tail()) varsIntvsMap testSATPolyConstraints testUNSATPolyConstraints satVarsTCsMap generatedVarsSet polyConstraintsNum varsSATDirectionMap varsSet
        )
      )
      else
      (
        let neededVarsSet = VariablesSet.diff varsSet assignedVarsSet in
        let neededVarsList =  
          let add_var var current_list = 
            var :: current_list
          in
          VariablesSet.fold add_var neededVarsSet [] 
        in

        (* Printf.printf "Generating test values for %d variables using pairwise strategy\n" (List.length neededVarsList);
        flush stdout; *)

        assert (List.length neededVarsList = 10);

        let add_interval var (interval, miniSATCode) current_map =
          let intv = {low=interval#l; high=interval#h} in
          StringMap.add var intv current_map
        in
        let special_vars_intvs_map = 
          StringMap.fold add_interval varsIntvsMap StringMap.empty
        in

        let varsTCsMaps = pareWiseTesting varsTCsMap neededVarsList special_vars_intvs_map in
        
        let rec test_all = function
          | [] -> (-1, testSATPolyConstraints, testUNSATPolyConstraints,satVarsTCsMap, generatedVarsSet)
          | varsTCsMap :: t ->
            let newAbstractTCInfList = 
              Cons((varsTCsMap, [], varsSet, testedPolyCons, remainingPolyConstraints, priorityNum), tail)
            in
            let (sTest, testSATPolyConstraints, testUNSATPolyConstraints, satVarsTCsMap, generatedVarsSet) = 
              test_extra_incremental newAbstractTCInfList varsIntvsMap testSATPolyConstraints remainingPolyConstraints satVarsTCsMap generatedVarsSet polyConstraintsNum varsSATDirectionMap varsSet
            in
            if sTest = 1 then 
              (sTest, testSATPolyConstraints, testUNSATPolyConstraints, satVarsTCsMap, generatedVarsSet)              
            else
              test_all t
        in test_all varsTCsMaps
      )
    | (var, [])::remainingTCs -> 
        test_extra_incremental (tail()) varsIntvsMap testSATPolyConstraints (testedPolyCons :: remainingPolyConstraints) satVarsTCsMap generatedVarsSet polyConstraintsNum varsSATDirectionMap varsSet
    | (var, nextTC::remainingTC)::remainingTCs -> 
        let newFirstAss = StringMap.add var nextTC varsTCsMap in
        let newAssignedVarsSet = VariablesSet.add var assignedVarsSet in
        let newAbstractTCInfList = 
          Cons((newFirstAss, remainingTCs, newAssignedVarsSet, testedPolyCons, remainingPolyConstraints, priorityNum), 
                fun() -> Cons((varsTCsMap, (var, remainingTC)::remainingTCs, assignedVarsSet, testedPolyCons, remainingPolyConstraints, priorityNum), tail))
        in
        test_extra_incremental newAbstractTCInfList varsIntvsMap testSATPolyConstraints remainingPolyConstraints satVarsTCsMap generatedVarsSet polyConstraintsNum varsSATDirectionMap varsSet
      

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

  let add_var var sat_direction current_set = 
    VariablesSet.add var current_set 
  in
  let varsSet = StringMap.fold add_var varsSATDirectionMap VariablesSet.empty in

  (* print_endline ("Total number of variables: " ^ string_of_int( VariablesSet.cardinal varsSet));
  flush stdout; *)

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
  let neededVarsNum = VariablesSet.cardinal varsSet - 10 in
  let (generatedTCs, newPriorityNum) = 
    if neededVarsNum <> 0 then 
      let neededVarsNum = 
        if neededVarsNum < 0 then 10 + neededVarsNum
        else neededVarsNum
      in
      firstPolyCons#generateTCs VariablesSet.empty varsIntvsMaps priorityNum varsSATDirectionMap neededVarsNum
    else 
      ([], priorityNum)
  in
  
  let abstractTCInfList = 
    Cons((StringMap.empty, generatedTCs, VariablesSet.empty, firstPolyCons, 
          remainingPolyConstraints, newPriorityNum), fun() -> Nil) 
  in 
  (*let (tc, sTest, clTest_US, varsTCsMap, b) = test_extra abstractTCInfList varsIntvsMaps firstPolyCons (*indicesSortedPolyConstraintsMap*) polyConstraintsNum (*1*) varsSATDirectionMap IntMap.empty (remainingTime -. Sys.time() +. startTime) in*)
  let (sTest, testSATPolyConstraints, testUNSATPolyConstraints, satVarsTCsMap, generatedVarsSet) = 
    test_extra_incremental abstractTCInfList varsIntvsMaps [] [] StringMap.empty VariablesSet.empty (*indicesSortedPolyConstraintsMap*) polyConstraintsNum (*1*) varsSATDirectionMap  varsSet
  in
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