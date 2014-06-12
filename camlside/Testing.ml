open Ast
open Variable
open InfiniteList
open Assignments

(* This function generates test cases for one variable *)
let rec generate_tc_var var interval tcNum isFirst =
  if tcNum <= 0 then []
  else
    let lowerBound = interval#l in
    let upperBound = interval#h in
		
		(* if the upper bound is -max_float, stop testing*)
		if upperBound < 0.1 -. max_float then []
		(* if the lowerBound is max_float, stop testing *)
		else if lowerBound -. 0.1 > max_float then []
		else
		  let bound = upperBound -. lowerBound in
		  Random.self_init();
			let newBound =
				if bound = infinity then max_float
				else bound
			in
		  let randomNum = Random.float newBound in (* random number from 0 to bound *)
			let baseNum =
				if lowerBound = neg_infinity then -.max_float
				else lowerBound	
			in
		  let tc =
		    if isFirst && (baseNum >= 0.1 -. max_float) then baseNum (*+. randomNum*)
		    else baseNum +. randomNum 
		  in
		  tc :: (generate_tc_var var interval (tcNum - 1) false)
      

(* This function generates the test cases for a list of variables *)
let rec generate_tc_vars varsList assIntv = match varsList with
  | [] -> []
  | (var, tcNum)::remainings -> 
    let interval = List.assoc var assIntv in
    let testCases = generate_tc_var var interval tcNum true in
    (var, testCases)::(generate_tc_vars remainings assIntv)
    
    
(* This function check if an assignment satisfies a list of apis *)
let rec check_SAT testedEBoolExps assignments unsatBoolExp = match testedEBoolExps with
  | [] -> (true, unsatBoolExp)
  | (boolExp, _, _, _) :: remaining -> 
    (*print_endline (bool_expr_to_infix_string boolExp);*)
    let (sat, leftValue, rightValue) = checkSAT_computeValues boolExp assignments in
    if sat then check_SAT remaining assignments unsatBoolExp
    else (false, boolExp)
    
    
(* This function return the number of apis which has the same set of variables
with a given api *)    
let rec get_sizeOfClass (boolExp, varsSen, boolExpVarsSet, boolExpVarsNum) eBoolExps = match eBoolExps with 
  | [] -> 0
  | (nextBoolExp, nextVarsSen, nextBoolExpVarsSet, nextBoolExpVarsNum)::t -> 
    if VariablesSet.equal boolExpVarsSet nextBoolExpVarsSet then
      1 + get_sizeOfClass (boolExp, varsSen, boolExpVarsSet, boolExpVarsNum) t
    else get_sizeOfClass (boolExp, varsSen, boolExpVarsSet, boolExpVarsNum) t
    
    
(* This function add information about the number of other apis which has the same
variables with each api in the list *)    
let rec add_more_info varsSet remainingEBoolExps eBoolExps = match remainingEBoolExps with
  | [] -> []
  | eBoolExp::t -> 
    let sizeOfClass = get_sizeOfClass eBoolExp eBoolExps in
    match eBoolExp with 
      | (boolExp, varsSen, boolExpVarsSet, boolExpVarsNum) -> 
        let additionalVarsSet = VariablesSet.diff boolExpVarsSet varsSet in
        let additionalVarsNum = VariablesSet.cardinal additionalVarsSet in
        (boolExp, varsSen, boolExpVarsSet, boolExpVarsNum, additionalVarsNum, sizeOfClass)::(add_more_info varsSet t eBoolExps)
    

(* This function finds the next boolean expression to be tested
with the critia that the number of additional generated test cases is minimum *)    
let rec get_nextTested_mEBoolExp mEBoolExps varsSet currentBest (* current best solution is of the form singleton list *)
  = match mEBoolExps with
  | [] -> currentBest
  | (boolExp, varsSen, boolExpVarsSet, boolExpVarsNum, additionalVarsNum, sizeOfClass)::t -> 
    match currentBest with
    | [] -> get_nextTested_mEBoolExp t varsSet [(boolExp, varsSen, boolExpVarsSet, boolExpVarsNum, additionalVarsNum, sizeOfClass)]
    | [(currentBoolExp, currentVarsSen, currentBoolExpVarsSet, currentBoolExpVarsNum, currentAdditionalVarsNum, currentSizeOfClass)] ->
      if currentAdditionalVarsNum > additionalVarsNum then
        get_nextTested_mEBoolExp t varsSet [(boolExp, varsSen, boolExpVarsSet, boolExpVarsNum, additionalVarsNum, sizeOfClass)]
      else if currentAdditionalVarsNum = additionalVarsNum && currentSizeOfClass < sizeOfClass then 
        get_nextTested_mEBoolExp t varsSet [(boolExp, varsSen, boolExpVarsSet, boolExpVarsNum, additionalVarsNum, sizeOfClass)]
      else 
        get_nextTested_mEBoolExp t varsSet currentBest
    | _ -> currentBest (* never happens *)
        
        
(* This function devides list of more expressive boolean expressions into two list
based on the selected more expressive expressions. The first list contain all the 
expressions which has the exactly same variables set with the selected expression *)
let rec devide_mEBoolExps mEBoolExps (sBoolExp, sVarsSen, sBoolExpVarsSet, sBoolExpVarsNum, sAdditionalVarsNum, sSizeOfClass) first second = match mEBoolExps with
  | [] -> (first, second)
  | (boolExp, varsSen, boolExpVarsSet, boolExpVarsNum, additionalVarsNum, sizeOfClass)::t -> 
    if VariablesSet.equal sBoolExpVarsSet boolExpVarsSet then
      devide_mEBoolExps t (sBoolExp, sVarsSen, sBoolExpVarsSet, sBoolExpVarsNum, sAdditionalVarsNum, sSizeOfClass) ((boolExp, varsSen, boolExpVarsSet, boolExpVarsNum)::first) second
    else 
      devide_mEBoolExps t (sBoolExp, sVarsSen, sBoolExpVarsSet, sBoolExpVarsNum, sAdditionalVarsNum, sSizeOfClass) first ((boolExp, varsSen, boolExpVarsSet, boolExpVarsNum)::second)
        
    
(* This function finds the next boolean expressions to be tested
with the critia that the number of additional generated test cases is minimum *)    
let get_tested_untested_eBoolExps eBoolExps varsSet =
  let mEBoolExps (* more expressive boolean expression :) *) = add_more_info varsSet eBoolExps eBoolExps in 
  let selectedMEBoolExp = get_nextTested_mEBoolExp mEBoolExps varsSet [] in
  match selectedMEBoolExp with
  | [] -> ([], [])
  | _ -> devide_mEBoolExps mEBoolExps (List.hd selectedMEBoolExp) [] []
  
  
(* This function check if a variable with sensitivity is in a set or not *)  
let check_varWithSen_inSet varsSet (var, sen) =
  VariablesSet.mem var varsSet
  
  
(* This functions set the number of test cases for each variable based on their sensitivity *)
let rec set_tc_num_extra varsSen priorityNum = match varsSen with
  | [] -> []
  | (var, _) :: t -> 
    if priorityNum > 0 then (var, 2) :: (set_tc_num_extra t (priorityNum - 1))
    else (var, 4) :: (set_tc_num_extra t 0)  
    
let set_tc_num varsSen = 
  let length = List.length varsSen in
  let priorityNum = length / 2 in
  set_tc_num_extra varsSen priorityNum
  

(* This is the helping function for the test function*)
let rec test_extra abstractTCInfList assIntv unsatBoolExp remainingTime = 
  match abstractTCInfList with
  | Nil -> ([],-1, [unsatBoolExp],[])
  | Cons((assignments, testCases, vars, testedBoolExpVarsSet, testedEBoolExps, remainingEBoolExps), tail) ->
    if remainingTime <= 0. then ([],-1, [unsatBoolExp],[])
    else 
      let startTime = Sys.time() in
      if VariablesSet.subset testedBoolExpVarsSet vars then (
        (*print_endline (assignments_toString assignments);*)
        let (sat, nextUnsatBoolExp) = check_SAT testedEBoolExps assignments unsatBoolExp in
        if sat then
          if remainingEBoolExps = [] then ([], 1, [unsatBoolExp], assignments)
          else
            let (nextTestedEBoolExps, nextRemainingEBoolExps) = get_tested_untested_eBoolExps remainingEBoolExps vars in
            let (nextTestedBoolExp, nextTestedBoolExpVarsSen, nextTestedBoolExpVarsSet, nextTestedBoolExpVarsNum) = List.hd nextTestedEBoolExps in
            let nextVarsToGenerate = VariablesSet.diff nextTestedBoolExpVarsSet vars in
            let nextVarsToGenerateSen = List.filter (check_varWithSen_inSet nextVarsToGenerate) nextTestedBoolExpVarsSen in
            let nextTestedVars = set_tc_num nextVarsToGenerateSen in
            let nextTestCases = generate_tc_vars nextTestedVars assIntv in
            let newAbstractTCInfList = Cons((assignments, nextTestCases, vars, nextTestedBoolExpVarsSet, nextTestedEBoolExps, nextRemainingEBoolExps), tail) in
            test_extra newAbstractTCInfList assIntv unsatBoolExp (remainingTime -. Sys.time() +. startTime)
        else
          test_extra (tail()) assIntv nextUnsatBoolExp (remainingTime -. Sys.time() +. startTime)
      )
      else match testCases with 
        | (var, nextTC::remainingTC)::remainingTCs -> 
          let newFirstAss = (var, nextTC):: assignments in
          let newVars = VariablesSet.add var vars in
          let newAbstractTCInfList = 
            if remainingTC = [] then 
              Cons((newFirstAss, remainingTCs, newVars, testedBoolExpVarsSet, testedEBoolExps, remainingEBoolExps), tail)
            else
              Cons((newFirstAss, remainingTCs, newVars, testedBoolExpVarsSet, testedEBoolExps, remainingEBoolExps), 
                    fun() -> Cons((assignments, (var, remainingTC)::remainingTCs, vars, testedBoolExpVarsSet, testedEBoolExps, remainingEBoolExps), tail))
          in
          test_extra newAbstractTCInfList assIntv unsatBoolExp (remainingTime -. Sys.time() +. startTime)
        | _ -> ([],-1, [unsatBoolExp],[]) (* Never happens*)


(* This function test the list of unknow clauses, trying to find an SAT instance *)
let test boolExpsWithVarsSen assIntv strTestUS remainingTime =
  (*print_endline "Start Test";*)
  let startTime = Sys.time() in
  (*print_endline(bool_expr_list_to_infix_string boolExps);*)
  let eBoolExps = add_info boolExpsWithVarsSen in
  
  (* Recursively generate test cases for each boolean expression *)
  let (testedEBoolExps, remainingEBoolExps) = get_tested_untested_eBoolExps eBoolExps VariablesSet.empty in
  let (firstBoolExp, firstBoolExpVarsSen, firstBoolExpVarsSet,_) = List.hd testedEBoolExps in
  let firstBoolExpTestedVars = set_tc_num firstBoolExpVarsSen in  
  let firstTestCases = generate_tc_vars firstBoolExpTestedVars assIntv in
  let abstractTCInfList = Cons(([], firstTestCases, VariablesSet.empty, firstBoolExpVarsSet, testedEBoolExps, remainingEBoolExps), fun() -> Nil) in 
  test_extra abstractTCInfList assIntv firstBoolExp (remainingTime -. Sys.time() +. startTime)
  
  
