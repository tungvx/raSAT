open Ast
open Variable
open InfiniteList
open Assignments

(* This function generates test cases for one variable *)
let rec generate_tc_var var interval =
    let lowerBound = interval#l in
    let upperBound = interval#h in
    let bound = upperBound -. lowerBound in
    Random.self_init();
    let firstRandomNum = Random.float bound in (* random number from 0 to bound *)
    let firstTC = lowerBound +. firstRandomNum in
    let secondRandomNum = Random.float bound in (* random number from 0 to bound *)
    let secondTC = lowerBound +. secondRandomNum in
    [firstTC; secondTC]


(* This function generates the test cases for a list of variables *)
let rec generate_tc_vars varsList assIntv = match varsList with
  | [] -> []
  | var::remainings -> 
    let interval = List.assoc var assIntv in
    let testCases = generate_tc_var var interval in
    (var, testCases)::(generate_tc_vars remainings assIntv)
    
    
(* This function check if an assignment satisfies a list of apis *)
let rec check_SAT testedEBoolExps assignments unsatBoolExp = match testedEBoolExps with
  | [] -> (true, unsatBoolExp)
  | (boolExp, boolExpVarsSet, boolExpVarsNum) :: remaining -> 
    (*print_endline (bool_expr_to_infix_string boolExp);*)
    let (sat, leftValue, rightValue) = checkSAT_computeValues boolExp assignments in
    if sat then check_SAT remaining assignments unsatBoolExp
    else (false, boolExp)
    
    
(* This function return the number of apis which has the same set of variables
with a given api *)    
let rec get_sizeOfClass (boolExp, boolExpVarsSet, boolExpVarsNum) eBoolExps = match eBoolExps with 
  | [] -> 0
  | (nextBoolExp, nextBoolExpVarsSet, nextBoolExpVarsNum)::t -> 
    if VariablesSet.equal boolExpVarsSet nextBoolExpVarsSet then
      1 + get_sizeOfClass (boolExp, boolExpVarsSet, boolExpVarsNum) t
    else get_sizeOfClass (boolExp, boolExpVarsSet, boolExpVarsNum) t
    
    
(* This function add information about the number of other apis which has the same
variables with each api in the list *)    
let rec add_more_info varsSet remainingEBoolExps eBoolExps = match remainingEBoolExps with
  | [] -> []
  | eBoolExp::t -> 
    let sizeOfClass = get_sizeOfClass eBoolExp eBoolExps in
    match eBoolExp with 
      | (boolExp, boolExpVarsSet, boolExpVarsNum) -> 
        let additionalVarsSet = VariablesSet.diff boolExpVarsSet varsSet in
        let additionalVarsNum = VariablesSet.cardinal additionalVarsSet in
        (boolExp, boolExpVarsSet, boolExpVarsNum, additionalVarsNum, sizeOfClass)::(add_more_info varsSet t eBoolExps)
    

(* This function finds the next boolean expression to be tested
with the critia that the number of additional generated test cases is minimum *)    
let rec get_nextTested_mEBoolExp mEBoolExps varsSet currentBest (* current best solution is of the form singleton list *)
  = match mEBoolExps with
  | [] -> currentBest
  | (boolExp, boolExpVarsSet, boolExpVarsNum, additionalVarsNum, sizeOfClass)::t -> 
    match currentBest with
    | [] -> get_nextTested_mEBoolExp t varsSet [(boolExp, boolExpVarsSet, boolExpVarsNum, additionalVarsNum, sizeOfClass)]
    | [(currentBoolExp, currentBoolExpVarsSet, currentBoolExpVarsNum, currentAdditionalVarsNum, currentSizeOfClass)] ->
      if currentAdditionalVarsNum > additionalVarsNum then
        get_nextTested_mEBoolExp t varsSet [(boolExp, boolExpVarsSet, boolExpVarsNum, additionalVarsNum, sizeOfClass)]
      else if currentAdditionalVarsNum = additionalVarsNum && currentSizeOfClass < sizeOfClass then 
        get_nextTested_mEBoolExp t varsSet [(boolExp, boolExpVarsSet, boolExpVarsNum, additionalVarsNum, sizeOfClass)]
      else 
        get_nextTested_mEBoolExp t varsSet currentBest
    | _ -> currentBest (* never happens *)
        
        
(* This function devides list of more expressive boolean expressions into two list
based on the selected more expressive expressions *)
let rec devide_mEBoolExps mEBoolExps (sBoolExp, sBoolExpVarsSet, sBoolExpVarsNum, sAdditionalVarsNum, sSizeOfClass) first second = match mEBoolExps with
  | [] -> (first, second)
  | (boolExp, boolExpVarsSet, boolExpVarsNum, additionalVarsNum, sizeOfClass)::t -> 
    if VariablesSet.equal sBoolExpVarsSet boolExpVarsSet then
      devide_mEBoolExps t (sBoolExp, sBoolExpVarsSet, sBoolExpVarsNum, sAdditionalVarsNum, sSizeOfClass) ((boolExp, boolExpVarsSet, boolExpVarsNum)::first) second
    else 
      devide_mEBoolExps t (sBoolExp, sBoolExpVarsSet, sBoolExpVarsNum, sAdditionalVarsNum, sSizeOfClass) first ((boolExp, boolExpVarsSet, boolExpVarsNum)::second)
        
    
(* This function finds the next boolean expressions to be tested
with the critia that the number of additional generated test cases is minimum *)    
let get_tested_untested_eBoolExps eBoolExps varsSet =
  let mEBoolExps (* more expressive boolean expression :) *) = add_more_info varsSet eBoolExps eBoolExps in 
  let selectedMEBoolExp = get_nextTested_mEBoolExp mEBoolExps varsSet [] in
  match selectedMEBoolExp with
  | [] -> ([], [])
  | _ -> devide_mEBoolExps mEBoolExps (List.hd selectedMEBoolExp) [] []

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
            let (nextTestedBoolExp, nextTestedBoolExpVarsSet, nextTestedBoolExpVarsNum) = List.hd nextTestedEBoolExps in
            let nextVarsToGenerate = VariablesSet.diff nextTestedBoolExpVarsSet vars in
            let nextTestedEBoolExpsVarsList = VariablesSet.elements nextVarsToGenerate in
            let nextTestCases = generate_tc_vars nextTestedEBoolExpsVarsList assIntv in
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
let test boolExps assIntv strTestUS remainingTime =
  (*print_endline "Start Test";*)
  let startTime = Sys.time() in
  (*print_endline(bool_expr_list_to_infix_string boolExps);*)
  let eBoolExps = add_info boolExps in
  
  (* Recursively generate test cases for each boolean expression *)
  let (testedEBoolExps, remainingEBoolExps) = get_tested_untested_eBoolExps eBoolExps VariablesSet.empty in
  let (firstBoolExp, firstBoolExpVarsSet, firstBoolExpVarsNum) = List.hd testedEBoolExps in
  let firstBoolExpVarsList = VariablesSet.elements firstBoolExpVarsSet in  
  let firstTestCases = generate_tc_vars firstBoolExpVarsList assIntv in
  let abstractTCInfList = Cons(([], firstTestCases, VariablesSet.empty, firstBoolExpVarsSet, testedEBoolExps, remainingEBoolExps), fun() -> Nil) in 
  test_extra abstractTCInfList assIntv firstBoolExp (remainingTime -. Sys.time() +. startTime)
  
  
