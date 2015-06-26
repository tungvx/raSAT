(* /\/\/\/\/\/\/\/\/\/\ MODULE for assignments related definitions and operations /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\*)

open Variable
open Interval

(* |============================== START getLeftBound ==========================================| *)	
(* | get the lower bounds of assignments intervals			            	                          | *)
(* | . assIntv: is the list of intervals of variables.			                                    | *)
(* | . result: is the returned list which contains the map from variables to their lower bounds.| *)
(* |____________________________________________________________________________________________| *) 
let getLowerBound assIntv =
  let add_new_lowerBound var (intv, _) oldBounds = 
    StringMap.add var intv#l oldBounds 
  in
  StringMap.fold add_new_lowerBound assIntv StringMap.empty
(* =============================== END getLeftBound ============================================ *)


(* |=================================== START getRightBound ====================================|*)	
(* | get the upper bounds of assignments intervals						                                  |*)
(* | . assIntv: is the list of intervals of variables.						                              |*)
(* | . result: is the returned list which contains maps from variables to their lower bound. 	  |*)
(* |____________________________________________________________________________________________|*) 
let rec getUpperBound assIntv =
	let add_new_upperBound var (intv, _) oldBounds = 
	  StringMap.add var intv#h oldBounds 
	in
  StringMap.fold add_new_upperBound assIntv StringMap.empty
(* ==================================== END getRightBound ====================================== *)


(* |================================ START string_of_assignment ================================|*)	
(* | Function for converting the assignments to string, in order to pass to c functions.        |*)
(* | . ass: is the map from the variables into assigned values                                  |*)
(* |____________________________________________________________________________________________|*)
let rec string_of_assignment ass =
  let add_string_of_newAssignment var testcase oldString = 
    var ^" "^ string_of_float testcase ^ " " ^ oldString
  in
  StringMap.fold add_string_of_newAssignment ass ""
(* |================================= END string_of_assignment =================================|*)


(* |================================ START log_assignment ================================|*)	
(* | Function for logging the assignments as string        |*)
(* | . ass: is the map from the variables into assigned values                                  |*)
(* |____________________________________________________________________________________________|*)
let rec log_assignment ass =
  let add_string_of_newAssignment var testcase oldString = 
    var ^" = "^ string_of_float testcase ^ "\n" ^ oldString
  in
  StringMap.fold add_string_of_newAssignment ass ""
(* |================================= END string_of_assignment =================================|*)


(* =================================== START string_of_intervals =========================================== *)
(* This functions converts the list of intervals of variables into the string format*)
let rec string_of_intervals varsIntvsMap =
  let add_string_of_newInterval var intv oldString = 
    var ^ " " ^ string_of_float intv.low ^ " " ^ string_of_float intv.high ^ " " ^ oldString
  in 
  StringMap.fold add_string_of_newInterval varsIntvsMap ""
(* =================================== END string_of_intervals ============================================== *)


let rec string_of_varsIntvsMaps varsIntvsMaps = match varsIntvsMaps with
  | [] -> ""
  | h::t -> string_of_intervals h ^ "\n" ^ string_of_varsIntvsMaps t

let rec string_of_varsIntvsPrioritiesMap varsIntvsPrioritiesMap =
  let add_string_of_newPriorityInterval priority varsIntvsMaps oldString = 
    string_of_float priority ^ "\n" ^ string_of_varsIntvsMaps varsIntvsMaps ^ oldString
  in
  FloatMap.fold add_string_of_newPriorityInterval varsIntvsPrioritiesMap ""

(* =================================== START log_intervals =========================================== *)
(* This functions converts the list of intervals of variables into the string format*)
let rec log_intervals varsIntvsMap =
  let add_string_of_newInterval var intv oldString =
    var ^ " in " ^ sprintf_I "%f" intv ^ "\n" ^ oldString
  in 
  StringMap.fold add_string_of_newInterval varsIntvsMap ""
(* =================================== END log_intervals ============================================== *)
 

(* \/\/\/\/\/\/\/\/\/\/ MODULE for assignments related definitions and operations \/\/\/\/\/\/\\/\/\/\/\/\/\/\/\/\/*)
