(* /\/\/\/\/\/\/\/\/\/\ MODULE for assignments related definitions and operations /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\*)

open Variable

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
(* |================================= END log_assignment =================================|*)

(* |================================ START log_assignment_smtlib ================================|*) 
(* | Function for logging the assignments as smt-lib format        |*)
(* | . ass: is the map from the variables into assigned values                                  |*)
(* |____________________________________________________________________________________________|*)
let rec log_assignment_smtlib ass =
  let add_string_of_newAssignment var testcase oldString = 
    "assert(= " ^ var ^ " " ^ string_of_float testcase ^ ")\n" ^ oldString
  in
  StringMap.fold add_string_of_newAssignment ass ""
(* |================================= END log_assignment_smtlib =================================|*)


(* =================================== START string_of_intervals =========================================== *)
(* This functions converts the list of intervals of variables into the string format*)
let rec string_of_intervals intvMap =
  let add_string_of_newInterval var (interval, _) oldString = 
    var ^ " " ^ string_of_float interval#l ^ " " ^ string_of_float interval#h ^ " " ^ oldString
  in 
  StringMap.fold add_string_of_newInterval intvMap ""
(* =================================== END string_of_intervals ============================================== *)


(* =================================== START log_intervals =========================================== *)
(* This functions converts the list of intervals of variables into the string format*)
let rec log_intervals intvMap =
  let add_string_of_newInterval var (interval, _) oldString =
    var ^ " in" ^ interval#to_string ^ "\n" ^ oldString
  in 
  StringMap.fold add_string_of_newInterval intvMap ""
(* =================================== END log_intervals ============================================== *)
 

(* =================================== START log_intervals_testcases =========================================== *)
(* This functions converts the list of intervals of variables or their test cases
    into the smt-lib format*)
let rec log_intervals_testcases intvMap ass =
  let add_string_of_newInterval var (interval, _) oldString =
    try
      let testcase = StringMap.find var ass in
      "(assert (= " ^ var ^ " " ^ Printf.sprintf "%f" testcase ^ "))\n" ^ oldString
    with 
      | _ -> "(assert (>= " ^ var ^ " " ^ Printf.sprintf "%f" interval#l ^ "))\n" 
              ^ "(assert (<= " ^ var ^ " " ^ Printf.sprintf "%f" interval#h ^ "))\n" 
              ^ oldString
  in 
  StringMap.fold add_string_of_newInterval intvMap ""
(* =================================== END log_intervals_testcases ============================================== *) 

(* \/\/\/\/\/\/\/\/\/\/ MODULE for assignments related definitions and operations \/\/\/\/\/\/\\/\/\/\/\/\/\/\/\/\/*)
