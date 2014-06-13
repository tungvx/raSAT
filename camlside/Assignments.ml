(* /\/\/\/\/\/\/\/\/\/\ MODULE for assignments related definitions and operations /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\*)


(* |============================== START getLeftBound ==========================================| *)	
(* | get the lower bounds of assignments intervals			            	                          | *)
(* | . assIntv: is the list of intervals of variables.			                                    | *)
(* | . result: is the returned list which contains the map from variables to their lower bounds.| *)
(* |____________________________________________________________________________________________| *) 
let rec getLowerBound assIntv result = match assIntv with
	| [] -> result
	| (x, it)::t -> getLowerBound t ((x, it#l)::result)
(* =============================== END getLeftBound ============================================ *)


(* |=================================== START getRightBound ====================================|*)	
(* | get the upper bounds of assignments intervals						                                  |*)
(* | . assIntv: is the list of intervals of variables.						                              |*)
(* | . result: is the returned list which contains maps from variables to their lower bound. 	  |*)
(* |____________________________________________________________________________________________|*) 
let rec getUpperBound assIntv result = match assIntv with
	| [] -> result
	| (x, it)::t -> getUpperBound t ((x, it#h)::result)
(* ==================================== END getRightBound ====================================== *)


(* |================================ START assignments_toString ================================|*)	
(* | Function for converting the assignments to string, in order to pass to c functions.        |*)
(* | . ass: is the list of assignments of variables.						                                |*)
(* |____________________________________________________________________________________________|*)
let rec assignments_toString ass = match ass with
  | [] -> ""
  | (x, a):: t -> (x ^" "^ string_of_float a) ^ " " ^ (assignments_toString t)
(* |================================= END assignments_toString =================================|*)


(* =================================== START intervals_toString =========================================== *)
(* This functions converts the list of intervals of variables into the string format*)
let rec string_of_intervals intervals = match intervals with
  | [] -> ""
  | (x, it)::t -> (x ^ " " ^ string_of_float it#l ^ " " ^ string_of_float it#h) ^ " " ^ (string_of_intervals t)
(* =================================== END intervals_toString ============================================== *)

(* This function extracts the intervals of variables in varsList from the first intervals and attach them 
   to the beginning of the second intervals *)
let rec extract_append_first varsList firstIntv secondIntv =
  match varsList with
  | [] -> secondIntv
  | var::remainingVars -> 
  (
    try 
      let intv = List.assoc var firstIntv in
      (*let newSecondIntv = List.remove_assoc var secondIntv in*)
      extract_append_first remainingVars firstIntv ((var, intv)::(List.remove_assoc var secondIntv)) (* remove var in the second Intv would improve the perfomance because of IA operations.*)
    with Not_found -> extract_append_first remainingVars firstIntv secondIntv
  )


(* This function check if an list of intervals contain any infinity number *)
let rec check_infinity assIntv = match assIntv with
  | [] -> false
  | (x, intv)::t -> 
    if intv#l = neg_infinity || intv#h = infinity then true
    else check_infinity t
  

(* \/\/\/\/\/\/\/\/\/\/ MODULE for assignments related definitions and operations \/\/\/\/\/\/\\/\/\/\/\/\/\/\/\/\/*)
