(* /\/\/\/\/\/\/\/\/\/\ MODULE for assignments related definitions and operations /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\*)


open Variable


(* |============================== START getLeftBound ==========================================| *)	
(* | get the lower bounds of assignments intervals			            	                          | *)
(* | . assIntv: is the list of intervals of variables.			                                    | *)
(* | . result: is the returned list which contains the map from variables to their lower bounds.| *)
(* |____________________________________________________________________________________________| *) 
let getLowerBound assIntv =
  let add_new_lowerBound var (intv, _) oldBounds = (var, intv#l)::oldBounds in
  VarIntvMap.fold add_new_lowerBound assIntv []
(* =============================== END getLeftBound ============================================ *)


(* |=================================== START getRightBound ====================================|*)	
(* | get the upper bounds of assignments intervals						                                  |*)
(* | . assIntv: is the list of intervals of variables.						                              |*)
(* | . result: is the returned list which contains maps from variables to their lower bound. 	  |*)
(* |____________________________________________________________________________________________|*) 
let rec getUpperBound assIntv =
	let add_new_upperBound var (intv, _) oldBounds = (var, intv#h)::oldBounds in
  VarIntvMap.fold add_new_upperBound assIntv []
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
let rec string_of_intervals intvMap =
  let add_string_of_newInterval var (interval, _) oldString = 
    var ^ " " ^ string_of_float interval#l ^ " " ^ string_of_float interval#h ^ " " ^ oldString
  in 
  VarIntvMap.fold add_string_of_newInterval intvMap ""
(* =================================== END intervals_toString ============================================== *)

(* This function extracts the intervals of variables in varsList from the first intervals and attach them 
   to the beginning of the second intervals *)
let rec extract_append_first varsList firstIntvMap secondIntvMap =
  match varsList with
  | [] -> secondIntvMap
  | var::remainingVars -> 
  (
    try 
      let newMap = VarIntvMap.add var (VarIntvMap.find var firstIntvMap) secondIntvMap in
      extract_append_first remainingVars firstIntvMap newMap
    with Not_found -> extract_append_first remainingVars firstIntvMap secondIntvMap
  )


(* This function check if an list of intervals contain any infinity number *)
let rec check_infinity x (intv, _) =
  intv#l = neg_infinity || intv#h = infinity
  

(* \/\/\/\/\/\/\/\/\/\/ MODULE for assignments related definitions and operations \/\/\/\/\/\/\\/\/\/\/\/\/\/\/\/\/*)
