(* This function add information about variables list and 
number of variables into boolean expression *)
let add_info boolExps = match boolExps with
  | [] -> []
  | h::t ->
    let variablesList = 


(* This function sort the list of the apis using variables dependency *)
let sort_boolExp_dependency boolExps = 
  let expressiveBoolExps = add_info boolExps in
  


(* This function test the list of unknow clauses, tryingn to find an SAT instance s*)
let test uk_cl assIntv strTestUS esl =


