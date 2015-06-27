open Variable
open Ast
open Assignments

let rec insertionSort_byEasiness polyCons polyConstraints = match polyConstraints with
  | [] -> [polyCons]
  | h :: t -> 
    let currentEasiness = h#get_easiness in
    let newEasiness = polyCons#get_easiness in
    
    (* (1) (2) needs to change (10) *)
    (*(* (2) need to change (1) *)
    if currentEasiness < newEasiness then polyCons :: polyConstraints
    else if currentEasiness > newEasiness then h :: (insertionSort_byEasiness polyCons t)*)
    
    (* (1) need to change (2) *)
    if currentEasiness < newEasiness then h :: (insertionSort_byEasiness polyCons t)
    else if currentEasiness > newEasiness then polyCons :: polyConstraints
    
    else (
      Random.self_init();
      if Random.bool() then h :: (insertionSort_byEasiness polyCons t)
      else polyCons :: polyConstraints
    )
    
    (*(* (10) need to change (1) and (2) *)
    Random.self_init();
    if Random.bool() then h :: (insertionSort_byEasiness polyCons t)
    else polyCons :: polyConstraints*)

(*Rewrite eval_all for UNSAT cores computations*)
let rec eval_all res unsatPolyConstraintsCodes uk_cl validPolyConstraints polyConstraints varsIntvsMap iaTime usTime remainingTime =
  match polyConstraints with
   |[] -> (res, unsatPolyConstraintsCodes, uk_cl, validPolyConstraints, iaTime, usTime)
   |h::t -> 
      let startTime = Sys.time() in
      (* print_endline ("\nStart check sat: " ^ h#to_string_infix);
      print_endline ("\n With " ^ string_of_intervals varsIntvsMap);
      flush stdout; *)
      (* print_varsSet (get_vars_set_boolExp h); (* print_varsSet is in Variable.ml and get_vars_set_boolExp is in ast.ml *)
      flush stdout;*)
      let res1 = h#check_sat_varsSen_setIsInfinite_setBounds_setEasiness varsIntvsMap in
      (* print_endline ("Bounds: " ^ h#get_iaValue#to_string);
      print_endline ("Easiness: " ^ string_of_float h#get_easiness);
      flush stdout; *)
      (*print_endline ("End check sat IA of " ^ h#to_string_infix ^ ", result: " ^ string_of_int res1);

      flush stdout;*)
      let iaTime = iaTime +. Sys.time() -. startTime in
      if (res1 = 1) then 
        eval_all res unsatPolyConstraintsCodes uk_cl (h::validPolyConstraints) t varsIntvsMap iaTime usTime (remainingTime -. Sys.time() +. startTime)
      else if (res1 = -1) then (
        eval_all (-1) (IntSet.add (h#get_miniSATCode) unsatPolyConstraintsCodes) [] [] t varsIntvsMap iaTime usTime (remainingTime -. Sys.time() +. startTime)
      )
      else ( (*res1 = 0*)     
        if res = -1 then
          eval_all (-1) unsatPolyConstraintsCodes [] [] t varsIntvsMap iaTime usTime (remainingTime -. Sys.time() +. startTime)
        else (
          eval_all 0 unsatPolyConstraintsCodes ((*h::uk_cl*) insertionSort_byEasiness h uk_cl) validPolyConstraints t varsIntvsMap iaTime usTime (remainingTime -. Sys.time() +. startTime)
        )
      )



let rec contract_polyConstraints uk_cl unsatPolyConstraintsCodes varsIntvsMap esl contracted = match uk_cl with
  | [] -> (contracted, unsatPolyConstraintsCodes, varsIntvsMap)
  | h :: t ->    
    let (newContracted, varsIntvsMap) = contract_polyExpr h#get_polyExpr h#get_contractedIntv varsIntvsMap esl in
    if newContracted && StringMap.is_empty varsIntvsMap then 
      (true, IntSet.add h#get_miniSATCode unsatPolyConstraintsCodes, varsIntvsMap)
    else
      let unsatPolyConstraintsCodes =
        if newContracted then IntSet.add h#get_miniSATCode unsatPolyConstraintsCodes
        else unsatPolyConstraintsCodes
      in
      (* if newContracted then 
      (
        print_endline ("Contracted using " ^ h#to_string_infix ^ " to\n" ^ log_intervals varsIntvsMap);
  flush stdout;
      ); *)
      contract_polyConstraints t unsatPolyConstraintsCodes varsIntvsMap esl (contracted || newContracted)


let rec icp res unsatPolyConstraintsCodes uk_cl validPolyConstraints polyConstraints varsIntvsMap esl iaTime usTime remainingTime =
  let (res, unsatPolyConstraintsCodes, uk_cl, validPolyConstraints, iaTime, usTime) = 
    eval_all res unsatPolyConstraintsCodes uk_cl validPolyConstraints polyConstraints varsIntvsMap iaTime usTime remainingTime
  in
  if res <> 0 then (res, unsatPolyConstraintsCodes, uk_cl, validPolyConstraints, varsIntvsMap, iaTime, usTime)
  else (* implement ICP here *) (
    (* print_endline ("Contracting \n" ^ log_intervals varsIntvsMap);
    flush stdout; *)
    let (contracted, unsatPolyConstraintsCodes, varsIntvsMap) = contract_polyConstraints uk_cl unsatPolyConstraintsCodes varsIntvsMap esl false in
    (* print_endline ("Finished contracting");
    flush stdout; *)
    if contracted then (
      (* print_endline ("Contracted to \n" ^ log_intervals varsIntvsMap);
      flush stdout; *)
      if StringMap.is_empty varsIntvsMap then (-1, unsatPolyConstraintsCodes, uk_cl, validPolyConstraints, varsIntvsMap, iaTime, usTime)
      else icp 1 unsatPolyConstraintsCodes [] validPolyConstraints uk_cl varsIntvsMap esl iaTime usTime remainingTime
    )  
    else (res, unsatPolyConstraintsCodes, uk_cl, validPolyConstraints, varsIntvsMap, iaTime, usTime)
  )