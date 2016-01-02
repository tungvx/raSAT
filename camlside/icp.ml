open Variable
open Ast
open Assignments
open Interval

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
let rec eval_all res unsatPolyConstraintsCodes uk_cl validPolyConstraints polyConstraints varsIntvsMap iaTime usTime =
  match polyConstraints with
   |[] -> (res, unsatPolyConstraintsCodes, uk_cl, validPolyConstraints, iaTime, usTime)
   |h::t -> 
      let startTime = Sys.time() in
      
      (* print_endline ("\nStart check sat: " ^ h#to_string_infix);
      flush stdout;
      print_endline ("\n\n\n With " ^ string_of_intervals varsIntvsMap);
      flush stdout; *)
      (* print_varsSet (get_vars_set_boolExp h);
      flush stdout; *)
      
      let res1 = h#check_sat_varsSen_setIsInfinite_setBounds_setEasiness varsIntvsMap in
      

      (* print_endline (h#log_ia);
      print_endline ("Easiness: " ^ string_of_float h#get_easiness);
      flush stdout;
      print_endline ("End check sat IA of " ^ h#to_string_infix ^ ", result: " ^ string_of_int res1); *)
      (* print_endline ("\nIntervals1: " ^ string_of_intervals varsIntvsMap); *)
      (* flush stdout; *)
      

      let iaTime = iaTime +. Sys.time() -. startTime in
      if (res1 = 1) then 
        eval_all res unsatPolyConstraintsCodes uk_cl (h::validPolyConstraints) t varsIntvsMap iaTime usTime
      else if (res1 = -1) then
        let newUnsatPolyConstraintsCodes = IntSet.add (h#get_miniSATCode) unsatPolyConstraintsCodes in
        (-1, newUnsatPolyConstraintsCodes, uk_cl, validPolyConstraints, iaTime, usTime)  
      else
        (* print_endline ("\nIntervals2: " ^ string_of_intervals varsIntvsMap); *)
        eval_all 0 unsatPolyConstraintsCodes ((*h::uk_cl*) insertionSort_byEasiness h uk_cl) 
            validPolyConstraints t varsIntvsMap iaTime usTime



let rec contract_polyConstraints uk_cl unsatPolyConstraintsCodes varsIntvsMap esl contracted = match uk_cl with
  | [] -> (contracted, unsatPolyConstraintsCodes, varsIntvsMap)
  | h :: t ->    
    
    (* let printString = "Contracting using " ^ h#to_string_infix ^ " in intervals " in
    print_string printString;
    print_I h#get_contractedIntv;
    print_endline "";
    flush stdout; *)

    let (newContracted, varsIntvsMap) = 
      contract_polyExpr h#get_polyExpr h#get_contractedIntv varsIntvsMap esl 
    in
    
    (* print_endline "Finished Contracting";
    flush stdout; *)
    
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

      (* if newContracted then
        contract_polyConstraints uk_cl unsatPolyConstraintsCodes varsIntvsMap esl (contracted || newContracted)
      else *)
      contract_polyConstraints t unsatPolyConstraintsCodes varsIntvsMap esl (contracted || newContracted)


let rec icp unsatPolyConstraintsCodes uk_cl validPolyConstraints polyConstraints varsIntvsMap esl iaTime usTime =
  (* let printString = "\nEpsilon: " ^ string_of_float esl in
  print_endline printString;
  flush stdout; *)
  let (res, unsatPolyConstraintsCodes, uk_cl, validPolyConstraints, iaTime, usTime) = 
    (* let printString = "\n\n\n With " ^ string_of_intervals varsIntvsMap in
    print_endline printString;
    flush stdout; *)
    eval_all 1 unsatPolyConstraintsCodes uk_cl validPolyConstraints polyConstraints varsIntvsMap iaTime usTime
  in
  (* raise (Failure "Tung dep trai tinh tao"); *)
  if res <> 0 then (res, unsatPolyConstraintsCodes, uk_cl, validPolyConstraints, varsIntvsMap, iaTime, usTime)
  else (* implement ICP here *) (
    (* print_endline ("\nContracting \n" ^ log_intervals varsIntvsMap ^ "\n with esl: " ^ string_of_float esl);
    flush stdout; *)
    let (contracted, unsatPolyConstraintsCodes, varsIntvsMap) = contract_polyConstraints uk_cl unsatPolyConstraintsCodes varsIntvsMap esl false in
    (* print_endline ("Finished contracting \n" ^ log_intervals varsIntvsMap);
    flush stdout; *)
    (* raise (Failure "Tung dep trai"); *)
    if contracted then (
      
      (* print_endline ("Contracted to \n" ^ log_intervals varsIntvsMap);
      flush stdout; *)

      if StringMap.is_empty varsIntvsMap then (-1, unsatPolyConstraintsCodes, uk_cl, validPolyConstraints, varsIntvsMap, iaTime, usTime)
      else icp unsatPolyConstraintsCodes [] validPolyConstraints uk_cl varsIntvsMap esl iaTime usTime
    )  
    else (res, unsatPolyConstraintsCodes, uk_cl, validPolyConstraints, varsIntvsMap, iaTime, usTime)
  )