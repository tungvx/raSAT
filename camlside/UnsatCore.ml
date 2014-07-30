open Util
open Ast
open Assignments
open InfiniteList
open Variable

let check_unsatcore_vars polyCons varsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap varsList isInfinite = 
  (*print_endline ("Checking: " ^ Util.vars_to_string varsList);
  flush stdout;*)
  let add_currentIntvsMiniSATCodes currentVarsIntvsMiniSATCodesMap var =
    let currentIntvMiniSATCode = StringMap.find var varsIntvsMiniSATCodesMap in
    StringMap.add var currentIntvMiniSATCode currentVarsIntvsMiniSATCodesMap
  in
  let newVarsIntvsMiniSATCodesMap = List.fold_left add_currentIntvsMiniSATCodes originalVarsIntvsMiniSATCodesMap varsList in
  (*print_endline (intervals_to_string newIntv); (* intervals_to_string is definied in Assignments.ml *)
  flush stdout;*)
  let sat = 
    if isInfinite then polyCons#check_sat_ci newVarsIntvsMiniSATCodesMap
    else polyCons#check_sat_af_two_ci newVarsIntvsMiniSATCodesMap (* check_sat_af_two_ci is in ast.ml *)
  in
  sat = -1


(*let rec get_unsatcore_vars_from_list boolExp currentIntv originalIntv unsatCoreVarsCandidates limitedTime = 
  if limitedTime <= 0. then []
  else 
    match unsatCoreVarsCandidates with
    | Nil -> []
    | Cons(varsList, remainingCandidates) ->
      let startTime = Sys.time() in
      if check_unsatcore_vars boolExp currentIntv originalIntv varsList then (
        (*print_endline ("UnSAT core of " ^ bool_expr_to_infix_string boolExp ^ " is " ^ Util.vars_to_string varsList ^ "with " ^ intervals_to_string currentIntv);
        (* bool_expr_to_infix_string is defined in ast.ml *)
        (* intervals_to_string is defined in Assignments.ml *)
        flush stdout;*)
        varsList
      )
      else  
        get_unsatcore_vars_from_list boolExp currentIntv originalIntv (remainingCandidates()) (limitedTime -. (Sys.time() -. startTime))
*)        

(* This functions check whether a new unsat core constains a current unsat core *)
let rec checkCompact newUnsatCore unsatCore = 
  if newUnsatCore = [] then false (* We assume that new unsat core is longer than current unsat core *)
  else if unsatCore = [] then newUnsatCore != [] (* The same assumption as above *)
  else 
    let newUnsatCoreHead = List.hd newUnsatCore in 
    let unsatCoreHead = List.hd unsatCore in
    if newUnsatCoreHead = unsatCoreHead then
      let newUnsatCoreTail = List.tl newUnsatCore in
      let unsatCoreTail = List.tl unsatCore in
      checkCompact newUnsatCoreTail unsatCoreTail
    else false
        
        
(* This function add a new unsat core into the current list of unsat cores *)        
(* If, for example, {x_1} is already in the current list, unsat cores like {x_1, x_2}
   cannot be added *)
let rec addUnsatCores newUnsatCore unsatCores = 
  match unsatCores with 
  | [] -> [newUnsatCore]
  | unsatCore :: nextUnsatCores -> (
    if checkCompact newUnsatCore unsatCore then unsatCores
    else unsatCore :: (addUnsatCores newUnsatCore nextUnsatCores)
  )
        
let rec get_unsatcore_vars_extra polyCons varsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap isInfinite limitedTime remainingCandidates result =
  if limitedTime <= 0. then result
  else 
    match remainingCandidates with 
    | Nil -> result 
    | Cons((choosenVars, remainingVars, shouldCheck, nChoosenVars, nVars), tail) -> ( 
      (*print_endline ("PreChecking: choosen " ^ Util.vars_to_string choosenVars ^ " remaining " ^ Util.vars_to_string remainingVars);
      flush stdout;*)
      let startTime = Sys.time() in
      if (nChoosenVars >= nVars / 2) then get_unsatcore_vars_extra polyCons varsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap isInfinite (limitedTime -. (Sys.time() -. startTime)) (tail()) result 
      else 
        let (newResult, newRemainingCandidates) = 
          if shouldCheck && check_unsatcore_vars polyCons varsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap choosenVars isInfinite then (
            (*print_endline ("UnSAT core of " ^ polyCons#to_string_infix ^ " is " ^ Util.vars_to_string choosenVars ^ "with " ^ string_of_intervals varsIntvsMiniSATCodesMap);
            (* bool_expr_to_infix_string is defined in ast.ml *)
            (* intervals_to_string is defined in Assignments.ml *)
            flush stdout;*)
            (*let tempNewResult = addUnsatCores choosenVars result in*)
            (choosenVars::result, tail())
          )
          else (
            if remainingVars = [] then (
              (*print_endline "Remaining empty";
              flush stdout;*)
              (result, tail())
            )
            else 
              let nextVar = List.hd remainingVars in
              let nextRemainingVars = List.tl remainingVars in
              let nextVarNotChoosenCandidates = Cons((choosenVars, nextRemainingVars, false, nChoosenVars, nVars), fun() -> Nil) in
              let nextVarChoosenCandidates = Cons((nextVar::choosenVars, nextRemainingVars, true, nChoosenVars + 1, nVars), fun() -> nextVarNotChoosenCandidates) in
              (result, tail() @@ nextVarChoosenCandidates)
          )
        in 
        get_unsatcore_vars_extra polyCons varsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap isInfinite (limitedTime -. (Sys.time() -. startTime)) newRemainingCandidates newResult
    )
      
let get_unsatcore_vars polyCons varsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap isInfinite limitedTime =
  let startTime = Sys.time() in 
  let varsList = polyCons#get_varsList in
  (*print_endline ("Variables List: " ^ Util.vars_to_string varsList);
  flush stdout;*)
  let nVars = polyCons#get_varsNum in
  let initialCandidate = Cons(([], varsList, false, 0, nVars), fun() -> Nil) in
  let unsatVarsCores = get_unsatcore_vars_extra polyCons varsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap isInfinite (limitedTime -. (Sys.time() -. startTime)) initialCandidate [] in
  if unsatVarsCores = [] then Util.learn_vars varsList varsIntvsMiniSATCodesMap
  else Util.learn_vars_cores unsatVarsCores varsIntvsMiniSATCodesMap
  
  (*let varsPowerSet = gen_power_set varsList 1 (nVars - 1) in 
  let unsatCoreVars = get_unsatcore_vars_from_list boolExp currentIntv originalIntv varsPowerSet (limitedTime -. (Sys.time() -. startTime)) in
  if unsatCoreVars = [] then 
    Util.learn_vars varsList varsId
  else 
    Util.learn_vars unsatCoreVars varsId*)
