open Variable
open Interval
open Ast
open Testing

let isUnknown = ref false;;

let off_set = 8.;;

let new_esl esl = 
  ldexp esl (-3)

let add_new_varsIntvsPriority priority addedVarsIntvsMap currentvarsIntvsMapPrioritiesMaps =
  try
    let varsIntvsMaps = FloatMap.find priority currentvarsIntvsMapPrioritiesMaps in
    FloatMap.add priority (addedVarsIntvsMap::varsIntvsMaps) currentvarsIntvsMapPrioritiesMaps
  with Not_found -> FloatMap.add priority ([addedVarsIntvsMap]) currentvarsIntvsMapPrioritiesMaps


let decompose_var esl varsIntvsMap polyCons var (intv, varSen, isPositiveSen) (unsatPolyConstraintsCodes, varsIntvsMapPrioritiesMaps) =
  (*let (narrowed, newIntv) = polyCons#backward_interval_propagate var intv varsIntvsMiniSATCodesMap in*)
  let lowerBound = intv.low in
  let upperBound = intv.high in
  let newPoint = 
    if lowerBound = neg_infinity then 
      if upperBound = infinity then 0.
      else upperBound -. off_set
    else 
      if upperBound = infinity then lowerBound +. off_set
      else ldexp lowerBound (-1) +. ldexp upperBound (-1)
  in
  let varType = polyCons#get_varType var in
  let unknown = 
    if varType = intType then floor newPoint < lowerBound || ceil newPoint > upperBound
    else newPoint < lowerBound || newPoint > upperBound
  in
  if unknown then
    (unsatPolyConstraintsCodes, add_new_varsIntvsPriority (new_esl esl) varsIntvsMap varsIntvsMapPrioritiesMaps)
  else
    let lowerIntv = 
      if varType = intType then
        let tmpNewPoint = floor newPoint in
        {low=lowerBound; high=tmpNewPoint}
      else 
        {low=lowerBound; high=newPoint}
    in
    (* print_endline("lowerIntv" ^ lowerIntv#to_string);
    flush stdout; *)
    let upperIntv = 
      if varType = intType then
        let tmpNewPoint = ceil newPoint in
        {low=tmpNewPoint;high=upperBound}
      else 
        {low=newPoint;high=upperBound}
    in
    (* Compute the SAT length of lower interval by IA *)
    (* print_string ("Decomposing var " ^ var ^ " in ");
    print_I intv;
    print_endline (" with " ^ string_of_float newPoint);
    print_endline ("Within " ^ polyCons#to_string_infix);
    flush stdout; *)
    let lowerVarsIntvsMap = StringMap.add var lowerIntv varsIntvsMap in
    (* print_endline "Start Computing for lower interval";
    flush stdout; *)
    let (lowerSAT, lowerSatLength, lowerEasiness) = polyCons#check_sat_get_satLength lowerVarsIntvsMap in
    (* print_endline ("Lower: " ^ string_of_int lowerSAT ^ " - " ^ string_of_float lowerSatLength ^ " - easiness: " ^ string_of_float lowerEasiness);
    flush stdout; *)
    
    (* Compute the SAT length of upper interval by IA *)
    let upperVarsIntvsMap = StringMap.add var upperIntv varsIntvsMap in
    let (upperSAT, upperSatLength, upperEasiness) = polyCons#check_sat_get_satLength upperVarsIntvsMap in
    (*print_endline ("Upper: " ^ string_of_int upperSAT ^ " - satLength: " ^ string_of_float upperSatLength ^ " - easiness: " ^ string_of_float upperEasiness);
    flush stdout;*)

    let unsatPolyConstraintsCodes = 
      if lowerSAT = -1 || upperSAT = -1 then IntSet.add (polyCons#get_miniSATCode) unsatPolyConstraintsCodes 
      else unsatPolyConstraintsCodes
    in

    let varsIntvsMapPrioritiesMaps =
      if lowerSAT = -1 && upperSAT = -1 then 
        varsIntvsMapPrioritiesMaps
      else if lowerSAT = -1 then 
        add_new_varsIntvsPriority esl upperVarsIntvsMap varsIntvsMapPrioritiesMaps
      else if upperSAT = -1 then 
        add_new_varsIntvsPriority esl lowerVarsIntvsMap varsIntvsMapPrioritiesMaps
      else if lowerBound = neg_infinity then 
        let varsIntvsMapPrioritiesMaps = add_new_varsIntvsPriority (new_esl esl) lowerVarsIntvsMap varsIntvsMapPrioritiesMaps in
        add_new_varsIntvsPriority esl upperVarsIntvsMap varsIntvsMapPrioritiesMaps  
      else if upperBound = infinity then 
        let varsIntvsMapPrioritiesMaps = add_new_varsIntvsPriority (new_esl esl) upperVarsIntvsMap varsIntvsMapPrioritiesMaps in
        add_new_varsIntvsPriority esl lowerVarsIntvsMap varsIntvsMapPrioritiesMaps  
      else if lowerEasiness < upperEasiness then 
        let varsIntvsMapPrioritiesMaps = add_new_varsIntvsPriority esl lowerVarsIntvsMap varsIntvsMapPrioritiesMaps in
        add_new_varsIntvsPriority esl upperVarsIntvsMap varsIntvsMapPrioritiesMaps
      else
        let varsIntvsMapPrioritiesMaps = add_new_varsIntvsPriority esl upperVarsIntvsMap varsIntvsMapPrioritiesMaps in
        add_new_varsIntvsPriority esl lowerVarsIntvsMap varsIntvsMapPrioritiesMaps
    in
    (unsatPolyConstraintsCodes, varsIntvsMapPrioritiesMaps)
    
let get_decomposed_vars_list_pol polyCons esl varsIntvsMap maxDecomposedVarsNum = 
  let varsSet = polyCons#get_varsSet in

  let not_smallIntv intv =

    let lowerBound = intv.low in
    let upperBound = intv.high in
    if lowerBound = neg_infinity && upperBound < esl -. max_float then 
      false
    else if upperBound = infinity && lowerBound > max_float -. esl then
      false
    else if upperBound <= esl +. lowerBound then 
      false
    else
      true
  in
  let add_notSmallInterval var (reducedVarsSet, infVar) = 
    let intv = Util.get_var_intv var varsIntvsMap in
    if not_smallIntv intv then 
      let newInfVar = 
        if intv.high = infinity || intv.low = neg_infinity then var
        else infVar
      in
      (VariablesSet.add var reducedVarsSet, newInfVar)
    else (
      (* print_endline ("small interval: " ^ var ^ ": " ^ sprintf_I "%f" intv);
      flush stdout; *)
      (reducedVarsSet, infVar)
    )
  in
  let (reducedVarsSet, infVar) = (*varsSet*) VariablesSet.fold add_notSmallInterval varsSet (VariablesSet.empty, "") in
  if VariablesSet.is_empty reducedVarsSet then
    []
  else 
    if infVar <> "" then [(infVar, 0., 0)]
    else 
      polyCons#get_n_varsSen_fromSet maxDecomposedVarsNum 
      (*(VariablesSet.cardinal reducedVarsSet)*) reducedVarsSet

let rec get_decomposed_vars_list_pols polyConstraints esl varsIntvsMap maxDecomposedVarsNum =
  match polyConstraints with
  | [] -> []
  | _ ->
    let (h, t) = get_element polyConstraints in
    get_decomposed_vars_list h t esl varsIntvsMap maxDecomposedVarsNum

and get_decomposed_vars_list polyCons unkownPolyConstraints esl varsIntvsMap maxDecomposedVarsNum = 
  match get_decomposed_vars_list_pol polyCons esl varsIntvsMap maxDecomposedVarsNum with
  | [] -> get_decomposed_vars_list_pols unkownPolyConstraints esl varsIntvsMap maxDecomposedVarsNum
  | decomposedVarsList -> decomposedVarsList

(*Binary balance decomposition on intervals*)
let dynamicDecom varsIntvsMap unsatPolyConstraintsCodes varsIntvsMapPrioritiesMaps polyCons 
    unkownPolyConstraints maxDecomposedVarsNum esl usedVarsSet = 
  (* print_endline ("Decomposing: " ^ polyCons#to_string_infix ^ ": " ^ string_of_int polyCons#get_miniSATCode);
  flush stdout; *)
  let startTime = Sys.time() in
  (* let add_varsSet currentVarsSet polyCons = VariablesSet.union polyCons#get_varsSet currentVarsSet in
  let varsSet = List.fold_left add_varsSet VariablesSet.empty unkownPolyConstraints in *)
  
  match get_decomposed_vars_list polyCons unkownPolyConstraints esl varsIntvsMap maxDecomposedVarsNum with
  | [] ->
    let newEsl = new_esl esl in
    if newEsl = esl then (* No more search is possible *)
      (isUnknown := true;
       (unsatPolyConstraintsCodes, varsIntvsMapPrioritiesMaps))
    else
      (unsatPolyConstraintsCodes, add_new_varsIntvsPriority (new_esl esl) varsIntvsMap
                                                                         varsIntvsMapPrioritiesMaps)
  | decomposedVarsList ->
    let add_varIntvMiniSATCode currentVarsIntvsMiniSATCodesIsPositiveSenMap (var, varSen, isPositiveSen) = 
      let intvMiniSATCode = StringMap.find var varsIntvsMap in
      StringMap.add var (intvMiniSATCode, varSen, isPositiveSen) currentVarsIntvsMiniSATCodesIsPositiveSenMap
    in
    let decomposedVarsIntvsMiniSATCodesIsPositiveMap = List.fold_left add_varIntvMiniSATCode StringMap.empty decomposedVarsList in 
    StringMap.fold (decompose_var esl varsIntvsMap polyCons) decomposedVarsIntvsMiniSATCodesIsPositiveMap (unsatPolyConstraintsCodes, varsIntvsMapPrioritiesMaps)


(* (*Binary balance decomposition on intervals*)
let dynamicDecom_noStrategy varsIntvsMap varsIntvsMapPrioritiesMaps polyCons unkownPolyConstraints maxDecomposedVarsNum esl remainingTime =
  print_endline ("Decomposing: " ^ polyCons#to_string_infix ^ ": " ^ string_of_int polyCons#get_miniSATCode);
  flush stdout;
  let startTime = Sys.time() in
  let add_varsSet currentVarsSet polyCons = VariablesSet.union polyCons#get_varsSet currentVarsSet in
  let varsSet = List.fold_left add_varsSet VariablesSet.empty unkownPolyConstraints in


  let get_largest var intv (currentVar, currentIntv) = 
    if currentIntv.high -. currentIntv.low < intv.high -. intv.low then (var, intv)
    else (currentVar, currentIntv)
  in
  let (var, intv) = StringMap.fold get_largest varsIntvsMap ("", {low=0.;high=0.}) in
  if intv.high -. intv.low < esl then add_new_varsIntvsPriority (esl /. 10.) varsIntvsMap varsIntvsMapPrioritiesMaps
  else
    let varType =
      if polyCons#get_logic = "QF_NIA" then "Int"
      else "Real"
    in 
    decompose_var varType esl varsIntvsMap polyCons var (intv, 0., false) varsIntvsMapPrioritiesMaps *)