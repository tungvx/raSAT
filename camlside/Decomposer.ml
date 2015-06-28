open Variable
open Interval


let add_new_varsIntvsPriority priority addedVarsIntvsMap currentvarsIntvsMapPrioritiesMaps =
  try
    let varsIntvsMaps = FloatMap.find priority currentvarsIntvsMapPrioritiesMaps in
    FloatMap.add priority (addedVarsIntvsMap::varsIntvsMaps) currentvarsIntvsMapPrioritiesMaps
  with Not_found -> FloatMap.add priority ([addedVarsIntvsMap]) currentvarsIntvsMapPrioritiesMaps


let decompose_var varType esl varsIntvsMap polyCons var (intv, varSen, isPositiveSen) varsIntvsMapPrioritiesMaps =
  (*let (narrowed, newIntv) = polyCons#backward_interval_propagate var intv varsIntvsMiniSATCodesMap in*)
  let lowerBound = intv.low in
  let upperBound = intv.high in
  let newPoint = 
    if lowerBound = neg_infinity then 
      if upperBound = infinity then 0.
      else upperBound -. 10.
    else 
      if upperBound = infinity then lowerBound +. 10. 
      else 0.5 *. lowerBound +. 0.5 *. upperBound
  in
  let unknown = 
    if varType = "Int" then floor newPoint < lowerBound || ceil newPoint > upperBound
    else newPoint < lowerBound || newPoint > upperBound
  in
  if unknown then
    add_new_varsIntvsPriority (esl /. 10.) varsIntvsMap varsIntvsMapPrioritiesMaps
  else
    let lowerIntv = 
      if varType = "Int" then
        let tmpNewPoint = floor newPoint in
        {low=lowerBound; high=tmpNewPoint}
      else 
        {low=lowerBound; high=newPoint}
    in
    (* print_endline("lowerIntv" ^ lowerIntv#to_string);
    flush stdout; *)
    let upperIntv = 
      if varType = "Int" then
        let tmpNewPoint = ceil newPoint in
        {low=tmpNewPoint;high=upperBound}
      else 
        {low=newPoint;high=upperBound}
    in
    (* Compute the SAT length of lower interval by IA *)
    let lowerVarsIntvsMap = StringMap.add var lowerIntv varsIntvsMap in
    (*print_endline "Start Computing for lower interval";
    flush stdout;*)
    let (lowerSAT, lowerSatLength, lowerEasiness) = polyCons#check_sat_get_satLength lowerVarsIntvsMap in
    (*print_endline ("Lower: " ^ string_of_int lowerSAT ^ " - " ^ string_of_float lowerSatLength ^ " - easiness: " ^ string_of_float lowerEasiness);
    flush stdout;*)
    
    (* Compute the SAT length of upper interval by IA *)
    let upperVarsIntvsMap = StringMap.add var upperIntv varsIntvsMap in
    let (upperSAT, upperSatLength, upperEasiness) = polyCons#check_sat_get_satLength upperVarsIntvsMap in
    (*print_endline ("Upper: " ^ string_of_int upperSAT ^ " - satLength: " ^ string_of_float upperSatLength ^ " - easiness: " ^ string_of_float upperEasiness);
    flush stdout;*)

    if lowerSAT = -1 then 
      if upperSAT = -1 then varsIntvsMapPrioritiesMaps
      else add_new_varsIntvsPriority esl upperVarsIntvsMap varsIntvsMapPrioritiesMaps
    else if lowerSAT = 1 then add_new_varsIntvsPriority esl lowerVarsIntvsMap varsIntvsMapPrioritiesMaps
    else if upperSAT = -1 then add_new_varsIntvsPriority esl lowerVarsIntvsMap varsIntvsMapPrioritiesMaps
    else if upperSAT = 1 then add_new_varsIntvsPriority esl upperVarsIntvsMap varsIntvsMapPrioritiesMaps
    else if lowerBound = neg_infinity then
      let varsIntvsMapPrioritiesMaps = add_new_varsIntvsPriority (esl /. 10.) lowerVarsIntvsMap varsIntvsMapPrioritiesMaps in
      add_new_varsIntvsPriority esl upperVarsIntvsMap varsIntvsMapPrioritiesMaps
    else if upperBound = infinity then 
      let varsIntvsMapPrioritiesMaps = add_new_varsIntvsPriority (esl /. 10.) upperVarsIntvsMap varsIntvsMapPrioritiesMaps in
      add_new_varsIntvsPriority esl lowerVarsIntvsMap varsIntvsMapPrioritiesMaps
    else if lowerEasiness < upperEasiness then 
      let varsIntvsMapPrioritiesMaps = add_new_varsIntvsPriority esl lowerVarsIntvsMap varsIntvsMapPrioritiesMaps in
      add_new_varsIntvsPriority esl upperVarsIntvsMap varsIntvsMapPrioritiesMaps
    else
      let varsIntvsMapPrioritiesMaps = add_new_varsIntvsPriority esl upperVarsIntvsMap varsIntvsMapPrioritiesMaps in
      add_new_varsIntvsPriority esl lowerVarsIntvsMap varsIntvsMapPrioritiesMaps


(*Binary balance decomposition on intervals*)
let dynamicDecom varsIntvsMap varsIntvsMapPrioritiesMaps polyCons unkownPolyConstraints maxDecomposedVarsNum esl remainingTime = 
  print_endline ("Decomposing: " ^ polyCons#to_string_infix ^ ": " ^ string_of_int polyCons#get_miniSATCode);
  flush stdout;
  let startTime = Sys.time() in
  (* let add_varsSet currentVarsSet polyCons = VariablesSet.union polyCons#get_varsSet currentVarsSet in
  let varsSet = List.fold_left add_varsSet VariablesSet.empty unkownPolyConstraints in *)
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
    let intv = StringMap.find var varsIntvsMap in
    if not_smallIntv intv then 
      let newInfVar = 
        if intv.high = infinity || intv.low = neg_infinity then var
        else infVar
      in
      (VariablesSet.add var reducedVarsSet, newInfVar)
    else (
      print_endline ("small interval: " ^ var ^ ": " ^ sprintf_I "%f" intv);
      flush stdout;
      (reducedVarsSet, infVar)
    )
  in
  let (reducedVarsSet, infVar) = (*varsSet*) VariablesSet.fold add_notSmallInterval varsSet (VariablesSet.empty, "") in
  if VariablesSet.is_empty reducedVarsSet then (*Stop decomposition*) 
    (* let add_learnt_var var learntVars = 
      let (_, varId) = StringMap.find var varsIntvsMiniSATCodesMap in
      "-" ^ string_of_int varId ^ " " ^ learntVars
    in
    let polysMiniSATCodeString = 
      if polyCons#get_miniSATCode > 0 then "-" ^ string_of_int polyCons#get_miniSATCode
      else string_of_int (polyCons#get_miniSATCode)
    in
    let learntClauses = VariablesSet.fold add_learnt_var varsSet (polysMiniSATCodeString ^ " 0") in
    ((miniSATCodesVarsIntvsMap, nextMiniSATCode), learntClauses, "", false) *)
    add_new_varsIntvsPriority (esl /. 10.) varsIntvsMap varsIntvsMapPrioritiesMaps
  else (*Continue decomposition*)
    
    (*print_endline (string_of_bool polyCons#isInfinite);
    flush stdout;*)
    let decomposedVarsList = 
      if infVar <> "" then [(infVar, 0., 0)]
      else polyCons#get_n_varsSen_fromSet maxDecomposedVarsNum (*(VariablesSet.cardinal reducedVarsSet)*) reducedVarsSet 
    in
    let add_varIntvMiniSATCode currentVarsIntvsMiniSATCodesIsPositiveSenMap (var, varSen, isPositiveSen) = 
      let intvMiniSATCode = StringMap.find var varsIntvsMap in
      StringMap.add var (intvMiniSATCode, varSen, isPositiveSen) currentVarsIntvsMiniSATCodesIsPositiveSenMap
    in
    let decomposedVarsIntvsMiniSATCodesIsPositiveMap = List.fold_left add_varIntvMiniSATCode StringMap.empty decomposedVarsList in
    let varType =
      if polyCons#get_logic = "QF_NIA" then "Int"
      else "Real"
    in 
    StringMap.fold (decompose_var varType esl varsIntvsMap polyCons) decomposedVarsIntvsMiniSATCodesIsPositiveMap varsIntvsMapPrioritiesMaps


(*Binary balance decomposition on intervals*)
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
    decompose_var varType esl varsIntvsMap polyCons var (intv, 0., false) varsIntvsMapPrioritiesMaps