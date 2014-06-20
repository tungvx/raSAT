(*open Ast           (*type: literal, clause, formula... declaration*)
open Util
open Assignments
open IA

let decompose var interval decomposedPoint lstVarID iVar dIntv isLowerBound =
  let id = List.assoc var lstVarID in
  let si = "(or ("^ var ^ " in " ^ string_of_float interval#l ^ " " ^ string_of_float decomposedPoint ^ ") " ^
		             "(" ^ var ^ " in " ^ string_of_float decomposedPoint ^ " " ^ string_of_float interval#h ^ "))" in
  let sl = "-" ^ string_of_int id ^ " " ^ string_of_int (iVar + 1) ^ " " ^ string_of_int (iVar+2) ^ " 0 " ^
		           string_of_int id ^ " -" ^ string_of_int (iVar + 1) ^ " 0 " ^
		           "-" ^ string_of_int (iVar + 1) ^ " -"^string_of_int (iVar+2) ^ " 0 " ^
		           string_of_int id ^ " -"^string_of_int (iVar+2) ^ " 0 " in		             
  let bumpVarInt = 
    if isLowerBound then iVar + 1
    else iVar + 2
  in
  let bumpVarString = string_of_int bumpVarInt in
  (*print_endline ("Bump vars: " ^ bumpVarString ^ " " ^ string_of_bool isLowerBound);
  flush stdout;*)
  if (dIntv <> "") then
    ("(ic "^dIntv ^" "^si^")", sl, bumpVarString, true) 
  else
    (si, sl, bumpVarString, true)
  

let rec decompose_new_clause_extra (currentDIntv, currentMinisatCode, currentBumpVars, currentWasDecomposed) clause assIntv dIntv assignments lstVarID iVar esl varsList leftValue rightValue isSat isLowerBound =
	match varsList with 
	| [] -> (currentDIntv, currentMinisatCode, currentBumpVars, currentWasDecomposed)
	| var::t -> 
		(
    let polyExp = get_exp clause in
		let derivative = getDerivative polyExp var in
		let derivativeValue = evalFloat assignments derivative in
		let differentValue = leftValue -. rightValue in
		if isLowerBound = (differentValue *. derivativeValue < 0.) then
			let decomposedPoint = (List.assoc var assignments) -. differentValue /. finalDerivative in
			let interval = List.assoc var assIntv in
				if decomposedPoint -. interval#l > 0. && interval#h -. decomposedPoint > 0. then (
					let id = List.assoc var lstVarID in
					let si = "(or ("^ var ^ " in " ^ string_of_float interval#l ^ " " ^ string_of_float decomposedPoint ^ ") " ^
		             "(" ^ var ^ " in " ^ string_of_float decomposedPoint ^ " " ^ string_of_float interval#h ^ "))" in

		  		let sl = "-" ^ string_of_int id ^ " " ^ string_of_int (iVar + 1) ^ " " ^ string_of_int (iVar+2) ^ " 0 " ^
		           string_of_int id ^ " -" ^ string_of_int (iVar + 1) ^ " 0 " ^
		           "-" ^ string_of_int (iVar + 1) ^ " -"^string_of_int (iVar+2) ^ " 0 " ^
		           string_of_int id ^ " -"^string_of_int (iVar+2) ^ " 0 " in
          let bumpVarInt = 
            if isLowerBound then 
              if isSat then 
                iVar + 2
              else 
                iVar + 1
            else 
              if isSat then 
                iVar + 1
              else      
                iVar + 2        
          in
          let bumpVarString = string_of_int bumpVarInt in
					let newResult =
						if (currentDIntv <> "") then
							if currentWasDecomposed then
								("(ic "^currentDIntv ^" "^si^")", currentMinisatCode ^ sl, currentBumpVars ^ " " ^ bumpVarString, true)
							else 
								("(ic "^currentDIntv ^" "^si^")", sl, bumpVarString, true)
						else
							if currentWasDecomposed then
								(si, currentMinisatCode ^ sl, currentBumpVars ^ " " ^ bumpVarString, true)
							else 
								(si, sl, bumpVarString, true)
					in (*decompose_new_clause_extra newResult clause assIntv dIntv assignments lstVarID (iVar + 2) esl t leftValue rightValue isSat isLowerBound )*)
            newResult )
        else
					decompose_new_clause_extra (currentDIntv, currentMinisatCode, currentBumpVars, currentWasDecomposed) clause assIntv dIntv assignments lstVarID iVar esl t leftValue rightValue isSat isLowerBound
		else 
			decompose_new_clause_extra (currentDIntv, currentMinisatCode, currentBumpVars, currentWasDecomposed) clause assIntv dIntv assignments lstVarID iVar esl t leftValue rightValue isSat isLowerBound
		)

  
(* ====================== START dynamicDecom_new_clause ======================== *)
(* | decompose a variable of an unknown clause based on Newtow's method *)
(* | . clause: an unknown clause *)
(* | . assIntv: the intervals of variables *)
(* | . dIntv: the decomposed intervals from previous decompositions *)
(* | . lstVarID: the list of maps from variables into their literal code in dIntv *)
(* | . iVar: the current code of the last literal (in dIntv) *)
(* | . esl: the minimum values of intervals for stopping decomposition *)
let decompose_new_clause clause assIntv dIntv lstVarID iVar esl =
  let lowerBound = getLowerBound assIntv [] in
  let (lowerSat, lowerBoundLeftValue, lowerBoundRightValue) = checkSAT_computeValues clause lowerBound in
  if lowerSat then 
    let upperBound = getUpperBound assIntv [] in
		let (upperSat, upperBoundLeftValue, upperBoundRightValue) = checkSAT_computeValues clause upperBound in
		if upperSat then
			("","","",false)
		else 
			let varsList = Util.red_list (bool_vars clause) in
			decompose_new_clause_extra (dIntv, "", "", false) clause assIntv dIntv upperBound lstVarID iVar esl varsList upperBoundLeftValue upperBoundRightValue upperSat true
	else
		let varsList = Util.red_list (bool_vars clause) in
		let (newDIntv, minisatCode, bumpVars, wasDecomposed) =
		  decompose_new_clause_extra (dIntv, "", "", false) clause assIntv dIntv lowerBound lstVarID iVar esl varsList lowerBoundLeftValue lowerBoundRightValue lowerSat true
		in 
		  if wasDecomposed then 
				(newDIntv, minisatCode, bumpVars, wasDecomposed)
			else 
				let upperBound = getUpperBound assIntv [] in
				let (upperSat, upperBoundLeftValue, upperBoundRightValue) = checkSAT_computeValues clause upperBound in
				if upperSat then
					("","","",false)
				else
					let varsList = Util.red_list (bool_vars clause) in
					decompose_new_clause_extra (dIntv, "", "", false) clause assIntv dIntv upperBound lstVarID iVar esl varsList upperBoundLeftValue upperBoundRightValue upperSat false
(* ======================= END dynamicDecom_new_clause ========================== *)


(* This function find an unsat point for decomposition *)
let rec decompose_find_unsat_point boolExp assIntv dIntv lstVarID ivar esl var lowerBound upperBound currentPoint isFound isIncreasing =
  let newPoint = 
    if isIncreasing then currentPoint +. esl
    else currentPoint -. esl
  in 
  if (isIncreasing && (upperBound -. newPoint) > esl) || ((not isIncreasing) && (newPoint -. lowerBound) > esl) then 
    let newIntv = 
      if isIncreasing then new IA.interval lowerBound newPoint
      else new IA.interval newPoint upperBound
    in 
    (*print_endline ("Next Point: " ^ string_of_float newPoint ^ " Lower Bound: " ^ string_of_float lowerBound ^ " " ^ string_of_bool (newPoint > lowerBound));
    flush stdout;*)
    let (sat,_) = check_sat_af_two_ci boolExp ((var, newIntv)::assIntv) in
    if sat = -1 then decompose_find_unsat_point boolExp assIntv dIntv lstVarID ivar esl var lowerBound upperBound newPoint true isIncreasing
    else (isFound, currentPoint)
  else (isFound, currentPoint)


(* This functions try to decompose the interval of a specified variable so that one of the decomposed
   interval is UNSAT *)
let rec decompose_unsat_detection_for_var boolExp assIntv dIntv lstVarID iVar esl var =
  let intv = List.assoc var assIntv in
  let (isFound, decomposedPoint) = decompose_find_unsat_point boolExp assIntv dIntv lstVarID iVar esl var intv#l intv#h intv#l false true in
  if isFound then (
    print_endline ("Decomposed lower: " ^ var ^ " " ^ string_of_float intv#l ^ " " ^ string_of_float intv#h ^ ": " ^ string_of_float decomposedPoint);
    flush stdout;
    decompose var intv decomposedPoint lstVarID iVar dIntv true
  )
  else 
    let (isFound, decomposedPoint) = decompose_find_unsat_point boolExp assIntv dIntv lstVarID iVar esl var intv#l intv#h intv#h false false in
    if isFound then (
      print_endline ("Decomposed upper: " ^ var ^ " " ^ string_of_float intv#l ^ " " ^ string_of_float intv#h ^ ": " ^ string_of_float decomposedPoint);
      flush stdout;
      decompose var intv decomposedPoint lstVarID iVar dIntv false
    )
    else ("","","",false)


(* This is the help function for decompose_unsat_detection *)
let rec decompose_unsat_detection_extra boolExp assIntv dIntv lstVarID iVar esl varsList =
  match varsList with
  | [] -> ("","","",false)
  | var::remainingVars -> (
    let (newDIntv, minisatCode, bumpVars, wasDecomposed) = decompose_unsat_detection_for_var boolExp assIntv dIntv lstVarID iVar esl var in
    if wasDecomposed then (newDIntv, minisatCode, bumpVars, wasDecomposed)
    else decompose_unsat_detection_extra boolExp assIntv dIntv lstVarID iVar esl remainingVars
  )

(* This function tries to extends the length of decomposed intervals so that the api is unsat inside that interval *)
let decompose_unsat_detection boolExp assIntv dIntv lstVarID iVar esl =
  (*print_endline (bool_expr_to_infix_string boolExp);
  flush stdout;*)
  let varsList = Util.red_list (bool_vars boolExp) in
  decompose_unsat_detection_extra boolExp assIntv dIntv lstVarID iVar esl varsList
  
  
let rec decompose_list_unsat_detection boolExpList assIntv dIntv lstVarID iVar esl = 
  match boolExpList with 
  | [] -> ("","","",false)
  | boolExp::remainingBoolExps -> 
    let (newDIntv, minisatCode, bumpVars, wasDecomposed) = decompose_unsat_detection boolExp assIntv dIntv lstVarID iVar esl in
    if wasDecomposed then (newDIntv, minisatCode, bumpVars, wasDecomposed)
    else decompose_list_unsat_detection remainingBoolExps assIntv dIntv lstVarID iVar esl *)

