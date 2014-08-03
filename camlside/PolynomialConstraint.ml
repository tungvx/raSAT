open Ast
open IA
open Variable

(* ============================= START of polynomialConstraint class ================================= *)
(* Class for storing informations of a constraint *)
class polynomialConstraint boolExprInit =
  let varsSetInit = get_vars_set_boolExpr boolExprInit in
  let varsNumInit = VariablesSet.cardinal varsSetInit in
  let varsListInit = VariablesSet.elements varsSetInit in
  object (self)
    val boolExpr = boolExprInit
    val varsSet = varsSetInit
    val varsNum = varsNumInit
    val varsList = varsListInit
    val mutable varsSen = ([]:(string * float) list) (* varsSen is always sorted *)
    val mutable miniSATCode = 0
    method get_constraint = boolExpr
    
    method get_varsSet = varsSet
    
    method get_varsNum = varsNum
    
    method get_varsSen = varsSen
    method set_varsSen setVarsSen = varsSen <- setVarsSen
    
    method get_miniSATCode = miniSATCode
    method set_miniSATCode code = miniSATCode <- code
    
    (* convert the constraint into infix string *)
    method to_string_infix = string_infix_of_boolExp boolExpr
    
    method get_varsList = 
      if List.length varsSen = varsNum then self#get_n_varsSen varsNum
      else varsList
    
    (* check sat of this polynomial using ci*)
    method check_sat_ci (varsIntvsMiniSATCodesMap:((IA.interval * int) Variable.StringMap.t)) = check_sat_ci_boolExpr boolExpr varsIntvsMiniSATCodesMap
    
    (* check sat of this polynomial using combination of af2 and ci*)
    method check_sat_af_two_ci (varsIntvsMiniSATCodesMap:((IA.interval * int) Variable.StringMap.t)) = check_sat_af_two_ci_boolExpr boolExpr varsSet varsNum varsIntvsMiniSATCodesMap
        
    (* check sat of this polynomial using combination of af2 and ci, variables sensitivities are also returned *)
    method check_sat_af_two_ci_varsSens (varsIntvsMiniSATCodesMap:((IA.interval * int) Variable.StringMap.t)) = check_sat_af_two_ci_boolExpr_varsSens boolExpr varsSet varsNum varsIntvsMiniSATCodesMap
    
    (* get length of SAT by af2 and ci *)
    method check_sat_get_satLength (varsIntvsMiniSATCodesMap:((IA.interval * int) Variable.StringMap.t)) = check_sat_get_satLength_boolExpr boolExpr varsSet varsNum varsIntvsMiniSATCodesMap
    
    (* get n-first variables by varsSen *)
    method get_n_varsSen n = 
      let rec get_n_first varsSen n = match varsSen with 
        | [] -> []
        | (var, sen) :: t ->
          if n >= 1 then var :: (get_n_first t (n - 1))
          else []
      in
      get_n_first varsSen n
      
   (* get n-first variables by varsSen, the selected variables must be in the given set *)
    method get_n_varsSen_fromSet n varsSet = 
      (*let rec string_of_varsSen varsSen = 
        match varsSen with 
          | [] -> ""
          | (var, sen) :: t -> var ^ ": " ^ string_of_float sen ^ "\n" ^ string_of_varsSen t
      in
      print_endline (string_of_varsSen varsSen);
      flush stdout;*)
      let rec get_n_first varsSen n = match varsSen with 
        | [] -> []
        | (var, sen) :: t ->
          if n >= 1 then 
            if VariablesSet.mem var varsSet then var :: (get_n_first t (n - 1))
            else get_n_first t n
          else []
      in
      get_n_first varsSen n
    
    method get_varsDiffNum otherVarsSet = 
      let varsDiff = VariablesSet.diff varsSet otherVarsSet in
      VariablesSet.cardinal varsDiff
      
    method check_SAT varsTCsMap = 
      let (sat, _) = checkSAT_computeValues boolExpr varsTCsMap in
      sat
      
    method generateTCs assignedVarsSet (varsIntvsMiniSATCodesMap:((IA.interval * int) Variable.StringMap.t)) priorityNum = 
      (*print_endline self#to_string_infix;
      flush stdout;*)
      let neededVarsSet = VariablesSet.diff varsSet assignedVarsSet in
      let check_mem (var, sen) = VariablesSet.mem var neededVarsSet in
      let neededVarsSen = List.filter check_mem varsSen in
      (* This function generates test cases for one variable *)
      let rec generate_tc_var interval tcNum isFirst =
        if tcNum <= 0 then []
        else
          let lowerBound = interval#l in
          let upperBound = interval#h in
		
		      (* if the upper bound is -max_float, stop testing*)
		      if upperBound < 0.1 -. max_float then []
		      (* if the lowerBound is max_float, stop testing *)
		      else if lowerBound -. 0.1 > max_float then []
		      else
		        let bound = upperBound -. lowerBound in
		        Random.self_init();
			      let newBound =
				      if bound = infinity then max_float
				      else bound
			      in
		        let randomNum = Random.float newBound in (* random number from 0 to bound *)
			      let baseNum =
				      if lowerBound = neg_infinity then -.max_float
				      else lowerBound	
			      in
		        let tc =
		          if isFirst && (baseNum >= 0.1 -. max_float) then baseNum (*+. randomNum*)
		          else baseNum +. randomNum 
		        in
		        tc :: (generate_tc_var interval (tcNum - 1) false)
		  in
      let rec generateTCs_extra varsSen generatedTCs priorityNum = match varsSen with
        | [] -> (generatedTCs, priorityNum);
        | (var, sen) :: t ->
          let (interval, _) = StringMap.find var varsIntvsMiniSATCodesMap in
          let (testcases, newPriorityNum) =
            if priorityNum > 0 then
               (generate_tc_var interval 2 false, priorityNum - 1)
            else 
              (generate_tc_var interval 1 false, 0)
          in
          generateTCs_extra t ((var, testcases)::generatedTCs) newPriorityNum
      in
      generateTCs_extra neededVarsSen [] priorityNum
  end;;
(* ============================= END of polynomialConstraint class =================================== *)

type constraints = 
  | Single of polynomialConstraint
  | And of constraints * constraints
  | BOr of constraints * constraints
  

(* encode the constraints into the form of miniSAT lit *)  
let rec miniSATExpr_of_constraints constraints index miniSATCodesConstraintsMap = match constraints with
  | And (b1, b2) -> 
    let (mB1, index1, miniSATCodesConstraintsMap1, maxVarsNum1) = miniSATExpr_of_constraints b1 index miniSATCodesConstraintsMap in
    let (mB2, index2, miniSATCodesConstraintsMap2, maxVarsNum2) = miniSATExpr_of_constraints b2 index1 miniSATCodesConstraintsMap1 in
    (MAnd (mB1, mB2), index2, miniSATCodesConstraintsMap2, max maxVarsNum1 maxVarsNum2)
  | BOr (b1, b2) -> 
    let (mB1, index1, miniSATCodesConstraintsMap1, maxVarsNum1) = miniSATExpr_of_constraints b1 index miniSATCodesConstraintsMap in
    let (mB2, index2, miniSATCodesConstraintsMap2, maxVarsNum2) = miniSATExpr_of_constraints b2 index1 miniSATCodesConstraintsMap1 in
    (MOr (mB1, mB2), index2, miniSATCodesConstraintsMap2, max maxVarsNum1 maxVarsNum2)
  | Single polyCons -> (
      polyCons#set_miniSATCode index;
      let newMiniSATCodesConstraintsMap = IntMap.add index polyCons miniSATCodesConstraintsMap in
      (Lit index, index + 1, newMiniSATCodesConstraintsMap, polyCons#get_varsNum)  
    )
    

(* Function for converting constraints into infix string *)    
let rec string_infix_of_constraints constraints = match constraints with 
  | Single polyCons ->  polyCons#to_string_infix
  | And(c1, c2)  -> 
	  string_infix_of_constraints c1 ^ " And\n" ^ string_infix_of_constraints c2
	| BOr(c1, c2) -> 
	  string_infix_of_constraints c1 ^ " Or\n" ^ string_infix_of_constraints c2
	
	
(*==================== START string_infix_of_polynomialConstraints ==============================*)		
(* This function converts a list of polynomial constraints into the string of infix form *)
let rec string_infix_of_polynomialConstraints polyConses = 
  match polyConses with
  | [] -> ""
  | h::t ->
    string_infix_of_boolExp h#get_constraint ^ "\n" ^ string_infix_of_polynomialConstraints t
(*==================== END bool_expr_list_to_string_infix ==============================*)	  


(*==================== START string_prefix_of_constraints ==============================*)
(* prefix string format of constraints *)      
let rec string_prefix_of_constraints  = function
  | Single polyCons -> string_prefix_of_boolExpr polyCons#get_constraint
  | And(e1, e2) -> "(and "^(string_prefix_of_constraints e1)^" " ^ (string_prefix_of_constraints e2)^")"
  | BOr(e1, e2) -> "(or "^(string_prefix_of_constraints e1)^" " ^ (string_prefix_of_constraints e2)^")"
(*==================== END string_prefix_of_constraints ==============================*)
  

(*==================== START string_postfix_of_constraints ==============================*)
(* postfix string format of a boolean expression *)      
let rec string_postfix_of_constraints  = function
  | Single polyCons -> string_postfix_of_boolExpr polyCons#get_constraint
  | And(e1, e2) -> (string_postfix_of_constraints e1) ^ (string_postfix_of_constraints e2) ^ "and "
  | BOr(e1, e2) -> (string_postfix_of_constraints e1) ^ (string_postfix_of_constraints e2) ^ "or "
(*==================== START string_postfix_of_constraints ==============================*)


(*=== Function for converting list of contraints to string of postfix form ===*)      
let rec string_postfix_of_polyConstraints polyConstraints = match polyConstraints with
  |[] -> ""
  |[polyCons] -> string_postfix_of_boolExpr polyCons#get_constraint   
  |polyCons::t->
    (string_postfix_of_boolExpr polyCons#get_constraint) ^ " , " ^ (string_postfix_of_polyConstraints t)
(*===== end of contraints_list_toString ======*)


(*==================== START insertion_sort_polyCons ==============================*)
(* insert one polynomial constraint into a list of sorted polynomials constraints *)
(* the sort critia is the number of variables *)
let rec insertion_sort_polyCons polyCons polyConstraints = match polyConstraints with
  | [] -> [polyCons]
  | h::t -> 
    (*print_endline ("VarsNum of h: " ^ string_of_int h#get_varsNum);
    print_endline ("VarsNum of polyCons: " ^ string_of_int polyCons#get_varsNum);*)
    flush stdout;
    if polyCons#get_varsNum <= h#get_varsNum then polyCons::polyConstraints
    else h::(insertion_sort_polyCons polyCons t)
(*==================== END insertion_sort_polyCons ==============================*)


(*==================== START is_all_equalities ==============================*)		
(* This function checks if a list of boolean expressions are all equalities *)
let rec is_all_equations polyConstraints = 
  match polyConstraints with
  | [] -> true
  | h::t -> (is_boolExpr_equation h#get_constraint) && (is_all_equations t)
(*==================== END is_all_equalities ==============================*)


(*==================== START first_uk_cl ==============================*)		
(* This function tries to find a first inequality expression *)
let rec first_inequation polyConstraints = 
  match polyConstraints with
  | [] -> []
  | h::t -> (
    match h#get_constraint with 
    | Eq e -> first_inequation t
    | _ -> [h]
  )
(*==================== END first_uk_cl ==============================*)
