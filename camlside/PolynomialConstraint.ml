open Ast
open IA
open Variable

(* ============================= START of polynomialConstraint class ================================= *)
(* Class for storing informations of a constraint *)
class polynomialConstraint boolExprInit =
  let varsSetInit = get_vars_set_boolExpr boolExprInit in
  let isPositiveDirected = 
    match boolExprInit with
    | Eq _ -> false
    | Neq _ -> false
    | Geq _ -> true
    | Leq _ -> false
    | Gr _ -> true
    | Le _ -> false
  in
  let varsNumInit = VariablesSet.cardinal varsSetInit in
  let varsListInit = VariablesSet.elements varsSetInit in
  object (self)
    val boolExpr = boolExprInit
    val varsSet = varsSetInit
    val varsNum = varsNumInit
    val varsList = varsListInit
    val mutable varsSen =
      let addVarSen varSenList var = (var, 0., false)::varSenList in
      List.fold_left addVarSen [] varsListInit
    val mutable miniSATCode = 0
    val mutable satLength = 0. 
    val mutable isInfinite = false
    val mutable testValue = 0.
    val mutable iaValue = new IA.interval 0. 0.
    val mutable logic = ""
    
    method get_constraint = boolExpr
    
    method get_varsSet = varsSet
    
    method get_varsNum = varsNum
    
    method get_varsSen = varsSen
    method set_varsSen setVarsSen = varsSen <- setVarsSen
    
    method get_miniSATCode = miniSATCode
    method set_miniSATCode code = miniSATCode <- code
    
    method get_satLength = satLength
    
    method isPositiveDirected = isPositiveDirected
    
    method isInfinite = isInfinite
    
    method set_logic setLogic = logic <- setLogic
    method get_logic = logic
    
    (* convert the constraint into infix string *)
    method to_string_infix = string_infix_of_boolExp boolExpr
    
    method get_varsList = 
      if List.length varsSen = varsNum then self#get_n_varsSen varsNum
      else varsList
    
    (* check sat of this polynomial using ci*)
    method private check_sat_getBound_ici (varsIntvsMap:(IA.interval Variable.StringMap.t)) = check_sat_getBound_ici_boolExpr boolExpr varsIntvsMap
    
    (* check sat of this polynomial using combination of af2 and ci*)
    method private check_sat_af_two_ci (varsIntvsMap:(IA.interval Variable.StringMap.t)) = check_sat_af_two_ci_boolExpr boolExpr varsSet varsNum varsIntvsMap
        
    (* check sat of this polynomial using combination of af2 and ci, variables sensitivities are also returned *)
    method private check_sat_getBound_af_two_ci_varsSens (varsIntvsMap:(IA.interval Variable.StringMap.t)) = 
      let (sat, computedSatLength, sortedVarsSen, bound) = check_sat_getBound_af_two_ci_boolExpr_varsSens boolExpr varsSet varsNum varsIntvsMap in
      varsSen <- sortedVarsSen;
      satLength <- computedSatLength;
      (sat, bound)
    
    (* This method does not update isInfinite field, and varsSen is not computed *)
    method check_sat (varsIntvsMiniSATCodesMap:((IA.interval * int) Variable.StringMap.t)) =
      let add_intv (isInfinite, varsIntvsMap) var =
        let (intv, _) = StringMap.find var varsIntvsMiniSATCodesMap in
        (isInfinite || intv#h = infinity || intv#l = neg_infinity, StringMap.add var intv varsIntvsMap)
      in
      let (isInfiniteTmp, varsIntvsMap) = List.fold_left add_intv (false, StringMap.empty) varsList in
      
      if isInfiniteTmp then 
        let (sat, _) = self#check_sat_getBound_ici varsIntvsMap in
        sat
      else 
        self#check_sat_af_two_ci varsIntvsMap
    
    method check_sat_varsSen_setIsInfinite_setBounds (varsIntvsMiniSATCodesMap:((IA.interval * int) Variable.StringMap.t)) =
      let add_intv (isInfinite, varsIntvsMap) var =
        let (intv, _) = StringMap.find var varsIntvsMiniSATCodesMap in
        (isInfinite || intv#h = infinity || intv#l = neg_infinity, StringMap.add var intv varsIntvsMap)
      in
      let (isInfiniteTmp, varsIntvsMap) = List.fold_left add_intv (false, StringMap.empty) varsList in
      isInfinite <- isInfiniteTmp;
      let (sat, bound) =
        if isInfinite then 
          self#check_sat_getBound_ici varsIntvsMap
        else 
          self#check_sat_getBound_af_two_ci_varsSens varsIntvsMap
      in
      iaValue <- bound;
      sat
    
    (* get length of SAT by af2 and ci *)
    method check_sat_get_satLength (varsIntvsMiniSATCodesMap:((IA.interval * int) Variable.StringMap.t)) = 
      (*print_endline ("Start get length: " ^ self#to_string_infix);
      flush stdout;*)
      let add_intv (isInfinite, varsIntvsMap) var =
        let (intv, _) = StringMap.find var varsIntvsMiniSATCodesMap in
        (isInfinite || intv#h = infinity || intv#l = neg_infinity, StringMap.add var intv varsIntvsMap)
      in
      let (isInfiniteTmp, varsIntvsMap) = List.fold_left add_intv (false, StringMap.empty) varsList in
      if isInfiniteTmp then
        check_sat_ici_get_satLength_boolExpr boolExpr varsIntvsMap
      else 
        check_sat_af_two_ci_get_satLength_boolExpr boolExpr varsSet varsNum varsIntvsMap
        
    
    (* get n-first variables by varsSen *)
    method get_n_varsSen n = 
      let rec get_n_first varsSen n = match varsSen with 
        | [] -> []
        | (var, sen, _) :: t ->
          if n >= 1 then var :: (get_n_first t (n - 1))
          else []
      in
      get_n_first varsSen n
      
   (* get n-first variables by varsSen, the selected variables must be in the given set *)
    method get_n_varsSen_fromSet n varsSet = 
      (*let rec string_of_varsSen varsSen = 
        match varsSen with 
          | [] -> ""
          | (var, sen, _) :: t -> var ^ ": " ^ string_of_float sen ^ "\n" ^ string_of_varsSen t
      in
      print_endline (string_of_varsSen varsSen);
      flush stdout;*)
      let rec get_n_first varsSen n = match varsSen with 
        | [] -> []
        | (var, varSen, isPositiveSen)::t ->
          if n >= 1 then 
            if VariablesSet.mem var varsSet then (var, varSen, isPositiveSen) :: (get_n_first t (n - 1))
            else get_n_first t n
          else []
      in
      let rec get_n_random varsSen n = match varsSen with
        | [] -> []
        | _ ->
          if n >= 1 then (
            let rec remove aList index checkedList = 
              if index = 0 then
                match aList with
                | [] -> raise (Failure "Not found")
                | h::t -> (h, checkedList@t)
              else if index > 0 then
                match aList with
                | [] -> raise (Failure "Not found")
                | h::t -> remove t (index - 1) (h::checkedList)
              else raise (Failure "Not found")
            in
            Random.self_init();
            let randomIndex = Random.int (List.length varsSen) in
            let ((var, varSen, isPositiveSen), remainingVarsSen) = remove varsSen randomIndex [] in
            if VariablesSet.mem var varsSet then (var, varSen, isPositiveSen) :: (get_n_random remainingVarsSen (n - 1))
            else get_n_random remainingVarsSen n
          )
          else []
      in
      get_n_first varsSen n
      (*get_n_random varsSen n*)
    
    method get_varsDiffNum otherVarsSet = 
      let varsDiff = VariablesSet.diff varsSet otherVarsSet in
      VariablesSet.cardinal varsDiff
      
    method check_SAT varsTCsMap =
      (*print_endline (self#to_string_infix); 
      flush stdout;*)
      let (sat, value) = checkSAT_computeValues boolExpr varsTCsMap in
      testValue <- value; 
      sat
      
    method log_test = 
      let testValueString = string_of_float testValue in
      match boolExpr with
      |Eq e -> 
		    (string_infix_of_polyExpr e) ^ " = 0"
	    |Neq e -> 
		    (string_infix_of_polyExpr e) ^ " = " ^ testValueString ^ " != 0"
	    |Leq e -> 
		    (string_infix_of_polyExpr e) ^ " = " ^ testValueString ^ " <= 0"
	    |Le e -> 
        (string_infix_of_polyExpr e) ^ " = " ^ testValueString ^ " < 0"
	    |Geq e -> 
		    (string_infix_of_polyExpr e) ^ " = " ^ testValueString ^ " >= 0"
	    |Gr e -> 
		    (string_infix_of_polyExpr e) ^ " = " ^ testValueString ^ " > 0"  
		    
		method log_ia = 
		  let lowerString = string_of_float iaValue#l in
      let upperString = string_of_float iaValue#h in
      let iaString = "[" ^ lowerString ^ ", " ^ upperString ^ "]" in
      match boolExpr with
      |Eq e -> 
		    (string_infix_of_polyExpr e) ^ " = " ^ iaString ^ " = 0"
	    |Neq e -> 
		    (string_infix_of_polyExpr e) ^ " = " ^ iaString ^ " != 0"
	    |Leq e -> 
		    (string_infix_of_polyExpr e) ^ " = " ^ iaString ^ " <= 0"
	    |Le e -> 
        (string_infix_of_polyExpr e) ^ " = " ^ iaString ^ " < 0"
	    |Geq e -> 
		    (string_infix_of_polyExpr e) ^ " = " ^ iaString ^ " >= 0"
	    |Gr e -> 
		    (string_infix_of_polyExpr e) ^ " = " ^ iaString ^ " > 0"  
    
    method generateTCs assignedVarsSet (varsIntvsMiniSATCodesMap:((IA.interval * int) Variable.StringMap.t)) priorityNum = 
      (*print_endline ("\n" ^ self#to_string_infix);
      flush stdout;*)
      let neededVarsSet = VariablesSet.diff varsSet assignedVarsSet in
      let check_mem (var, _, _) = VariablesSet.mem var neededVarsSet in
      let neededVarsSen = List.filter check_mem varsSen in
      (* This function generates test cases for one variable *)
      let rec generate_tc_var interval tcNum isFirst varSen isPositiveSen =
        if tcNum <= 0 then []
        else
          let lowerBound = interval#l in
          let upperBound = interval#h in
		      (*print_endline ("[" ^ string_of_float lowerBound ^ ", " ^ string_of_float upperBound ^ "]");
		      flush stdout;*)
		      (*(* if the upper bound is -max_float, stop testing*)
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
		        in*)
		        let tc =
		          (*if isInfinite || tcNum > 1 || varSen = 0. then
		            let lowerBase = 
		              if lowerBound = neg_infinity then min_float
		              else lowerBound
		            in
	              let bound = upperBound -. lowerBound in
	              let bound = 
	                if bound = infinity then max_float
	                else bound
	              in
	              Random.self_init();
	              let randomNum = Random.float bound in (* random number from 0 to bound *)
		            lowerBase +. randomNum
	            else if isPositiveSen = isPositiveDirected then upperBound
	            else lowerBound*)
	            let lowerBase = 
	              if lowerBound = neg_infinity then min_float
	              else lowerBound
	            in
              let bound = upperBound -. lowerBound in
              let bound = 
                if bound = infinity then max_float
                else bound
              in
              Random.self_init();
              let randomNum = Random.float bound in (* random number from 0 to bound *)
              if logic = "QF_NIA" then 
                ceil(lowerBase +. randomNum)
              else 
	              lowerBase +. randomNum
		        in
		        (*print_endline (string_of_float tc);
		        flush stdout;*)
		        tc :: (generate_tc_var interval (tcNum - 1) false varSen isPositiveSen)
		  in
      let rec generateTCs_extra varsSen generatedTCs priorityNum = match varsSen with
        | [] -> (generatedTCs, priorityNum);
        | (var, varSen, isPositiveSen) :: t ->
          (*print_endline (var ^ ": " ^ string_of_float varSen ^ ": " ^ string_of_bool isPositiveSen);
          flush stdout;*)
          let (interval, _) = StringMap.find var varsIntvsMiniSATCodesMap in
          let (testcases, newPriorityNum) =
            if priorityNum > 0 then (
              (*print_endline var;
              flush stdout;*)
              (generate_tc_var interval 2 true varSen isPositiveSen, priorityNum - 1)
            )
            else 
              (generate_tc_var interval 1 true varSen isPositiveSen, 0)
          in
          generateTCs_extra t ((var, testcases)::generatedTCs) newPriorityNum
      in
      let rec generateTCs_extra_random varsSen generatedTCs priorityNum = match varsSen with
        | [] -> (generatedTCs, priorityNum);
        | (var, varSen, isPositiveSen)::t ->
          if priorityNum > 0 then (
            Random.self_init();
            let randomIndex = Random.int (List.length varsSen) in
            let rec remove aList index checkedList = 
              if index = 0 then
                match aList with
                | [] -> raise (Failure "Not found")
                | h::t1 -> (h, checkedList@t1)
              else if index > 0 then
                match aList with
                | [] -> raise (Failure "Not found")
                | h::t1 -> remove t1 (index - 1) (h::checkedList)
              else raise (Failure "Not found")
            in
            let ((selectedVar, _, _), remainingVarsSen) = remove varsSen randomIndex [] in
            (*print_endline selectedVar;
            flush stdout;*)
            let (interval, _) = StringMap.find selectedVar varsIntvsMiniSATCodesMap in
            let testcases = generate_tc_var interval 2 true 0 false in
            generateTCs_extra_random remainingVarsSen ((selectedVar, testcases)::generatedTCs) (priorityNum - 1)
          )
          else
            let (interval, _) = StringMap.find var varsIntvsMiniSATCodesMap in 
            let testcases = generate_tc_var interval 1 true 0 false in
            generateTCs_extra_random t ((var, testcases)::generatedTCs) 0
      in
      generateTCs_extra neededVarsSen [] priorityNum
      (*generateTCs_extra_random neededVarsSen [] priorityNum*)
  end;;
(* ============================= END of polynomialConstraint class =================================== *)

type constraints = 
  | Single of polynomialConstraint
  | And of constraints * constraints
  | BOr of constraints * constraints
  

(* encode the constraints into the form of miniSAT lit *)  
let rec miniSATExpr_of_constraints constraints index miniSATCodesConstraintsMap logic = match constraints with
  | And (b1, b2) -> 
    let (mB1, index1, miniSATCodesConstraintsMap1, maxVarsNum1) = miniSATExpr_of_constraints b1 index miniSATCodesConstraintsMap logic in
    let (mB2, index2, miniSATCodesConstraintsMap2, maxVarsNum2) = miniSATExpr_of_constraints b2 index1 miniSATCodesConstraintsMap1 logic in
    (MAnd (mB1, mB2), index2, miniSATCodesConstraintsMap2, max maxVarsNum1 maxVarsNum2)
  | BOr (b1, b2) -> 
    let (mB1, index1, miniSATCodesConstraintsMap1, maxVarsNum1) = miniSATExpr_of_constraints b1 index miniSATCodesConstraintsMap logic in
    let (mB2, index2, miniSATCodesConstraintsMap2, maxVarsNum2) = miniSATExpr_of_constraints b2 index1 miniSATCodesConstraintsMap1 logic in
    (MOr (mB1, mB2), index2, miniSATCodesConstraintsMap2, max maxVarsNum1 maxVarsNum2)
  | Single polyCons -> (
      polyCons#set_miniSATCode index;
      polyCons#set_logic logic;
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


(*==================== START log_test ==============================*)		
(* log the sat values of constraints *)
let rec log_test polyConstraints = 
  match polyConstraints with
  | [] -> ""
  | polyCons::remaining -> polyCons#log_test ^ "\n" ^ log_test remaining
(*==================== END log_test ==============================*)


(*==================== START log_ia ==============================*)		
(* log the bounds of IA-VALID constraints *)
let rec log_ia polyConstraints = match polyConstraints with
  | [] -> ""
  | polyCons::remaining -> polyCons#log_ia ^ "\n" ^ log_ia remaining
(*==================== START log_ia ==============================*)		
