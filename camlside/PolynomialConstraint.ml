open Ast
open IA
open Variable
open Interval
open Assignments
open Expr

(* ============================= START of polynomialConstraint class ================================= *)
(* Class for storing informations of a constraint *)
class polynomialConstraint boolExprInit (variables:(int Variable.StringMap.t)) =
  let varsSetInit = get_varsSet_polyCons boolExprInit in
  let (isPositiveDirected, isEquation, isNotEquation) = 
    match boolExprInit with
    | Eq _ -> (false, true, false)
    | Neq _ -> (false, false, true)
    | Geq _ -> (true, false, false)
    | Leq _ -> (false, false, false)
    | Gr _ -> (true, false, false)
    | Le _ -> (false, false, false)
  in
  let varsNumInit = VariablesSet.cardinal varsSetInit in
  let varsListInit = VariablesSet.elements varsSetInit in
  object (self)
    val mutable boolExpr = boolExprInit
    val mutable neg_boolExpr = not_of_polyConstraint boolExprInit
    val mutable is_negated = false

    val mutable derivatives = 
      
      (* let printString = "Start getting derivatives of " ^ string_infix_of_polyExpr (get_exp boolExprInit) in
      print_endline printString;
      flush stdout; *)

      let add_derivative var currentMap = 
        let derivative = reduce (get_derivative var (get_exp boolExprInit)) variables in
        StringMap.add var derivative currentMap
      in

      (* print_endline "Finished getting derivatives";
      flush stdout; *)

      VariablesSet.fold add_derivative varsSetInit StringMap.empty

    val varsSet = varsSetInit
    val varsIndicesMap = 
      let addVarsIndex var (currentvarsIndicesMap, nextIndex) = 
        (StringMap.add var nextIndex currentvarsIndicesMap, nextIndex + 1)
      in
      let (finalVarsIndicesMap, _) = VariablesSet.fold addVarsIndex varsSetInit (StringMap.empty, 1) in 
      finalVarsIndicesMap
    val varsNum = varsNumInit
    val varsList = varsListInit
    val mutable varsSen =
      let addVarSen varSenList var = (var, 0., false)::varSenList in
      List.fold_left addVarSen [] varsListInit
    val mutable miniSATCode = 0
    val mutable satLength = 0. 
    val mutable isInfinite = false
    val mutable testValue = 0.
    val mutable iaValue = {low=neg_infinity;high=infinity}
    val mutable logic = ""
    val mutable easiness = 0.
    val mutable log = ""
    val mutable varsSATDirectionMap = StringMap.empty
    val variables = variables

    method get_varType var = StringMap.find var variables

    method get_varsSATDirectionMap = varsSATDirectionMap
    method get_varSATDirection var = StringMap.find var self#get_varsSATDirectionMap
    method private set_varSATDirectionMap sortedVarsSen =
      let add_varSATDirection (var, varSen, isPositiveSen) currentMap =
        let isVarPositiveDirected = 
          if varSen = 0. then 0
          else if isPositiveSen == isPositiveDirected then 1
          else -1
        in
        StringMap.add var isVarPositiveDirected currentMap
      in
      varsSATDirectionMap <- List.fold_right add_varSATDirection sortedVarsSen StringMap.empty 

    
    method get_log = log
    method set_log setLog = log <- setLog

    method get_derivatives = derivatives
    method set_derivatives setDerivatives = derivatives <- setDerivatives
    (* method print_derivatives = 
      print_endline ("Derivatives of " ^ string_infix_of_polyExpr self#get_polyExpr);
      let print_derivative var derivative = 
        print_endline (var ^ ": " ^ derivative#to_string_infix);
      in
      flush stdout;
      StringMap.iter print_derivative self#get_derivatives *)
    
    method get_constraint = 
      if is_negated then neg_boolExpr
      else boolExpr

    method is_negated = is_negated
    method set_negated set_is_negated = is_negated <- set_is_negated

    method set_polyExpr polyExpr =
      let newBoolExpr = match boolExpr with
      | Eq _ -> Eq polyExpr
      | Neq _ -> Neq polyExpr 
      | Geq _ -> Geq polyExpr
      | Leq _ -> Leq polyExpr
      | Gr _ -> Gr polyExpr
      | Le _ -> Le polyExpr
      in 
      let new_neg_boolExpr = not_of_polyConstraint newBoolExpr in
      boolExpr <- newBoolExpr;
      neg_boolExpr <- new_neg_boolExpr
    method get_polyExpr = get_exp self#get_constraint
    
    method get_varsSet = varsSet
    
    method get_varsNum = varsNum
    
    method get_varsSen = varsSen
    method set_varsSen setVarsSen = varsSen <- setVarsSen
    
    method get_miniSATCode = 
      if is_negated then -miniSATCode
      else miniSATCode
    method set_miniSATCode code = miniSATCode <- code
    
    method get_satLength = satLength
    
    method isPositiveDirected = isPositiveDirected
    
    method isEquation = if isEquation then 1 else 0
    
    method isNotEquation = if isNotEquation then 1 else 0
    
    method isInfinite = isInfinite
    
    method set_logic setLogic = logic <- setLogic
    method get_logic = logic
    
    method get_iaValue = iaValue
    method get_contractedIntv = match self#get_constraint with
      | Eq _ -> {low=0.; high=0.}
      | Neq _ -> iaValue
      | Geq _ -> inter_I_I iaValue {low=0.;high=infinity}
      | Leq _ -> inter_I_I iaValue {low=neg_infinity;high=0.}
      | Gr _ -> inter_I_I iaValue {low=0.;high=infinity}
      | Le _ -> inter_I_I iaValue {low=neg_infinity;high=0.}
    
    method get_easiness = easiness
    
    (* convert the constraint into infix string *)
    method to_string_infix = string_infix_of_boolExp self#get_constraint
    
    (* convert the varsSen into string *)
    method string_of_varsSen = 
      let add_string_var_sen oldString (var, sen, _) = 
        oldString ^ var ^ ": " ^ string_of_float sen ^ "; "
      in
      List.fold_left add_string_var_sen "" varsSen

    method get_varsList = 
      if List.length varsSen = varsNum then self#get_n_varsSen varsNum
      else varsList

    method private check_sat_posDerivative lowerSign upperSign = match self#get_constraint with
    | Eq _ -> 
      if lowerSign = 1 || upperSign = -1 then -1
      else 0
    | Neq _ -> 
      if lowerSign = 1 || upperSign = -1 then 1
      else 0
    | Geq _ -> 
      if lowerSign >= 0 then 1
      else if upperSign = -1 then -1
      else 0
    | Leq _ -> 
      if upperSign = -1 || upperSign = 0 then 1
      else if lowerSign = 1 then -1
      else 0
    | Gr _ -> 
      if lowerSign = 1 then 1
      else if upperSign = 0 || upperSign = -1 then -1
      else 0
    | Le _ -> 
      if upperSign = -1 then 1
      else if lowerSign >= 0 then -1
      else 0

    method private check_sat_zeroDerivative lowerSign upperSign = match self#get_constraint with
    | Eq _ -> 
      if lowerSign = 0 then 1
      else if lowerSign = 1 || lowerSign = -1 then -1
      else 0
    | Neq _ -> 
      if lowerSign = 1 || upperSign = -1 then 1
      else if lowerSign = 0 then -1
      else 0
    | Geq _ -> 
      if lowerSign >= 0 then 1
      else if lowerSign = -1 then -1
      else 0
    | Leq _ -> 
      if lowerSign = -1 || lowerSign = 0 then 1
      else if lowerSign = 1 then -1
      else 0
    | Gr _ -> 
      if lowerSign = 1 then 1
      else if lowerSign = 0 || lowerSign = -1 then -1
      else 0
    | Le _ -> 
      if lowerSign = -1 then 1
      else if lowerSign >= 0 then -1
      else 0

    method private check_sat_negDerivative lowerSign upperSign = match self#get_constraint with
    | Eq _ -> 
      if lowerSign = -1 || upperSign = 1 then -1
      else 0
    | Neq _ -> 
      if lowerSign = -1 || upperSign = 1 then 1
      else 0
    | Geq _ -> 
      if upperSign >= 0 then 1
      else if lowerSign = -1 then -1
      else 0
    | Leq _ -> 
      if lowerSign = -1 || lowerSign = 0 then 1
      else if upperSign = 1 then -1
      else 0
    | Gr _ -> 
      if upperSign = 1 then 1
      else if lowerSign = 0 || lowerSign = -1 then -1
      else 0
    | Le _ -> 
      if lowerSign = -1 then 1
      else if upperSign >= 0 then -1
      else 0

    method private check_sat_providedDerivatives derivative lowerSign upperSign =
      if derivative = 1 then 
        self#check_sat_posDerivative lowerSign upperSign
      else if derivative = 0 then 
        self#check_sat_zeroDerivative lowerSign upperSign
      else if derivative = -1 then 
        self#check_sat_negDerivative lowerSign upperSign
      else 0

    method private check_sat_using_derivatives (varsIntvsMap:(Interval.interval Variable.StringMap.t)) =
      let check_sat_using_derivative_var var derivative =
        let varIntv = StringMap.find var varsIntvsMap in

        if varIntv.low = neg_infinity && varIntv.high = infinity then
          0
        else

          (* Compute lower value of the poly with the lower one of var *)
          let lowerBound = 
            if varIntv.low != neg_infinity then
              let lowerIntv = StringMap.add var {low=varIntv.low;high=varIntv.low} varsIntvsMap in
              self#get_bound lowerIntv
            else {low=neg_infinity;high=infinity}
          in
          let lowerSign = get_sign lowerBound in

          (* Compute upper value of the polynomial with the upper one of var *)
          let upperBound = 
            if varIntv.high != infinity then
              let upperIntv = StringMap.add var {low=varIntv.high;high=varIntv.high} varsIntvsMap in
              self#get_bound upperIntv
            else {low=neg_infinity;high=infinity}
          in            
          let upperSign = get_sign upperBound in

          if lowerSign != -2 || upperSign != -2 then 
            (* Checking whether the derivative is less than 0 *)
            let (newDerivative, _, _, _, bound) = 
                    check_sat_getBound_af_two_ci_boolExpr_varsSens (Eq derivative) varsSet varsNum 
                                    varsIntvsMap varsIndicesMap 
            in
            let sign = get_sign bound in 
            self#set_derivatives (StringMap.add var newDerivative derivatives);
            
            let sat = self#check_sat_providedDerivatives sign lowerSign upperSign in

            (* (if sat != 0 then
              let printString = "\nDetected using derivative of " ^ var ^ 
                      string_infix_of_polyExpr derivative ^
                      " in " ^ self#to_string_infix 
              in
              print_endline printString;
              print_I bound;
              print_endline(": " ^ string_of_int sign);
              print_I lowerBound;
              print_endline(": " ^ string_of_int lowerSign);
              print_I upperBound;
              print_endline(": " ^ string_of_int upperSign);
              print_endline ("SAT: " ^ string_of_int sat);
              flush stdout;
            ); *)

            sat

          else
            0
      in 
      let rec check_sat_using_derivative derivatives =
        if StringMap.is_empty derivatives then 
          0
        else
          let (var, derivative) = 
            StringMap.choose derivatives
          in
          let sat = check_sat_using_derivative_var var derivative in
          if sat != 0 then 
            sat
          else
            check_sat_using_derivative (StringMap.remove var derivatives)
      in
      check_sat_using_derivative self#get_derivatives
    

    (* check sat of this polynomial using ci*)
    (*method private check_sat_getBound_ici (varsIntvsMap:(IA.interval Variable.StringMap.t)) = check_sat_getBound_getSATLength_ici_boolExpr boolExpr varsIntvsMap*)
    
    (* check sat of this polynomial using combination of af2 and ci*)
    (* method private check_sat_af_two_ci (varsIntvsMap:(Interval.interval Variable.StringMap.t)) = check_sat_af_two_ci_boolExpr boolExpr varsSet varsNum varsIntvsMap *)
        
    (* check sat of this polynomial using combination of af2 and ci, variables sensitivities are also returned *)
    method private check_sat_getBound_af_two_ci_varsSens 
                              (varsIntvsMap:(Interval.interval Variable.StringMap.t)) = 
      let (newPolyExpr, sat, computedSatLength, sortedVarsSen, bound) = 
          check_sat_getBound_af_two_ci_boolExpr_varsSens self#get_constraint varsSet varsNum 
                                      varsIntvsMap varsIndicesMap 
      in
      self#set_polyExpr newPolyExpr;
      self#set_varsSen sortedVarsSen;
      self#set_varSATDirectionMap sortedVarsSen;
      satLength <- computedSatLength;
      let sat = 
        if sat = 0 then 
          self#check_sat_using_derivatives varsIntvsMap
        else 
          sat
      in
      (sat, bound, computedSatLength)
    
    (* This method does not update isInfinite field, and varsSen is not computed *)
    (* method check_sat (varsIntvsMap:(Interval.interval Variable.StringMap.t)) = 
        self#check_sat_af_two_ci varsIntvsMap *)
    
    method check_sat_varsSen_setIsInfinite_setBounds_setEasiness (varsIntvsMap:(Interval.interval Variable.StringMap.t)) 
                                                                    =
      (* let exist_infinity var intv =
        intv.high = infinity || intv.low = neg_infinity
      in
      let isInfiniteTmp = StringMap.exists exist_infinity varsIntvsMap in
      isInfinite <- isInfiniteTmp; *)
      let (sat, bound, satLength) =
        (*if isInfinite then 
          self#check_sat_getBound_ici varsIntvsMap
        else*) 
          self#check_sat_getBound_af_two_ci_varsSens varsIntvsMap
      in
      let setEasiness = 
        if satLength = infinity then (
          if bound.high = infinity && bound.low = neg_infinity then 0.5
          else 1.
        )

        else satLength /. (bound.high -. bound.low)
      in
      easiness <- setEasiness;
      iaValue <- bound;
      sat
    
    method get_bound (varsIntvsMap:(Interval.interval Variable.StringMap.t))
             = 
      let (_, _, _, _, bound) = check_sat_getBound_af_two_ci_boolExpr_varsSens 
                                        self#get_constraint varsSet varsNum varsIntvsMap varsIndicesMap in
      
      bound
    
    (* get length of SAT by af2 and ci *)
    method check_sat_get_satLength (varsIntvsMap:(Interval.interval Variable.StringMap.t))  =
      let (sat, bound, satLength) =
        (*if isInfiniteTmp then 
          self#check_sat_getBound_ici varsIntvsMap
        else *)
          let (_, sat, computedSatLength, sortedVarsSen, bound) = 
                    check_sat_getBound_af_two_ci_boolExpr_varsSens self#get_constraint varsSet varsNum 
                                    varsIntvsMap varsIndicesMap 
          in
          (sat, bound, computedSatLength)
      in
      (sat, satLength, satLength /. (bound.high -. bound.low))
        
    
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
            if VariablesSet.mem var varsSet then 
              let isVarPositiveDirected = 
                if varSen = 0. then 0
                else if isPositiveSen = isPositiveDirected then 1
                else -1
              in
              (var, varSen, isVarPositiveDirected) :: (get_n_first t (n - 1))
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
      get_n_first varsSen n (*(8)*)
      (*get_n_random varsSen n (*(9)*)*)
    
    method get_varsDiffNum otherVarsSet = 
      let varsDiff = VariablesSet.diff varsSet otherVarsSet in
      VariablesSet.cardinal varsDiff
      
    method get_varsDiff otherVarsSet = VariablesSet.diff varsSet otherVarsSet

    method check_SAT varsTCsMap =
      (*print_endline (self#to_string_infix); 
      flush stdout;*)
      let (sat, value) = checkSAT_computeValues self#get_constraint varsTCsMap in
      
      (* print_endline ("Test case: " ^ log_assignment varsTCsMap);
      flush stdout; *)

      testValue <- value; 
      sat
   
    method verify_SAT varsTCsMap = 
      (* print_endline ("verifying " ^ self#to_string_infix);
      flush stdout; *)
      let convert_toIntv var tc currentVarsIntvsMap = 
        StringMap.add var {low=tc;high=tc} currentVarsIntvsMap 
      in
      let varsIntvsMap = StringMap.fold convert_toIntv varsTCsMap StringMap.empty in
      let polyExpr = self#get_polyExpr in
      let (_,_, ciBound, _)  = poly_eval_ci varsIntvsMap varsNum false polyExpr in
      (*print_endline ("Bound" ^ ciBound#to_string);
      flush stdout;*)
      iaValue <- ciBound;
      check_sat_providedBounds self#get_constraint ciBound
      
    method add_sat_direction currentVarSATDirectionMap = 
      let add_sat_direction_extra currentMap (var, varSen, isPositiveSen) =
        if varSen = 0. then StringMap.add var 0 currentMap
        else 
          let newValue = 
            if isPositiveSen == isPositiveDirected then 1
            else -1
          in
          try
            let oldValue = StringMap.find var currentMap in
            if oldValue = newValue then currentMap
            else StringMap.add var 0 currentMap
          with Not_found -> StringMap.add var newValue currentMap
      in
      if self#get_logic = realTheory && isEquation then
        currentVarSATDirectionMap
      else
        List.fold_left add_sat_direction_extra currentVarSATDirectionMap varsSen
      
      
    (*method backward_interval_propagate var intv (varsIntvsMiniSATCodesMap:(IA.interval * int) Variable.StringMap.t) =
      let (polyExprInterval, polyExpr) = 
        match boolExpr with
        |Eq e -> (new IA.interval 0. 0., e)
        |Neq e -> (iaValue, e)
        |Leq e -> (new IA.interval iaValue#l 0., e)
        |Le e -> (new IA.interval iaValue#l 0., e)
        |Geq e -> (new IA.interval 0. iaValue#h, e)
        |Gr e -> (new IA.interval 0. iaValue#h, e)
      in
      let add_intv (isInfinite, varsIntvsMap) var =
        let (intv, _) = StringMap.find var varsIntvsMiniSATCodesMap in
        (isInfinite || intv#h = infinity || intv#l = neg_infinity, StringMap.add var intv varsIntvsMap)
      in
      let (isInfiniteTmp, varsIntvsMap) = List.fold_left add_intv (false, StringMap.empty) varsList in
      backward_propagate_boolExpr var intv varsIntvsMap polyExpr polyExprInterval false*)
      
    method log_test = 
      let testValueString = sprintf_I "%f" iaValue in
      match self#get_constraint with
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
      let iaString = sprintf_I "%f" iaValue in
      match self#get_constraint with
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
    
    method generate_tc_var intv isVarPositiveDirected = 
      let lowerBound = intv.low in
      let upperBound = intv.high in   
      if isVarPositiveDirected = -1 then lowerBound
      else if isVarPositiveDirected = 1 then upperBound
      else
        if lowerBound = neg_infinity then 
          if upperBound = infinity then 0.
          else upperBound
        else if upperBound = infinity then lowerBound
        else
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
          if logic = "QF_NIA" then (
            Random.self_init();
            if Random.bool() then 
              let testcase = ceil(lowerBase +. randomNum) in
              if testcase > upperBound then floor(lowerBase +. randomNum)
              else testcase
            else 
              let testcase = floor(lowerBase +. randomNum) in
              if testcase < lowerBound then ceil(lowerBase +. randomNum)
              else testcase
          )
          else 
            lowerBase +. randomNum

    method generateTCs assignedVarsSet (varsIntvsMap:(Interval.interval Variable.StringMap.t)) priorityNum (varsSATDirectionMap: (int Variable.StringMap.t)) = 
      (*print_endline ("\n" ^ self#to_string_infix);
      flush stdout;*)
      (* print_endline ("\n\nSelecting api: " ^ self#to_string_infix ^ " - miniSATCode: " ^ string_of_int self#get_miniSATCode);
      print_endline ("Variables sensitivity: " ^ self#string_of_varsSen);
      print_string ("Selecting variables: ");
      flush stdout; *)
      let neededVarsSet = VariablesSet.diff varsSet assignedVarsSet in
      let check_mem (var, _, _) = VariablesSet.mem var neededVarsSet in
      let neededVarsSen = List.filter check_mem varsSen in
      (* This function generates test cases for one variable *)
      let rec generate_tc_var intv tcNum isFirst varSen isVarPositiveDirected =
        (* print_endline ("TcNum: " ^ string_of_int tcNum);
        print_endline ("isVarPositiveDirected: " ^ string_of_int isVarPositiveDirected);
        flush stdout; *)
        if tcNum <= 0 then []
        else
          let lowerBound = intv.low in
          let upperBound = intv.high in
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
		        if isVarPositiveDirected = -1 then 
              lowerBound :: (generate_tc_var intv (tcNum - 1) false varSen 0)
		        else if isVarPositiveDirected = 1 then 
              upperBound :: (generate_tc_var intv (tcNum - 1) false varSen 0)
		        else
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
                if logic = "QF_NIA" then (
                  Random.self_init();
                  if Random.bool() then 
                    let testcase = ceil(lowerBase +. randomNum) in
                    if testcase > upperBound then floor(lowerBase +. randomNum)
                    else testcase
                  else 
                    let testcase = floor(lowerBase +. randomNum) in
                    if testcase < lowerBound then ceil(lowerBase +. randomNum)
                    else testcase
                )
                else 
	                lowerBase +. randomNum
		          in
		          (*print_endline (string_of_float tc);
		          flush stdout;*)
		          if tc >= lowerBound && tc <= upperBound then (* for QF_NIA, sometimes the test cases are not in the interval due to rounding *)
		            tc :: (generate_tc_var intv (tcNum - 1) false varSen 0)
		          else generate_tc_var intv (tcNum - 1) false varSen 0
		  in
      let rec generateTCs_extra varsSen generatedTCs priorityNum = match varsSen with
        | [] -> (generatedTCs, priorityNum);
        | (var, varSen, isPositiveSen) :: t ->
          (*print_endline (var(* ^ ": " ^ string_of_float varSen ^ ": " ^ string_of_bool isPositiveSen*));
          flush stdout;*)
          let isVarPositiveDirected = StringMap.find var varsSATDirectionMap in
          let isVarPositiveDirected = 0 in
          let intv = StringMap.find var varsIntvsMap in
          (*print_endline ("isVarPositiveDirected: " ^ string_of_int isVarPositiveDirected);
          print_endline ("isVarPositiveDirected = 0: " ^ string_of_bool (isVarPositiveDirected = 0));
          print_endline ("priorityNum: " ^ string_of_int priorityNum);
          print_endline ("priorityNum > 0: " ^ string_of_bool (priorityNum > 0));
          print_endline ("isVarPositiveDirected = 0 && priorityNum > 0: " ^ string_of_bool (isVarPositiveDirected = 0 && priorityNum > 0));
          flush stdout;*)
          let (testcases, newPriorityNum) =
            if isVarPositiveDirected = 0 && priorityNum > 0 then (
              (*print_string (var ^ " ");
              flush stdout;*)
              (generate_tc_var intv 2 true varSen 0, priorityNum - 1)
            )
            else 
              (generate_tc_var intv 1 true varSen isVarPositiveDirected, priorityNum)
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
            (*print_string (selectedVar ^ " ");
            flush stdout;*)
            (*print_endline selectedVar;
            flush stdout;*)
            let intv = StringMap.find selectedVar varsIntvsMap in
            let testcases = generate_tc_var intv 2 true 0 0 in
            generateTCs_extra_random remainingVarsSen ((selectedVar, testcases)::generatedTCs) (priorityNum - 1)
          )
          else (
            (*print_endline var;
            flush stdout;*)
            let intv = StringMap.find var varsIntvsMap in 
            let testcases = generate_tc_var intv 1 true 0 0 in
            generateTCs_extra_random t ((var, testcases)::generatedTCs) 0
          )
      in
      let rec generateTCs_extra_1VarChosen_random varsSen generatedTCs priorityNum isFirst = match varsSen with
        | [] -> (generatedTCs, priorityNum);
        | (var, varSen, isPositiveSen) :: t ->
          (*print_endline (var(* ^ ": " ^ string_of_float varSen ^ ": " ^ string_of_bool isPositiveSen*));
          flush stdout;*)
          let isVarPositiveDirected = 0 in
          (*print_endline ("isVarPositiveDirected: " ^ string_of_int isVarPositiveDirected);
          print_endline ("isVarPositiveDirected = 0: " ^ string_of_bool (isVarPositiveDirected = 0));
          print_endline ("priorityNum: " ^ string_of_int priorityNum);
          print_endline ("priorityNum > 0: " ^ string_of_bool (priorityNum > 0));
          print_endline ("isVarPositiveDirected = 0 && priorityNum > 0: " ^ string_of_bool (isVarPositiveDirected = 0 && priorityNum > 0));
          flush stdout;*)
          if isFirst && isVarPositiveDirected = 0 && priorityNum > 0 then (
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
            (*print_endline (selectedVar);
            flush stdout;*)
            let intv = StringMap.find selectedVar varsIntvsMap in
            let testcases = generate_tc_var intv 2 true 0 0 in
            generateTCs_extra_1VarChosen_random remainingVarsSen ((selectedVar, testcases)::generatedTCs) (priorityNum - 1) false
          )
          else (
            (*print_endline (var);
            flush stdout;*)
            let intv = StringMap.find var varsIntvsMap in
            let testcases = generate_tc_var intv 1 true varSen isVarPositiveDirected in
            generateTCs_extra_1VarChosen_random t ((var, testcases)::generatedTCs) priorityNum false
         )
      in
      let rec generateTCs_extra_1VarChosen varsSen generatedTCs priorityNum isFirst = match varsSen with
        | [] -> (generatedTCs, priorityNum);
        | (var, varSen, isPositiveSen) :: t ->
          
          (* print_endline (var ^ ": " ^ string_of_float varSen ^ ": " ^ string_of_bool isPositiveSen);
          flush stdout; *)

          let isVarPositiveDirected = 
            try
              StringMap.find var varsSATDirectionMap   
            with 
              | _ ->  0

            (* StringMap.find var self#get_varsSATDirectionMap  *)
          in
          (*let isVarPositiveDirected = 0 in (* not (11) *)*)
          let intv = StringMap.find var varsIntvsMap in
          (*print_endline ("isVarPositiveDirected: " ^ string_of_int isVarPositiveDirected);
          print_endline ("isVarPositiveDirected = 0: " ^ string_of_bool (isVarPositiveDirected = 0));
          print_endline ("priorityNum: " ^ string_of_int priorityNum);
          print_endline ("priorityNum > 0: " ^ string_of_bool (priorityNum > 0));
          print_endline ("isVarPositiveDirected = 0 && priorityNum > 0: " ^ string_of_bool (isVarPositiveDirected = 0 && priorityNum > 0));
          flush stdout;*)
          let (testcases, newPriorityNum, newIsFirst) =
            if isFirst && priorityNum > 0 then (
              (*print_string (var ^ " ");
              flush stdout;*)
              (generate_tc_var intv 2 true varSen isVarPositiveDirected, priorityNum - 1, false)
            )
            else 
              (generate_tc_var intv 1 true varSen isVarPositiveDirected, priorityNum, isFirst)
          in
          generateTCs_extra_1VarChosen t ((var, testcases)::generatedTCs) newPriorityNum newIsFirst
      in
      
      (*generateTCs_extra neededVarsSen [] priorityNum*)
      (*generateTCs_extra_random neededVarsSen [] priorityNum*)
      generateTCs_extra_1VarChosen neededVarsSen [] priorityNum true (*(8)*)
      (* generateTCs_extra_1VarChosen_random neededVarsSen [] priorityNum true (*(9)*) *)
  end;;
(* ============================= END of polynomialConstraint class =================================== *)
  
type bool_constraint =
  | Single of polynomialConstraint
  | NSingle of polynomialConstraint
  | BVar of string
  | NBVar of string
  | And of bool_constraint * bool_constraint
  | Or of bool_constraint * bool_constraint 
  | True
  | False

type smt_poly_expr =
  | SPoly of poly_expr
  | Poly of bool_constraint * poly_expr
  | POr of smt_poly_expr * smt_poly_expr

let rec get_varsSet_boolCons = function
  | Single polyCons -> polyCons#get_varsSet
  | NSingle polyCons -> polyCons#get_varsSet
  | And (boolCons1, boolCons2) -> VariablesSet.union (get_varsSet_boolCons boolCons1) (get_varsSet_boolCons boolCons2)
  | Or (boolCons1, boolCons2) -> VariablesSet.union (get_varsSet_boolCons boolCons1) (get_varsSet_boolCons boolCons2)
  | BVar var -> VariablesSet.empty
  | NBVar var -> VariablesSet.empty

and not_of_boolCons = function
  | Single polyCons ->
    (* let polyConstraint = not_of_polyConstraint polyCons#get_constraint in
    Single (new polynomialConstraint(polyConstraint)) *)
    NSingle polyCons
  | NSingle polyCons ->
    (* let polyConstraint = not_of_polyConstraint polyCons#get_constraint in
    Single (new polynomialConstraint(polyConstraint)) *)
    Single polyCons
  | And (boolCons1, boolCons2) -> Or (not_of_boolCons boolCons1, not_of_boolCons boolCons2)
  | Or (boolCons1, boolCons2) -> And (not_of_boolCons boolCons1, not_of_boolCons boolCons2)
  | BVar var -> NBVar var
  | NBVar var -> BVar var
  | True -> False
  | False -> True

(* encode the constraints into the form of miniSAT lit *)  
let rec miniSATExpr_of_constraints constraints index miniSATCodesConstraintsMap logic bVarMiniSatCodeMap = match constraints with
  | And (b1, b2) -> 
    let (mB1, index1, miniSATCodesConstraintsMap1, maxVarsNum1, isEquation1, isNotEquation1, bVarMiniSatCodeMap1) = miniSATExpr_of_constraints b1 index miniSATCodesConstraintsMap logic bVarMiniSatCodeMap in
    let (mB2, index2, miniSATCodesConstraintsMap2, maxVarsNum2, isEquation2, isNotEquation2, bVarMiniSatCodeMap2) = miniSATExpr_of_constraints b2 index1 miniSATCodesConstraintsMap1 logic bVarMiniSatCodeMap1 in
    (MAnd (mB1, mB2), index2, miniSATCodesConstraintsMap2, max maxVarsNum1 maxVarsNum2, isEquation1 + isEquation2, isNotEquation1 + isNotEquation2, bVarMiniSatCodeMap2)
  | Or (b1, b2) -> 
    let (mB1, index1, miniSATCodesConstraintsMap1, maxVarsNum1, isEquation1, isNotEquation1, bVarMiniSatCodeMap1) = miniSATExpr_of_constraints b1 index miniSATCodesConstraintsMap logic bVarMiniSatCodeMap in
    let (mB2, index2, miniSATCodesConstraintsMap2, maxVarsNum2, isEquation2, isNotEquation2, bVarMiniSatCodeMap2) = miniSATExpr_of_constraints b2 index1 miniSATCodesConstraintsMap1 logic bVarMiniSatCodeMap1 in
    (MOr (mB1, mB2), index2, miniSATCodesConstraintsMap2, max maxVarsNum1 maxVarsNum2, isEquation1 + isEquation2, isNotEquation1 + isNotEquation2, bVarMiniSatCodeMap2)
  | Single polyCons -> (
    (* polyCons#set_miniSATCode index; *)
    polyCons#set_logic logic;
    let newMiniSATCodesConstraintsMap = IntMap.add polyCons#get_miniSATCode polyCons miniSATCodesConstraintsMap in
    (Lit polyCons#get_miniSATCode, index, newMiniSATCodesConstraintsMap, polyCons#get_varsNum, polyCons#isEquation, polyCons#isNotEquation, bVarMiniSatCodeMap) 
    )
  | NSingle polyCons -> (
    (* polyCons#set_miniSATCode index; *)
    polyCons#set_logic logic;
    let newMiniSATCodesConstraintsMap = IntMap.add polyCons#get_miniSATCode polyCons miniSATCodesConstraintsMap in
    (Lit (-polyCons#get_miniSATCode), index, newMiniSATCodesConstraintsMap, polyCons#get_varsNum, polyCons#isEquation, polyCons#isNotEquation, bVarMiniSatCodeMap) 
    )
  | BVar var ->
    (try
      let miniSATCode = StringMap.find var bVarMiniSatCodeMap in
      (Lit miniSATCode, index, miniSATCodesConstraintsMap, 0, 0, 0, bVarMiniSatCodeMap)
    with Not_found -> (Lit index, index + 1, miniSATCodesConstraintsMap, 0, 0, 0, StringMap.add var index bVarMiniSatCodeMap)  
    )
  | NBVar var -> 
    (try
      let miniSATCode = StringMap.find var bVarMiniSatCodeMap in
      (Lit (-miniSATCode), index, miniSATCodesConstraintsMap, 0, 0, 0, bVarMiniSatCodeMap)
    with Not_found -> (Lit (-index), index + 1, miniSATCodesConstraintsMap, 0, 0, 0, StringMap.add var index bVarMiniSatCodeMap)  
    )

(* Function for converting constraints into infix string *)    
let rec string_infix_of_constraints constraints = match constraints with 
  | Single polyCons ->  polyCons#to_string_infix
  | NSingle polyCons -> "not" ^ polyCons#to_string_infix
  | And(c1, c2)  -> 
	  "( " ^ string_infix_of_constraints c1 ^ " And\n" ^ string_infix_of_constraints c2 ^ ")"
	| Or(c1, c2) -> 
	  "( " ^ string_infix_of_constraints c1 ^ " Or\n" ^ string_infix_of_constraints c2 ^ ")"
	| BVar var -> var
	| NBVar var -> "not " ^ var
	  

(* Function for converting constraints into infix string for mapple input *)    
(* let rec string_infix_of_constraints_maple constraints = match constraints with 
  | Single polyCons ->  polyCons#to_string_infix
  | And(c1, c2)  -> 
	  string_infix_of_constraints_maple c1 ^ ", " ^ string_infix_of_constraints_maple c2
	| Or(c1, c2) -> 
	  string_infix_of_constraints_maple c1 ^ ", " ^ string_infix_of_constraints_maple c2	   *)
	
	
(*==================== START string_infix_of_polynomialConstraints ==============================*)		
(* This function converts a list of polynomial constraints into the string of infix form *)
let rec string_infix_of_polynomialConstraints polyConses = 
  match polyConses with
  | [] -> ""
  | h::t ->
    string_infix_of_boolExp h#get_constraint ^ " Easiness: " ^ string_of_float h#get_easiness ^ "\n" ^ string_infix_of_polynomialConstraints t
(*==================== END bool_expr_list_to_string_infix ==============================*)	  


(*==================== START string_prefix_of_constraints ==============================*)
(* prefix string format of constraints *)      
let rec string_prefix_of_constraints  = function
  | Single polyCons -> string_prefix_of_boolExpr polyCons#get_constraint
  | NSingle polyCons -> "not" ^ string_prefix_of_boolExpr polyCons#get_constraint
  | And(e1, e2) -> "(and "^(string_prefix_of_constraints e1)^" " ^ (string_prefix_of_constraints e2)^")"
  | Or(e1, e2) -> "(or "^(string_prefix_of_constraints e1)^" " ^ (string_prefix_of_constraints e2)^")"
(*==================== END string_prefix_of_constraints ==============================*)
  

(*==================== START string_postfix_of_constraints ==============================*)
(* postfix string format of a boolean expression *)      
let rec string_postfix_of_constraints  = function
  | Single polyCons -> string_postfix_of_boolExpr polyCons#get_constraint
  | NSingle polyCons -> string_postfix_of_boolExpr polyCons#get_constraint ^ "not "
  | And(e1, e2) -> (string_postfix_of_constraints e1) ^ (string_postfix_of_constraints e2) ^ "and "
  | Or(e1, e2) -> (string_postfix_of_constraints e1) ^ (string_postfix_of_constraints e2) ^ "or "
(*==================== START string_postfix_of_constraints ==============================*)

(*=== Function for getting logs of all constraints ===*)   
let rec get_allLogs polyConstraints = match polyConstraints with
  |[] -> ""
  |polyCons::t->
    polyCons#get_log ^ "\n" ^ (get_allLogs t)
(*===== end of get_allLogs ======*)    


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


(*==================== START can_apply_imvt ==============================*)		
(* This function checks if a list of boolean expressions are all equalities *)
let rec can_apply_imvt polyConstraints = 
  match polyConstraints with
  | [] -> true
  | h::t -> 
    let logic = h#get_logic in
    logic = "QF_NRA"
(*==================== END can_apply_imvt ==============================*)



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
 