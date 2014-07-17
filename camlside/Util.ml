open IA;;
open Variable
module Util = struct

  (*CI to AF1 conversion, id <=size*)
   let toAf1 (it: IA.interval) (id:int) (size: int) = 
     let a = (it#l +.it#h)*.0.5 in
     let b = (it#h -.it#l)*.0.5 in
     let result = new IA.af1 size in
     result#set_a a;
     result#set_k 0.0;
     let ar1 = Array.create size 0.0 in
     if id > 0 then
	 Array.set ar1 (id-1) b;
     result#set_ar ar1;
     result

   (*CI to AF2 conversion, id <=size*)
   let toAf2 (it: IA.interval) (id:int) (size: int) = 
     let a = (it#l +.it#h)*.0.5 in
     let b = (it#h -.it#l)*.0.5 in
     let result = new IA.af2 size in
     result#set_a a;
     result#set_kp 0.0;
     result#set_kn 0.0;
     result#set_k 0.0;
     let ar1 = Array.create size 0.0 in
     if id > 0 then
       Array.set ar1 (id-1) b;
     result#set_ar ar1;
     result
   
   (*CI to CAF conversion, id <=size*)
   let toCaf2 (it: IA.interval) (id:int) (size: int) = 
     let a = new IA.interval it#l ((it#l+.it#h)*.0.5) in
     let b = new IA.interval 0. ((it#h -.it#l)*.0.5) in
     let result = new IA.caf2 size in
     result#set_a a;
     result#set_k (new IA.interval 0.0 0.0);
     let ar1 = Array.create size (new IA.interval 0. 0.) in
     if id > 0 then
       Array.set ar1 (id-1) b;
     result#set_ar ar1;
     result

   (*CI to CAI1 conversion, id <=size*)
   let toCai1 (it: IA.interval) (id:int) (size: int) = 
     let a = new IA.interval ((it#l+.it#h)*.0.5) ((it#l+.it#h)*.0.5) in
     let b = new IA.interval ((it#h-.it#l)*.0.5) ((it#h-.it#l)*.0.5) in
     let result = new IA.cai1 size in
     result#set_a a;
     let zero = new IA.interval 0.0 0.0 in
     result#set_k zero;
     let ar1 = Array.create (2*size) zero in
     if id > 0 then
       Array.set ar1 (id-1) b;
     result#set_ar ar1;
     result

   (*CI to CAI2 conversion, id <=size*)
   let toCai2 (it: IA.interval) (id:int) (size: int) = 
     let a = new IA.interval ((it#l+.it#h)*.0.5) ((it#l+.it#h)*.0.5) in
     let b = new IA.interval ((it#h-.it#l)*.0.5) ((it#h-.it#l)*.0.5) in
     let result = new IA.cai2 size in
     result#set_a a;
     let zero = new IA.interval 0.0 0.0 in
     result#set_k zero;
     let ar1 = Array.create (2*size) zero in
     if id > 0 then
       Array.set ar1 (id-1) b;
     result#set_ar ar1;

     let ma1 = (Array.make_matrix size size (new IA.interval 0.0 0.0)) in
     result#set_m1 ma1;

     let ma2 = (Array.make_matrix size size (new IA.interval 0.0 0.0)) in
     result#set_m2 ma2;

     let ma3 = (Array.make_matrix size size (new IA.interval 0.0 0.0)) in
     result#set_m3 ma3;     
     result  

   (*CI to CAI3 conversion, id <=size*)
   let toCai3 (it: IA.interval) (id:int) (size: int) = 
     let a = new IA.interval ((it#l+.it#h)*.0.5) ((it#l+.it#h)*.0.5) in
     let b = new IA.interval ((it#h-.it#l)*.0.5) ((it#h-.it#l)*.0.5) in
     let result = new IA.cai3 size in
     result#set_a a;
     let zero = new IA.interval 0.0 0.0 in
     result#set_k zero;
     let ar1 = Array.create (2*size) zero in
     if id > 0 then
       Array.set ar1 (id-1) b;
     result#set_ar ar1;

     let ma = (Array.make_matrix size size (new IA.interval 0.0 0.0)) in
     result#set_m ma;

     result  

	(*Reduce list to set*)
	let rec set_list l1 l2 = 
		match l2 with
		|[] -> l1
		|h::t -> 
			if (List.mem h l1) then 
			  set_list l1 t
			else
			  set_list (h::l1) t

		let red_list l = set_list [] l
		
	(* =============== START has_common_element ======================= *)	
	(* This function return true if two lists have some common elements*)
  let rec has_common_element list1 list2 = 
    match list1 with 
    | [] -> false 
    | h::t -> (List.mem h list2) || (has_common_element t list2)		
	(* ============== END has_common_element ========================== *)	  
	
	
	(* Function for converting a list of variables to be learned into minisat codes*)
	let rec learn_vars varsList intvMap = match varsList with
	  | [] -> ""
    | h::t -> 
      let (_, varId) = VarIntvMap.find h intvMap in
      "-" ^ string_of_int varId ^ " " ^ (learn_vars  t intvMap)
      
      
  (* This function convert the list of unsat cores into minisat learnt clauses.*)
  let rec learn_vars_cores varsCores intvMap = match varsCores with
    | [] -> ""
    | varsList :: [] -> learn_vars varsList intvMap
    | varsList :: remainingVarsCores -> (
      let learntVars = learn_vars varsList intvMap in
      learntVars ^ " 0 " ^ learn_vars_cores remainingVarsCores intvMap
    )
    
      
  (* Function for converting a list of vars to a string *)
  let rec vars_to_string varsList = match varsList with
    | [] -> ""
    | var::remainingVars -> var ^ " " ^ vars_to_string remainingVars
  

  (* This function compares two boolean expressions using 
  dependency between variables. Two criteria:
  . boolExp1 <= boolExp2 if vars(boolExp1) is a subset of vars(boolExp2) 
  . boolExp1 < boolExp2 if length(vars(boolExp1)) < length(vars(boolExp2)) 
  . otherwise, boolExp1 > boolExp2
  . each argument contains a boolean expression, its compact sorted variables
  and its number of variables *)
  let rec compare_dependency (boolExp1, _, variablesSet1, variablesNum1) (boolExp2, _, variablesSet2, variablesNum2) = 
    if VariablesSet.subset variablesSet1 variablesSet2 then -1 
    else if VariablesSet.subset variablesSet2 variablesSet1 then 1
    else if variablesNum1 < variablesNum2 then -1
    else 1


  (* This function extracts a list of boolean expresions 
  from the list of expressive boolean expressions *)
  let rec extract_boolExps = function 
    | [] -> []
    | (boolExp, _, vars, varsNum)::t -> boolExp::(extract_boolExps t)
    
end
