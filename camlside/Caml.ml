open Ast
open Util
open IA
open Expr
open Decomposer
open Assignments
open EqualitiesHandler
open UnsatCore
open Testing
open Variable
open PolynomialConstraint


module Caml = struct

  (*=======================================*)
  (* Encode - Decode literal and get all kind of assignments of interval variables*)
  (*=======================================*)

  (*idx generates list of number from n0 to n0 + n - 1*)
  let rec idx n0 n = 
    if n = 0 then []
    else n0 :: idx (n0 + 1) (n - 1)
   

(*toIntList convert a string to an int list*)
let rec toIntList str = 
  try let id = String.index str ' ' in
    try int_of_string (String.sub str 0 id):: toIntList (String.sub str (id+1) ((String.length str)-(id+1)))
    with Failure "int_of_string" ->
      toIntList (String.sub str (id+1) ((String.length str)-(id+1)) )
  with Not_found ->    
    try  [int_of_string str] with Failure "int_of_string" -> []  

  (*For CNF of interval constraints*)
  (*Compute all cnf clause in an interval constraints for a variables*)
  let rec cnf_list a l = match l with
    |[] -> ""
    |h::t -> ("-"^string_of_int a) ^ " " ^ ("-"^string_of_int h) ^" 0\n" ^ 
  (cnf_list a t)

  (*cnf of clause 1 -> not 2...*)
  let rec cnf_clause = function
    |[] -> ""
    |h::t -> (cnf_list h t) ^ (cnf_clause t)

  (*cnf clause of all elements in list*)
  let rec cnf_all = function         
    |[] -> "0\n"
    |h::t -> (string_of_int h) ^" "^ (cnf_all t)

  (*Represent information of interval variables*)
  let rec toString_ass intvMap =
    let add_string_of_newElement key (intv, _) oldString 
      = key ^ " = " ^ "[" ^ (string_of_float intv#l) ^ "," ^ (string_of_float intv#h) ^ "]\n" ^ oldString
    in 
    StringMap.fold add_string_of_newElement intvMap ""
  
  (*Some function for logging result*)
  let isVar = function
  | Real c -> true 
  | Var x -> true
  | Mul (e1, e2) -> true  
  | _ -> false

let rev = function
  | "+" -> "-"
  | "-" -> "+"
  | _ -> "-"

(*Round off a float number to n-fractional part*)
let round_off (r: float) (n:int) = 
  let a = 10.0**(float_of_int n) in
  let b = floor (r *. a +. 0.5) in
  b/.a

let rec round_test = function
  |[] -> []
  |(x, v)::t -> (x, round_off v 5)::round_test t

let sign_simp sign (num: float) = match sign with
| "" -> string_of_float num
| _ ->
  if  (num < 0.) then rev sign ^ string_of_float (abs_float num)
  else sign ^ string_of_float num  
  

let rec getMB m intvList = match intvList with
  |[] -> m
  |(x, intv)::t -> 
    let m1 = max (intv#h -. intv#l) m in
    getMB m1 t

let rec sum_total_var init mb = 
  if init >= mb then init
  else init + sum_total_var (init*2) mb

  (*==================================================*)
  (* C++ - OCaml interface *)
  (*==================================================*)

(*get miniSat form of interval constraints*)
let genSatForm sAss sIntv esl logic =
  (*print_endline ("Logic: " ^ logic);
  flush stdout;*)
  let constraints = ParserConstraints.main LexerConstraints.lex (Lexing.from_string sAss) in
  (*print_endline ("solve({" ^ string_infix_of_constraints_maple  constraints ^ "})");
  flush stdout;*)
  let (miniSATExpr, index, miniSATCodesConstraintsMap, maxVarsNum, isEquation, isNotEquation) = miniSATExpr_of_constraints constraints 1 IntMap.empty logic in 
  (* miniSATExpr_of_constraints is defined in PolynomialConstraint.ml *)
  
  (* convert miniSATExpr into CNF *)
  let cnfMiniSATExpr = cnf_of_miniSATExpr miniSATExpr in
  
  (* convert cnfMiniSATExpr to string under the format of miniSAT input *)
  let (cnfMiniSATExprString, miniSATClauses) = string_of_cnf_miniSATExpr cnfMiniSATExpr true in
  (*print_endline cnfMiniSATExprString;
  flush stdout;*)
  
  let eIntv = ParserIntervals.main LexerIntervals.lex (Lexing.from_string sIntv) in
  let nVars = List.length eIntv in
   
  (*let rec string_of_vars varsIntvList = match varsIntvList with
    | [] -> ""
    | [(var, _)] -> var
    | (h,_)::t -> h ^ ", " ^ string_of_vars t
  in
  print_endline (string_of_vars eIntv ^ "])");
  flush stdout;*)
   
  (* generate the minisat constraints for intervals *)
  let rec genMiniSATIntvString index nVars =
    if nVars = 0 then ""
    else (string_of_int index) ^ " 0\n" ^ genMiniSATIntvString (index + 1) (nVars - 1)
  in
  let miniSATIntvString = genMiniSATIntvString index nVars in
  
  let rec generateIntvInfo varsIntvsMiniSATCodesMap miniSATCodesVarsIntvsMap eIntv index = match eIntv with
    | [] -> (varsIntvsMiniSATCodesMap, miniSATCodesVarsIntvsMap, index)
    | (var, intv)::t -> 
      (*print_endline ("Adding interval: " ^ var ^ " in " ^ intv#to_string ^ " with index of " ^ string_of_int index);
      flush stdout;*)
      let newVarsIntvsMiniSATCodesMap = StringMap.add var (intv, index) varsIntvsMiniSATCodesMap in
      let newMiniSATCodesVarsIntvsMap = IntMap.add index (var, intv) miniSATCodesVarsIntvsMap in
      generateIntvInfo newVarsIntvsMiniSATCodesMap newMiniSATCodesVarsIntvsMap t (index + 1)
  in
  let intvInfo = generateIntvInfo StringMap.empty IntMap.empty eIntv index in
  (*Compute the total of variables for SAT encoding*)
  (*let max_bound = getMB 0.0 eIntv in
  let para = int_of_float (max_bound /. esl) in
  (*let totalVars = iLit * 4 * para in*)
  let totalVars = 
    if max_bound = infinity then 10000
    else index + 2* nVars * (sum_total_var 1 para)
  in*)
  (*let sTrivialClause = "-" ^ string_of_int totalVars ^ " " ^string_of_int totalVars^ " 0" in*)
  let totalMiniSATVars = nVars + index - 1 in
  (nVars, "p cnf " ^ string_of_int totalMiniSATVars ^ " " ^ string_of_int (nVars(*+1*)+miniSATClauses) ^"\n"^ cnfMiniSATExprString ^ miniSATIntvString (*^ sTrivialClause*), intvInfo, miniSATCodesConstraintsMap, index-1, maxVarsNum, isEquation, isNotEquation)    

  (*Generate information about a test case*)
  let rec logTestCase ass = match ass with
    | [] -> ""
    | (x, a):: t -> (x ^" = "^ string_of_float a) ^ "\n" ^ (logTestCase t)
  

(*remove unnecessary assignment*)
let rec red_ass assIntv lstVar = match assIntv with
  |[] -> []
  |(x, it)::t -> 
    if (List.mem x lstVar) then
      (x,it):: red_ass t lstVar 
    else
      red_ass t lstVar
      

  (*New version for testing -------------------------*)
  (*=================================================*)

  (*Compute the list of boolean constraints without and*)
  let rec f_toList e = match e with
    |And (e1, e2) -> List.append (f_toList e1) (f_toList e2)
    |BOr (e1, e2) -> List.append (f_toList e1) (f_toList e2)
    |_ -> [e]

  (*Inclusion between two lists: l1 is a subset of l2*)
  let rec subset_list l1 l2 = match l1 with
    |[] -> true
    |h::t -> 
  if (List.mem h l2) then 
    subset_list t l2
  else 
    false

  (*Sub set equal between two lists: l1 is equal to l2*)
  let subset_eq l1 l2 =
    if (subset_list l1 l2) && (subset_list l2 l1) then
      true
    else 
      false

  (*Substraction between 2 list*)
  let rec sub_list l1 l2 = 
    if (l2 = []) then l1 
    else (
      match l1 with
      |[] -> []      
      |h::t -> 
        if (List.mem h l2) then sub_list t l2
        else h:: sub_list t l2
    )
 
let logTest assIntv ass all_cl polyConstraints ia = ""
  (*let varsSet = get_vars_set_boolExps uk_cl in
  let checkMem key _ = VariablesSet.mem key varsSet in
  let rm_ass = StringMap.filter checkMem assIntv in
  let rm_cl = sub_list all_cl uk_cl in

  let s1 = toString_ass rm_ass in

  let rec toString_cons e = match e with
    |[] ->""
    |h::t -> (toString_cons t) ^ "\n" ^ (logSat h ia assIntv) in

  let s2 = toString_cons rm_cl in 
  
  let stmp = "======================[ Detail SAT for each constraint ]===========================\n" in 
  if (s2 <> "") then
    s1^(logTestCase ass)^"\n" ^stmp^ s2^"\n"^ (logValue_all uk_cl ass)
  else
    s1^(logTestCase ass)^"\n"^stmp^"\n"^ (logValue_all uk_cl ass)*)
  
let rec decomp_reduce ass esl = match ass with
  |[] -> []
  |(x, ci)::t ->
    let lowerBound = ci#l in
    let upperBound = ci#h in
    if lowerBound = neg_infinity && upperBound < (round_off (esl -. max_float) 5) then 
      decomp_reduce t esl
    else if upperBound = infinity && lowerBound > (round_off (max_float -. esl) 5) then
      decomp_reduce t esl
    else if (ci#h > (round_off (esl +. ci#l) 5)) then 
      (x,ci):: (decomp_reduce t esl)
    else
      decomp_reduce t esl
       
  let var_decomp (var, ci) lstVarID code = 
    let id = List.assoc var lstVarID in
    let si = "(or ("^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float (0.5*.(ci#l+.ci#h)) ^ ") " ^
               "(" ^ var ^ " in " ^ string_of_float (0.5*.(ci#l+.ci#h)) ^ " " ^ string_of_float ci#h ^ "))" in
    let sl = "-" ^ string_of_int id ^ " " ^ string_of_int code ^ " " ^string_of_int (code+1) ^ " 0 " ^
             "-" ^ string_of_int code ^ " -"^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -"^string_of_int code ^ " 0 " ^
             string_of_int id ^ " -"^string_of_int (code+1) ^ " 0 " in
             
    (si, sl)

  let rec ass_decomp ass lstVarID code esl = match ass with
    |[] -> ("", "")
    |h::t ->
      let (s1, s2) = ass_decomp t lstVarID (code + 2) esl in
      let (s3, s4) = var_decomp h lstVarID code in
      if (s1 = "") then
        (s3, s4^s2)
      else 
        ("(ic "^s3 ^" "^s1^")", s4^s2)        
  (*=====================================================*)  
  (*============= Unbalance decomposition ===============*)

  (*Target decomposition on Positive part*)
  let var_decomp_pos (var, ci) lstVarID code esl = 
    let id = List.assoc var lstVarID in
    (* afsmt1
    let si = "(or ("^ var ^ " in " ^ string_of_float (0.5*.(ci#l+.ci#h)) ^ " " ^ string_of_float ci#h ^ ") "  ^
               "(" ^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float (0.5*.(ci#l+.ci#h)) ^ "))" in
    *)
    (* afsmt2
    let si = "(or ("^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float (0.5*.(ci#l+.ci#h)) ^ ") "  ^
               "(" ^ var ^ " in " ^ string_of_float (0.5*.(ci#l+.ci#h)) ^ " " ^ string_of_float ci#h ^ "))" in
    *)
    (* afsmt3 
    let si = "(or ("^ var ^ " in " ^ string_of_float (0.75*.(ci#l+.ci#h)) ^ " " ^ string_of_float ci#h ^ ") "  ^
               "(" ^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float (0.25*.(ci#l+.ci#h)) ^ "))" in
    *)
    (* afsmt_sat *)
    let si = "(or ("^ var ^ " in " ^ string_of_float (0.5*.(ci#l+.ci#h)) ^ " " ^ string_of_float ci#h ^ ") "  ^
               "(" ^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float (0.5*.(ci#l+.ci#h)) ^ "))" in
    
    
    (* afsmt_sat with esl
    let si = "(or ("^ var ^ " in " ^ string_of_float (ci#h-.esl) ^ " " ^ string_of_float ci#h ^ ") "  ^
               "(" ^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float (ci#h-.esl) ^ "))" in
    *)
    
    (* afsmt_us  
    let si = "(or ("^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float (0.5*.(ci#h+.ci#l)) ^ ") "  ^
               "(" ^ var ^ " in " ^ string_of_float (0.5*.(ci#h+.ci#l)) ^ " " ^ string_of_float ci#h ^ "))" in
    *)

    let sl = 
             (*string_of_int code ^ " -"^string_of_int code ^ " 0 " ^*)
             "-" ^ string_of_int id ^ " " ^ string_of_int code ^ " " ^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -" ^ string_of_int code ^ " 0 " ^
             "-" ^ string_of_int code ^ " -"^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -"^string_of_int (code+1) ^ " 0 " in
             
    (si, sl)

  (*Target decomposition on Negative part*)
  let var_decomp_neg (var, ci) lstVarID code esl = 
    let id = List.assoc var lstVarID in
    (* afsmt1
    let si = "(or ("^ var ^ " in " ^ string_of_float (0.5*.(ci#l+.ci#h)) ^ " " ^ string_of_float ci#h ^ ") " ^
               "(" ^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float (0.5*.(ci#l+.ci#h)) ^ "))" in
    *)
    (* afsmt2
    let si = "(or ("^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float (0.5*.(ci#l+.ci#h)) ^ ") " ^
               "(" ^ var ^ " in " ^ string_of_float (0.5*.(ci#l+.ci#h)) ^ " " ^ string_of_float ci#h ^ "))" in
    *)

    (* afsmt_sat *)
    let si = "(or ("^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float (0.5*.(ci#l+.ci#h)) ^ ") " ^
               "(" ^ var ^ " in " ^ string_of_float (0.5*.(ci#l+.ci#h)) ^ " " ^ string_of_float ci#h ^ "))" in
    
       
    (* afsmt_sat with esl 
    let si = "(or ("^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float (ci#h-.esl) ^ ") "  ^
               "(" ^ var ^ " in " ^ string_of_float (ci#h-.esl) ^ " " ^ string_of_float ci#h ^ "))" in
    *)

    (* afsmt_us 
    let si = "(or ("^ var ^ " in " ^ string_of_float (0.5*.(ci#h+.ci#l)) ^ " " ^ string_of_float ci#h ^ ") " ^
               "(" ^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float (0.5*.(ci#h+.ci#l)) ^ "))" in
    *)

    let sl = 
             (*string_of_int code ^ " -" ^ string_of_int code ^ " 0 "  ^*)
             "-" ^ string_of_int id ^ " " ^ string_of_int code ^ " " ^ string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -" ^ string_of_int code ^ " 0 " ^
             "-" ^ string_of_int code ^ " -"^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -"^string_of_int (code+1) ^ " 0 " in
             
    (si, sl)
 
  (*Target decomposition on neither Positive nor Negative part*)
  let var_decomp_pn (var, ci) lstVarID code = 
    let lowerBound = ci#l in
    let upperBound = ci#h in
    let id = List.assoc var lstVarID in
    (* afsmt1
    let si = "(or ("^ var ^ " in " ^ string_of_float (0.5*.(ci#l+.ci#h)) ^ " " ^ string_of_float ci#h ^ ") " ^
               "(" ^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float (0.5*.(ci#l+.ci#h)) ^ "))" in
    *)
    (* afsmt2
    let si = "(or ("^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float (0.5*.(ci#l+.ci#h)) ^ ") " ^
               "(" ^ var ^ " in " ^ string_of_float (0.5*.(ci#l+.ci#h)) ^ " " ^ string_of_float ci#h ^ "))" in
    *)

    (* afsmt_sat *)
    let newPoint = 
      if lowerBound = neg_infinity then 
        if upperBound = infinity then 0.
        else upperBound -. 10.
      else 
        if upperBound = infinity then lowerBound +. 10. 
        else 0.5 *. lowerBound +. 0.5 *. upperBound
    in    
    let si = "(or ("^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float newPoint ^ ") " ^
               "(" ^ var ^ " in " ^ string_of_float newPoint ^ " " ^ string_of_float ci#h ^ "))" in
    
    (* afsmt_us 
    let si = "(or ("^ var ^ " in " ^ string_of_float (0.5*.(ci#h+.ci#l)) ^ " " ^ string_of_float ci#h ^ ") " ^


               "(" ^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float (0.5*.(ci#h+.ci#l)) ^ "))" in
    *)
    let bumpVar =
      if newPoint < 0. then code + 1
      else code
    in

    let sl = 
     (*string_of_int code ^ " -" ^ string_of_int code ^ " 0 "  ^*)
     "-" ^ string_of_int id ^ " " ^ string_of_int code ^ " " ^ string_of_int (code+1) ^ " 0 " ^
     string_of_int id ^ " -" ^ string_of_int code ^ " 0 " ^
     "-" ^ string_of_int code ^ " -"^string_of_int (code+1) ^ " 0 " ^
     string_of_int id ^ " -"^string_of_int (code+1) ^ " 0 " in
             
    (si, sl, bumpVar)

  (*Target decomposition on Positive part of a test point*)
  let var_decomp_test_pos (var, ci) lstVarID code esl tc= 
    let id = List.assoc var lstVarID in  
    let test_point = List.assoc var tc in  
    
    let new_code = ref 0 in
    let bump_vars = ref "" in
    let si = ref "" in
    let sl = ref "" in    
    
    if (test_point = ci#l) then(
      new_code := code + 1;
      bump_vars := string_of_int (code + 1);

      si := "(or ("^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float (ci#l+.esl) ^ ") "  ^
                "("^ var ^ " in " ^ string_of_float (ci#l+.esl) ^ " " ^ string_of_float ci#h ^ "))";

      sl :=  "-" ^ string_of_int id ^ " " ^ string_of_int code ^ " " ^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -" ^ string_of_int code ^ " 0 " ^
             "-" ^ string_of_int code ^ " -"^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -"^string_of_int (code+1) ^ " 0 ";
      (!si, !sl, !bump_vars, !new_code)
    )
    else if (test_point = ci#h) then(
      new_code := code + 1;
      bump_vars := string_of_int (code + 1);

      si := "(or ("^ var ^ " in " ^ string_of_float (ci#h-.esl) ^ " " ^ string_of_float ci#h ^ ") "  ^
                "("^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float (ci#h-.esl) ^ "))";

      sl :=  "-" ^ string_of_int id ^ " " ^ string_of_int code ^ " " ^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -" ^ string_of_int code ^ " 0 " ^
             "-" ^ string_of_int code ^ " -"^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -"^string_of_int (code+1) ^ " 0 ";
      (!si, !sl, !bump_vars, !new_code)
    )
    else if ((round_off (test_point +. esl) 5) >= ci#h) then (
      new_code := code + 1;
      bump_vars := string_of_int (code + 1);

      si := "(or ("^ var ^ " in " ^ string_of_float test_point ^ " " ^ string_of_float ci#h ^ ") "  ^
                "("^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float test_point ^ "))";

      sl :=  "-" ^ string_of_int id ^ " " ^ string_of_int code ^ " " ^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -" ^ string_of_int code ^ " 0 " ^
             "-" ^ string_of_int code ^ " -"^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -"^string_of_int (code+1) ^ " 0 ";
     (!si, !sl, !bump_vars, !new_code)
    )
    else (
      new_code := code + 2;
      bump_vars := string_of_int (code + 1) ^ " " ^ string_of_int (code + 2);

      si := "(or ("^ var ^ " in " ^ string_of_float test_point ^ " " ^ string_of_float (test_point +. esl) ^ ") "  ^
                "(or ("^ var ^ " in " ^ string_of_float (test_point +.esl) ^ " " ^ string_of_float ci#h ^ ") " ^
                "("^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float test_point ^ ")))";

      sl :=  "-" ^ string_of_int id ^ " " ^ string_of_int code ^ " " ^string_of_int (code+1) ^ 
       " " ^string_of_int (code+2) ^" 0 " ^
             string_of_int id ^ " -" ^ string_of_int code ^ " 0 " ^
             string_of_int id ^ " -"^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -"^string_of_int (code+2) ^ " 0 " ^ 
             "-" ^ string_of_int code ^ " -"^string_of_int (code+1) ^ " 0 " ^
             "-" ^ string_of_int code ^ " -"^string_of_int (code+2) ^ " 0 " ^
             "-" ^ string_of_int (code+1) ^ " -"^string_of_int (code+2) ^ " 0 ";
    (!si, !sl, !bump_vars, !new_code)
    )             

  (*Target decomposition on Negative part of a test point*)
  let var_decomp_test_neg (var, ci) lstVarID code esl tc= 
    let id = List.assoc var lstVarID in  
    let test_point = List.assoc var tc in  
    
    let new_code = ref 0 in
    let bump_vars = ref "" in
    let si = ref "" in
    let sl = ref "" in    

    if (test_point = ci#l) then(
      new_code := code + 1;
      bump_vars := string_of_int (code + 1);

      si := "(or ("^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float (ci#l+.esl) ^ ") "  ^
                "("^ var ^ " in " ^ string_of_float (ci#l+.esl) ^ " " ^ string_of_float ci#h ^ "))";

      sl :=  "-" ^ string_of_int id ^ " " ^ string_of_int code ^ " " ^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -" ^ string_of_int code ^ " 0 " ^
             "-" ^ string_of_int code ^ " -"^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -"^string_of_int (code+1) ^ " 0 ";
      (!si, !sl, !bump_vars, !new_code)
    )
    else if (test_point = ci#h) then(
      new_code := code + 1;
      bump_vars := string_of_int (code + 1);

      si := "(or ("^ var ^ " in " ^ string_of_float (ci#h-.esl) ^ " " ^ string_of_float ci#h ^ ") "  ^
                "("^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float (ci#h-.esl) ^ "))";

      sl :=  "-" ^ string_of_int id ^ " " ^ string_of_int code ^ " " ^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -" ^ string_of_int code ^ " 0 " ^
             "-" ^ string_of_int code ^ " -"^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -"^string_of_int (code+1) ^ " 0 ";
      (!si, !sl, !bump_vars, !new_code)
    )
    else if (test_point <= (round_off (ci#l +. esl) 5)) then (
      new_code := code + 1;
      bump_vars := string_of_int (code + 1);

      si := "(or ("^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float test_point ^ ") "  ^
                "("^ var ^ " in " ^ string_of_float test_point ^ " " ^ string_of_float ci#h ^ "))";

      sl :=  "-" ^ string_of_int id ^ " " ^ string_of_int code ^ " " ^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -" ^ string_of_int code ^ " 0 " ^
             "-" ^ string_of_int code ^ " -"^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -"^string_of_int (code+1) ^ " 0 ";
     (!si, !sl, !bump_vars, !new_code)
    )
    else (
      new_code := code + 2;
      bump_vars := string_of_int (code + 1) ^ " " ^ string_of_int (code + 2);

      si := "(or ("^ var ^ " in " ^ string_of_float (test_point-.esl) ^ " " ^ string_of_float test_point ^ ") "  ^
                "(or ("^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float (test_point-.esl) ^ ") " ^
                "("^ var ^ " in " ^ string_of_float test_point ^ " " ^ string_of_float ci#h ^ ")))";

      sl :=  "-" ^ string_of_int id ^ " " ^ string_of_int code ^ " " ^string_of_int (code+1) ^ 
       " " ^string_of_int (code+2) ^" 0 " ^
             string_of_int id ^ " -" ^ string_of_int code ^ " 0 " ^
             string_of_int id ^ " -"^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -"^string_of_int (code+2) ^ " 0 " ^ 
             "-" ^ string_of_int code ^ " -"^string_of_int (code+1) ^ " 0 " ^
             "-" ^ string_of_int code ^ " -"^string_of_int (code+2) ^ " 0 " ^
             "-" ^ string_of_int (code+1) ^ " -"^string_of_int (code+2) ^ " 0 ";
     (!si, !sl, !bump_vars, !new_code)
    )             

  (*Target decomposition on both Positive and Negative part of a test point *)
  let var_decomp_test_both (var, ci) lstVarID code esl tc= 
    let id = List.assoc var lstVarID in  
    let test_point = List.assoc var tc in  
    
    let new_code = ref 0 in
    let bump_vars = ref "" in
    let si = ref "" in
    let sl = ref "" in    

    if (test_point = ci#l) then(
      new_code := code + 1;
      bump_vars := string_of_int (code + 1);

      si := "(or ("^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float (ci#l+.esl) ^ ") "  ^
                "("^ var ^ " in " ^ string_of_float (ci#l+.esl) ^ " " ^ string_of_float ci#h ^ "))";

      sl :=  "-" ^ string_of_int id ^ " " ^ string_of_int code ^ " " ^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -" ^ string_of_int code ^ " 0 " ^
             "-" ^ string_of_int code ^ " -"^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -"^string_of_int (code+1) ^ " 0 ";
      (!si, !sl, !bump_vars, !new_code)
    )
    else if (test_point = ci#h) then(
      new_code := code + 1;
      bump_vars := string_of_int (code + 1);

      si := "(or ("^ var ^ " in " ^ string_of_float (ci#h-.esl) ^ " " ^ string_of_float ci#h ^ ") "  ^
                "("^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float (ci#h-.esl) ^ "))";

      sl :=  "-" ^ string_of_int id ^ " " ^ string_of_int code ^ " " ^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -" ^ string_of_int code ^ " 0 " ^
             "-" ^ string_of_int code ^ " -"^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -"^string_of_int (code+1) ^ " 0 ";
      (!si, !sl, !bump_vars, !new_code)
    )
    else if (test_point > (round_off (ci#l +. 0.5*.esl) 5))&&((round_off (test_point +. 0.5*.esl) 5) < ci#h) then (
      new_code := code + 2;
      bump_vars := string_of_int (code + 1) ^ " " ^ string_of_int (code + 2);

      si := "(or ("^ var ^ " in " ^ string_of_float (test_point-.0.5*.esl) ^ " " ^ 
       string_of_float (test_point+.0.5*.esl) ^ ") "  ^
                "(or ("^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float (test_point-.0.5*.esl) ^ ") " ^
                "("^ var ^ " in " ^ string_of_float (test_point+.0.5*.esl) ^ " " ^ string_of_float ci#h ^ ")))";

      sl :=  "-" ^ string_of_int id ^ " " ^ string_of_int code ^ " " ^string_of_int (code+1) ^ 
       " " ^string_of_int (code+2) ^" 0 " ^
             string_of_int id ^ " -" ^ string_of_int code ^ " 0 " ^
             string_of_int id ^ " -"^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -"^string_of_int (code+2) ^ " 0 " ^ 
             "-" ^ string_of_int code ^ " -"^string_of_int (code+1) ^ " 0 " ^
             "-" ^ string_of_int code ^ " -"^string_of_int (code+2) ^ " 0 " ^
             "-" ^ string_of_int (code+1) ^ " -"^string_of_int (code+2) ^ " 0 ";
      (!si, !sl, !bump_vars, !new_code)
    )
    else if (test_point <= (round_off (ci#l +. 0.5*.esl) 5)) then (
      new_code := code + 1;
      bump_vars := string_of_int (code + 1);

      si := "(or ("^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float (test_point+.0.5*.esl) ^ ") "  ^
                "("^ var ^ " in " ^ string_of_float (test_point+.0.5*.esl) ^ " " ^ string_of_float ci#h ^ "))";

      sl :=  "-" ^ string_of_int id ^ " " ^ string_of_int code ^ " " ^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -" ^ string_of_int code ^ " 0 " ^


             "-" ^ string_of_int code ^ " -"^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -"^string_of_int (code+1) ^ " 0 ";
      (!si, !sl, !bump_vars, !new_code)
    )             
    else (
      new_code := code + 1;
      bump_vars := string_of_int (code + 1);

      si := "(or ("^ var ^ " in " ^ string_of_float (test_point-.0.5*.esl) ^ " " ^ string_of_float ci#h ^ ") "  ^
                "("^ var ^ " in " ^ string_of_float ci#l ^ " " ^ string_of_float (test_point-.0.5*.esl) ^ "))";

      sl :=  "-" ^ string_of_int id ^ " " ^ string_of_int code ^ " " ^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -" ^ string_of_int code ^ " 0 " ^
             "-" ^ string_of_int code ^ " -"^string_of_int (code+1) ^ " 0 " ^
             string_of_int id ^ " -"^string_of_int (code+1) ^ " 0 ";
     (!si, !sl, !bump_vars, !new_code)
    )             
  
  let rec ass_decomp_pos ass lstVarID code esl = match ass with
    |[] -> ("", "", "")
    |h::t ->
      let (s1, s2, svars) = ass_decomp_pos t lstVarID (code + 2) esl in
      let (s3, s4) = var_decomp_pos h lstVarID code esl in
      if (s1 = "") then
         (s3, s4^s2, (string_of_int (code+1)) ^ " " ^ svars )
      else 
         ("(ic "^s3 ^" "^s1^")", s4^s2, (string_of_int (code+1)) ^ " " ^ svars )       

  let rec ass_decomp_neg ass lstVarID code esl = match ass with
    |[] -> ("", "", "")
    |h::t ->
      let (s1, s2, svars) = ass_decomp_neg t lstVarID (code + 2) esl in
      let (s3, s4) = var_decomp_neg h lstVarID code esl in
      if (s1 = "") then
         (s3, s4^s2, (string_of_int (code+1)) ^ " " ^ svars )
      else 
         ("(ic "^s3 ^" "^s1^")", s4^s2, (string_of_int (code+1)) ^ " " ^ svars )       

  let rec ass_decomp_pn ass lstVarID code esl = match ass with
    |[] -> ("", "", "")
    |h::t ->
      let (s1, s2, svars) = ass_decomp_pn t lstVarID (code + 2) esl in
      let (s3, s4, bumpVar) = var_decomp_pn h lstVarID code in (* s3 is the decomposed prefix form, s4 is some kind of encoding to pass to minisat*)
      if (s1 = "") then
         (s3, s4^s2, (string_of_int bumpVar) ^ " " ^ svars )
      else 
         ("(ic "^s3 ^" "^s1^")", s4^s2, (string_of_int bumpVar) ^ " " ^ svars )       

  let rec ass_decomp_test_pos ass lstVarID code esl tc = match ass with
    |[] -> ("", "", "", code)
    |h::t ->
      let (s3, s4, bump_vars, new_code) = var_decomp_test_pos h lstVarID (code+1) esl tc in
      let (s1, s2, svars, final_code) = ass_decomp_test_pos t lstVarID new_code esl tc in
      if (s1 = "") then
         (s3, s4^s2, bump_vars ^ " " ^ svars, final_code )
      else 
         ("(ic "^s3 ^" "^s1^")", s4^s2, bump_vars ^ " " ^ svars, final_code )       

  let rec ass_decomp_test_neg ass lstVarID code esl tc = match ass with
    |[] -> ("", "", "", code)
    |h::t ->
      let (s3, s4, bump_vars, new_code) = var_decomp_test_neg h lstVarID (code+1) esl tc in
      let (s1, s2, svars, final_code) = ass_decomp_test_neg t lstVarID new_code esl tc in
      if (s1 = "") then
         (s3, s4^s2, bump_vars ^ " " ^ svars, final_code )
      else 
         ("(ic "^s3 ^" "^s1^")", s4^s2, bump_vars ^ " " ^ svars, final_code )       

  let rec ass_decomp_test_both ass lstVarID code esl tc = match ass with
    |[] -> ("", "", "", code)
    |h::t ->
      let (s3, s4, bump_vars, new_code) = var_decomp_test_both h lstVarID (code+1) esl tc in
      let (s1, s2, svars, final_code) = ass_decomp_test_both t lstVarID new_code esl tc in
      if (s1 = "") then
         (s3, s4^s2, bump_vars ^ " " ^ svars, final_code)
      else 
         ("(ic "^s3 ^" "^s1^")", s4^s2, bump_vars ^ " " ^ svars, final_code)
  
let rec insertionSort_byEasiness polyCons polyConstraints = match polyConstraints with
  | [] -> [polyCons]
  | h :: t -> 
    let currentEasiness = h#get_easiness in
    let newEasiness = polyCons#get_easiness in
    
    (* (1) (2) needs to change line 952 *)
    (*(* (2) need to change 942 *)
    if currentEasiness < newEasiness then polyCons :: polyConstraints
    else if currentEasiness > newEasiness then h :: (insertionSort_byEasiness polyCons t)*)
    
    (* (1) need to change 938 *)
    if currentEasiness < newEasiness then h :: (insertionSort_byEasiness polyCons t)
    else if currentEasiness > newEasiness then polyCons :: polyConstraints
    
    else (
      Random.self_init();
      if Random.bool() then h :: (insertionSort_byEasiness polyCons t)
      else polyCons :: polyConstraints
    )
    
    (*(* (10) need to change line 937 and 950 *)
    Random.self_init();
    if Random.bool() then h :: (insertionSort_byEasiness polyCons t)
    else polyCons :: polyConstraints*)
   
(*Rewrite eval_all for UNSAT cores computations*)
let rec eval_all res us uk_cl validPolyConstraints polyConstraints ia varsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap iaTime usTime remainingTime =
  match polyConstraints with
   |[] -> (res, us, uk_cl, validPolyConstraints, iaTime, usTime)
   |h::t -> 
      let startTime = Sys.time() in
      (*print_endline ("\nStart check sat: " ^ h#to_string_infix);
      flush stdout;*)
      (* print_varsSet (get_vars_set_boolExp h); (* print_varsSet is in Variable.ml and get_vars_set_boolExp is in ast.ml *)
      flush stdout;*)
      let res1 = h#check_sat_varsSen_setIsInfinite_setBounds_setEasiness varsIntvsMiniSATCodesMap in
      (*print_endline ("Bounds: " ^ h#get_iaValue#to_string);
      print_endline ("Easiness: " ^ string_of_float h#get_easiness);
      flush stdout;*)
      (*print_endline ("End check sat IA of " ^ h#to_string_infix ^ ", result: " ^ string_of_int res1);

      flush stdout;*)
      let iaTime = iaTime +. Sys.time() -. startTime in
      if (res1 = 1) then 
        eval_all res us uk_cl (h::validPolyConstraints) t ia varsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap iaTime usTime (remainingTime -. Sys.time() +. startTime)
      else if (res1 = -1) then (
        (*let str = (var_exp h checkVarID)^"0 " in *)
        (*let lstUC = get_unsatcore h 0 assIntv in *)
        let startUSCoreTime = Sys.time() in
        (*print_string "Start UNSAT core: ";
        print_endline (bool_expr_to_infix_string h);
        flush stdout;*)
        (*let lstUC = get_unsatcore h ia assIntv in*)
        (*print_endline ("UNSAT API: " ^ h#to_string_infix ^ ": " ^ string_of_int h#get_miniSATCode);
        flush stdout;*)
        let unsatCoreVars = get_unsatcore_vars h varsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap ((remainingTime -. Sys.time() +. startTime) (*/. 4.*)) in (* get_unsatcore_vars is defined in Unsat_core.ml *)
        let usTime = usTime +. Sys.time() -. startUSCoreTime in
        (*let str = 
          let s = var_exp_list lstUC checkVarID in 

          if (s = "") then

            (var_exp h checkVarID)^"0 "

          else
            s

        in*)

        (*let s2 = 
         if (str = "") then

           (bool_toString h) ^ str
         else
           str
        in*)
        (*eval_all (-1) (str^us) [] t ia assIntv originalIntv checkVarID iaTime usTime (remainingTime -. Sys.time() +. startTime)*)
        eval_all (-1) (unsatCoreVars^"0 "^us) [] [] t ia varsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap iaTime usTime (remainingTime -. Sys.time() +. startTime)
      )
      else ( (*res1 = 0*)     
        if res = -1 then
          eval_all (-1) us [] [] t ia varsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap iaTime usTime (remainingTime -. Sys.time() +. startTime)
        else (
          eval_all 0 us ((*h::uk_cl*) insertionSort_byEasiness h uk_cl) validPolyConstraints t ia varsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap iaTime usTime (remainingTime -. Sys.time() +. startTime)
        )
      )


  (*Binary balance decomposition on intervals*)
  let dynamicDecom varsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap miniSATCodesVarsIntvsMap nextMiniSATCode polyCons unkownPolyConstraints maxDecomposedVarsNum esl remainingTime = 
    (*print_endline ("Decomposing: " ^ polyCons#to_string_infix ^ ": " ^ string_of_int polyCons#get_miniSATCode);
    flush stdout;*)
    let startTime = Sys.time() in
    let varsSet = polyCons#get_varsSet in
    let not_smallIntv intv =
      let lowerBound = intv#l in
      let upperBound = intv#h in
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
      let (intv, miniSATCode) = StringMap.find var varsIntvsMiniSATCodesMap in
      if not_smallIntv intv then 
        let newInfVar = 
          if intv#h = infinity || intv#l = neg_infinity then var
          else infVar
        in
        (VariablesSet.add var reducedVarsSet, newInfVar)
      else (
        (*print_endline ("small interval: " ^ var ^ ": [" ^ string_of_float intv#l ^ ", " ^ string_of_float intv#h ^ "]");
        flush stdout;*)
        (reducedVarsSet, infVar)
      )
    in
    let (reducedVarsSet, infVar) = (*varsSet*) VariablesSet.fold add_notSmallInterval varsSet (VariablesSet.empty, VariablesSet.choose varsSet) in
    if VariablesSet.is_empty reducedVarsSet then (*Stop decomposition*) 
      let add_learnt_var var learntVars = 
        let (_, varId) = StringMap.find var varsIntvsMiniSATCodesMap in
        "-" ^ string_of_int varId ^ " " ^ learntVars
      in
      let learntClauses = VariablesSet.fold add_learnt_var varsSet ("-" ^ string_of_int polyCons#get_miniSATCode ^ " 0") in
      ((miniSATCodesVarsIntvsMap, nextMiniSATCode), learntClauses, "", false)
    else (*Continue decomposition*)
      let decompose_var var ((intv, varId), varSen, isPositiveSen) ((miniSATCodesVarsIntvsMap, nextMiniSATCode), learntClauses, bumpedVars, _) =
        let (narrowed, newIntv) = polyCons#backward_interval_propagate var intv varsIntvsMiniSATCodesMap in
        let lowerBound = intv#l in
        let upperBound = intv#h in
        let newPoint = 
          if lowerBound = neg_infinity then 
            if upperBound = infinity then 0.
            else upperBound -. 10.
          else 
            if upperBound = infinity then lowerBound +. 10. 
            else 0.5 *. lowerBound +. 0.5 *. upperBound
              (*if varSen = 0. || upperBound /. 2. -. lowerBound /. 2. <  esl then 0.5 *. lowerBound +. 0.5 *. upperBound
              else if isPositiveSen = polyCons#isPositiveDirected then upperBound -. esl 
              else lowerBound +. esl*)
        in
        (*let newPoint =
          if polyCons#get_logic = "QF_NIA" then (
            Random.self_init();
            if Random.bool() then
              ceil newPoint
            else
              floor newPoint
          )
          else newPoint
        in*)
        (*print_endline ("VarsSen: " ^ polyCons#string_of_varsSen);
        print_endline ("Decomposing: " ^ var ^ " of " ^ polyCons#to_string_infix ^ " - easiness: " ^ string_of_float polyCons#get_easiness ^ " in [" ^ string_of_float intv#l ^ ", " ^ string_of_float intv#h ^ "] with " ^ string_of_float newPoint);
        flush stdout;*)
        let lowerIntv = 
          if polyCons#get_logic = "QF_NIA" && ceil lowerBound = floor newPoint then
            let tmpNewPoint = floor newPoint in
            new IA.interval tmpNewPoint tmpNewPoint
          else new IA.interval lowerBound newPoint 
        in
        (*print_endline("lowerIntv" ^ lowerIntv#to_string);
        flush stdout;*)
        let upperIntv = 
          if polyCons#get_logic = "QF_NIA" && ceil newPoint = floor upperBound then
            let tmpNewPoint = floor upperBound in
            new IA.interval tmpNewPoint tmpNewPoint
          else new IA.interval newPoint upperBound 
        in
        (*print_endline("upperIntv" ^ upperIntv#to_string);
        flush stdout;*)
        let (bumpVar, unsatCore) =
          if lowerBound = neg_infinity || upperBound = infinity then
            if newPoint > 0. then (nextMiniSATCode, "")
            else (nextMiniSATCode + 1, "")
          (*(* (3) and (4) *)  
          else (
            (*print_endline ("Current Intervals: " ^ string_of_intervals varsIntvsMiniSATCodesMap);
            flush stdout;*)
            (*print_endline ("Original Intervals: " ^ string_of_intervals originalVarsIntvsMiniSATCodesMap);
            flush stdout;*)
            let lowerVarsIntvsMiniSATCodesMap = StringMap.add var (lowerIntv, nextMiniSATCode) varsIntvsMiniSATCodesMap in
            (*print_endline ("Lower Intervals: " ^ string_of_intervals lowerVarsIntvsMiniSATCodesMap);
            flush stdout;*)
            (*print_endline("UNKNOWN Constraints: \n" ^ string_infix_of_polynomialConstraints unkownPolyConstraints);
            flush stdout;*)
            let (lowerRes, lowerUS, lowerUnknownPolyConstraints, _, _, _) = eval_all 1 "" [] [] unkownPolyConstraints 0 lowerVarsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap 0. 0. (remainingTime -. Sys.time() +. startTime) in
            (*let lowerUnknownPolyConstraints = List.rev lowerUnknownPolyConstraints in*)
            (*print_endline ("Length of unkownPolyConstraints: " ^ string_of_int (List.length unkownPolyConstraints));
            print_endline ("Length of lowerUnknownPolyConstraints: " ^ string_of_int (List.length lowerUnknownPolyConstraints));
            print_endline ("lowerRes: " ^ string_of_int lowerRes);
            flush stdout;*)
            if lowerRes = -1 then (nextMiniSATCode+1, lowerUS)
            else if lowerRes = 1 then (nextMiniSATCode, "")
            else
              let upperVarsIntvsMiniSATCodesMap = StringMap.add var (upperIntv, nextMiniSATCode + 1) varsIntvsMiniSATCodesMap in
              (*print_endline ("Upper Intervals: " ^ string_of_intervals upperVarsIntvsMiniSATCodesMap);
              flush stdout;*)
              let (upperRes, upperUS, upperUnknownPolyConstraints, _, _, _) = eval_all 1 "" [] [] unkownPolyConstraints 0 upperVarsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap 0. 0. (remainingTime -. Sys.time() +. startTime) in
              (*let upperUnknownPolyConstraints = List.rev upperUnknownPolyConstraints in*)
              (*print_endline ("Length of upperUnknownPolyConstraints: " ^ string_of_int (List.length upperUnknownPolyConstraints));
              print_endline ("upperRes: " ^ string_of_int upperRes);
              flush stdout;*)
              if upperRes = -1 then (nextMiniSATCode, upperUS)
              else if upperRes = 1 then (nextMiniSATCode + 1, "")
              else (
                (*print_endline ("Remaining time: " ^ string_of_float (remainingTime -. Sys.time() +. startTime));
                flush stdout;*)
                let (_, _, _, _, lowerUNSATPolyConstraintsNum) = 
                  test lowerUnknownPolyConstraints lowerVarsIntvsMiniSATCodesMap (remainingTime -. Sys.time() +. startTime) (* test is defined in Testing.ml *)
                in
                (*print_endline("lowerUNSATPolyConstraintsNum: " ^ string_of_int lowerUNSATPolyConstraintsNum);
                flush stdout;*)
                let (_, _, _, _, upperUNSATPolyConstraintsNum) = 
                  test upperUnknownPolyConstraints upperVarsIntvsMiniSATCodesMap (remainingTime -. Sys.time() +. startTime) (* test is defined in Testing.ml *)
                in
                (*print_endline("upperUNSATPolyConstraintsNum: " ^ string_of_int upperUNSATPolyConstraintsNum);
                flush stdout;*)
                
                (*(* (3) SAT directed *)
                if lowerUNSATPolyConstraintsNum < upperUNSATPolyConstraintsNum then (nextMiniSATCode, "")
                else if lowerUNSATPolyConstraintsNum > upperUNSATPolyConstraintsNum then (nextMiniSATCode + 1, "")*)
                
                (* (4) UNSAT directed *)
                if lowerUNSATPolyConstraintsNum < upperUNSATPolyConstraintsNum then (nextMiniSATCode + 1, "")
                else if lowerUNSATPolyConstraintsNum > upperUNSATPolyConstraintsNum then (nextMiniSATCode, "")
                
                else (
                  Random.self_init();
                  if Random.bool() then (nextMiniSATCode + 1, "")
                  else (nextMiniSATCode, "")
                )
              )
          )*)
          (*else 
            (* Compute the SAT length of lower interval by IA *)
            let add_satLength (satLength, varsIntvsMiniSATCodesMap) polyCons =
              let (_, newSatLength) = polyCons#check_sat_get_satLength varsIntvsMiniSATCodesMap in
              (satLength +. newSatLength, varsIntvsMiniSATCodesMap)
            in
            
            let lowerVarsIntvsMiniSATCodesMap = StringMap.add var (lowerIntv, nextMiniSATCode) varsIntvsMiniSATCodesMap in
            let (totalLowerSatLength, _) = List.fold_left add_satLength (0., lowerVarsIntvsMiniSATCodesMap) unkownPolyConstraints in
            (*print_endline ("Total Lower: " ^ string_of_float totalLowerSatLength);
            flush stdout;*)
            (* Compute the SAT length of upper interval by IA *)
            let upperVarsIntvsMiniSATCodesMap = StringMap.add var (upperIntv, nextMiniSATCode + 1) varsIntvsMiniSATCodesMap in
            let (totalUpperSatLength, _) = List.fold_left add_satLength (0., upperVarsIntvsMiniSATCodesMap) unkownPolyConstraints in
            (*print_endline ("Total Upper: " ^ string_of_float totalUpperSatLength);
            flush stdout;*)
            
            if totalLowerSatLength > totalUpperSatLength then (nextMiniSATCode, "")
            else (nextMiniSATCode+1, "")*)
          (* (5) and (6) *)
          else (*if varSen = 0. then*)
            (* Compute the SAT length of lower interval by IA *)
            let lowerVarsIntvsMiniSATCodesMap = StringMap.add var (lowerIntv, nextMiniSATCode) varsIntvsMiniSATCodesMap in
            (*print_endline "Start Computing for lower interval";
            flush stdout;*)
            let (lowerSAT, lowerSatLength, lowerEasiness) = polyCons#check_sat_get_satLength lowerVarsIntvsMiniSATCodesMap in
            (*print_endline ("Lower: " ^ string_of_int lowerSAT ^ " - " ^ string_of_float lowerSatLength ^ " - easiness: " ^ string_of_float lowerEasiness);
            flush stdout;*)
            
            (* Compute the SAT length of upper interval by IA *)
            let upperVarsIntvsMiniSATCodesMap = StringMap.add var (upperIntv, nextMiniSATCode + 1) varsIntvsMiniSATCodesMap in
            let (upperSAT, upperSatLength, upperEasiness) = polyCons#check_sat_get_satLength upperVarsIntvsMiniSATCodesMap in
            (*print_endline ("Upper: " ^ string_of_int upperSAT ^ " - satLength: " ^ string_of_float upperSatLength ^ " - easiness: " ^ string_of_float upperEasiness);
            flush stdout;*)
            
            if lowerSAT = 1 then 
              if upperSAT = 1 then
                if Random.bool () then (nextMiniSATCode, "")
                else (nextMiniSATCode + 1, "")
              else if upperSAT = 0 then (nextMiniSATCode, "")
              else 
                let unsatCore = get_unsatcore_vars polyCons lowerVarsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap (remainingTime -. Sys.time() +. startTime) in
                (nextMiniSATCode, unsatCore)
            else if lowerSAT = -1 then 
              let lowerUnsatCore = get_unsatcore_vars polyCons lowerVarsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap (remainingTime -. Sys.time() +. startTime) in
              if upperSAT = -1 then 
                let upperUnsatCore = get_unsatcore_vars polyCons upperVarsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap (remainingTime -. Sys.time() +. startTime) in
                (*if Random.bool () then (nextMiniSATCode, lowerUnsatCore ^ "0 " ^ upperUnsatCore)
                else (nextMiniSATCode + 1, lowerUnsatCore ^ "0 " ^ upperUnsatCore)*)
                (0, lowerUnsatCore ^ "0 " ^ upperUnsatCore)
              else (nextMiniSATCode + 1, lowerUnsatCore)
            else 
              if upperSAT = 1 then (nextMiniSATCode + 1, "")
              else if upperSAT = -1 then (* UNSAT, we learn the intervals *) 
                let unsatCore = get_unsatcore_vars polyCons upperVarsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap (remainingTime -. Sys.time() +. startTime) in
                (nextMiniSATCode, unsatCore)
                
              (* (5) Strategy: SAT directed using Easiness, need to change line 1224 *)
              else if (*lowerSatLength < upperSatLength*) lowerEasiness < upperEasiness then (nextMiniSATCode + 1, "")
              else if (*lowerSatLength > upperSatLength*) lowerEasiness > upperEasiness then (nextMiniSATCode, "")
              
              (*(* (6) Strategy: UNSAT directedusing Easiness, need to change line 1220 *)
              else if (*lowerSatLength < upperSatLength*) lowerEasiness < upperEasiness then (nextMiniSATCode, "")
              else if (*lowerSatLength > upperSatLength*) lowerEasiness > upperEasiness then (nextMiniSATCode + 1, "")*)
              
              else 
                if Random.bool() then (nextMiniSATCode + 1, "")
                else (nextMiniSATCode, "")
          
          (*(*(7) random choice of box, need to change line 1183 and 1234 *)
          else (*if isPositiveSen = polyCons#isPositiveDirected then (nextMiniSATCode + 1, "")
          else (nextMiniSATCode, "")*)
            if Random.bool() then (nextMiniSATCode + 1, "")
            else (nextMiniSATCode, "")*)
        in
        (*print_endline ("UNSAT core: (" ^ unsatCore ^ ")");
        print_endline ("nextMiniSATcode: " ^ string_of_int nextMiniSATCode ^ ", bumped: " ^ string_of_int bumpVar);
        flush stdout;*)
        let newLearntClauses = 
          "-" ^ string_of_int varId ^ " " ^ string_of_int nextMiniSATCode ^ " " ^ string_of_int (nextMiniSATCode+1) ^ " 0 " ^
          string_of_int varId ^ " -" ^ string_of_int nextMiniSATCode ^ " 0 " ^
          "-" ^ string_of_int nextMiniSATCode ^ " -"^string_of_int (nextMiniSATCode+1) ^ " 0 " ^
          string_of_int varId ^ " -"^string_of_int (nextMiniSATCode+1) ^ " 0 " 
        in
        let newLearntClauses = 
          if unsatCore = "" then newLearntClauses
          else 
            (* (5) (6) box selection using easiness*)
            unsatCore ^ "0 " ^ newLearntClauses
            
            (*(* (3) (4) box selection using number of unsat apis *)
            unsatCore ^ newLearntClauses*)
        in


        let miniSATCodesVarsIntvsMap = IntMap.add nextMiniSATCode (var, lowerIntv) miniSATCodesVarsIntvsMap in
        let miniSATCodesVarsIntvsMap = IntMap.add (nextMiniSATCode + 1) (var, upperIntv) miniSATCodesVarsIntvsMap in
        let newBumpedVars = 
          if bumpVar = 0 then bumpedVars
          else bumpedVars ^ string_of_int bumpVar ^ " "
        in
        ((miniSATCodesVarsIntvsMap, nextMiniSATCode+2),learntClauses ^  newLearntClauses, newBumpedVars, true)
      in
      (*print_endline (string_of_bool polyCons#isInfinite);
      flush stdout;*)
      let decomposedVarsList = 
        if polyCons#isInfinite then [(infVar, 0., false)]
        else polyCons#get_n_varsSen_fromSet maxDecomposedVarsNum (*(VariablesSet.cardinal reducedVarsSet)*) reducedVarsSet 
      in
      let add_varIntvMiniSATCode currentVarsIntvsMiniSATCodesIsPositiveSenMap (var, varSen, isPositiveSen) = 
        let intvMiniSATCode = StringMap.find var varsIntvsMiniSATCodesMap in
        StringMap.add var (intvMiniSATCode, varSen, isPositiveSen) currentVarsIntvsMiniSATCodesIsPositiveSenMap
      in
      let decomposedVarsIntvsMiniSATCodesIsPositiveMap = List.fold_left add_varIntvMiniSATCode StringMap.empty decomposedVarsList in
      StringMap.fold decompose_var decomposedVarsIntvsMiniSATCodesIsPositiveMap ((miniSATCodesVarsIntvsMap, nextMiniSATCode), "", "", true)
      
      
  let rec dynamicDecomPolyConstraints varsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap miniSATCodesVarsIntvsMap nextMiniSATCode polyConstraints unkownPolyConstraints
                maxDecomposedVarsNum esl firstMiniSATCode isFirst learntNotDecomposedIntv remainingTime =
    match polyConstraints with
    | [] -> ((miniSATCodesVarsIntvsMap, nextMiniSATCode), learntNotDecomposedIntv ^ "0", "", false)
    | polyCons :: remainingPolyConstraints -> 
      let startTime = Sys.time() in
      if not isFirst && polyCons#get_miniSATCode = firstMiniSATCode then 
        dynamicDecomPolyConstraints varsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap miniSATCodesVarsIntvsMap nextMiniSATCode 
              remainingPolyConstraints unkownPolyConstraints maxDecomposedVarsNum esl firstMiniSATCode false learntNotDecomposedIntv (remainingTime -. Sys.time() +. startTime)
      else 
        let ((miniSATCodesVarsIntvsMap, nextMiniSATCode), sLearn, bump_vars, isDecomp) = dynamicDecom varsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap 
                              miniSATCodesVarsIntvsMap nextMiniSATCode polyCons unkownPolyConstraints maxDecomposedVarsNum esl (remainingTime -. Sys.time() +. startTime)
        in
        if isDecomp then 
          ((miniSATCodesVarsIntvsMap, nextMiniSATCode), sLearn, bump_vars, isDecomp)
        else 
          dynamicDecomPolyConstraints varsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap miniSATCodesVarsIntvsMap nextMiniSATCode 
              remainingPolyConstraints unkownPolyConstraints maxDecomposedVarsNum esl firstMiniSATCode false (sLearn ^ learntNotDecomposedIntv) (remainingTime -. Sys.time() +. startTime)

  (* compute the list of chosen constraints and *)
  let rec getConsAndIntv solution nextMiniSATCode clausesNum miniSATCodesConstraintsMap miniSATCodesVarsIntvsMap chosenPolyConstraints chosenVarsIntvsMiniSATCodesMap = match solution with
    |[] -> (chosenPolyConstraints, chosenVarsIntvsMiniSATCodesMap)
    |h::t ->
      if h >= 1 && h <= clausesNum then (
        (*print_endline ("Getting constraint number: " ^ string_of_int h);
        flush stdout;*)
        let nextChosenPolyConstraint = IntMap.find h miniSATCodesConstraintsMap in
        (*print_endline ("Got constraint: " ^ nextChosenPolyConstraint#to_string_infix);
        flush stdout;*)
        let newChosenPolyConstraints = (*insertion_sort_polyCons nextChosenPolyConstraint chosenPolyConstraints in (* insertion_sort_polyCons is defined in PolynomialConstraint.ml *)*)
                                       nextChosenPolyConstraint::chosenPolyConstraints in
        (*print_endline "Finish adding constraint";
        flush stdout;*)
        getConsAndIntv t nextMiniSATCode clausesNum miniSATCodesConstraintsMap miniSATCodesVarsIntvsMap newChosenPolyConstraints chosenVarsIntvsMiniSATCodesMap
      )
      else if h < nextMiniSATCode then (
        (*print_endline ("Getting interval number: " ^ string_of_int h);
        flush stdout;*)
        let (var, interval) = IntMap.find h miniSATCodesVarsIntvsMap in
        (*print_endline ("Got interval: " ^ var ^ " in " ^ interval#to_string);
        flush stdout;*)
        let newChosenVarsIntvsMiniSATCodesMap = StringMap.add var (interval, h) chosenVarsIntvsMiniSATCodesMap in
        getConsAndIntv t nextMiniSATCode clausesNum miniSATCodesConstraintsMap miniSATCodesVarsIntvsMap chosenPolyConstraints newChosenVarsIntvsMiniSATCodesMap
      )
      else
        getConsAndIntv t nextMiniSATCode clausesNum miniSATCodesConstraintsMap miniSATCodesVarsIntvsMap chosenPolyConstraints chosenVarsIntvsMiniSATCodesMap


  (*=========================== START DYNTEST =======================================*)  
  (*dynTest: Interval arithmetic, Testing and Dynamic interval decomposition*)
  let dynTest (originalVarsIntvsMiniSATCodesMap, miniSATCodesVarsIntvsMap, nextMiniSATCode) miniSATCodesConstraintsMap clausesNum strCheck ia esl strTestUS iaTime testingTime usTime parsingTime decompositionTime remainingTime =
    Random.self_init();
		let startTime = Sys.time() in
		(*print_endline ("Solution: " ^ strCheck);
		flush stdout;*)
    let solution = toIntList strCheck in

    (*print_endline "Start get constraints and intervals";
    flush stdout;*)
    (*print_endline ("ClausesNum: " ^ string_of_int clausesNum);
    flush stdout;*)
    (*print_endline ("Next MiniSAT code: " ^ string_of_int nextMiniSATCode);
    flush stdout;*)
    let (polyConstraints, varsIntvsMiniSATCodesMap) = getConsAndIntv solution nextMiniSATCode clausesNum miniSATCodesConstraintsMap miniSATCodesVarsIntvsMap [] StringMap.empty in

    (*let polyConstraints = List.rev polyConstraints in*)
    (*print_endline(string_infix_of_polynomialConstraints polyConstraints); (* In PolynomialConstraint.ml *)
    flush stdout;*)
    (*print_endline ("\nIntervals: " ^ string_of_intervals varsIntvsMiniSATCodesMap); (* string_of_intervals is defined in Assignments.ml *)
    flush stdout;*)
    let parsingTime = parsingTime +. Sys.time() -. startTime in
    (*print_endline "Start IA";
    flush stdout;*)
    
    let (res, us, uk_cl, validPolyConstraints, iaTime, usTime) = eval_all 1 "" [] [] polyConstraints ia varsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap iaTime usTime (remainingTime -. Sys.time() +. startTime) in
    (*print_endline ("EndIA, result: " ^ string_of_int res);
    flush stdout;*)
    if (res = -1) then (*if existing unsat clause*) (
      (res, us, "", "", (originalVarsIntvsMiniSATCodesMap, miniSATCodesVarsIntvsMap, nextMiniSATCode), "", "", "", "", iaTime, testingTime, usTime, parsingTime, decompositionTime) 
    )
    else if (res = 1) then (*if all clauses are satisfiable*)
      (res, "", "", (*(allLog polyConstraints ia varsIntvsMiniSATCodesMap)*) "", (originalVarsIntvsMiniSATCodesMap, miniSATCodesVarsIntvsMap, nextMiniSATCode), "", "", "", "", 
            iaTime, testingTime, usTime, parsingTime, decompositionTime)      
    else (*if unknown, testing will be implemented here*)(
      (*let uk_cl = List.rev uk_cl in (* reverse the list so that the apis are sorted based on variables dependency *)*)
      (*print_endline("IA Unkown constraints: " ^ string_infix_of_polynomialConstraints uk_cl); (* string_infix_of_polynomialConstraints is defined in polynomialConstraint.ml *)
      flush stdout;*)
      if (uk_cl = []) then (*This case will never happen*)
        (res, us, "", "", (originalVarsIntvsMiniSATCodesMap, miniSATCodesVarsIntvsMap, nextMiniSATCode), "", "", "", "", iaTime, testingTime, usTime, parsingTime, decompositionTime)
      else (
        (*print_endline "Start Testing";
        flush stdout;*)
        let startTestingTime = Sys.time() in
        let (tc, sTest, clTest_US, a, b) = 
          test uk_cl varsIntvsMiniSATCodesMap (remainingTime -. Sys.time() +. startTime) (* test is defined in Testing.ml *)
        in
        (*print_endline ("UNSAT constraints num: " ^ string_of_int b);
        flush stdout;*)
        (*print_endline ("SAT: " ^ assignments_toString tc);*)
        (*let (sTest, clTest_US, a) = evalTest assIntv uk_cl checkVarID strTestUS in*)
        (*let (tc, sTest, clTest_US, a) =  search_tc2 uk_cl assIntv strTestUS esl in *)
        (*print_endline ("End Testing, result: " ^ string_of_int sTest);
        flush stdout;*)
        let testingTime = testingTime +. Sys.time() -. startTestingTime in
           if (sTest = 1) then (
             let intvLog = log_intervals varsIntvsMiniSATCodesMap in
             let validPolyConstraints = List.rev validPolyConstraints in
             let iaLog = log_ia validPolyConstraints in
             let assignmentsLog = log_assignment a in (* log_assignment is in Assignments.ml *)
             let testLog = log_test uk_cl in
             let assignmentsString = string_of_assignment a in
             let uk_cl_string = string_postfix_of_polyConstraints uk_cl in
             (sTest, assignmentsString , uk_cl_string, intvLog ^ iaLog ^ assignmentsLog ^ testLog, (originalVarsIntvsMiniSATCodesMap, miniSATCodesVarsIntvsMap, nextMiniSATCode), "", "", "", 
                  string_postfix_of_polyConstraints validPolyConstraints ^ " ; " ^ string_of_intervals varsIntvsMiniSATCodesMap
                  , iaTime, testingTime, usTime, parsingTime, decompositionTime)
           )
           else
           (
             let startDecompositionTime = Sys.time() in
             (* If the uk_cl are equalities, then we implement some tricks for solving them. *)
             let (isEqualitiesSAT, unsatPolyConstraints) = 
               if is_all_equations uk_cl then (* is_all_equalities is defined in ast.ml *)
                 check_equalities uk_cl varsIntvsMiniSATCodesMap VariablesSet.empty (* check_equalities is defined in Equalities_handler.ml *)
               else (false, uk_cl)
             in
               if isEqualitiesSAT then 
                  (1, "", "", get_allLogs uk_cl, (originalVarsIntvsMiniSATCodesMap, miniSATCodesVarsIntvsMap, nextMiniSATCode), 
                       "", "", "", "", iaTime, testingTime, usTime, parsingTime, decompositionTime)
               else (
                  (*Applied for Dynamic interval decomposition*)
                  (*Balance interval decomposition*)
                  (*let (sInterval, sLearn, isDecomp) = dynamicDecom assIntv dIntv checkVarID nextMiniSATCode clTest_US esl in*)

                  let decomposedExpr = 
                    (*if (is_boolExpr_equation (List.hd clTest_US)#get_constraint) then
                      let firstInequation = first_inequation uk_cl in
                      match firstInequation with
                      |[] -> [List.hd unsatPolyConstraints]
                      | _ -> firstInequation
                    else*) clTest_US
                  in
                  (*print_endline "decomposing";
                  print_endline(bool_expr_list_to_infix_string decomposedExpr);
                  flush stdout;*)
                  (*let testUNSATPolyCons = List.hd clTest_US in
                  let decomposedPolyConstraints = testUNSATPolyCons :: uk_cl in*)
                  let maxDecomposedVarsNum = 1 in (* only $maxDecomposedVarsNum are allowed to decomposed, the priority is based on sensitivity *)
                  let ((miniSATCodesVarsIntvsMap, nextMiniSATCode), sLearn, bump_vars, isDecomp) = 
                    (*let (newInterval, newLearn, newBumpVars, isDecomposed) = decompose_unsat_detection (List.hd decomposedExpr) assIntv dIntv checkVarID nextMiniSATCode esl in*)
                    (*let (newInterval, newLearn, newBumpVars, isDecomposed) = decompose_list_unsat_detection uk_cl assIntv dIntv checkVarID nextMiniSATCode esl in
                    if isDecomposed then 
                      (newInterval, newLearn, newBumpVars, isDecomposed)
                    else*)
                      dynamicDecom varsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap miniSATCodesVarsIntvsMap  nextMiniSATCode (List.hd decomposedExpr) 
                                    uk_cl maxDecomposedVarsNum esl (remainingTime -. Sys.time() +. startTime) in
									(*print_endline "after decomposed";
									flush stdout;*)
                  let decompositionTime = decompositionTime +. Sys.time() -. startDecompositionTime in
                             
                  (*decomposed interval based on test values 
                  let (sInterval, sLearn, bump_vars, isDecomp) = 
                                     dynamicDecom_test assIntv dIntv checkVarID nextMiniSATCode clTest_US esl tc in *)

                  if isDecomp then (
                    (*print_endline "decomposed";
                    flush stdout;*)
                    (-2, "", sLearn, "", (originalVarsIntvsMiniSATCodesMap, miniSATCodesVarsIntvsMap, nextMiniSATCode), "", 
                          bump_vars, "", "", iaTime, testingTime, usTime, parsingTime, decompositionTime)    
                  )
                  else  (
                    (*print_endline ("UNKNOWN constraints: " ^ (List.hd decomposedExpr)#to_string_infix);
                    print_endline ("UNKNOWN Learnt:" ^ sLearn);
                    flush stdout;*)
                    (res, "", sLearn, "", (originalVarsIntvsMiniSATCodesMap, miniSATCodesVarsIntvsMap, nextMiniSATCode), "", 
                          bump_vars, "", "", iaTime, testingTime, usTime, parsingTime, decompositionTime)    
                  )
               )
           )
      )
    )
  (* ================================= END OF DYNTEST ==========================================*) 
end


(*===========================================================*)
(* Export those functions to C/C++ *)
(*===========================================================*)
let _ = Callback.register "caml_genSatForm" Caml.genSatForm;;
let _ = Callback.register "caml_dynTest" Caml.dynTest;;

