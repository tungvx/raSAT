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

(*Represente a polynomial expression by a string*)
let rec poly_toString sign  = function
  | Real c -> sign_simp sign c
  | Var x -> sign ^ x
  | Mul (Real 1., Var x) -> sign ^ x
  | Mul (Var x, Real 1.) -> sign ^ x
  | Mul (Real -1., Var x) -> rev sign ^ x
  | Mul (Var x, Real -1.) -> rev sign ^ x
  | Add (e1, e2) -> (poly_toString sign e1) ^ (poly_toString "+" e2)
  | Sub (e1, e2) -> (poly_toString sign e1) ^
      (if (isVar e2) then                    
  (poly_toString "-" e2)
      else
  "-("^(poly_toString "" e2)^")")
  | Mul (e1, e2) -> 
      (if (isVar e1) then 
         (poly_toString sign e1)
      else
         sign^"("^(poly_toString "" e1)^")" )
      ^ "*" ^ 
      (if (isVar e2) then                    
  (poly_toString "" e2)
      else
  "("^(poly_toString "" e2)^")")
  | Pow (e1, n)  -> sign ^ "("^(poly_toString "" e1)^")" ^ "^" ^ (string_of_int n)
  

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
let genSatForm sAss sIntv esl =
  let constraints = ParserConstraints.main LexerConstraints.lex (Lexing.from_string sAss) in
  (*print_endline (bool_expr_to_infix_string ass);*)
  let (miniSATExpr, index, miniSATCodesConstraintsMap, maxVarsNum) = miniSATExpr_of_constraints constraints 1 IntMap.empty in 
  (* miniSATExpr_of_constraints is defined in PolynomialConstraint.ml *)
  
  (* convert miniSATExpr into CNF *)
  let cnfMiniSATExpr = cnf_of_miniSATExpr miniSATExpr in
  
  (* convert cnfMiniSATExpr to string under the format of miniSAT input *)
  let (cnfMiniSATExprString, miniSATClauses) = string_of_cnf_miniSATExpr cnfMiniSATExpr true in
  (*print_endline cnfMiniSATExprString;
  flush stdout;*)
  
  let eIntv = ParserIntervals.main LexerIntervals.lex (Lexing.from_string sIntv) in
  let nVars = List.length eIntv in
   
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
  let max_bound = getMB 0.0 eIntv in
  let para = int_of_float (max_bound /. esl) in
  (*let totalVars = iLit * 4 * para in*)
  let totalVars = 
    if max_bound = infinity then 10000
    else index + 2* nVars * (sum_total_var 1 para)
  in
  let sTrivialClause = "-" ^string_of_int totalVars ^ " " ^string_of_int totalVars^ " 0" in
  (nVars, "p cnf " ^ string_of_int totalVars ^ " " ^ string_of_int (nVars+1+miniSATClauses) ^"\n"^ cnfMiniSATExprString ^ miniSATIntvString ^ sTrivialClause, intvInfo, miniSATCodesConstraintsMap, index-1, maxVarsNum)

  (*log result for satisfiable solution in an expression e*)
  let logSat polyCons ia varsIntvsMiniSATCodesMap = 
    let boolExpr = polyCons#get_constraint in
    let polyExp = get_exp boolExpr in
    let (bound, _)  = poly_eval polyExp polyCons#get_varsSet ia varsIntvsMiniSATCodesMap in
    match boolExpr with
    |Eq e -> 
      (poly_toString "" e) ^ "=" ^
        "["^(string_of_float bound#l) ^","^(string_of_float bound#h) ^ "]"^" = 0"  
    |Neq e -> 
      (poly_toString "" e) ^ "=" ^
        "["^(string_of_float bound#l) ^","^(string_of_float bound#h) ^ "]"^" != 0"  
    |Leq e -> 
      (poly_toString "" e) ^ "=" ^
        "["^(string_of_float bound#l) ^","^(string_of_float bound#h) ^ "]"^" <= 0"

    |Le e -> 
      (poly_toString "" e) ^ "=" ^
      "["^(string_of_float bound#l) ^","^(string_of_float bound#h) ^ "]"^" < 0"

    |Geq e -> 
      (poly_toString "" e) ^ "=" ^
        "["^(string_of_float bound#l) ^","^(string_of_float bound#h) ^ "]"^" >= 0"

    |Gr e -> 
      (poly_toString "" e) ^ "=" ^
        "["^(string_of_float bound#l) ^","^(string_of_float bound#h) ^ "]"^" > 0"
    

  (*Generate information about a test case*)
  let rec logTestCase ass = match ass with
    | [] -> ""
    | (x, a):: t -> (x ^" = "^ string_of_float a) ^ "\n" ^ (logTestCase t)
  
  (*record the result for each constraint by a test case*)
  let logValue boolExp ass = 
    let polyExp = get_exp boolExp in
    
    let value = evalFloat ass polyExp in
    
    match boolExp with
    |Eq e -> 
      (poly_toString "" e) ^"="^ (string_of_float value) ^" = 0"
    |Neq e -> 
      (poly_toString "" e) ^"="^ (string_of_float value) ^" != 0"  
    |Leq e -> 
      (poly_toString "" e) ^"="^ (string_of_float value) ^" <= 0" 
      (*^"="^ (string_of_float rightVal)*)
    |Le e -> 
      (poly_toString "" e) ^"="^ (string_of_float value) ^" < 0"
    |Geq e -> 
      (poly_toString "" e) ^"="^ (string_of_float value) ^" >= 0"
    |Gr e -> 
      (poly_toString "" e) ^"="^ (string_of_float value) ^" > 0"

  (*End logValue*)
  

  let rec logValue_all e ass = match e with
    |[] -> ""
    |[a] -> logValue a ass    
    |h::t->
      (logValue h ass) ^"\n"^ (logValue_all t ass)

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

  (*get positive, negative part from a constraint*)
  let rec get_pos_neg (pos, neg) sign e = match e with
    |Real c -> 
           if (c *.sign > 0.) then (1.0, pos, neg)
           else if (c *. sign < 0.) then (-1.0, pos, neg)
           else (1.0, pos, neg)
    |Var x -> 
      if (sign > 0.0) then (1.0, x::pos, neg)
      else (1.0, pos, x::neg)
    |Mul (Real c, e1) -> 
      if (c *. sign > 0.) then get_pos_neg (pos, neg) sign e1
      else if (c *.sign < 0.) then get_pos_neg (pos, neg) (-1. *. sign) e1
      else (1.0, pos, neg)
    |Mul (e1, Real c) -> 
      if (c *. sign > 0.) then get_pos_neg (pos, neg) sign e1
      else if (c *.sign < 0.) then get_pos_neg (pos, neg) (-1. *. sign) e1
      else (1.0, pos, neg)
    |Mul (e1, e2) -> 
      let (s1, pos1, neg1) = get_pos_neg ([], []) sign e1 in
      let (s2, pos2, neg2) = get_pos_neg ([], []) 1.0 e2 in
      let p = ref pos in
      let n = ref neg in
      if (pos1 != []) && (pos2 != []) then
         p := List.append !p (List.append pos1 pos2); 
      if (neg1 != []) && (neg2 != []) then
         p := List.append !p (List.append neg1 neg2); 
      if (pos1 != []) && (neg2 != []) then
         n := List.append !n (List.append pos1 neg2); 
      if (neg1 != []) && (pos2 != []) then
         n := List.append !n (List.append neg1 pos2); 

      if (sign *. s1 *. s2 < 0.) then
         (1.0, !n, !p)
      else
         (1.0, !p, !n)
    |Add (e1, e2) -> 
      let (s, pos1, neg1) = get_pos_neg (pos, neg) sign e1 in
      get_pos_neg (pos1, neg1) sign e2
    |Sub (e1, e2) ->
      let (s, pos1, neg1) = get_pos_neg (pos, neg) sign e1 in
      get_pos_neg (pos1, neg1) (-1. *. sign) e2
    |Pow (e1, n) -> (*This case will not happen in our implementation*)
      get_pos_neg (pos, neg) sign e1

  (*Decomposition intervals based on positive and negative parts*)
  (*let dynamicDecom_pos assIntv dIntv lstVarID nextMiniSATCode uk_cl esl =
    let cl_TestUS = get_exp (List.hd uk_cl) in

    (*Get the positive and negative list of variables*)
    let (sign, pos, neg) = get_pos_neg ([], []) 1.0 cl_TestUS in

    let pos_ = Util.red_list pos in
    let neg_ = Util.red_list neg in

    (* let list_vars = lstVars uk_cl in
    let new_ass = red_ass assIntv list_vars in *)
    let pos_ass = red_ass assIntv pos_ in (* pos_ass contains intervals of only positive variables *)
    let neg_ass = red_ass assIntv neg_ in (* neg_ass contains intervals of only negative variables *)

    (*let red_ass = decomp_reduce new_ass esl in*)
    let red_pos = decomp_reduce pos_ass esl in (* red_pos contains intervals of positive vars with length greater than epsilon esl *)
    let red_neg = decomp_reduce neg_ass esl in (* red_neg contains intervals of negative vars with length greater than epsilon esl *)
		(*print_endline "after reduce";
		flush stdout;*)

    if (red_pos = []) && (red_neg = []) then (* all the intervals are small enough, stop decomposition*) 
    (
      (*print_endline ("UNKNOWN API: " ^ (bool_expr_to_infix_string (List.hd uk_cl))); (* bool_expr_to_infix_string is defined in ast.ml *)
      print_endline ("Intervals: " ^ (string_of_intervals assIntv));
      flush stdout;*)
      let s = uk_lit uk_cl lstVarID in
      (dIntv, s, "", false)
    )
    else (*Continue decomposition*)
    (
      let subPos = sub_list red_pos red_neg in (* remove the common variables with red_neg from red_pos *)
      let subNeg = sub_list red_neg red_pos in(* remove the common variables with red_pos from red_neg *)
      if (subPos = []) && (subNeg =[]) then ( (* red_pos and red_neg are exactly the same *)
        let (sInterval, sLearn, bump_vars) = ass_decomp_pn red_pos lstVarID (nextMiniSATCode + 1) esl in (* sInterval is the decomposed intervals of variables in prefix form
                               sLearn: is the list of new literals to be added into minisat
                               bump_vars: list of codes (nextMiniSATCode) *)
        if (dIntv <> "") then (
          (*print_endline sInterval;
          print_endline sLearn;
          flush stdout;*)
          ("(ic "^dIntv ^" "^sInterval^")", sLearn, bump_vars, true) 
        )
        else
          (sInterval, sLearn, bump_vars, true)
      )
      else (  
        let (sInterval_pos, sLearn_pos, bump_pos) = ass_decomp_pn subPos lstVarID (nextMiniSATCode + 1) esl in
        let iPos = List.length subPos in
        let (sInterval_neg, sLearn_neg, bump_neg) = ass_decomp_pn subNeg lstVarID (nextMiniSATCode + iPos * 2 + 1) esl in    
        let sInterval = ref "" in
        let sLearn = ref "" in
        if (sInterval_pos <> "") && (sInterval_neg <> "") then
          sInterval := "(ic "^sInterval_pos ^" "^sInterval_neg^")"
        else if (sInterval_pos <> "") then
          sInterval := sInterval_pos
        else
          sInterval := sInterval_neg;
          sLearn := sLearn_pos ^ sLearn_neg;
          if (dIntv <> "") then (
            (*print_endline !sInterval;
            print_endline !sLearn;
            flush stdout;*)
            ("(ic "^dIntv ^" "^(!sInterval)^")", !sLearn, bump_pos ^ bump_neg, true) 
          )
          else
            (!sInterval, !sLearn, bump_pos ^ bump_neg, true)
      )
    )
  (* ====================== END dynamicDecom_pos ======================== *)

  (*Decomposition based on a test case*)
  let dynamicDecom_test assIntv dIntv lstVarID nextMiniSATCode uk_cl esl t = 
    let cl_TestUS = get_exp (List.hd uk_cl) in

    (*round off test cases tc*)
    let tc = round_test t in

    (*Get the positive and negative list of variables*)
    let (sign, pos, neg) = get_pos_neg ([], []) 1.0 cl_TestUS in

    let pos_ = Util.red_list pos in
    let neg_ = Util.red_list neg in

    (* let list_vars = lstVars uk_cl in
    let new_ass = red_ass assIntv list_vars in *)
    let pos_ass = red_ass assIntv pos_ in
    let neg_ass = red_ass assIntv neg_ in

    (*let red_ass = decomp_reduce new_ass esl in*)
    let red_pos = decomp_reduce pos_ass esl in
    let red_neg = decomp_reduce neg_ass esl in    

    if (red_pos = []) && (red_neg = []) then (*Stop decomposition*) 
    (
      let s = uk_lit uk_cl lstVarID in
      (dIntv, s, "", false)
    )
    else (*Continue decomposition*)
    (
      let subPos = sub_list red_pos red_neg in
      let subNeg = sub_list red_neg red_pos in
      if (subPos = []) && (subNeg =[]) then (
        let (sInterval, sLearn, bump_vars, code) = ass_decomp_test_both red_pos lstVarID nextMiniSATCode esl tc in
          if (dIntv <> "") then
            ("(ic "^dIntv ^" "^sInterval^")", sLearn, bump_vars, true)
          else
            (sInterval, sLearn, bump_vars, true)
      )
      else (  
        let (sInterval_pos, sLearn_pos, bump_pos, code_pos) = ass_decomp_test_pos subPos lstVarID nextMiniSATCode esl tc in
        let (sInterval_neg, sLearn_neg, bump_neg, code_neg) = ass_decomp_test_neg subNeg lstVarID code_pos esl tc in
        let intersec = sub_list red_pos subPos in
        let (sInterval_both, sLearn_both, bump_both, code_both) = 
              ass_decomp_test_both intersec lstVarID code_neg esl tc in
        let sInterval = ref "" in
        let sLearn = ref "" in
        sInterval := sInterval_pos;
        if (!sInterval <> "") then (
          if (sInterval_neg <> "") then 
            sInterval := "(ic "^ !sInterval ^" "^sInterval_neg^")";
            if (sInterval_both <> "") then
              sInterval := "(ic " ^ (!sInterval) ^ " " ^ sInterval_both ^ ")";
        )
        else (
          if (sInterval_neg <> "") then ( 
            sInterval := sInterval_neg;
            if (sInterval_both <> "") then
              sInterval := "(ic " ^ (!sInterval) ^ " " ^ sInterval_both^")";
          )
          else
            sInterval := sInterval_both;
        );
        sLearn := sLearn_pos ^ sLearn_neg ^ sLearn_both;
        if (dIntv <> "") then
          ("(ic "^dIntv ^" "^ (!sInterval) ^")", !sLearn, bump_pos ^ bump_neg ^ bump_both, true)
        else
          (!sInterval, !sLearn, bump_pos ^ bump_neg ^ bump_both, true)
      )
    )
  *)
  (*======= end unbalance decomposition =======*)   

  (*Binary balance decomposition on intervals*)
  let dynamicDecom varsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap miniSATCodesVarsIntvsMap nextMiniSATCode polyCons unkownPolyConstraints maxDecomposedVarsNum esl remainingTime = 
    (*print_endline ("Decomposing: " ^ polyCons#to_string_infix);
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
      let learntClauses = VariablesSet.fold add_learnt_var varsSet "" in
      ((miniSATCodesVarsIntvsMap, nextMiniSATCode), learntClauses, "", false)
    else (*Continue decomposition*)
      let decompose_var var ((intv, varId), varSen, isPositiveSen) ((miniSATCodesVarsIntvsMap, nextMiniSATCode), learntClauses, bumpedVars, _) =
        let lowerBound = intv#l in
        let upperBound = intv#h in
        let newPoint = 
          if lowerBound = neg_infinity then 
            if upperBound = infinity then 0.
            else upperBound -. 10.
          else 
            if upperBound = infinity then lowerBound +. 10. 
            else (*0.5 *. lowerBound +. 0.5 *. upperBound*)
              if varSen = 0. then 0.5 *. lowerBound +. 0.5 *. upperBound
              else if isPositiveSen = polyCons#isPositiveDirected then upperBound -. esl (*9. *. (upperBound /. 10.) +. lowerBound /. 10.*)
              else lowerBound +. esl (*upperBound /. 10. +. 9. *. (lowerBound /. 10.)*)
              (*let noiseErrCoeff = 0.5 *. upperBound -. 0.5 *. lowerBound in
              let satLength = polyCons#get_satLength in
              let varChange = noiseErrCoeff *. satLength /. varSen in
              if lowerBound +. varChange < upperBound then (
                print_endline ("Decomposing: " ^ var ^ " of " ^ polyCons#to_string_infix ^ " in [" ^ string_of_float intv#l ^ ", " ^ string_of_float intv#h ^ "] with change: " ^ string_of_float varChange);
                flush stdout;
                if isPositiveSen = polyCons#isPositiveDirected then upperBound -. varChange
                else lowerBound +. varChange )
              else 0.5 *. upperBound +. 0.5 *. lowerBound*)
        in
        (*print_endline ("Decomposing: " ^ var ^ " of " ^ polyCons#to_string_infix ^ " in [" ^ string_of_float intv#l ^ ", " ^ string_of_float intv#h ^ "] with " ^ string_of_float newPoint);
        flush stdout;*)
        let lowerIntv = new IA.interval lowerBound newPoint in
        let upperIntv = new IA.interval newPoint upperBound in
        let (bumpVar, unsatCore) =
          if lowerBound = neg_infinity || upperBound = infinity then
            if newPoint > 0. then (nextMiniSATCode, "")
            else (nextMiniSATCode + 1, "")
          else 
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
            else (nextMiniSATCode+1, "")
          (*else if varSen = 0. then
            (* Compute the SAT length of lower interval by IA *)
            let lowerVarsIntvsMiniSATCodesMap = StringMap.add var (lowerIntv, nextMiniSATCode) varsIntvsMiniSATCodesMap in
            (*print_endline "Start Computing for lower interval";
            flush stdout;*)
            let (lowerSAT, lowerSatLength) = polyCons#check_sat_get_satLength lowerVarsIntvsMiniSATCodesMap in
            (*print_endline ("Lower: " ^ string_of_int lowerSAT ^ " - " ^ string_of_float lowerSatLength);
            flush stdout;*)
            
            (* Compute the SAT length of upper interval by IA *)
            let upperVarsIntvsMiniSATCodesMap = StringMap.add var (upperIntv, nextMiniSATCode + 1) varsIntvsMiniSATCodesMap in
            let (upperSAT, upperSatLength) = polyCons#check_sat_get_satLength upperVarsIntvsMiniSATCodesMap in
            (*print_endline ("Upper: " ^ string_of_int upperSAT ^ " - " ^ string_of_float upperSatLength);
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
                if Random.bool () then (nextMiniSATCode, lowerUnsatCore ^ "0 " ^ upperUnsatCore)
                else (nextMiniSATCode + 1, lowerUnsatCore ^ "0 " ^ upperUnsatCore)
              else (nextMiniSATCode + 1, lowerUnsatCore)
            else 
              if upperSAT = 1 then (nextMiniSATCode + 1, "")
              else if upperSAT = -1 then (* UNSAT, we learn the intervals *) 
                let unsatCore = get_unsatcore_vars polyCons upperVarsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap (remainingTime -. Sys.time() +. startTime) in
                (nextMiniSATCode, unsatCore)
              else if lowerSatLength < upperSatLength then (nextMiniSATCode + 1, "")
              else if lowerSatLength > upperSatLength then (nextMiniSATCode, "")
              else 
                if Random.bool() then (nextMiniSATCode + 1, "")
                else (nextMiniSATCode, "")
          else if isPositiveSen = polyCons#isPositiveDirected then (nextMiniSATCode + 1, "")
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
          else unsatCore ^ "0 " ^ newLearntClauses
        in
        let miniSATCodesVarsIntvsMap = IntMap.add nextMiniSATCode (var, lowerIntv) miniSATCodesVarsIntvsMap in
        let miniSATCodesVarsIntvsMap = IntMap.add (nextMiniSATCode + 1) (var, upperIntv) miniSATCodesVarsIntvsMap in
        ((miniSATCodesVarsIntvsMap, nextMiniSATCode+2),learntClauses ^  newLearntClauses, bumpedVars ^ string_of_int bumpVar ^ " ", true)
      in
      (*print_endline (string_of_bool polyCons#isInfinite);
      flush stdout;*)
      let decomposedVarsList = 
        if polyCons#isInfinite then [(infVar, 0., false)]
        else polyCons#get_n_varsSen_fromSet maxDecomposedVarsNum reducedVarsSet 
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
(* 
 (*============= UNSAT Cores Analysis ===============*)
 (*Get the sign in an multiplication expression*)
  let rec get_sign sign e = match e with
  | Real c -> 
      if (c *.sign >= 0.) then 1.0
      else -1.0
  | Mul (e1, e2) ->
      let s1 = get_sign sign e1 in
      let s2 = get_sign 1.0 e2 in
      if (s1 *.s2 >= 0.) then 1.0
      else -1.0
  | _ -> sign

  (*Get positive and negative monomials*)
  let rec get_pn sign e = match e with
  |Real c ->
      if (sign *. c >= 0.) then ([Real c], [])
      else ([], [Real c])
  |Var x ->
      if (sign *.1.0 >= 0.) then ([Var x], [])
      else ([], [Var x])
  |Mul (e1, e2) -> 
      let s = get_sign sign e in
      if (s >= 0.) then ([e],[])
      else ([], [e])
  |Add(e1, e2) ->
      let (p1, n1) = get_pn sign e1 in
      let (p2, n2) = get_pn sign e2 in
      (List.append p1 p2, List.append n1 n2)
  |Sub(e1, e2) ->
      let (p1, n1) = get_pn sign e1 in
      let (p2, n2) = get_pn sign (Mul (Real (-1.0), e2)) in
      (List.append p1 p2, List.append n1 n2)
  |Pow(e1, n) ->
      if (sign >= 0.) then ([e],[])
      else ([], [e])

  (*Add all elements in a list*)
  let rec collect exp e = match e with
    |[] -> exp
    |h:: t-> 
  if (exp = Real 0.) then collect h t
  else collect (Add (exp, h)) t

  (*Extract a list of expressions to list of list expressions with length reduced -1*)
  let rec uc_extract result initial l = match l with
  |[] -> result
  |h::t -> 
      let l1 = List.append initial t in
      let l2 = h:: initial in
      uc_extract (l1::result) l2 t

  (*Check whether polynomials are unsat or not*)
  let isUnsat fpos neg ia assIntv =
    let poly = collect (Real 0.) (List.append fpos neg) in
    let (bound,_) = poly_eval poly ia assIntv in
    if (bound#h < 0.) then
      true
    else 
      false

  (*compute all unsat cores*)
  let rec uc_analysis fpos flist ia assIntv = match flist with
  |[] -> []
  |h::t ->
    (*Get unsat cores for h*)  
    let len = List.length h in
    let bUnsat = isUnsat fpos h ia assIntv in
    (*print_endline "End isUnsat";
    flush stdout;*)
    let lstUC = 
    if (bUnsat) then (
      if (len > 1) then ( 
        let l = uc_extract [] [] h in
        let subUC = uc_analysis fpos l ia assIntv in
        if (subUC = []) then 
          [h]
        else 
          subUC
      )
      else 
        [h]
    )
    else 
      []
    in
    List.append lstUC (uc_analysis fpos t ia assIntv)  

  (*Assume that all constraints are formed as f > 0.*)
  let rec merge_list fpos lstUc = match lstUc with
    |[] -> []
    |h::t -> 
       let l = collect (Real 0.) (List.append fpos h) in
       (Gr l):: (merge_list fpos t)

(*
  (*Decide whether l is a minimal element in list lst*)
  let rec is_minimal l lst = match lst with
  |[] -> true
  |h::t ->
      let lstVars = Util.red_list (bool_vars h) in
      if (subset_list lstVars l) then
  false
      else
  is_minimal l t
      
  (*optimizing UNSAT Cores by removing redundants*)
  let rec get_minimal res lst = match lst with
    |[] -> res
    |h::t ->
  let l1 = Util.red_list (bool_vars h) in
  if ((is_minimal l1 res) && (is_minimal l1 t)) then
    get_minimal (h::res) t
  else
    get_minimal res t*)

  (*Assume that all constraints are formed as f > 0.*)
(*  let get_unsatcore f ia assIntv =
    let f_new = Sub (leftExp f, rightExp f) in
    let (p, n) = get_pn 1.0 f_new in
    (*print_int ia;
    print_endline " End get_pn";
    print_endline("Positive: " ^ (poly_expr_list_to_infix_string p));
    print_endline("Negative: " ^ (poly_expr_list_to_infix_string n));
    flush stdout;*)
    let lstUnsatCores = uc_analysis p [n] ia assIntv in
    (*print_endline "End uc_analysis";
    flush stdout;*)
    let lstRed_UC =  
       if (lstUnsatCores = []) then  
         merge_list p [n] 
       else
         merge_list p lstUnsatCores 
    in
    (*lstRed_UC*)
    get_minimal [] lstRed_UC
    (*merge_list p lstUnsatCores*)
*)
(*let rec var_exp_list lst checkVarID = match lst with
  |[] -> ""
  |h::t -> 
   let s = var_exp h checkVarID in
   if (s = "") then  
     var_exp_list t checkVarID
   else
     s ^ "0 " ^ (var_exp_list t checkVarID)
(*============= End UNSAT Cores Analysis ===========*)
*) *)  
 
(*Rewrite eval_all for UNSAT cores computations*)
let rec eval_all res us uk_cl validPolyConstraints polyConstraints ia varsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap iaTime usTime remainingTime =
  match polyConstraints with
   |[] -> (res, us, uk_cl, validPolyConstraints, iaTime, usTime)
   |h::t -> 
      let startTime = Sys.time() in
      (*print_endline ("Start check sat: " ^ h#to_string_infix);
      flush stdout;*)
      (* print_varsSet (get_vars_set_boolExp h); (* print_varsSet is in Variable.ml and get_vars_set_boolExp is in ast.ml *)
      flush stdout;*)
      let res1 = h#check_sat_varsSen_setIsInfinite_setBounds varsIntvsMiniSATCodesMap in
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
          eval_all 0 us (h::uk_cl) validPolyConstraints t ia varsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap iaTime usTime (remainingTime -. Sys.time() +. startTime)
        )
      )

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
        let newChosenPolyConstraints = insertion_sort_polyCons nextChosenPolyConstraint chosenPolyConstraints in (* insertion_sort_polyCons is defined in PolynomialConstraint.ml *)
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

  let rec allLog polyConstraints ia varsIntvsMiniSATCodesMap = match polyConstraints with
    |[] -> ""
    |h::t -> (logSat h ia varsIntvsMiniSATCodesMap) ^ "\n" ^ (allLog t ia varsIntvsMiniSATCodesMap)  

  (*=========================== START DYNTEST =======================================*)  
  (*dynTest: Interval arithmetic, Testing and Dynamic interval decomposition*)
  let dynTest (originalVarsIntvsMiniSATCodesMap, miniSATCodesVarsIntvsMap, nextMiniSATCode) miniSATCodesConstraintsMap clausesNum strCheck ia esl strTestUS iaTime testingTime usTime parsingTime decompositionTime remainingTime =
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
      let uk_cl = List.rev uk_cl in (* reverse the list so that the apis are sorted based on variables dependency *)
      (*print_endline("IA Unkown constraints: " ^ string_infix_of_polynomialConstraints uk_cl); (* string_infix_of_polynomialConstraints is defined in polynomialConstraint.ml *)
      flush stdout;*)
      if (uk_cl = []) then (*This case will never happen*)
        (res, us, "", "", (originalVarsIntvsMiniSATCodesMap, miniSATCodesVarsIntvsMap, nextMiniSATCode), "", "", "", "", iaTime, testingTime, usTime, parsingTime, decompositionTime)
      else (
        (*print_endline "Start Testing";
        flush stdout;*)
        let startTestingTime = Sys.time() in
        let (tc, sTest, clTest_US, a) = 
          test uk_cl varsIntvsMiniSATCodesMap strTestUS (remainingTime -. Sys.time() +. startTime) (* test is defined in Testing.ml *)
        in
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
             let isEqualitiesSAT = 
               if is_all_equations uk_cl then (* is_all_equalities is defined in ast.ml *)
                 check_equalities uk_cl varsIntvsMiniSATCodesMap VariablesSet.empty (* check_equalities is defined in Equalities_handler.ml *)
               else false
             in
               if isEqualitiesSAT then 
                  (1, "", "", (*(allLog polyConstraints ia varsIntvsMiniSATCodesMap)*) "", (originalVarsIntvsMiniSATCodesMap, miniSATCodesVarsIntvsMap, nextMiniSATCode), 
                       "", "", "", "", iaTime, testingTime, usTime, parsingTime, decompositionTime)
               else (
                  (*Applied for Dynamic interval decomposition*)
                  (*Balance interval decomposition*)
                  (*let (sInterval, sLearn, isDecomp) = dynamicDecom assIntv dIntv checkVarID nextMiniSATCode clTest_US esl in*)

                  (* Unbalance interval decomposition *)
                  (*print_endline "decomposing";
                  print_endline(bool_expr_list_to_infix_string decomposedExpr);
                  flush stdout;*)
                  let testUNSATPolyCons = List.hd clTest_US in
                  let decomposedPolyConstraints = testUNSATPolyCons :: uk_cl in
                  let maxDecomposedVarsNum = 1 in (* only $maxDecomposedVarsNum are allowed to decomposed, the priority is based on sensitivity *)
                  let ((miniSATCodesVarsIntvsMap, nextMiniSATCode), sLearn, bump_vars, isDecomp) = 
                    (*let (newInterval, newLearn, newBumpVars, isDecomposed) = decompose_unsat_detection (List.hd decomposedExpr) assIntv dIntv checkVarID nextMiniSATCode esl in*)
                    (*let (newInterval, newLearn, newBumpVars, isDecomposed) = decompose_list_unsat_detection uk_cl assIntv dIntv checkVarID nextMiniSATCode esl in
                    if isDecomposed then 
                      (newInterval, newLearn, newBumpVars, isDecomposed)
                    else*)
                      dynamicDecomPolyConstraints varsIntvsMiniSATCodesMap originalVarsIntvsMiniSATCodesMap miniSATCodesVarsIntvsMap nextMiniSATCode decomposedPolyConstraints uk_cl
                                    maxDecomposedVarsNum esl (testUNSATPolyCons#get_miniSATCode) true "" (remainingTime -. Sys.time() +. startTime) in
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
                    (*print_endline ("UNKNOWN Learnt:" ^ sLearn);
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

