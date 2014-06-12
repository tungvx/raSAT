open Ast
open Util
open IA
open Expr
open Decomposer
open Assignments
open Equalities_handler
open Unsat_Core
open Testing
open Variable

module Caml = struct

  (*=======================================*)
  (* Encode - Decode literal and get all kind of assignments of interval variables*)
  (*=======================================*)

  (* Read: get an expression from a string *)
  let read s = Parser.main Lexer.lex (Lexing.from_string s)

  (* read_smt: get an SMT expression from a string *)
  (* let smt_read s = SmtParser.main SmtLexer.lex (Lexing.from_string s) *)

  (*cl_toLit computes the list of literals in each interval clause*)
  let rec cl_toLit = function
    | In (x, l, h) -> [In(x,l,h)]
    | Or (cl1, cl2) -> List.append (cl_toLit cl1) (cl_toLit cl2) 

  (*f_toLit compute the list of literals in a formula*)
  let rec f_toLit = function 
    | Ic (f1, f2) -> List.append (f_toLit f1) (f_toLit f2)
    | Cl cl -> cl_toLit cl

  (*idx generates list of number from 1 to n*)
  let rec idx n = match n with
    | 0 -> []
    | _ -> List.append (idx (n-1))[n]

  (*cl_count counts number of clauses in interval constraints*)
  let rec cl_count e = match e with 
    |Cl c -> 1
    |Ic (f1, f2) -> cl_count f1 + cl_count f2

  (*e_toIntv generates a interval assignment from interval constraints*)
  let rec e_toIntv exp = match exp with
    |In (x, l, h) -> [(x, new IA.interval l h)]
    |Or (e1,e2) -> List.append (e_toIntv e1) (e_toIntv e2)
  

  (*get the interval constraint or assertion constraint*)
  let sub_intv = function
    | Intv (e) -> e
    | _ -> Cl (In ("0", 0., 0.))           (*This case never happen*)  

  let sub_ass = function
    | Ass (e) -> e
    | _ -> Eq (Real 0., Real 0.)                (*This case never happen*)  

  (*toIntList convert a string to an int list*)
  let rec toIntList str = 
    try let id = String.index str ' ' in
     try int_of_string (String.sub str 0 id):: toIntList (String.sub str (id+1) ((String.length str)-(id+1)))
     with Failure "int_of_string" ->
   toIntList (String.sub str (id+1) ((String.length str)-(id+1)) )
    with Not_found ->    
      try  [int_of_string str] with Failure "int_of_string" -> []  

  (*get the string of encoded ID of variable in a bool expression*)      
  let var_exp e litID = 
    let lstVars = Util.red_list (bool_vars e) in
    let rec strEncoded l1 l2 = match l1 with
      |[] -> ""
      |h::t -> ("-" ^ string_of_int (List.assoc h l2)) ^ " " ^ (strEncoded  t l2) in

    strEncoded lstVars litID

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

  (*get a variable in an interval constraint*)
  let getVar = function
    | In (x, l, h) -> x
    | Or (e1, e2) -> ""      (*This case never happen*)

  (*Represent information of interval variables*)
  let bound_var (x, it) = x ^ " = " ^ "[" ^ (string_of_float it#l) ^ "," ^ (string_of_float it#h) ^ "]"
  let rec toString_ass assign = match assign with
     | [] -> ""
     | h::t -> (bound_var h) ^ "\n" ^ (toString_ass t)
  
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

(*Represent a bool expression by a string*)
let rec bool_toString = function
  | Eq (e1, e2) -> (poly_toString "" e1)^" = " ^ (poly_toString "" e2)
  | Le (e1, e2) -> (poly_toString "" e1)^" < " ^ (poly_toString "" e2)
  | Leq(e1, e2) -> (poly_toString "" e1)^" <= " ^(poly_toString "" e2)
  | Gr (e1, e2) -> (poly_toString "" e1)^" > " ^ (poly_toString "" e2)
  | Geq(e1, e2) -> (poly_toString "" e1)^" >= " ^(poly_toString "" e2)
  | And(e1, e2) -> (bool_toString  e1)^"\nand "^(bool_toString  e2)

let rec getMaxBound m cl = match cl with
  |In (x, l, h) -> max (h-.l) m
  |Or (cl1, cl2) -> 
    let m1 = getMaxBound m cl1 in
    getMaxBound m1 cl2

let rec getMB m constr = match constr with
    |Cl c -> getMaxBound m c
    |Ic (f1, f2) -> 
  let m1 = getMB m f1 in
  getMB m1 f2

  let rec sum_total_var init mb = 
    if init >= mb then init
    else init + sum_total_var (init*2) mb

  (*polynomial functions to prefix representation*)
  let rec poly_toPrefix = function
  | Real c -> string_of_float c
  | Var x -> x
  | Add (e1, e2) -> "(+ " ^ (poly_toPrefix e1) ^ " " ^ (poly_toPrefix e2) ^ ")"
  | Sub (e1, e2) -> "(- " ^ (poly_toPrefix e1) ^ " " ^ (poly_toPrefix e2) ^ ")"
  | Mul (e1, e2) -> "(* " ^ (poly_toPrefix e1) ^ " " ^ (poly_toPrefix e2) ^ ")"
  | Pow (e1, n)  -> "(^ " ^ (poly_toPrefix e1) ^ " " ^ (string_of_int n)  ^ ")"

  (*Constraints to prefix representation*)      
  let rec toPrefix  = function
  | Eq (e1, e2) -> "(= " ^ (poly_toPrefix e1)^" " ^ (poly_toPrefix e2)^")"
  | Le (e1, e2) -> "(< " ^ (poly_toPrefix e1)^" " ^ (poly_toPrefix e2)^")"
  | Leq(e1, e2) -> "(<= "^ (poly_toPrefix e1)^" " ^ (poly_toPrefix e2)^")"
  | Gr (e1, e2) -> "(> " ^ (poly_toPrefix e1)^" " ^ (poly_toPrefix e2)^")"
  | Geq(e1, e2) -> "(>= "^ (poly_toPrefix e1)^" " ^ (poly_toPrefix e2)^")"
  | And(e1, e2) -> "(and "^(toPrefix e1)^" " ^ (toPrefix e2)^")"

  (*=== polynomial functions to postfix representation ===*)
  let rec poly_toPostfix = function
  | Real c -> " real " ^ string_of_float c ^ " "
  | Var x -> " var " ^ x ^ " "
  | Add (e1, e2) -> (poly_toPostfix e1) ^ (poly_toPostfix e2) ^ "+ " 
  | Sub (e1, e2) -> (poly_toPostfix e1) ^ (poly_toPostfix e2) ^ "- "
  | Mul (e1, e2) -> (poly_toPostfix e1) ^ (poly_toPostfix e2) ^ "* "
  | Pow (e1, n)  -> (poly_toPostfix e1) ^ (string_of_int n) ^ "^ "
  (*=== End poly_toPostfix function ===*)

  (*=== Constraints to postfix representation ===*)      
  let rec contraint_toPostfix  = function
  | Eq (e1, e2) -> (poly_toPostfix e1) ^ (poly_toPostfix e2) ^ "= "
  | Le (e1, e2) -> (poly_toPostfix e1) ^ (poly_toPostfix e2) ^ "< " 
  | Leq(e1, e2) -> (poly_toPostfix e1) ^ (poly_toPostfix e2) ^ "<= "
  | Gr (e1, e2) -> (poly_toPostfix e1) ^ (poly_toPostfix e2) ^ "> " 
  | Geq(e1, e2) -> (poly_toPostfix e1) ^ (poly_toPostfix e2) ^ ">= " 
  | And(e1, e2) -> (contraint_toPostfix e1) ^ (contraint_toPostfix e2) ^ "and "
  (*=== End contraint_toPostfix function ===*)

  (*=== Function for converting list of contraints to string of postfix form ===*)      
  let rec contraints_list_toString e = match e with
      |[] -> ""
      |[a] -> contraint_toPostfix a   
      |h::t->
    (contraint_toPostfix h) ^" , "^ (contraints_list_toString t)
  (*===== end of contraints_list_toString ======*)

  (*==================================================*)
  (* C++ - OCaml interface *)
  (*==================================================*)

  (*get miniSat form of interval constraints*)
  let genSatForm sIntv esl =
    let eIntv = sub_intv (read sIntv) in
    (* lstLit stored the list of literals from interval constraints*)
    let lstLit = f_toLit eIntv in

    (* iLit stored the number of literals from interval constraints*)
    let iLit = List.length lstLit in

    (* iClause stored the number of clauses in interval constraints
       = number of variables in a problem *)
    let iClause = cl_count eIntv in
    
    let lst = idx iLit in 
    (*Encode each literal with an ID: Integer number*)    
    let lstLitID = List.combine lstLit lst in

    (*function for compute id or literal from parameter*)
    let id_of_lit l = List.assoc l lstLitID in

    (* cnf_of_clause generate cnf form in each clause*)
    let cnf_of_clause cl=
      let rec id_clause = function    
        | In (x,l,h) -> [id_of_lit (In (x,l,h))]
        | Or (cl1, cl2) -> List.append (id_clause cl1) (id_clause cl2) in

      let lst = id_clause cl in
      let s = ref "" in
      let iCl = ref 0 in
      let n = (List.length lst) -1 in
      if (n > 1) then (
        s := cnf_clause lst;
        iCl := n*(n+1)/2
      );  
      (!iCl, !s ^ (cnf_all lst)) in

    (* genCnfCons generates miniSAT form of assertion constraints after encoding *)
    let rec genCnfCons = function
      |Cl c -> (cnf_of_clause c)
      |Ic (f1, f2) -> 
    let (i1, s1) = genCnfCons f1 in
    let (i2, s2) = genCnfCons f2 in
      (i1+i2, s1^s2) in

    (* sCnfCons stored miniSAT from of assertion constraints after encoding *)
    let (cl, sCnfCons) = genCnfCons eIntv in

    (*add 1 variables and 1 clause for an unique assertion constraint*)
    (*With SMT benchmark assume that there is an unique assertion constraint*)
    let totalClauses = cl + iClause in 

    (*Compute the total of variables for SAT encoding*)
    let max_bound = getMB 0.0 eIntv in
    let para = int_of_float (max_bound /. esl) in

    (*let totalVars = iLit * 4 * para in*)
    let totalVars = 
      if max_bound = infinity then 10000
      else 1 + 2* iLit * (sum_total_var 1 para) 
    in

    let sTrivialClause = "-" ^string_of_int totalVars ^ " " ^string_of_int totalVars^ " 0" in

    (iClause, "p cnf " ^ string_of_int totalVars ^ " " ^ string_of_int (totalClauses+1) ^"\n"^ sCnfCons ^ sTrivialClause)

    (*^ string_of_int totalVars ^ " 0"*)

  (*log result for satisfiable solution in an expression e*)
  let logSat e ia assIntv = 
    let left = leftExp e in
    let right = rightExp e in
    let (leftBound, _)  = poly_eval left ia assIntv in
    let (rightBound, _) = poly_eval right ia assIntv in
    match e with
    |Eq (e1, e2) -> 
      (poly_toString "" e1) ^ "=" ^
      "["^(string_of_float leftBound#l) ^","^(string_of_float leftBound#h) ^ "]"^" = "^
      ( match e2 with 
        | Real c -> (poly_toString "" e2)
        | _ -> 
          (poly_toString "" e2) ^ "=" ^ 
          "["^(string_of_float rightBound#l) ^","^(string_of_float rightBound#h) ^ "]" 
          )  

    |Leq(e1, e2) -> 
  (poly_toString "" e1) ^ "=" ^
  "["^(string_of_float leftBound#l) ^","^(string_of_float leftBound#h) ^ "]"^" <= "^
  ( match e2 with 
    | Real c -> (poly_toString "" e2)
    | _ -> 
  (poly_toString "" e2) ^ "=" ^ 
  "["^(string_of_float rightBound#l) ^","^(string_of_float rightBound#h) ^ "]" 
     )

     |Le (e1, e2) -> 
  (poly_toString "" e1) ^ "=" ^
  "["^(string_of_float leftBound#l) ^","^(string_of_float leftBound#h) ^ "]"^" < "^
  ( match e2 with 
    | Real c -> (poly_toString "" e2)
    | _ -> 
  (poly_toString "" e2) ^ "=" ^ 
  "["^(string_of_float rightBound#l) ^","^(string_of_float rightBound#h) ^ "]" 
     )

    |Geq(e1, e2) -> 
  (poly_toString "" e1) ^ "=" ^
  "["^(string_of_float leftBound#l) ^","^(string_of_float leftBound#h) ^ "]"^" >= "^
  ( match e2 with 
    | Real c -> (poly_toString "" e2)
    | _ -> 
  (poly_toString "" e2) ^ "=" ^ 
  "["^(string_of_float rightBound#l) ^","^(string_of_float rightBound#h) ^ "]" 
     )

    |Gr (e1, e2) -> 
  (poly_toString "" e1) ^ "=" ^
  "["^(string_of_float leftBound#l) ^","^(string_of_float leftBound#h) ^ "]"^" > "^
  ( match e2 with 
    | Real c -> (poly_toString "" e2)
    | _ -> 
  (poly_toString "" e2) ^ "=" ^ 
  "["^(string_of_float rightBound#l) ^","^(string_of_float rightBound#h) ^ "]" 
     )

    | _ -> ""        (*This case never happen*)

 
  (*=====================================================================================*)    
  (* isTheoConsis will check minisat result is consistent with Interval Arithmetic Theory*)    
  let isTheoConsis sIntv sAss strCheck ia =
      let lstCheck = toIntList strCheck in
      let eIntv = sub_intv (read sIntv) in 
      let eAss = sub_ass (read sAss) in      
      
      (* lstLit stored the list of literals from interval constraints*)
      let lstLit = f_toLit eIntv in

      (* iVar stored the number of variables from interval constraints*)
      let iVar = List.length lstLit in
  
      let lst = idx iVar in 
      (*Encoded each literal with an ID: Integer number*)    
      let lstIDLit = List.combine lst lstLit in
      (*let lstLitID = List.combine lstLit lst in*)

      (*function for compute the list of pairs of variable and ID from the list of ID of interval constraints*)
      let rec var_list = function
  | [] -> []
  | h::t ->
      if (h>=1) &&(h<=iVar) then (
        let lit = List.assoc h lstIDLit in
        (getVar lit, h):: (var_list t) )
      else
        var_list t in
        
      (*checkVarID stored the list of paris of variable and ID*)
      let checkVarID = var_list lstCheck in

      let rec ci_ass = function
  | [] -> []
  | h::t -> 
      if (h>=1) && (h<=iVar) then 
        List.append (e_toIntv (List.assoc h lstIDLit)) (ci_ass t) 
      else 
        (ci_ass t) in

      (*assIntv is an assignment of classical interval*)
      let assIntv = ci_ass lstCheck in

  (*check all constraints in assertion constraints satisfiable or not*)
  let rec isConsistent e = match e with
        |And (e1, e2) -> 
    let (res, s) = isConsistent e1 in
    if (res=1) then isConsistent e2  
    else (res, s)
  | _ -> 
      let res = checkSat e ia assIntv in
            if (res=1) then 
        (res, "")
         else (
        let str = var_exp e checkVarID in 
        (res, str)
      )
    in

  let (sat,s) = isConsistent eAss in
  (sat, s)
  
  (*============================================*)
  (*logResult will log all bounds of constraints*)
  let logResult sIntv sAss strCheck ia =
      let lstCheck = toIntList strCheck in
      let eIntv = sub_intv (read sIntv) in 
      let eAss = sub_ass (read sAss) in      
      
      (* lstLit stored the list of literals from interval constraints*)
      let lstLit = f_toLit eIntv in

      (* iVar stored the number of variables from interval constraints*)
      let iVar = List.length lstLit in
  
      let lst = idx iVar in 
      (*Encoded each literal with an ID: Integer number*)    
      let lstIDLit = List.combine lst lstLit in
      (*let lstLitID = List.combine lstLit lst in*)

      (*function for compute the list of pairs of variable and ID from the list of ID of interval constraints*)
      (*let rec var_list = function
  | [] -> []
  | h::t ->
      if (h>=1) &&(h<=iVar) then (
        let lit = List.assoc h lstIDLit in
        (getVar lit, h):: (var_list t) )
      else
        var_list t in 
  *)     

      let rec ci_ass = function
  | [] -> []
  | h::t -> 
      if (h>=1) && (h<=iVar) then 
        List.append (e_toIntv (List.assoc h lstIDLit)) (ci_ass t) 
      else 
        (ci_ass t) in

      (*assIntv is an assignment of classical interval*)
      let assIntv = ci_ass lstCheck in

  (*get logBound for all constraints in assertion constraints*)
  let rec allLog e = match e with
    |And (e1, e2) -> (allLog e1) ^ (allLog e2)  
    | _ -> (logSat e ia assIntv) ^ "\n" in
  
  (toString_ass assIntv)^(allLog eAss)
  (*End logResult*)
  
  (*====================================== TESTING =====================================*)  
  (*New functions for Testing*)            
  (*let rec f1 l a = match l with
    | [] -> [[a]]
    | [e] -> [(a::e)]
    | h::t-> List.append (f1 t a) [a::h]*)
  let rec f1 r l a = match l with
    | [] -> r
    | h::t-> f1 ((a::h)::r) t a

  (*let rec f l1 l2 = match l2 with
    | [] -> l1
    | [e] -> f1 l1 e
    | h::t -> List.append (f l1 t) (f1 l1 h) *)
  let rec f r l1 l2 = match l2 with
    | [] -> r
    | h::t -> f (f1 r l1 h) l1 t 

  (*let decomp l = List.fold_left f [] l*)
  
  (*New version for decomp*)
  let rec f2 r l a = match l with
    | [] -> [a]:: r
    | [e] -> (a::e) :: r
    | h::t-> f2 ((a::h)::r) t a
  
  let rec f3 r l1 l2 = match l2 with
    |[] -> r
    |h::t -> f3 (f2 r l1 h) l1 t

  let rec f4 r l = match l with
    | [] -> r
    | h::t -> f4 (f3 [] r h) t

  let decomp l = f4 [] l

  (*Relate to decomposition for group test*)
  let rec f22 r l a = match l with
    | [] -> a::r
    | [e] -> (List.append a e) :: r
    | h::t-> f22 ((List.append a h)::r) t a
  
  let rec f33 r l1 l2 = match l2 with
    |[] -> r
    |h::t -> f33 (f22 r l1 h) l1 t

  let rec f44 r l = match l with
    | [] -> r
    | h::t -> f44 (f33 [] r h) t
  
  (*Generate all test cases for a variable x, 
    Modifying strategy for generating test cases will be taken in this function*)
  (*
  let gen_test3 x lb ub = 
    (*Random.self_init();*)
    (*Random.init(Random.bits());*)
    (*Random.init(Sys.time());*)
    let seed = 0.5*.(ub-.lb) in
    let r1 = Random.float seed in
    let r2 = Random.float seed in
    (*Each variable has 3 test cases*)
    if (r1>= 0.5*.seed) then       
      (*[(x, lb); (x, lb+.r1); (x, ub -. (min r2 seed-.r2)); (x, ub)]*)
      [(x, lb); (x, lb+.r1); (x, ub -. (min r2 (seed-.r2)))]
    else 
      (*[(x, lb); (x, lb+.r1); (x, ub -. (max r2 seed-.r2)); (x, ub)]*)   
      [(x, lb); (x, lb+.r1); (x, ub -. (max r2 (seed-.r2)))]
  *)
  let gen_test3 x lb ub = 
    let seed = 0.5*.(ub-.lb) in
    let r1 = Random.float seed in
    let r2 = Random.float seed in
    (*Each variable has 3 test cases*)
    if (r1>= 0.5*.seed) then       
      (*[(x, lb); (x, lb+.r1); (x, ub -. (min r2 seed-.r2)); (x, ub)]*)
      (x, [lb; lb+.r1; ub -. (min r2 (seed-.r2))])
    else 
      (*[(x, lb); (x, lb+.r1); (x, ub -. (max r2 seed-.r2)); (x, ub)]*)   
      (x, [lb; lb+.r1; ub -. (max r2 (seed-.r2))])

  let gen_test_semi3 x lb ub = 
    let seed = 0.5*.(ub-.lb) in
    let r1 = Random.float seed in
    let r2 = Random.float seed in
    (*Each variable has 3 test cases*)
    if (r1>= 0.5*.seed) then       
      (*[(x, lb); (x, lb+.r1); (x, ub -. (min r2 seed-.r2)); (x, ub)]*)
      if (lb=0.) || (lb=1.) then
  (x, [lb; lb+.r1; ub -. (min r2 (seed-.r2))])
      else
  (x, [lb+.r1; ub -. (min r2 (seed-.r2))])
    else 
      (*[(x, lb); (x, lb+.r1); (x, ub -. (max r2 seed-.r2)); (x, ub)]*)   
      if (lb=0.) || (lb =1.) then
  (x, [lb; lb+.r1; ub -. (max r2 (seed-.r2))])
      else
  (x, [lb+.r1; ub -. (max r2 (seed-.r2))])

  let gen_test31 x lb ub = 
    let seed = 0.5*.(ub-.lb) in
    let r1 = Random.float seed in
    let r2 = Random.float seed in
    (*Each variable has 3 test cases*)
    (x, [lb; lb+.r1; ub-.r2])

  let gen_test4 x lb ub = 
    (*Random.self_init ();*)
    (*Random.full_init(a);*)
    let seed = 0.3*.(ub-.lb) in
    let r1 = Random.float seed in
    let r2 = Random.float seed in
    let r3 = Random.float seed in
    (*Each variable has 4 test cases*)
    [(x, lb); (x, lb+.r1); (x, lb+.seed+.r2); (x, ub -.r3)]

  let gen_test22 x lb ub = [(x, lb); (x, ub)]

  let gen_test2 x lb ub = 
    let seed = 0.5*.(ub-.lb) in
    let r1 = Random.float seed in
    let r2 = Random.float seed in
    if (r1>= 0.5*.seed) then 
      (x, [lb+.r1; ub -. (min r2 (seed-.r2))])
    else 
      (x, [lb+.r1; ub -. (max r2 (seed-.r2))])    

  let gen_test21 x lb ub esl = 
    let seed = (ub-.lb) in
    let r1 = Random.float seed in
      (x, [lb; lb+.r1])

  (*gen_test_tick generates test cases that may contain 1.0 value*)
  let gen_test_tick x lb ub esl = 
    let seed = (ub-.lb) in
    let r1 = Random.float seed in
    (*if (seed <= esl)&&(lb <=1.0)&&(ub>=1.0) then*)
    if (lb <=1.0)&&(ub>=1.0) then
      (x, [1.0; lb+.r1])
    else
      (x, [lb; lb+.r1])

  (*gen fixed 2 test cases*)
  let rec gen_ftest l1 l2 assIntv = match assIntv with
    |[] -> [l1;l2]
    |(x, it)::t -> gen_ftest ((x, it#l):: l1) ((x, it#h)::l2) t
  
  (*Generate test for all variables in a list*)
  let rec genAss_test r g ass esl = match ass with
    | [] -> r
    | (x, it)::t -> genAss_test ((g x it#l it#h esl)::r) g t esl
  
  (*
  let rec genAss_test g assIntv = match assIntv with
    | [] -> []
    | (x, it)::t -> (g x it#l it#h):: (genAss_test g t)  
  *)

  (*check whether an expression is satisfiable for an expression e by binding a test case*)
  let checkValue e ass = 
    let left = leftExp e in
    let right = rightExp e in
    let leftValue  = evalFloat ass left in
    let rightValue = evalFloat ass right in

    match e with
    |Eq (e1, e2) -> 
      if (leftValue = rightValue) then 1  
      else -1 
    |Leq(e1, e2) -> 
      if (leftValue <= rightValue) then 1  
      else -1
    |Le (e1, e2) -> 
      if (leftValue < rightValue) then 1  
      else -1  
    |Geq(e1, e2) -> 
      if (leftValue >= rightValue) then 1  
      else -1  
    |Gr (e1, e2) -> 
      if (leftValue > rightValue) then 1  
      else -1  
    | _ -> 1     (*This case never happen*)

  (*new version of check whether an expression is satisfiable for an expression e by binding a test case*)
  let checkValue1 e ass = 
    let left = leftExp e in
    let right = rightExp e in
    let leftValue  = evalFloat ass left in
    let rightValue = evalFloat ass right in
    let d = abs_float (leftValue-.rightValue) in   

    match e with
    |Eq (e1, e2) -> 
      if (leftValue = rightValue) then (1, d)  
      else (-1, d) 
    |Leq(e1, e2) -> 
      if (leftValue <= rightValue) then (1, d)  
      else (-1, d) 
    |Le (e1, e2) -> 
      if (leftValue < rightValue) then (1, d)  
      else (-1, d) 
    |Geq(e1, e2) -> 
      if (leftValue >= rightValue) then (1, d)  
      else (-1, d) 
    |Gr (e1, e2) -> 
      if (leftValue > rightValue) then (1, d)   
      else (-1, d)   
    | _ -> (1, d)      (*This case never happen*)

  (*Generate information about a test case*)
  let rec logTestCase ass = match ass with
    | [] -> ""
    | (x, a):: t -> (x ^" = "^ string_of_float a) ^ "\n" ^ (logTestCase t)
  
  (*record the result for each constraint by a test case*)
  let logValue e ass = 
    let left = leftExp e in
    (*let right = rightExp e in*)
    
    let leftVal = evalFloat ass left in
    (*let rightVal = evalFloat ass right in*)
    
    match e with
    |Eq (e1, e2) -> 
      (poly_toString "" e1) ^"="^ (string_of_float leftVal) ^" = "^
      (poly_toString "" e2)
      (*"="^ (string_of_float rightVal)*)
    |Leq(e1, e2) -> 
      (poly_toString "" e1) ^"="^ (string_of_float leftVal) ^" <= "^
      (poly_toString "" e2) 
      (*^"="^ (string_of_float rightVal)*)
    |Le (e1, e2) -> 
      (poly_toString "" e1) ^"="^ (string_of_float leftVal) ^" < "^
      (poly_toString "" e2) 
      (*^"="^ (string_of_float rightVal)*)
    |Geq(e1, e2) -> 
      (poly_toString "" e1) ^"="^ (string_of_float leftVal) ^" >= "^
      (poly_toString "" e2) 
      (*^"="^ (string_of_float rightVal)*)
    |Gr (e1, e2) -> 
      (poly_toString "" e1) ^"="^ (string_of_float leftVal) ^" > "^
      (poly_toString "" e2) 
      (*^"="^ (string_of_float rightVal)*)
    | _ -> ""     (*This case never happen*)

  (*End logValue*)
  
  (*check all constraints in assertion constraints satisfiable or not*)
  let rec valSat e ass= match e with
    |[] -> 1
    |[a] -> checkValue a ass
    |h::t -> 
      let res = checkValue h ass in
      if (res=1) then valSat t ass 
      else res
    
  let rec valSat_all e ass = match ass with
    |[] -> (-1, [])
    |[a] -> (valSat e a, a)
    |h::t -> 
      let res = valSat e h in
      if (res =1) then (res, h)
      else valSat_all e t
    
  let rec unsat_test e ass = match ass with
    |[] -> -1
    |[a] -> checkValue e a
    |h::t -> let r = checkValue e h in
    if (r = -1) then 
      unsat_test e t
    else r

  let rec list_unsat e ass= match e with
    |[] -> []
    |[a]->
      let res = unsat_test a ass in    
      if (res = -1) then [a]
      else []
    |h::t -> List.append (list_unsat [h] ass) (list_unsat t ass)
        
  let rec logValue_all e ass = match e with
    |[] -> ""
    |[a] -> logValue a ass    
    |h::t->
      (logValue h ass) ^"\n"^ (logValue_all t ass)
    
  (*compute the list of variables from a list of constraints*)
  let lstVars lst = 
    let rec get_list = function
      |[] -> []
      |h::t -> List.append (bool_vars h) (get_list t) in
    Util.red_list (get_list lst)

  (*remove unnecessary assignment*)
  let rec red_ass assIntv lstVar = match assIntv with
    |[] -> []
    |(x, it)::t -> 
  if (List.mem x lstVar) then
    (x,it):: red_ass t lstVar 
        else
    red_ass t lstVar 

  (*get the constradiction of encoded literal for unknown reason*)
  let rec uk_reason l1 l2 = match l1 with
      |[] -> "0"
      |h::t -> ("-" ^ string_of_int (List.assoc h l2)) ^ " " ^ (uk_reason  t l2)

  (*get the reason from set of unknown literals*)
   let rec uk_lit l1 l2 = match l1 with
      |[] -> ""
      |h::t -> (var_exp h l2) ^ "0 " ^ (uk_lit t l2)


  (*New version for testing -------------------------*)
  (*=================================================*)

  (*Compute the list of boolean constraints without and*)
  let rec f_toList e = match e with
    |And (e1, e2) -> List.append (f_toList e1) (f_toList e2)
    |_ -> [e]

  (*Compute the list of variables from a boolean constraints*)
  let rec f_listVar e = match e with
    | And (e1, e2) -> List.append (f_listVar e1) (f_listVar e2)
    | _ -> bool_vars e

  (*Compute the list of variables from a list of constraints*)
  let rec e_listVar e = match e with
    | [] -> []
    | h::t -> List.append (bool_vars h) (e_listVar t)

  (*Compute the length of a boolean constraint by number of variables in constraint*)
  let length_cons e = List.length (Util.red_list (bool_vars e))

  (*Sort the order of boolean contraints by length*)
  let compare_cons e1 e2 = 
    let l1 = length_cons e1 in
    let l2 = length_cons e2 in
    if (l1 > l2) then 1
    else if (l1 < l2) then -1
    else 0

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

  (*Divide list of constraints to small one for generating test cases*)
  let rec build_ass eList lstVar = match eList with
    |[] -> []
    |h::t -> 
  if (lstVar = []) then []
  else (
    let l1 = bool_vars h in
    if (subset_list l1 lstVar) then h::(build_ass t (sub_list lstVar l1))
    else build_ass t lstVar
  )
  
  (*group related elements to each group*)
  let rec rel e lstAss lstVar = match lstAss with
    |[] -> [e]
    |h::t -> 
  let l = sub_list (bool_vars h) (bool_vars e) in
  if (subset_list l lstVar) then 
    h:: rel e t (sub_list lstVar l)
  else
    rel e t lstVar
   
  let rec rel_all l1 l2 lstVar = match l1 with
    |[] -> []
    |h::t ->
  let tmp = rel h l2 lstVar in 
  let l = Util.red_list (e_listVar tmp) in
  let newlist = sub_list lstVar l in
  tmp:: (rel_all t l2 newlist)  

    (*Group neccessary constraints for testing*)
  let group_cons listAss listVar = 
    let l1 = build_ass listAss listVar in
    let new_varlist = sub_list listVar (Util.red_list (e_listVar l1)) in
    let new_asslist = sub_list listAss l1 in
    rel_all l1 new_asslist new_varlist

  (*get the list of variable from remaining unbinding test cases*)
  let rec vars_unbind l = match l with
    |[] -> []
    |h::t -> 
  let (x, c) = List.hd h in x:: vars_unbind t

  (*get the list of variable from a test case*)
  let rec vars_in_tc tc = match tc with
    |[] -> []
    |(x,v) ::t -> x:: vars_in_tc t   

  (*get test case for a variable *)
  let rec get_index x ass = match ass with
    |[] -> []
    |(key, l)::t -> if (key = x) then [(key, l)]
                    else get_index x t
  (*let rec get_index x ass = match ass with
    |[] -> []
    |h::t -> 
  let (key, c) = List.hd h in
  if (x = key) then h
  else get_index x t*)

  let rec get_index_all r varlist ass = match varlist with
    |[]-> r
    |h::t -> get_index_all (List.append (get_index h ass) r)  t ass

  (*get satisfied test cases from set of test cases for a constraint
  let rec sat_find e ass = match ass with
    |[] -> []
    |h::t -> 
  let res = checkValue e h in
  if (res = 1) then h:: sat_find e t
        else sat_find e t *)
  
  (*New version of sat_find: tail recursion*)
  let rec sat_find r e vass ass = match ass with
    |[] -> r
    |h::t -> 
        let assign = List.combine vass h in
  let res = checkValue e assign in
  if (res = 1) then
    sat_find (h::r) e vass t
        else 
    sat_find r e vass t 

  (*New version of sat_find1: tail recursion*)
  let rec sat_find1 (e1, d1) r e vass ass =
    match ass with
    |[] -> (e1, d1, r)
    |h::t -> 
      let assign = List.combine vass h in
      let (res, d) = checkValue1 e assign in
      let (e2, d2) = 
        if d < d1 then (h, d)
        else (e1, d1) in
      if (res = 1) then
        sat_find1 (e2, d2) (h::r) e vass t
      else 
        sat_find1 (e2, d2) r e vass t
  
  (*remove empty element in list*)
  let rec remove_empty r l = match l with
    | [] -> r
    | h::t -> 
  if (h != []) then remove_empty (h::r) t
  else remove_empty r t          

  (*extract information from test cases of variables*)
  let rec extract lvar ltc tc = match tc with
    |[]-> (lvar, ltc) 
    |(x, l)::t -> extract ([x]::lvar) (l::ltc) t

  (*find test cases that satisfying a constraint (clause) e *)
  let find_tc lvars oAss e uAss =
    let vars = Util.red_list (bool_vars e) in
    let tc = get_index_all [] vars uAss in (* tc contains the test cases of variables in vars *)
    let big_number = max_float in 
    if (tc != []) then (
      let (l1, l2) = extract [] [] tc in (* l1 contains the list of variables and l2 contains the list of test cases corresponding to each variable. *)
      let vass = List.hd (f4 [lvars] l1) in 
      let ass = f4 oAss l2 in
      let (e1, d1, new_ass) = sat_find1 ([], big_number) [] e vass ass in
      (e1, vass, new_ass, sub_list uAss tc)
    )
    else (
      let (e1, d1, new_ass) = sat_find1 ([], big_number) [] e lvars oAss in
      (e1, lvars, new_ass, uAss)
    )
    
  (*find test cases that satisfying list of constraints 
  let rec find_tc_all oAss lstAss ass = match lstAss with
    |[]-> (oAss, ass, [])
    |h::t -> 
      let (new_ass, uAss) = find_tc oAss h ass in
      if (new_ass == []) then 
  (new_ass, uAss, [h])
      else
  find_tc_all new_ass t uAss *)

  (*Binding test cases for each group in list  
  let rec group_find lstGroup lstAss = match lstGroup with
     | [] -> []
     | h::t ->  
   let l = List.sort compare_cons h in
   let (aAss, nAss, r) = find_tc_all [] l lstAss in
   (aAss, nAss, r) :: group_find t nAss *)

  let rec eval_group ass rm rs tc = match tc with
     |[] -> (ass, rm, rs)
     |(aAss, nAss, r)::t -> eval_group (aAss::ass) nAss (List.append rs r) t    

  let rec is_unsat lstTest = match lstTest with
     |[] -> false
     |h::t -> if (h=[]) then true
          else is_unsat t

  let sat_group aAss rmAss rs =
     let bCheck = is_unsat aAss in
     (*There are constraints in group not satisfied*)
     if bCheck then ([], rs)
     else (*Generate remaint test cases*)
     (
       (*Generate test cases for already assign tc*)
       let lstTest = f4 (f44 [] aAss) rmAss in
       (lstTest, [])
     )

  let rec list_group r l = match l with
      |[] -> r
      |h::t -> list_group (List.append r h) t
 
  let remain_cons gr uk = 
      (*make group to list of constraints*)
      let lstGroup = list_group [] gr in
      sub_list uk lstGroup

  let rec is_sat e ass = match e with
      |[] -> 1
      |h::t -> 
    let res = checkValue h ass in
    if (res = 1) then is_sat t ass
    else -1

  let rec search_sat e ass = match ass with
      |[] -> (-1, [])
      |h::t -> 
    let res = is_sat e h in
    if (res = 1) then (1, h)
    else search_sat e t

  let rec unsat_test e ass = match ass with
        |[] -> -1
        |[a] -> checkValue e a
  |h::t -> let r = checkValue e h in
    if (r = -1) then 
      unsat_test e t
    else r

  let rec list_unsat l e ass= match e with
        |[] -> l
  |h::t->let res = unsat_test h ass in    
        if (res = -1) then list_unsat (h::l) t ass 
        else list_unsat l t ass

  (*The main function for testing*)
  (*
  let search_tc lstAss assIntv =
      (*generate test case: can be generate more test cases here*)    
      Random.self_init();
      let lstTc = genAss_test [] gen_test22 assIntv in
      (*let lst2 = genAss_test gen_test2 new_ass in*)
      
      (*compute the list of variables in *)
      let vars_list = Util.red_list (e_listVar lstAss) in
      (*group related constraint*)
      let gr = group_cons lstAss vars_list in

      (*find satisfied test cases for each group constraint*)
      let tc = group_find gr lstTc in
      
      (*decompose information in group*)
      let (a1, a2, a3) = eval_group [] [] [] tc in 
      
      (*generate test cases in group and remain test cases*)
      let (lstTest, rs) = sat_group a1 a2 a3 in
      
      if (lstTest == []) then (-1, rs, [])
      else ( (*Execute test for remain constraints except group constraints*)
  let rmCons = remain_cons gr lstAss in
  let (sat, satAss) = search_sat rmCons lstTest in
  if (sat = -1) then (  
    (*
      let lst = genAss_test gen_test2 assIntv in
    let tc = decomp lst in
    let cl_unknown = list_unsat [] lstAss tc in
    *)

    (* Find clauses which are not satisfied with any Test cases*)
    (*
    let fass = gen_ftest [] [] assIntv in
    let cl_unknown = list_unsat [] rmCons (List.append fass lstTest) in
    (sat, cl_unknown, []) *)
    
    (*Applied for Dynamic interval decomposition*)
    (sat, [], [])
  )
  else (sat, [], satAss)
      )
  *)
  (*===== New version(2) of computing SAT test cases ====*)
  (* find the clause which has many variables join with lstVar*)
    let rec find_join_clause  e lstVar lstConstr = match lstConstr with
      |[] -> e    
      |h::t -> 
    let l = Util.red_list (bool_vars h) in
    let le = Util.red_list (bool_vars e) in
    if (List.length (sub_list l lstVar) < List.length (sub_list le lstVar)) then
      find_join_clause h lstVar t
    else
      find_join_clause e lstVar t


  (* =========================== START first_search ============================== *) 
  (*| recursively looking for a SAT test cases from the first list of constraints |
    | . lvars:                             |
    | . oAss:                 |
    | . uAss: List of test cases for each variable. Each one has two values.    |  
    | . first_list: list of tested clauses             |
    |_____________________________________________________________________________|*)
  let rec first_search lvars oAss uAss first_list = 
    match first_list with
    |[] -> ([], lvars, oAss, uAss, [])
    |_ -> (
      let first_cons = List.hd (List.sort compare_cons first_list) in (* sort the list of tested clauses by their lengths *)
      let (tc, new_lvars, new_Ass, new_uAss) = find_tc lvars oAss first_cons uAss in
      if (new_Ass = []) then (tc, new_lvars, [], new_uAss, [first_cons])
      else (
        let new_uk_cl = sub_list first_list [first_cons] in
        first_search new_lvars new_Ass new_uAss new_uk_cl
      )
    )
  (* =========================== END first_search ================================= *)


  (*recursively looking for a SAT test cases*)
  let rec search_inside lvars oAss uAss uk_cl = 
    match uk_cl with
    |[] -> ([], lvars, oAss, [])
    |_ -> (
  (*
  let lstVar = (
    if (oAss == []) then []
    else vars_in_tc (List.hd oAss)
  ) in *)
  (*let jClause = find_join_clause (List.hd uk_cl) lvars (List.tl uk_cl) in*)
      let jClause = find_join_clause (List.hd uk_cl) lvars (sub_list uk_cl [List.hd uk_cl]) in
      let (tc, new_lvars, new_Ass, new_uAss) = find_tc lvars oAss jClause uAss in
      if (new_Ass = []) then (tc, new_lvars, [], [jClause])
      else (
        let new_uk_cl = sub_list uk_cl [jClause] in
        search_inside new_lvars new_Ass new_uAss new_uk_cl
      )
    )
  
  (*compute all sub constraints of e = subset variables of e from list l*)
  let rec sub_constraints e l = match l with
    |[] -> []
    |h::t ->
      let e_vars = Util.red_list (bool_vars e) in
      let h_vars = Util.red_list (bool_vars h) in
      if (subset_list h_vars e_vars) then
        h:: sub_constraints e t
      else
        sub_constraints e t

  (*Choose a constraint with maximal dependencies*)
  let rec get_dep n lst_vars lst = match lst with
  |[] -> n
  |h::t -> 
    let l1 = Util.red_list (bool_vars h) in
    if (subset_list lst_vars l1) then
      get_dep (n+1) lst_vars t
    else
      get_dep n lst_vars t


  (* ============================ START get_maxdep ========================= *)
  (* The function for finding the clause in which the number of other clauses which
   contain all of its variables are maximized. 
   . res: the current found result
   . maxdep: the current maximal value of number of other clauses which contain all the variables of res
   . olst: the list of all clauses
   . lst: the remaining list of clauses to be considered
  *)
  let rec get_maxdep res maxdep olst lst = match lst with
  |[] -> res
  |h::t ->
    let lvars = Util.red_list (bool_vars h) in (* get the list of variables in h *)
    let dep = get_dep 0 lvars olst in (* get the number of clauses containing all the variables in lvars *)
    if (dep > maxdep) then
      get_maxdep h dep olst t
    else get_maxdep res maxdep olst t
  (* ============================ END get_maxdep ========================= *)


  (* ============================ START search_tc2 ========================= *)
  (*search a satisfiable test case*)
  let search_tc2 uk_cl assIntv strTestUS esl =
    (*GENERATE TEST CASE: CAN BE GENERATE MORE TEST CASES HERE*)    
    Random.self_init();
    let lstTc = genAss_test [] gen_test21 assIntv esl in (* list of test cases, each variable has two tested values. *)
    (*let lstTc = genAss_test [] gen_test_tick assIntv esl in*)
    (*let lstTc = genAss_test [] gen_test_semi3 assIntv in*)
    (*let lst2 = genAss_test gen_test2 new_ass in*)  
   
    let first_cons = (* first_cons is the to-be-tested clause *)
      if (strTestUS = "") then (
        (*List.hd (List.sort compare_cons uk_cl) *)
        let new_uk_cl = List.sort compare_cons uk_cl in
        let e = List.hd new_uk_cl in (* The shortest unknown clause. *)
        let remains = List.tl new_uk_cl in (* The remaining clauses *)
        let lst_vars = Util.red_list (bool_vars e) in (* The list of all variables of the shortest clause *)
        let maxdep = get_dep 1 lst_vars remains in (* Get the number of clauses containing all the variables of the shortest clause *)
        get_maxdep e maxdep remains remains (* get the the first clause to be tested, based on the number of other clauses which contain all the variables of the chosen clause. 
            That number is maximal for the chosen clause. *)
      )  
      else (    
        let fTestUS = read ("(assert " ^ strTestUS ^ ")") in
        let cTestUS = sub_ass fTestUS in
          if List.mem cTestUS uk_cl then
            cTestUS
          else
            List.hd (List.sort compare_cons uk_cl) (* Get the shortest unknow clause *) 
      )
    in
      (*collect all constraint related to first_cons*)
      let sub = sub_constraints first_cons uk_cl in (* sub contains all the clauses whose variables are all also variables of first_cons *)
      let first_list = List.append sub [first_cons] in (* Because of common variables, we should test all of them *)
      (*print_endline "Start first search";
      flush stdout;*)
      let (tc, fvars, first_tc, uAss, reason) = first_search [] [] lstTc first_list in
      (*print_endline "End first search";
      flush stdout;*)
      if (first_tc = []) then
        (List.combine fvars tc, -1, reason, [])
      else (
        let new_uk_cl = sub_list uk_cl first_list in
        (*print_endline "Start search inside";
        flush stdout;*)
        let (tc1, lv, satTC, reason) = search_inside fvars first_tc uAss new_uk_cl in
        (*print_endline "End search inside";
        flush stdout;*)
        if satTC = [] then 
          (List.combine lv tc1, -1, reason, [])
        else 
          ([], 1, reason, List.combine lv (List.hd satTC))
      )
  (* ============================ END search_tc2 ========================= *)

      (*
      let (fvars, first_tc, uAss) = find_tc [] [] first_cons lstTc in
      if (first_tc = []) then
  (-1, [first_cons], [])
      else (
  let new_uk_cl = sub_list uk_cl [first_cons] in
  let (lv, satTC, reason) = search_inside fvars first_tc uAss new_uk_cl in
  if satTC = [] then 
    (-1, reason, [])
  else 
    (1, reason, List.combine lv (List.hd satTC))
      )
      *)
      
      (*      
      let satTC = search_inside [] lstTc uk_cl in
      if (satTC == []) then 
        (-1, [], [])
      else 
  (1, [], List.hd satTC) *)          
      
    
  (*===== End new version(2) of SAT test cases ====*)
    
  (*Do testing for all unknown clauses from interval assignment of variables
  let evalTest assIntv uk_cl lstVarID strTestUS =
    (*compute only neccesary variables*)
    (*let list_vars = lstVars uk_cl in*)
    (*let new_ass = red_ass assIntv list_vars in*)

    (*test with 2 fixed test cases*)
    (*
    let fass = gen_ftest [] [] new_ass in
    let (r, a) = search_sat uk_cl fass in
    if (r = 1) then (
      let str = (logTestCase a)^(logValue_all uk_cl a) in
      (r, "", a)
    )
    else *) 
    (  (*otherwise using random test cases*)     
      (*let sUk = uk_reason list_vars lstVarID in    *)
      let (tc, res, reason, ass) = search_tc2 uk_cl assIntv strTestUS in
      if (res = 1) then (
  (*let str = (logTestCase ass)^(logValue_all uk_cl ass) in*)
  (res, reason, ass)
       )
      else (
  (*
  let s = uk_lit reason lstVarID in
  if (s <> "") then (res, s, [])
  else (res, sUk, []) *)

        (*Process for dynamic interval decomposition here*)
  (res, reason, [])
       ) 
    )
  *)
     
  let logTest assIntv ass all_cl uk_cl ia = 
    let list_vars = lstVars uk_cl in
    let new_ass = red_ass assIntv list_vars in
    let rm_ass = sub_list assIntv new_ass in
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
      s1^(logTestCase ass)^"\n"^stmp^"\n"^ (logValue_all uk_cl ass)
    (*(logTestCase ass)^(logValue_all uk_cl ass)*)

  (*=============== ====================================================================*)
  (*main caml function*)
  (*doTest will combine to check satisfiable solution by Interval arithmetic and Testing
  let doTest sIntv sAss strCheck ia =
      let lstCheck = toIntList strCheck in
      let eIntv = sub_intv (read sIntv) in 
      let eAss = sub_ass (read sAss) in      
      
      (* lstLit stored the list of literals from interval constraints*)
      let lstLit = f_toLit eIntv in

      (* iVar stored the number of variables from interval constraints*)
      let iVar = List.length lstLit in
  
      let lst = idx iVar in 
      (*Encoded each literal with an ID: Integer number*)    
      let lstIDLit = List.combine lst lstLit in
      (*let lstLitID = List.combine lstLit lst in*)

      (*function for compute the list of pairs of variable and ID from the list of ID of interval constraints*)
      let rec var_list = function
  | [] -> []
  | h::t ->
      if (h>=1) &&(h<=iVar) then (
        let lit = List.assoc h lstIDLit in
        (getVar lit, h):: (var_list t) )
      else
        var_list t in
        
      (*checkVarID stored the list of pairs of variable and ID*)
      let checkVarID = var_list lstCheck in

      let rec ci_ass = function
  | [] -> []
  | h::t -> 
      if (h>=1) && (h<=iVar) then 
        List.append (e_toIntv (List.assoc h lstIDLit)) (ci_ass t) 
      else 
        (ci_ass t) in

      (*assIntv is an assignment of classical interval*)
      let assIntv = ci_ass lstCheck in

      (*evaluate all constraints in assertion constraints and
  return Satisfiable result, unsat reason, unknown reason, unkown clauses*)
      let rec eval_all e = match e with
        |And (e1, e2) -> 
    let (res1, us1, uk_cl1) = eval_all e1 in
    let (res2, us2, uk_cl2) = eval_all e2 in
      if (res1 = -1) then   
      (*(res1, us1^us2, uk1^uk2, List.append uk_cl1 uk_cl2)*)
      (res1, us1^us2, [])
    else if (res1 = 1) then   
      (res2, us1^us2, List.append uk_cl1 uk_cl2)
    else  (*res = 0, unknown*)
    (                 
      if (res2 = -1) then
            (*(res2, us1^us2, uk1^uk2, List.append uk_cl1 uk_cl2)*)
            (res2, us1^us2, [])
      else 
            (res1, us1^us2, List.append uk_cl1 uk_cl2)    
    )
  | _ -> 
      let res = checkSat e ia assIntv in
            if (res = 1) then 
        (res, "", [])
         else (
              let str = (var_exp e checkVarID)^"0 " in 
      if (res = -1) then 
        (res, str, [])      
      else (*for res = 0, unknown*)
        (res, "", [e])
      )
      in (*end eval_all*)
      
      let (res, us, uk_cl) = eval_all eAss in

      if (res = -1) then (*if existing unsat clause*)
  (res, us, "", "")
      else if (res = 1) then (*if all clauses are satisfiable*)
      (
  let rec allLog e = match e with
    |And (e1, e2) -> (allLog e1) ^ (allLog e2)  
    | _ -> (logSat e ia assIntv) ^ "\n" in  
  (res, "", "", (allLog eAss))
      )
      else (*if unknown, testing will be implemented here*)
      (
       if (uk_cl == []) then
   (res, us, "", "")
       else (
         let (sTest, sUk, a) = evalTest assIntv uk_cl checkVarID in
   if (sTest = 1) then (
        let all_cl = f_toList eAss in
     let sLog = logTest assIntv a all_cl uk_cl ia in
     (sTest, "", "", sLog)
   )
   else
   (     
      (res, "", sUk, "")    
   )
  )
      )
  *)
  (*=====================================================================================*)    
  (* getNumCons return the number of boolean constraints*)    
  let getNumCons sAss =
      let eAss = sub_ass (read sAss) in        

     (* lstCons stored the list of boolean constraints without and*)
      let lstCons = f_toList eAss in
      List.length lstCons

  (*===================================================================================*) 
   let getLU lit = match lit with
    |In (x, l, u) -> (l,u)
    |Or (cl1, cl2) -> (0.0, 0.0)

   let isChildLit lit1 lit2 = (*lit2 is a child literal of lit1*)
     if (getVar lit1 = getVar lit2) then (
       let (l1, u1) = getLU lit1 in
       let (l2, u2) = getLU lit2 in
       if (l2 >= l1)&&(u2 <= u1) then
   true
       else
   false
      )
     else false
  
  (*
  let rec remLstCheck l e = match l with
    |[] -> [e]
    |h::t -> 
  if (getVar h = getVar e) then (
    if (isChildLit h e) then 
            e::t
    else       
            h:: (remLstCheck t e)
        )
  else
    h::(remLstCheck t e)
  
  (*Remove unnecessary assign intervals *)  
  let rec redLstCheck l1 l2 = match l2 with
    |[]-> l1
    |h::t -> redLstCheck (remLstCheck l1 h) t
  
  *)
  let rec remLstCheck l e lstIDLit= match l with
    |[] -> [e]
    |h::t -> 
  let lit_e = List.assoc e lstIDLit in
  let lit_h = List.assoc h lstIDLit in
  if (getVar lit_h = getVar lit_e) then (
    if (isChildLit lit_h lit_e) then 
      e::t
    else 
      h::t
  )
  else
    h::(remLstCheck t e lstIDLit)

  (*Remove unnecessary assign intervals*)   
  let rec redLstCheck l1 l2 lstIDLit= match l2 with
    |[]-> l1
    |h::t -> redLstCheck (remLstCheck l1 h lstIDLit) t lstIDLit 

  (*Check whether all assign intervals get to the bound for decomposition hi - low < esl *)
  let rec isReachBound ass esl = match ass with
    |[] -> true
    |(x, ci) ::t -> 
  if (ci#h > (round_off (esl +. ci#l) 5)) then false
  else isReachBound t esl
  
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
  (*
  Wrong on the code for encoded literals
  if (isReachBound [h] esl) then
    (s1, s2)
  else ( *)
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
        else -.max_float
      else 
        if upperBound = infinity then max_float 
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
  let dynamicDecom_pos assIntv dIntv lstVarID iVar uk_cl esl =
    let cl_TestUS = leftExp (List.hd uk_cl) in

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
      (*print_endline ("Intervals: " ^ (intervals_toString assIntv));*)
      flush stdout;*)
      let s = uk_lit uk_cl lstVarID in
      (dIntv, s, "", false)
    )
    else (*Continue decomposition*)
    (
      let subPos = sub_list red_pos red_neg in (* remove the common variables with red_neg from red_pos *)
      let subNeg = sub_list red_neg red_pos in(* remove the common variables with red_pos from red_neg *)
      if (subPos = []) && (subNeg =[]) then ( (* red_pos and red_neg are exactly the same *)
        let (sInterval, sLearn, bump_vars) = ass_decomp_pn red_pos lstVarID (iVar + 1) esl in (* sInterval is the decomposed intervals of variables in prefix form
                               sLearn: is the list of new literals to be added into minisat
                               bump_vars: list of codes (ivar) *)
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
        let (sInterval_pos, sLearn_pos, bump_pos) = ass_decomp_pn subPos lstVarID (iVar + 1) esl in
        let iPos = List.length subPos in
        let (sInterval_neg, sLearn_neg, bump_neg) = ass_decomp_pn subNeg lstVarID (iVar + iPos * 2 + 1) esl in    
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


  (* ====================== START dynamicDecom_new ======================== *)
  (* Decomposing intervals based on Newton's method *)
  let rec dynamicDecom_new assIntv dIntv lstVarID iVar uk_cl esl = 
    let sorted_uk_cl = List.sort compare_cons uk_cl in (* sort the unknow clauses by their length *) 
      match sorted_uk_cl with
      | [] -> ("", "", "", false)
      | h::t -> (
        let (new_dIntv, minisat_code, bump_vars, was_decomposed) =
        let (tmp_dIntv, tmp_minisat_code, tmp_bump_vars, tmp_was_decomposed) = 
          decompose_new_clause h assIntv dIntv lstVarID iVar esl
        in
          if tmp_was_decomposed then (tmp_dIntv, tmp_minisat_code, tmp_bump_vars, tmp_was_decomposed)
          else dynamicDecom_new assIntv dIntv lstVarID iVar t esl
        in 
          if was_decomposed then (
            (new_dIntv, minisat_code, bump_vars, was_decomposed) )
          else (
            dynamicDecom_pos assIntv dIntv lstVarID iVar uk_cl esl )
      )
  (* ====================== END dynamicDecom_new ======================== *)

  (*Decomposition based on a test case*)
  let dynamicDecom_test assIntv dIntv lstVarID iVar uk_cl esl t = 
    let cl_TestUS = leftExp (List.hd uk_cl) in

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
        let (sInterval, sLearn, bump_vars, code) = ass_decomp_test_both red_pos lstVarID iVar esl tc in
          if (dIntv <> "") then
            ("(ic "^dIntv ^" "^sInterval^")", sLearn, bump_vars, true)
          else
            (sInterval, sLearn, bump_vars, true)
      )
      else (  
        let (sInterval_pos, sLearn_pos, bump_pos, code_pos) = ass_decomp_test_pos subPos lstVarID iVar esl tc in
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
  
  (*======= end unbalance decomposition =======*)   

  (*Binary balance decomposition on intervals*)
  let dynamicDecom assIntv dIntv lstVarID iVar uk_cl esl = 
    let list_vars = lstVars uk_cl in
    let new_ass = red_ass assIntv list_vars in
    (*let bContinue = isReachBound new_ass esl in
    if bContinue then (*Stop decomposition*) *)
    let red_ass = decomp_reduce new_ass esl in
    if (red_ass = []) then (*Stop decomposition*) 
    (
      let s = uk_lit uk_cl lstVarID in
      (dIntv, s, false)
    )
    else (*Continue decomposition*)
    (
      let (sInterval, sLearn) = ass_decomp red_ass lstVarID (iVar + 1) esl in
      if (dIntv <> "") then
  ("(ic "^dIntv ^" "^sInterval^")", sLearn, true)
      else
  (sInterval, sLearn, true)
    )

   let rec list_IDtoLit l lstIDLit = match l with
     |[] -> []
     |h::t -> (List.assoc h lstIDLit):: list_IDtoLit t lstIDLit

   let rec list_LittoID l lstLitID = match l with
     |[] -> []
     |h::t -> (List.assoc h lstLitID):: list_LittoID t lstLitID

   let rec ftemp = function
     |[] -> ""
     |(x, id)::t -> string_of_int id ^ ": " ^ x ^ " " ^ (ftemp t)

   let rec toString_list = function
     |[] -> ""
     |h::t -> string_of_int h ^ " " ^ (toString_list t)   

    (*evaluate all constraints in assertion constraints and
      return Satisfiable result, unsat reason, unknown reason, unkown clauses*)
     (*
   let rec eval_all e = match e with
        |And (e1, e2) -> 
    let (res1, us1, uk_cl1) = eval_all e1 in
    let (res2, us2, uk_cl2) = eval_all e2 in
      if (res1 = -1) then   
      (*(res1, us1^us2, uk1^uk2, List.append uk_cl1 uk_cl2)*)
      (res1, us1^us2, [])
    else if (res1 = 1) then   
      (res2, us1^us2, List.append uk_cl1 uk_cl2)
    else  (*res1 = 0, unknown*)
    (                 
      if (res2 = -1) then
            (*(res2, us1^us2, uk1^uk2, List.append uk_cl1 uk_cl2)*)
            (res2, us1^us2, [])
      else 
            (res1, us1^us2, List.append uk_cl1 uk_cl2)    
    )
  | _ -> 
      let res = checkSat e ia assIntv in
            if (res = 1) then 
        (res, "", [])
         else (
              let str = (var_exp e checkVarID)^"0 " in 
      if (res = -1) then 
        (res, str, [])      
      else (*for res = 0, unknown*)
        (res, "", [e])
      )
      in (*end eval_all*) *)

   (*Re-write another version of eval_all by list of sort constraints *)
(*
   let rec eval_all e ia assIntv checkVarID= match e with
     |[] -> (1, "", [])
     |h::t -> 
   let (res2, us2, uk_cl2) = eval_all t ia assIntv checkVarID in
   let res1 = checkSat h ia assIntv in
   if (res1 = 1) then 
     (res2, us2, uk_cl2)
   else if (res1 = -1) then (
         let str = (var_exp h checkVarID)^"0 " in 
     (res1, str ^ us2, [])
   )
   else ( (*res1 = 0*)     
     if (res2 = -1) then
       (res2, us2, [])
     else
       (res1, us2, h::uk_cl2)
    )
 *)       
 (*
   let rec eval_all res us uk_cl e ia assIntv checkVarID= match e with
     |[] -> (res, us, uk_cl)
     |h::t -> 
   let res1 = checkSat h ia assIntv in
   if (res1 = 1) then 
     eval_all res us uk_cl t ia assIntv checkVarID
   else if (res1 = -1) then (
         let str = (var_exp h checkVarID)^"0 " in 
     eval_all (-1) (str^us) [] t ia assIntv checkVarID
   )
   else ( (*res1 = 0*)     
     if (res = -1) then
       eval_all (-1) us [] t ia assIntv checkVarID
     else
       eval_all 0 us (h::uk_cl) t ia assIntv checkVarID
    )
   *)
 
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
       (Gr (l, Real 0.)):: (merge_list fpos t)

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
    get_minimal res t

  (*Assume that all constraints are formed as f > 0.*)
  let get_unsatcore f ia assIntv =
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

  let rec var_exp_list lst checkVarID = match lst with
  |[] -> ""
  |h::t -> 
     let s = var_exp h checkVarID in
     if (s = "") then  
       var_exp_list t checkVarID
     else
       s ^ "0 " ^ (var_exp_list t checkVarID)
   (*============= End UNSAT Cores Analysis ===========*)
   
 
  (*Rewrite eval_all for UNSAT cores computations*)
  let rec eval_all res us uk_cl e ia assIntv originalIntv checkVarID iaTime usTime remainingTime =
    match e with
     |[] -> (res, us, uk_cl, iaTime, usTime)
     |h::t -> 
        let startTime = Sys.time() in
        (*print_string "Start check sat: ";
        print_string (bool_expr_to_infix_string h ^ ": ");*)
        (* print_varsSet (get_vars_set_boolExp h); (* print_varsSet is in Variable.ml and get_vars_set_boolExp is in ast.ml *)
        flush stdout;*)
        (*let res1 = checkSat h ia assIntv in*)
        let (res1, varsSen) = check_sat_af_two_ci h assIntv in
        (*print_endline "End check sat";
        flush stdout;*)
        let iaTime = iaTime +. Sys.time() -. startTime in
        if (res1 = 1) then 
          eval_all res us uk_cl t ia assIntv originalIntv checkVarID iaTime usTime (remainingTime -. Sys.time() +. startTime)
        else if (res1 = -1) then (
          (*let str = (var_exp h checkVarID)^"0 " in *)
          (*let lstUC = get_unsatcore h 0 assIntv in *)
          let startUSCoreTime = Sys.time() in
          (*print_string "Start UNSAT core: ";
          print_endline (bool_expr_to_infix_string h);
          flush stdout;*)
          (*let lstUC = get_unsatcore h ia assIntv in*)
          let unsatCoreVars = get_unsatcore_vars h assIntv originalIntv checkVarID ((remainingTime -. Sys.time() +. startTime) (*/. 4.*)) in (* get_unsatcore_vars is defined in Unsat_core.ml *)
          (* bool_expr_to_infix_string and bool_expr_list_to_infix_string is defined in ast.ml *)
          (* print_endline ("End UNSAT core of " ^ (bool_expr_to_infix_string h) ^ " is " ^ (bool_expr_list_to_infix_string lstUC)); 
          flush stdout; *)
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
          eval_all (-1) (unsatCoreVars^"0 "^us) [] t ia assIntv originalIntv checkVarID iaTime usTime (remainingTime -. Sys.time() +. startTime)
        )
        else ( (*res1 = 0*)     
          if (res = -1) then
            eval_all (-1) us [] t ia assIntv originalIntv checkVarID iaTime usTime (remainingTime -. Sys.time() +. startTime)
          else
            eval_all 0 us ((h,varsSen)::uk_cl) t ia assIntv originalIntv checkVarID iaTime usTime (remainingTime -. Sys.time() +. startTime)
        )

  (*function for compute the list of pairs of variable and ID from the list of ID of interval constraints*)
  let rec var_list lstCheck iVar lstIDLit = match lstCheck with
    | [] -> []
    | h::t ->
      if (h>=1) &&(h<=iVar) then (
        let lit = List.assoc h lstIDLit in
        (getVar lit, h):: (var_list t iVar lstIDLit) )
      else
        var_list t iVar lstIDLit

  (*Just compute positive elements from olstCheck*)
  let rec getPositive l iVar = match l with
    |[] -> []
    |h::t -> 
      if (h>=1)&&(h<=iVar) then
        h:: getPositive t iVar
      else
        getPositive t iVar

  let rec ci_ass lstCheck iVar lstIDLit = match lstCheck with
    | [] -> []
    | h::t -> 
      if (h>=1) && (h<=iVar) then 
        List.append (e_toIntv (List.assoc h lstIDLit)) (ci_ass t iVar lstIDLit) 
      else     
        ci_ass t iVar lstIDLit              

  let rec allLog e ia assIntv = match e with
    |[] -> ""
    |h::t -> (logSat h ia assIntv) ^ "\n" ^ (allLog t ia assIntv)  

  (*=========================== START DYNTEST =======================================*)  
  (*dynTest: Interval arithmetic, Testing and Dynamic interval decomposition*)
  let dynTest sIntv dIntv sAss strCheck ia esl strTestUS iaTime testingTime usTime parsingTime decompositionTime remainingTime =
		(*print_endline dIntv;
		flush stdout;*)    
		let startTime = Sys.time() in
    let olstCheck = toIntList strCheck in 
    let eIntv = sub_intv (read sIntv) in
    let ass = sub_ass (read sAss) in      
      
    (*sort the list of constraints based on their length*)
    (*let eAss = List.rev (List.sort compare_cons (f_toList ass)) in *)
    let eAss = f_toList ass in
    (*print_endline(bool_expr_list_to_infix_string eAss); (* In ast.ml *)
    flush stdout;*)

    (* lstLit stored the list of literals from interval constraints *)
    let lstLit = f_toLit eIntv in
		(*print_endline "f_toLit";
		flush stdout;*)
    (* dlstLit stored the list of literals from dynamic interval constraints*)
    let dlstLit = 
      if (dIntv <> "") then (
        let edIntv = sub_intv (read dIntv) in
				(*print_endline "after reading decomposed intervals";
				flush stdout;*)
        f_toLit edIntv
      )
      else [] 
    in
    (*print_endline "End parsing";
    flush stdout;*)

    (* iVar stores the current number of intervals *)
    let iVar = (List.length lstLit) + (List.length dlstLit) in
    (*print_endline ("Vars: " ^ (string_of_int iVar));
    flush stdout;*)
  
    let lst = idx iVar in
    (*Encoded each literal with an ID: Integer number*)    
    let lstIDLit = List.combine lst (List.append lstLit dlstLit) in
    (*let lstLitID = List.combine (List.append lstLit dlstLit) lst in*)
    (*let lstLitID = List.combine lstLit lst in*)

    let positiveCheck = getPositive olstCheck iVar in

    (*Reduce lstCheck for Dynamic interval decomposition*)
    (*let l1 = list_IDtoLit positiveCheck lstIDLit in*)
    let lstCheck = redLstCheck [] positiveCheck lstIDLit in
    (*let lstCheck = list_LittoID l2 lstLitID in*)
        
    (*checkVarID stored the list of pairs of variable and ID*)
    let checkVarID = var_list lstCheck iVar lstIDLit in      
    let tmp = (ftemp checkVarID) ^ ". lstCheck = " ^ (toString_list lstCheck) in

    (*assIntv is an assignment of classical interval*)
    let assIntv = ci_ass lstCheck iVar lstIDLit in
    (*print_endline (string_of_intervals assIntv); (* string_of_intervals is defined in Assignments.ml *)
    flush stdout;*)
    let originalIntv = List.flatten (List.map e_toIntv lstLit) in
    (*print_endline (intervals_toString originalIntv); (* intervals_toString is defined in Assignments.ml *)
    flush stdout;*)
    let parsingTime = parsingTime +. Sys.time() -. startTime in        
    (*print_endline "Start IA";*)
    let (res, us, uk_cl, iaTime, usTime) = eval_all 1 "" [] eAss ia assIntv originalIntv checkVarID iaTime usTime (remainingTime -. Sys.time() +. startTime) in
      (*print_endline "EndIA";
      flush stdout;*)
      if (res = -1) then (*if existing unsat clause*) (
        (res, us, "", "", dIntv, "", "", tmp, "", iaTime, testingTime, usTime, parsingTime, decompositionTime) 
      )
      else if (res = 1) then (*if all clauses are satisfiable*)
        (res, "", "", (allLog eAss ia assIntv), dIntv, "", "", tmp, "", iaTime, testingTime, usTime, parsingTime, decompositionTime)      
      else (*if unknown, testing will be implemented here*)(
        let uk_cl = List.rev uk_cl in (* reverse the list so that the apis are sorted based on variables dependency *)
        (*print_endline(bool_expr_list_to_infix_string uk_cl); (* bool_expr_list_to_infix_string is defined in ast.ml *)
        flush stdout;*)
        if (uk_cl = []) then (*This case will never happen*)
          (res, us, "", "", dIntv, "", "", tmp, "", iaTime, testingTime, usTime, parsingTime, decompositionTime)
        else (
          (*print_endline "Start Testing";
          flush stdout;*)
          let startTestingTime = Sys.time() in
          let (tc, sTest, clTest_US, a) = test uk_cl assIntv strTestUS (remainingTime -. Sys.time() +. startTime) in
          let (uk_cl, _) = List.split uk_cl in
          (*print_endline ("SAT: " ^ assignments_toString tc);*)
          (*let (sTest, clTest_US, a) = evalTest assIntv uk_cl checkVarID strTestUS in*)
          (*let (tc, sTest, clTest_US, a) =  search_tc2 uk_cl assIntv strTestUS esl in *)
          (*print_endline "End Testing";
					print_endline (string_of_int sTest);
          flush stdout;*)
          let testingTime = testingTime +. Sys.time() -. startTestingTime in
             if (sTest = 1) then (
               let sLog = logTest assIntv a eAss uk_cl ia in
               let assignmentsString = assignments_toString a in
               let uk_cl_string = contraints_list_toString uk_cl in
               (sTest, assignmentsString , uk_cl_string, sLog, dIntv, "", "", tmp, 
                    contraints_list_toString (sub_list eAss uk_cl) ^ " ; " ^ string_of_intervals assIntv
                    , iaTime, testingTime, usTime, parsingTime, decompositionTime)
             )
             else
             (
               let startDecompositionTime = Sys.time() in
               (* If the uk_cl are equalities, then we implement some tricks for solving them. *)
               let isEqualitiesSAT = 
                 if (is_all_equations uk_cl) then (* is_all_equalities is defined in ast.ml *)
                   check_equalities uk_cl assIntv []
                 else false
               in
                 if isEqualitiesSAT then 
                    (1, "", "", (allLog eAss ia assIntv), dIntv, "", "", tmp, "", iaTime, testingTime, usTime, parsingTime, decompositionTime)
                 else (
                    (*Applied for Dynamic interval decomposition*)
                    (*Balance interval decomposition*)
                    (*let (sInterval, sLearn, isDecomp) = dynamicDecom assIntv dIntv checkVarID iVar clTest_US esl in*)

                    (* Unbalance interval decomposition *)
                    let decomposedExpr = 
                      if (is_equation (List.hd clTest_US)) then
                        let firstInequation = first_inequation uk_cl in
                        match firstInequation with
                        |[] -> clTest_US
                        | _ -> firstInequation
                      else clTest_US
                    in
                    (*print_endline "decomposing";
                    print_endline(bool_expr_list_to_infix_string decomposedExpr);
                    flush stdout;*)
                    let (sInterval, sLearn, bump_vars, isDecomp) = 
                      (*let (newInterval, newLearn, newBumpVars, isDecomposed) = decompose_unsat_detection (List.hd decomposedExpr) assIntv dIntv checkVarID iVar esl in*)
                      (*let (newInterval, newLearn, newBumpVars, isDecomposed) = decompose_list_unsat_detection uk_cl assIntv dIntv checkVarID iVar esl in
                      if isDecomposed then 
                        (newInterval, newLearn, newBumpVars, isDecomposed)
                      else*)
                        dynamicDecom_pos assIntv dIntv checkVarID iVar decomposedExpr esl in
										(*print_endline "after decomposed";
										flush stdout;*)
                    let decompositionTime = decompositionTime +. Sys.time() -. startDecompositionTime in
                               
                    (*decomposed interval based on test values 
                    let (sInterval, sLearn, bump_vars, isDecomp) = 
                                       dynamicDecom_test assIntv dIntv checkVarID iVar clTest_US esl tc in *)
                   
                    if (isDecomp) then (
                      (*print_endline "decomposed";
                      flush stdout;*)
                      (-2, "", sLearn, "", sInterval, toPrefix (List.hd clTest_US), bump_vars, tmp, "", iaTime, testingTime, usTime, parsingTime, decompositionTime)    
                    )
                    else
                      (res, "", sLearn, "", sInterval, toPrefix (List.hd clTest_US), bump_vars, tmp, "", iaTime, testingTime, usTime, parsingTime, decompositionTime)    
                 )
             )
        )
      )
  (* ================================= END OF DYNTEST ==========================================*) 
  
  (*Infinite bounds checking*)
  (*cl_infAssign generates a infinite interval assignment from interval constraints*)
  let rec cl_infAssign exp = match exp with
    |In (x, l, h) -> [(x, new IA.inf_interval IA.(Float 0.) IA.Pos_inf)]
    |Or (e1,e2) -> cl_infAssign e1

  (*f_infAssign generates a infinite interval assignment from interval constraints*)
  let rec f_infAssign exp = match exp with
    | Cl cl -> cl_infAssign cl
    | Ic (f1, f2) -> List.append (f_infAssign f1) (f_infAssign f2)

  (*check whether an expression is satisfiable for an expression e*)
  let inf_checkSat e assIntv = 
    let left = leftExp e in
    let right = rightExp e in
    let leftBound  = evalICI assIntv left in
    let rightBound = evalICI assIntv right in

    match e with
    |Eq (e1, e2) -> 
      if IA.ICI.(leftBound#l = rightBound#l) && 
        IA.ICI.(leftBound#l = leftBound#h) && 
        IA.ICI.(leftBound#h = rightBound#h) then 1
      else if IA.ICI.(leftBound#h < rightBound#l) ||
          IA.ICI.(leftBound#l > rightBound#h) then -1
      else 0
    |Leq(e1, e2) -> 
      if IA.ICI.(leftBound#h <= rightBound#l) then 1
      else if IA.ICI.(leftBound#l > rightBound#h) then -1
      else 0
    |Le (e1, e2) -> 
      if (leftBound#h < rightBound#l) then 1
      else if (leftBound#l >= rightBound#h) then -1
      else 0
    |Geq(e1, e2) -> 
      if IA.ICI.(leftBound#l >= rightBound#h) then 1
      else if IA.ICI.(leftBound#h < rightBound#l) then -1
      else 0
    |Gr (e1, e2) -> 
      if IA.ICI.(leftBound#l > rightBound#h) then 1
      else if IA.ICI.(leftBound#h <= rightBound#l) then -1
      else 0
    | _ -> 1     (*This case never happen*)

  let rec inf_eval_all e assIntv = match e with
    |And (e1, e2) -> 
      let res1 = inf_eval_all e1 assIntv in
      let res2 = inf_eval_all e2 assIntv in
        if (res1 = -1) then res1
      else if (res1 = 1) then res2
      else  (*res1 = 0, unknown*)
      (                 
        if (res2 = -1) then res2
        else res1
      )
    | _ -> inf_checkSat e assIntv
      (*end inf_eval_all*)

  let infCheck sIntv sAss = 
    let eIntv = sub_intv (read sIntv) in 
    let eAss = sub_ass (read sAss) in    
    let inf_ass = f_infAssign eIntv in
    let result = inf_eval_all eAss inf_ass in
    result

  (*let divide a b = a/b*)

(*========================================================================*)

(*This code is applied for convert SMT2 format to raSAT format *)

(*========================================================================*)

(*

type nil_expr = 
  | BC of smt_bool_expr
  | Nil
  | AND of nil_expr * nil_expr 

(*Get CheckSAT expression*)
let rec getAss_expr (e: Ast.ass_expr) = match e with
  | Ch (e1) -> e1
  | As (e1, e2) -> getAss_expr e2


(*List pair of based sub variables and associated expression from Let expression*)
let rec lstSubVar = function
  | As (e1, e2) -> 
      let rec getSubVar = function
  | PEq (svar, e) -> [(svar, e)]
  | Let (e1, e2) -> List.append (getSubVar e1) (getSubVar e2)
  | _ -> [] in
      List.append (getSubVar e1) (lstSubVar e2)
  | _ -> []

(*List set of boolean variable from expression*)
let rec lstBVar = function
  | As (e1, e2) -> 
      let rec getBVar = function
  | BEq (svar, e) -> [(svar, e)]
  | Let (e1, e2) -> List.append (getBVar e1) (getBVar e2)
  | _ -> [] in
      List.append (getBVar e1) (lstBVar e2)
  | _ -> []

(*Substitute sub variables for based variables*)
let rec subst_poly ass (e: Ast.smt_poly_expr) = match e with


  | Real c -> Real c

  | Var x -> Var x
  | SubVar u -> subst_poly ass (List.assoc u ass)
  | Add (e1, e2) -> Add (subst_poly ass e1, subst_poly ass e2)
  | Sub (e1, e2) -> Sub (subst_poly ass e1, subst_poly ass e2)
  | Mul (e1, e2) -> Mul (subst_poly ass e1, subst_poly ass e2)
  | Div (e1, e2) -> Div (subst_poly ass e1, subst_poly ass e2) 
  | Pow (e1, n)  -> Pow (subst_poly ass e1, n)

(*Substitute  sub bool variables for based variables*)
let rec subst_bool bass pass (e: Ast.smt_bool_expr) = match e with
  | BVar b -> subst_bool bass pass (List.assoc b bass)
  | Eq  (e1, e2) -> Eq  (subst_poly pass e1, subst_poly pass e2)
  | Le  (e1, e2) -> Le  (subst_poly pass e1, subst_poly pass e2)
  | Leq (e1, e2) -> Leq (subst_poly pass e1, subst_poly pass e2)
  | Gr  (e1, e2) -> Gr  (subst_poly pass e1, subst_poly pass e2)
  | Geq (e1, e2) -> Geq (subst_poly pass e1, subst_poly pass e2)
  | And (b1, b2) -> And (subst_bool bass pass b1, subst_bool bass pass b2)
  | Not (b1) -> Not (subst_bool bass pass b1)

let isVar (e: Ast.smt_poly_expr)= match e with
  | Real c -> true 
  | Var x -> true
  | SubVar u -> true
  | Mul (e1, e2) -> true  
  | _ -> false

let rev = function
  | "+" -> "-"
  | "-" -> "+"
  | _ -> "-"

(*Compute abs of float number
let abs_float (num: float) =
  if (num < 0.) then -.num
  else num
*)

let sign_simp sign (num: float) = match sign with
| "" -> string_of_float num
| _ ->
  if  (num < 0.) then rev sign ^ string_of_float (abs_float num)
  else sign ^ string_of_float num  

(*Represente a polynomial expression by a string*)
let rec smt_poly_toString sign  (e:Ast.smt_poly_expr) = match e with
  | Real c -> sign_simp sign c
  | Var x -> sign ^ x
  | SubVar u -> sign ^ u
  | Mul (Real 1., Var x) -> sign ^ x
  | Mul (Var x, Real 1.) -> sign ^ x
  | Mul (Real -1., Var x) -> rev sign ^ x
  | Mul (Var x, Real -1.) -> rev sign ^ x
  | Add (e1, e2) -> (smt_poly_toString sign e1) ^ (smt_poly_toString "+" e2)
  | Sub (e1, e2) -> (smt_poly_toString sign e1) ^
      (if (isVar e2) then                    
  (smt_poly_toString "-" e2)
      else
  "-("^(smt_poly_toString "" e2)^")")
  | Mul (e1, e2) -> 
      (if (isVar e1) then 
         (smt_poly_toString sign e1)
      else
         sign^"("^(smt_poly_toString "" e1)^")" )
      ^ "*" ^ 
      (if (isVar e2) then                    
  (smt_poly_toString "" e2)
      else
  "("^(smt_poly_toString "" e2)^")")
  | Div (e1, e2) -> (smt_poly_toString sign e1) ^"/"^ (smt_poly_toString "+" e2)
  | Pow (e1, n)  -> sign ^ "("^(smt_poly_toString "" e1)^")" ^ "^" ^ (string_of_int n)

(*Represent a bool expression by a string*)
let rec smt_bool_toString (e: Ast.smt_bool_expr) = match e with
  | BVar bVar -> bVar
  | Eq (e1, e2) -> (smt_poly_toString "" e1)^" = " ^ (smt_poly_toString "" e2)
  | Le (e1, e2) -> (smt_poly_toString "" e1)^" < " ^ (smt_poly_toString "" e2)
  | Leq(e1, e2) -> (smt_poly_toString "" e1)^" <= " ^(smt_poly_toString "" e2)
  | Gr (e1, e2) -> (smt_poly_toString "" e1)^" > " ^ (smt_poly_toString "" e2)
  | Geq(e1, e2) -> (smt_poly_toString "" e1)^" >= " ^(smt_poly_toString "" e2)
  | And(e1, e2) -> (smt_bool_toString  e1)^"\nand "^(smt_bool_toString  e2)
  | Not(e1) -> "!" ^ (smt_bool_toString  e1)


(*Remove 0. constant in an expression*)
let rec remove_zero (e: Ast.smt_poly_expr) = match e with
  |Var x -> Var x
  |Real c -> Real c
  |SubVar u -> SubVar u
  |Add (e1, Real 0.) -> e1
  |Add (Real 0., e1) -> e1
  |Add (e1, e2) -> Add (remove_zero e1, remove_zero e2)
  |Sub (e1, Real 0.) -> e1
  |Sub (Real 0., e1) -> Mul (Real (-1.), e1)
  |Sub (e1, e2) -> Sub (remove_zero e1, remove_zero e2)
  |Mul (e1, e2) -> Mul (remove_zero e1, remove_zero e2)
  |Div (e1, e2) -> Div (remove_zero e1, remove_zero e2)   
  |Pow (e1, n) -> Pow (remove_zero e1, n)
  | _ -> e

(*Reduce or simplify an bool expression*)
let rec bool_reduce (e: Ast.smt_bool_expr) = match e with
  | BVar bVar -> BVar bVar
  | Eq (e1, e2) -> Eq (remove_zero (Expr.reduce e1), remove_zero (Expr.reduce e2))
  | Le (e1, e2) -> Le (remove_zero (Expr.reduce e1), remove_zero (Expr.reduce e2))
  | Leq(e1, e2) -> Leq(remove_zero (Expr.reduce e1), remove_zero (Expr.reduce e2))
  | Gr (e1, e2) -> Gr (remove_zero (Expr.reduce e1), remove_zero (Expr.reduce e2))
  | Geq(e1, e2) -> Geq(remove_zero (Expr.reduce e1), remove_zero (Expr.reduce e2))
  | And(e1, e2) -> And(bool_reduce e1, bool_reduce e2)
  | Not (e1) -> Not (bool_reduce e1)


(*Represent a poly expression to a string by prefix order*)
let rec smt_poly_toPrefix (e: Ast.smt_poly_expr) = match e with
  | Real c -> string_of_float c
  | Var x -> x
  | SubVar u -> u
  | Add (e1, e2) -> "(+ "^ smt_poly_toPrefix e1 ^ " " ^ smt_poly_toPrefix e2 ^ ")"
  | Sub (e1, e2) -> "(- "^ smt_poly_toPrefix e1 ^ " " ^ smt_poly_toPrefix e2 ^ ")"
  | Mul (e1, e2) -> "(* "^ smt_poly_toPrefix e1 ^ " " ^ smt_poly_toPrefix e2 ^ ")"
  | Div (e1, e2) -> "(/ "^ smt_poly_toPrefix e1 ^ " " ^ smt_poly_toPrefix e2 ^ ")"
  | Pow (e1, n)  -> "(^ "^ smt_poly_toPrefix e1 ^ " " ^ (string_of_int n)^ ")"

(*Represent a bool expression to a string by prefix order*)
let rec bool_toPrefix (e: Ast.smt_bool_expr) = match e with
  | BVar bVar -> bVar
  | Eq (e1, e2) -> "(= " ^ smt_poly_toPrefix e1 ^ " " ^ smt_poly_toPrefix e2 ^ ")"
  | Le (e1, e2) -> "(< " ^ smt_poly_toPrefix e1 ^ " " ^ smt_poly_toPrefix e2 ^ ")"
  | Leq(e1, e2) -> "(<= "^ smt_poly_toPrefix e1 ^ " " ^ smt_poly_toPrefix e2 ^ ")"
  | Gr (e1, e2) -> "(> " ^ smt_poly_toPrefix e1 ^ " " ^ smt_poly_toPrefix e2 ^ ")"
  | Geq(e1, e2) -> "(>=" ^ smt_poly_toPrefix e1 ^ " " ^smt_ poly_toPrefix e2 ^ ")"
  | And(e1, e2) ->"(and "^ bool_toPrefix e1 ^ " " ^ bool_toPrefix e2^ ")"
  | Not (e1)    ->"(not "^bool_toPrefix e1^ ")"

(*Represent a nil expression to a string by prefix order*)
let rec bound_toPrefix = function
  | Nil -> ""
  | BC (b) -> bool_toPrefix b
  | AND (b1, b2) -> "(and "^ bound_toPrefix b1 ^ " " ^ bound_toPrefix b2^ ")"

let rec div_constr e = match e with
  | And(e1, e2) -> div_constr e1 ^ "\n" ^ div_constr e2
  | _ -> bool_toPrefix e

let poly_isCons (e: Ast.smt_poly_expr) = match e with
  | Real c -> true
  | _ -> false

(*Simplify a boolean expression: a constant placed in right hand side*)
let rec bool_simp  (e: Ast.smt_bool_expr) = match e with
  | BVar bVar -> BVar bVar
  | Eq (e1, e2) -> 
      if ((poly_isCons e1) || (poly_isCons e2)) then Eq (e1, e2)
      else Eq (Sub(e1, e2), Real 0.)
  | Le (e1, e2) -> 
      if (poly_isCons e1) || (poly_isCons e2) then Le (e1, e2)
      else Le (Sub(e1, e2), Real 0.)
  | Leq(e1, e2) -> 
      if (poly_isCons e1) || (poly_isCons e2) then Leq (e1, e2)
      else Leq (Sub(e1, e2), Real 0.)
  | Gr (e1, e2) -> 
      if (poly_isCons e1) || (poly_isCons e2) then Gr (e1, e2)
      else Gr (Sub(e1, e2), Real 0.)
  | Geq(e1, e2) -> 
      if (poly_isCons e1) || (poly_isCons e2) then Geq (e1, e2)
      else Geq (Sub(e1, e2), Real 0.)
  | And(e1, e2) -> And (bool_simp e1, bool_simp e2)


(*------ REFINEMENT STEP -------
  ------ Get an upper bound or lower a bound of based variables from SMT Benchmark constraints*)

(*get the list of variables from a polynomial expression*)
  let rec smt_get_vars = function
    | Var x -> [x]
    | Add(e1, e2) -> List.append (smt_get_vars e1) (smt_get_vars e2)
    | Sub(e1, e2) -> List.append (smt_get_vars e1) (smt_get_vars e2)
    | Mul(e1, e2) -> List.append (smt_get_vars e1) (smt_get_vars e2)
    | Pow(e1, n)  -> smt_get_vars e1
    | _ -> []

(*Assume all variables >= 0, get all the bounds for variables
 
A naive implementation

*)

(*Remove an element from a list*)
let rec rem_list l a = match l with
  | h::t -> 
      if (h=a) then t
      else h::(rem_list t a)
  | [] -> []

(*Get the common elements in two lists*)
let rec com_list l1 l2 = match l1 with
  | h::t ->
      if (List.mem h l2) then h::(com_list t (rem_list l2 h))
      else com_list t l2
  | [] -> []

let merge_list (l1, a) (l2, b) = (List.append l1 l2, a*.b)

let rec _lstVar = function
  | Var x  -> ([x], 1.)
  | Real c -> ([] , c)
  | Mul (e1, e2) -> merge_list (_lstVar e1) (_lstVar e2)
  | Add (e1, e2) -> let (l1, a1) = _lstVar e1 in
                    let (l2, a2) = _lstVar e2 in
        ((com_list l1 l2), 1.)
  | Sub (e1, e2) -> let (l1, a1) = _lstVar e1 in
                    let (l2, a2) = _lstVar e2 in
        ((com_list l1 l2), 1.)
  | _ -> ([], 1.)


(*Check an element existing in a list*)
let rec in_list l a = match l with
  | h::t ->
      if (h=a) then true
      else in_list t a
  | [] -> false


(*Check a variable in multiplication of expression*)
let rec in_expr e v = match e with
  | Var x -> 
      if (x=v) then true
      else false
  | Mul (e1, e2) ->
      if (in_expr e1 v) then true
      else (in_expr e2 v)
  | Add (e1, e2) -> 
      if (in_expr e1 v) & (in_expr e2 v) then true
      else false
  | Sub (e1, e2) -> 
      if (in_expr e1 v) & (in_expr e2 v) then true
      else false
(*  | Pow (e1, n) ->
      if (in_expr e1 v) then true
      else false
*)
  | _ -> false
 
(*Div a polynomial function by a variable*)
let rec div_var exp v = match exp with
  | Var x -> 
      if (x=v) then Real 1.
      else Var x
  | Mul (Var x, Var y) -> 
      if (x=v) then (Var y)
      else if (y=v) then (Var x)
      else Mul (Var x, Var y)
  | Mul (Var x, e) -> 
      if (x=v) then e
      else Mul (Var x, (div_var e v))
  | Mul (e, Var x) -> 
      if (x=v) then e
      else Mul ((div_var e v), Var x)
  | Mul (Real c, e) -> Mul (Real c, (div_var e v))
  | Mul (e1, e2) -> 
      if (in_expr e1 v) then Mul ((div_var e1 v), e2)
      else Mul (e1, (div_var e2 v))
  | Add (e1, e2) -> 
      if (in_expr e1 v) & (in_expr e2 v) then Add ((div_var e1 v), (div_var e2 v))
      else exp
  | Sub (e1, e2) -> 
      if (in_expr e1 v) & (in_expr e2 v) then Sub ((div_var e1 v), (div_var e2 v))
      else exp
(*  | Pow (e1, n) ->
      if (in_expr e1 v) then Pow (x, n-1)
      else exp
*)
  | _ -> exp

(*Div a polynomial function by a coefficient*)
let rec div_coef exp c = match exp with
  | Mul (Real a, e) -> Mul (Real (a/.c), e)
  | Mul (Var x, e) -> Mul (Var x, (div_coef e c))
  | Mul (e1, e2) -> Mul (e1, (div_coef e2 c))
  | _ -> exp

(*Div a polynomial function by a list of variables*)
let rec div_func exp l = match l with
  | h::t -> div_func (div_var exp h) t
  | [] -> exp

let simp_expr e1 e2 = 
  let (l1, a1) = _lstVar e1 in
  let (l2, a2) = _lstVar e2 in
  let l = com_list l1 l2 in
  let m = min (abs_float a1) (abs_float a2) in
  (div_coef (div_func e1 l) m, div_coef (div_func e2 l) m)

(*Reduce an polynomial function with assume that all variables >=0*)
let rem_var e = match e with 
  | Real c -> Real c
  | Var x -> Real 1.
  | Add (e1, e2) ->
      let (l1,a1) = _lstVar e1 in
      let (l2,a2) = _lstVar e2 in
      let l = com_list l1 l2 in
      let m = min (abs_float a1) (abs_float a2) in
      Add (div_coef (div_func e1 l) m, div_coef (div_func e2 l) m)
  | Sub (e1, e2) ->
      let (l1,a1) = _lstVar e1 in
      let (l2,a2) = _lstVar e2 in
      let l = com_list l1 l2 in
      let m = min (abs_float a1) (abs_float a2) in
      Sub (div_coef (div_func e1 l) m, div_coef (div_func e2 l) m)  
  | Mul (e1, e2) ->
      let (l,a) = _lstVar (Mul(e1, e2)) in
      div_coef (div_func (Mul(e1, e2)) l) (abs_float a)
  | _ -> e

(*Simplify constraint in boolean expression
  with assume that all based variables >=0
*)

let rem_var1 e =
  let lstVars = Util.red_list ( smt_get_vars e) in
  div_func e lstVars

(*Simplify constraint in boolean expression
  with assume that all based variables >=0 *)

let rec simp_cons  exp = match exp with
  | BVar b -> BVar b
  | Gr (e, Real 0.)  -> Gr  (rem_var1 e, Real 0.)
  | Gr (Real 0., e)  -> Le  (rem_var1 e, Real 0.)
  | Le (e, Real 0.)  -> Le  (rem_var1 e, Real 0.)
  | Le (Real 0., e)  -> Gr  (rem_var1 e, Real 0.)

  | Le (e1, e2)  -> 
      let (c1, c2) = (simp_expr e1 e2) in
      Le (c1, c2)
  | Gr (e1, e2)  -> 
      let (c1, c2) = (simp_expr e1 e2) in
      Gr (c1, c2)
  | And (e1, e2) -> And (simp_cons e1, simp_cons e2)
  | _ -> exp

(*Reformulate of relation between two expresions: x + b > c <=> x > c-b*)
let rec refor_expr e (c: float) = match e with
  | Mul (Var x, Real a) -> 
      if (a>0.) then (Var x, Real (c/.a), '+')
      else if (a<0.) then (Var x, Real (c/.a), '-')
      else (Real 0., Real c, '+')
  | Mul (Real a, Var x) -> 
      if (a>0.) then (Var x, Real (c/.a), '+')
      else if (a<0.) then (Var x, Real (c/.a), '-')
      else (Real 0., Real c, '+')
  | Add(e1, Real a) -> refor_expr e1 (c-.a)
  | Add(Real a, e1) -> refor_expr e1 (c-.a)
  | Sub(e1, Real a) -> refor_expr e1 (c+.a)
  | Sub(Real a, e1) -> refor_expr (Mul (Real (-1.), e1)) (c-.a)
  | _ -> (e, Real c, '.')

(*Get the lower bound or upper bound from simplify constraints called bound constraints*)
let rec getBound (e: bool_expr) = match e with 
  | Eq (Var x, Real c)  -> BC (Eq  (Var x, Real c))
  | Eq (Real c, Var x)  -> BC (Eq  (Var x, Real c))
  | Geq(Var x, Real c)  -> BC (Geq (Var x, Real c))
  | Geq(Real c, Var x)  -> BC (Leq (Var x, Real c))
  | Gr (Var x, Real c)  -> BC (Gr  (Var x, Real c))
  | Gr (Real c, Var x)  -> BC (Le  (Var x, Real c))
  | Le (Var x, Real c)  -> BC (Le  (Var x, Real c))
  | Le (Real c, Var x)  -> BC (Gr  (Var x, Real c))
  | Leq(Var x, Real c)  -> BC (Leq (Var x, Real c))
  | Leq(Real c, Var x)  -> BC (Geq (Var x, Real c))

  | Eq (e1, Real c) -> 
      let (e3, e4, s) = refor_expr e1 c in
      if (s='.') then Nil 
      else getBound (Eq (e3, e4))      
  | Eq (Real c, e1) -> 
      let (e3, e4, s) = refor_expr e1 c in
      if (s='.') then Nil 
      else getBound (Eq (e3, e4))      
  | Geq (Real c, e1) -> 
      let (e3, e4, s) = refor_expr e1 c in
      if (s='+') then getBound (Leq (e3, e4))
      else if (s='-') then getBound (Geq (e3, e4))
      else Nil
  | Geq (e1, Real c) -> 
      let (e3, e4, s) = refor_expr e1 c in
      if (s='+') then getBound (Geq (e3, e4))
      else if (s='-') then getBound (Leq (e3, e4))
      else Nil
  | Leq (Real c, e1) -> 
      let (e3, e4, s) = refor_expr e1 c in
      if (s='+') then getBound (Geq (e3, e4))    
      else if (s='-') then getBound (Leq (e3, e4))
      else Nil
  | Leq (e1, Real c) -> 
      let (e3, e4, s) = refor_expr e1 c in
      if (s='+') then getBound (Leq (e3, e4))
      else if (s='-') then getBound (Geq (e3, e4))
      else Nil
  | Gr (Real c, e1) -> 
      let (e3, e4, s) = refor_expr e1 c in
      if (s='+') then getBound (Le (e3, e4))
      else getBound (Gr (e3, e4))
  | Gr (e1, Real c) -> 
      let (e3, e4, s) = refor_expr e1 c in
      if (s='+') then getBound (Gr (e3, e4))
      else if (s='-') then getBound (Le (e3, e4))
      else Nil
  | Le (Real c, e1) -> 
      let (e3, e4, s) = refor_expr e1 c in
      if (s='+') then getBound (Gr (e3, e4))
      else if (s='-') then getBound (Le (e3, e4))
      else Nil
  | Le (e1, Real c) -> 
      let (e3, e4, s) = refor_expr e1 c in
      if (s='+') then getBound (Le (e3, e4))
      else if (s='-') then getBound (Gr (e3, e4))
      else Nil
  | And (e1, e2) -> AND (getBound e1, getBound e2)
  | _ -> Nil

(*Remove Nil constraints*)
let rec remov_nil expr = match expr with
  | Nil -> Nil
  | AND (Nil, e) -> remov_nil e
  | AND (e, Nil) -> remov_nil e
  | AND (e1, e2) -> 
      let n1 = remov_nil e1 in
      let n2 = remov_nil e2 in
      if (n1 <> Nil) & (n2<>Nil) then AND (n1, n2)
      else if (n1 <> Nil) then n1
      else if (n2 <> Nil) then n2
      else Nil
  | _ -> expr


(*Update bound of variable x from bCons constraint*)
let rec update_bound (x, lo, up, bCons) = match bCons with
  | BC (Leq (Var y, Real a))
  | BC (Le  (Var y, Real a)) -> 
      if (x=y)&(a<up) then (x, lo, a)
      else (x, lo, up)      
  | BC (Geq (Var y, Real a))
  | BC (Gr  (Var y, Real a)) -> 
      if (x=y)&(a>lo) then (x, a, up)
      else (x, lo, up)
  | AND (e1, e2) -> 
      let (x1, lo1, up1) = update_bound (x, lo, up, e1) in
      update_bound (x1, lo1, up1, e2)
  | _ -> (x, lo, up)


(*Generate a string of constraints*)
let gen_string (v, l1, u1, bst, bCons) = 
  let (x, lo, up) = update_bound (v, l1, u1, bCons) in
  let s = ref "" in
  let l = ref lo in 
  let n = int_of_float ((up-.lo)/.bst) in
  (*addition part*)
  (*if (l1 >= 0.0) && (u1 >= 1.) then *)
  (*  s := "(" ^ x ^ " in " ^ (string_of_float !l) ^ " " ^ (string_of_float !l) ^")"; *)
    (*s := "(or " ^ !s ^ " (" ^ x ^ " in " ^ (string_of_float 1.) ^ " " ^ (string_of_float 1.) ^"))");*)
  
  for i=1 to n do
    if (!s="") then
     s := "(" ^ x ^ " in " ^ (string_of_float !l) ^ " " ^ (string_of_float (!l+.bst)) ^")"
    else
     (s := "(or " ^ !s ^ " (" ^ x ^ " in " ^ (string_of_float !l) ^ " " ^ (string_of_float (!l+.bst)) ^"))");
    l := !l +.bst;
    (*addition part*)
    (*s := "(or " ^ !s ^ " (" ^ x ^ " in " ^ (string_of_float !l) ^ " " ^ (string_of_float (!l)) ^"))";*)
  done;
  
  let ch = (float_of_int n) *.bst in
  if (ch < up) then (
  if (!s="") then
    (s := "(" ^ x ^ " in " ^ (string_of_float !l) ^ " " ^ (string_of_float up) ^")")
  else
    (s := "(or " ^ !s ^ " (" ^ x ^ " in " ^ (string_of_float !l) ^ " " ^ (string_of_float up)^"))");
  );

  s := !s ^ "\n";
  !s

(*
let gen_string (v, l, u, bst, bCons) = 
  let (x, lo, up) = update_bound (v, l, u, bCons) in
  let s = ref "" in
  let l = ref lo in
  while (!l+.bst <=up) do
    if (!s="") then
     (s := !s ^ "(" ^ x ^ " in " ^ (string_of_float !l) ^ " " ^ (string_of_float (!l+.bst)) ^")")
    else
      (s := "or (" ^ !s ^ " (" ^ x ^ " in " ^ (string_of_float !l) ^ " " ^ (string_of_float (!l+.bst)) ^"))");
    l := !l +.bst;
  done;
  if (!l < up) then (
  if (!s="") then
    (s := !s ^ "(" ^ x ^ " in " ^ (string_of_float !l) ^ " " ^ (string_of_float up) ^")")
  else
    (s := "or (" ^ !s ^ " (" ^ x ^ " in " ^ (string_of_float !l) ^ " " ^ (string_of_float up)^"))");
  );
  s := "(assert (" ^ !s ^ ")\n";
  !s *)

let rec gen_intv e mb bst bCons= match e with
  | Geq (Var x, Real a) 
  | Gr  (Var x, Real a) -> gen_string (x, a, (a +.mb), bst, bCons)
  | Leq (Var x, Real a) 
  | Le  (Var x, Real a) -> gen_string (x, (a -.mb), a, bst, bCons)  
  | And (e1, e2) -> (gen_intv e1 mb bst bCons) ^ (gen_intv e2 mb bst bCons)
  | _ -> ""

(*get boolean expression from assert expression*)  
let rec get_bExpr e = match e with
  | Ch (b) -> b
  | As (l, a) -> get_bExpr a

  (*Reduce list to set
  let Util.red_list l = 
    let rec set_list l1 l2 = match l2 with
      |[] -> l1
      |h::t -> 
    if (List.mem h l1) then 
      set_list l1 t
    else
      set_list (h::l1) t in
    set_list [] l
*)

  (*Decompose a boolean constraints to list of bool constraints without and*)
  let rec bool_toList e = match e with
    | And (e1, e2) -> List.append (bool_toList e2) (bool_toList e1)
    | _ -> [e]

  (*Construct a boolean constraint from a list of boolean constraints*)
  let rec list_toBool lst = match lst with
    |[] -> Eq (Real 0., Real 0.) (*This case will never happen*)
    |[a] -> a
    |h::t -> And (list_toBool t, h)

  let list_remove element lst = List.filter (fun x-> x <> element) lst

  (*remove trivial constraints such as 1. > 0.*)

  let is_trivial = function
    |Geq (Real c1, Real c2) ->
        if (c1 >= c2) then 
           true
        else 
           false
    |Leq (Real c1, Real c2) ->
        if (c1 <= c2) then 
           true
        else 
           false
    |Gr (Real c1, Real c2) ->
        if (c1 > c2) then 
           true
        else 
           false
    |Le (Real c1, Real c2) ->
        if (c1 < c2) then 
           true
        else 
           false
    | _ -> false

  let rec remove_trivial = function
    |[] -> []
    |h::t -> if (is_trivial h) then 
          remove_trivial t
             else 
                h:: remove_trivial t  
  

  let rec red_cons lst = match lst with
    | [] -> []        
    |Gr(e, Real c)::t ->   
  if (List.mem (Geq(e, Real c)) t) then
    Gr(e, Real c):: red_cons (list_remove (Geq (e, Real c)) t)
  else
      Gr(e, Real c):: red_cons t
    |Geq(e, Real c)::t ->   
  if (List.mem (Gr(e, Real c)) t) then
    red_cons t
  else
      Geq(e, Real c):: red_cons t
    |Le(e, Real c)::t ->   
  if (List.mem (Leq(e, Real c)) t) then
    Le(e, Real c):: red_cons (list_remove (Leq (e, Real c)) t)
  else
      Le(e, Real c):: red_cons t
    |Leq(e, Real c)::t ->   
  if (List.mem (Le(e, Real c)) t) then
    red_cons t
  else
      Leq(e, Real c):: red_cons t
    |h::t -> h:: red_cons t

(*--------------------------------------------------------------------------
  This function will generate interval constraints from SMT benchmark files
  --------------------------------------------------------------------------*)
(*re-formula expression for not*)
  let neg e = match e with
    |Le (e1, e2) -> Geq (e1, e2)
    |Leq(e1, e2) -> Gr (e1, e2)
    |Gr (e1, e2) -> Leq (e1, e2)
    |Geq(e1, e2) -> Le (e1, e2)
    |Not(e1) -> e1
    |_ -> e (*This implementation is neither applied for equality "=" nor not (and (...))*)

(*reduce expression for "not" and "/" *)
  let rec remove_div e = match e with
    | Div (Real a1, Real a2) -> Real (a1 /. a2)
    | Add (e1, e2) -> Add (remove_div e1, remove_div e2)
    | Sub (e1, e2) -> Sub (remove_div e1, remove_div e2)
    | Mul (e1, e2) -> Mul (remove_div e1, remove_div e2)
    | _ -> e 

  let rec reduce_expr e = match e with 
    | Eq (e1, e2) -> Eq (remove_div e1, remove_div e2)
    | Geq (e1, e2) -> Geq (remove_div e1, remove_div e2)
    | Gr (e1, e2) -> Gr (remove_div e1, remove_div e2)
    | Leq (e1, e2) -> Leq (remove_div e1, remove_div e2)
    | Le (e1, e2) -> Le (remove_div e1, remove_div e2)
    | Not (a) -> reduce_expr (neg (a))
    | And (e1, e2) -> And (reduce_expr e1, reduce_expr e2)
    | _ -> e

 (*remove "not" from expression*)
 let rec remove_not e = match e with 
    | Not (a) -> remove_not (neg (a))
    | And (e1, e2) -> And (remove_not e1, remove_not e2)
    | _ -> e

  let rec make_lstIntv eIntv ub = match eIntv with
    | Geq (Var x, Real a) -> [(x, a, ub)]
    | And (e1, e2) -> List.append (make_lstIntv e1 ub) (make_lstIntv e2 ub) 

  let rec update_var (x, lb, ub) l = match l with
    | [] -> (x, lb, ub)
    | (y, a1, a2) :: t-> 
        if (x=y) then
          update_var (x, (max lb a1), (min ub a2)) t
        else
          update_var (x, lb, ub) t

  let rec update_list l l1 l2 = match l1 with
    | [] -> l
    | h:: t -> 
        let lst = (update_var h l2)::l in
          update_list lst t l2
  
  let rec tolist_bc l lb ub = match l with
    | Nil -> []
    | BC (e) -> (match e with
        |Geq (Var x, Real c) -> [(x, (max lb c), ub)]
        |Geq (Real c, Var x) -> [(x, lb, (min ub c))]
        |Gr (Var x, Real c) -> [(x, (max lb c), ub)]
        |Gr (Real c, Var x) -> [(x, lb, (min ub c))]
        |Leq (Var x, Real c) -> [(x, lb, (min ub c))]
        |Leq (Real c, Var x) -> [(x, (max lb c), ub)]
        |Le (Var x, Real c) -> [(x, lb, (min ub c))]
        |Le (Real c, Var x) -> [(x, (max lb c), ub)]
        |_ -> [])
    |AND (e1, e2) -> List.append (tolist_bc e1 lb ub) (tolist_bc e2 lb ub) 

  let rec toString_lstIntv l = match l with
    | [] -> ""
    | (x, lb, ub) :: t -> 
        "(" ^ x ^ " in " ^ (string_of_float lb) ^ " "^ (string_of_float ub) ^ ")\n"
          ^ (toString_lstIntv t)

(*Simplify expressions: operations on constants*)  
let rec simplify_expr e = match e with
  |Var x -> Var x
  |Real c -> Real c
  |Add (Real a, Real b) -> Real (a+.b)
  |Sub (Real a, Real b) -> Real (a-.b)
  |Mul (Real a, Real b) -> Real (a*.b)
  |Mul (Real 1., Var x) -> Var x
  |Mul (Var x, Real 1.) -> Var x
  |Div (Var x, Real 1.) -> Var x
  |Div (e, Real c) -> Mul (e, Real (1./.c))
  |Div (Real a, Real b) -> Real (a/.b)

  |Add (e1, e2) -> Add (simplify_expr e1, simplify_expr e2)
  |Sub (e1, e2) -> Sub (simplify_expr e1, simplify_expr e2)
  |Mul (e1, e2) -> Mul (simplify_expr e1, simplify_expr e2)
  |Div (e1, e2) -> Div (simplify_expr e1, simplify_expr e2)

  | _ -> e


(*simplify bool expressions*)
let rec simplify_bool e = match e with
  | BVar b -> BVar b
  | Eq  (e1, e2) -> Eq  (simplify_expr e1, simplify_expr e2)
  | Le  (e1, e2) -> Le  (simplify_expr e1, simplify_expr e2)
  | Leq (e1, e2) -> Leq (simplify_expr e1, simplify_expr e2)
  | Gr  (e1, e2) -> Gr  (simplify_expr e1, simplify_expr e2)
  | Geq (e1, e2) -> Geq (simplify_expr e1, simplify_expr e2)
  | And (b1, b2) -> And (simplify_bool b1, simplify_bool b2)
  | Not (e1) -> Not (simplify_bool e1)



let genSmtForm sIntv sAssert loBound upBound =       
  (*get Assert expression from sAssert *)  
  let eAss = smt_read sAssert in
 
  let pass = lstSubVar eAss in
  let bass = lstBVar eAss in   

  let ori_expr = getAss_expr eAss in

  (*Remove temporary variables and substitute them by based variables*)
  let sub_expr = subst_bool bass pass ori_expr in

  (*simplify expression: operations on constants, i.e., constant +-*/ constant..., Div (a, b) = a/b*)
  let simp_expr = simplify_bool sub_expr in

  (*remove not in expressions, "not" is allowed in quite restricted format, i.e., not (a > b)*)
  (*let red_expr = reduce_expr simp_expr in*)
  let red_expr = remove_not simp_expr in

  (*Placed constants to a left/right of equations, i.e., e1 > e2 <=> e1 - e2 > 0 *)
  let final_expr = bool_simp red_expr in

 (*simplify expression after substitute all temporary variables*)
  let expr = bool_reduce final_expr in

  (*remove redundant expression, i.e., remove a >= 0 when a >0 occurs*)
  let eList = bool_toList expr in
  let simp_list = red_cons (Util.red_list eList) in  
  let new_expr = list_toBool simp_list in

  (*Get bound constraints of variables from assert expression eAss*)
  let bound_cons = remov_nil (getBound new_expr) in

  (*generate interval constraints *)  
  let eIntv = get_bExpr (read sIntv) in
  let lstIntv = make_lstIntv eIntv upBound in

  (*get a list of bound constraints*)
  let lst_bounds = tolist_bc bound_cons loBound upBound in

  (*update bounds for interval constraints from bound_cons*)  
  let new_lstIntv = update_list [] lstIntv lst_bounds in

  let strIntv = toString_lstIntv new_lstIntv in  

  let strAss = "(assert "^(bool_toPrefix new_expr)^")" in
  let strBound = "(assert "^(bound_toPrefix bound_cons)^")" in

  (*strBound ^ "\n" ^ strIntv ^ strAss*)
  strIntv ^ strAss

*)
end


(*===========================================================*)
(* Export those functions to C/C++ *)
(*===========================================================*)


(*let _ = Callback.register "caml_genSmtForm" Caml.genSmtForm;;*)

let _ = Callback.register "caml_genSatForm" Caml.genSatForm;;
let _ = Callback.register "caml_isTheoConsis" Caml.isTheoConsis;; 
let _ = Callback.register "caml_logResult" Caml.logResult;; 
(*let _ = Callback.register "caml_doTest" Caml.doTest;; *)
let _ = Callback.register "caml_dynTest" Caml.dynTest;; 
let _ = Callback.register "caml_getNumCons" Caml.getNumCons;; 
let _ = Callback.register "caml_InfCheck" Caml.infCheck;; 
(*let _ = Callback.register "caml_divide" Caml.divide;; *) 

