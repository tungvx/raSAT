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
open Interval
open Smtlib_syntax
open InfiniteList
open Icp

module Caml = struct
  let inf_I = {low=neg_infinity;high=infinity}
   

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

    (*==================================================*)
    (* C++ - OCaml interface *)
    (*==================================================*)

  let rec get_string_symbol = function
    |Symbol (_ , str1) -> str1
    |SymbolWithOr (_ , str1) -> str1

  and get_string_identifier = function 
    |IdSymbol (_ , symbol1) ->  get_string_symbol symbol1 
    |IdUnderscoreSymNum (_ , symbol3 , idunderscoresymnum_identifier_numeral334) -> raise (Failure "Not supported syntax line 125")

  and get_string_qualidentifier = function
    |QualIdentifierId (_ , identifier1) ->  get_string_identifier identifier1
    |QualIdentifierAs (_ , identifier3 , sort4) ->  raise (Failure "Not supported syntax line 129")

  and get_poly_specReal = function 
    |SpecConstsDec (_ , str1) ->  
      SPoly (Real (float_of_string str1, false, inf_I, new IA.af2 0))
    |SpecConstNum (_ , str1) ->  SPoly(Real (float_of_string str1, false, inf_I, new IA.af2 0))
    |SpecConstString (_ , str1) ->  raise (Failure "Not supported syntax line 159")
    |SpecConstsHex (_ , str1) ->  SPoly(Real (float_of_string str1, false, inf_I, new IA.af2 0)) 
    |SpecConstsBinary (_ , str1) ->  SPoly(Real (float_of_string str1, false, inf_I, new IA.af2 0)) 



  and get_poly_symbol varTermMap functions variables varBindings = function 
    |Symbol (_ , str1) -> 
      (try
        let term = StringMap.find str1 varBindings in
        get_poly_term varTermMap functions variables varBindings term
      with Not_found -> 
        (try
          let term = StringMap.find str1 varTermMap in
          get_poly_term StringMap.empty functions variables varBindings term
        with Not_found -> 
          (try
            let varSort = StringMap.find str1 variables in
            if (varSort = "Real" || varSort = "Int") then SPoly(Var (str1, inf_I, new IA.af2 0, false))
            else raise (Failure "Wrong input")
          with Not_found -> raise (Failure "Need Real/Int variable"))
        )
      )
    |SymbolWithOr (_ , str1) -> 
      (try
        let term = StringMap.find str1 varBindings in
        get_poly_term varTermMap functions variables varBindings term
      with Not_found ->  
        (try
          let term = StringMap.find str1 varTermMap in
          get_poly_term StringMap.empty functions variables varBindings term
        with Not_found -> 
          (try
            let varSort = StringMap.find str1 variables in
            if (varSort = "Real" || varSort = "Int") then SPoly(Var (str1, inf_I, new IA.af2 0, false))
            else raise (Failure "Wrong input")
          with Not_found -> raise (Failure "Need Real/Int variable")
          )
        )
      )

  and get_constraint_symbol varTermMap functions variables varBindings = function 
    |Symbol (_ , str1) -> 
      (try
        let term = StringMap.find str1 varBindings in
        get_constraint_term varTermMap functions variables varBindings term
      with  
        Not_found -> 
          ( try
              let varSort = StringMap.find str1 variables in
              if varSort = "Bool" then [BVar str1]
              else raise (Failure "Wrong input")
            with Not_found -> 
              (try
                let term = StringMap.find str1 varTermMap in
                get_constraint_term StringMap.empty functions variables varBindings term
              with Not_found -> 
                if str1 = "true" then [Or(BVar "true", NBVar "true")]
                else if str1 = "false" then [And(BVar "false", NBVar "false")]
                else raise (Failure "Wrong input")
              )
          )
      )
    |SymbolWithOr (_ , str1) -> 
      (try
        let term = StringMap.find str1 varBindings in
        get_constraint_term varTermMap functions variables varBindings term
      with  
        Not_found -> 
          ( try
              let varSort = StringMap.find str1 variables in
              if varSort = "Bool" then [BVar str1]
              else raise (Failure "Wrong input")
            with Not_found -> 
              (try
                let term = StringMap.find str1 varTermMap in
                get_constraint_term StringMap.empty functions variables varBindings term
              with Not_found -> 
                if str1 = "true" then [Or(BVar "true", NBVar "true")]
                else if str1 = "false" then [And(BVar "false", NBVar "false")]
                else raise (Failure "Wrong input")
              )
          )
      )

  and get_poly_identifier varTermMap functions variables varBindings = function 
    |IdSymbol (_ , symbol1) ->  get_poly_symbol varTermMap functions variables varBindings symbol1 
    |IdUnderscoreSymNum (_ , symbol3 , idunderscoresymnum_identifier_numeral334) -> raise (Failure "Not supported syntax line 165")

  and get_constraint_identifier varTermMap functions variables varBindings = function 
    |IdSymbol (_ , symbol1) ->  get_constraint_symbol varTermMap functions variables varBindings symbol1 
    |IdUnderscoreSymNum (_ , symbol3 , idunderscoresymnum_identifier_numeral334) -> raise (Failure "Not supported syntax line 200")  

  and get_poly_qualidentifier varTermMap functions variables varBindings = function 
    |QualIdentifierId (_ , identifier1) ->  get_poly_identifier varTermMap functions variables varBindings identifier1
    |QualIdentifierAs (_ , identifier3 , sort4) ->  raise (Failure "Not supported syntax line 129")

  and get_constraint_qualidentifier varTermMap functions variables varBindings = function 
    |QualIdentifierId (_ , identifier1) ->  get_constraint_identifier varTermMap functions variables varBindings identifier1
    |QualIdentifierAs (_ , identifier3 , sort4) ->  raise (Failure "Not supported syntax line 204")  

  and get_mul_poly = function
    | [] -> raise (Failure "Need arguments for multiplication")
    | [poly] -> poly 
    | poly1::poly2::remainingPolys -> get_mul_poly ((get_mul_poly_extra poly1 poly2) :: remainingPolys)
    
  and get_mul_poly_extra smtPoly1 smtPoly2 = match smtPoly1, smtPoly2 with
    | (SPoly(poly1), SPoly(poly2)) -> SPoly (Mul(poly1, poly2, inf_I, new IA.af2 0))
    | (SPoly(poly1), Poly (boolConstraint2, poly2)) -> Poly (boolConstraint2, Mul(poly1, poly2, inf_I, new IA.af2 0))
    | (Poly(boolConstraint1, poly1), SPoly(poly2)) -> Poly (boolConstraint1, Mul(poly1, poly2, inf_I, new IA.af2 0))
    | (Poly(boolConstraint1, poly1), Poly (boolConstraint2, poly2)) -> Poly(And(boolConstraint1, boolConstraint2), Mul(poly1, poly2, inf_I, new IA.af2 0))
    | (POr(smtPoly11, smtPoly12), _) -> POr (get_mul_poly_extra smtPoly11 smtPoly2, get_mul_poly_extra smtPoly12 smtPoly2)
    | (_, POr(smtPoly21, smtPoly22)) -> POr (get_mul_poly_extra smtPoly1 smtPoly21, get_mul_poly_extra smtPoly1 smtPoly22)

  and get_add_poly = function
    | [] -> raise (Failure "Need arguments for addition") 
    | [poly] -> poly
    | poly1::poly2::remainingPolys -> get_add_poly((get_add_poly_extra poly1 poly2) :: remainingPolys)

  and get_add_poly_extra smtPoly1 smtPoly2 = match smtPoly1, smtPoly2 with
    | (SPoly(poly1), SPoly(poly2)) -> SPoly (Add(poly1, poly2, inf_I, new IA.af2 0))
    | (SPoly(poly1), Poly (boolConstraint2, poly2)) -> Poly (boolConstraint2, Add(poly1, poly2, inf_I, new IA.af2 0))
    | (Poly(boolConstraint1, poly1), SPoly(poly2)) -> Poly (boolConstraint1, Add(poly1, poly2, inf_I, new IA.af2 0))
    | (Poly(boolConstraint1, poly1), Poly (boolConstraint2, poly2)) -> Poly(And(boolConstraint1, boolConstraint2), Add(poly1, poly2, inf_I, new IA.af2 0))
    | (POr(smtPoly11, smtPoly12), _) -> POr (get_add_poly_extra smtPoly11 smtPoly2, get_add_poly_extra smtPoly12 smtPoly2)
    | (_, POr(smtPoly21, smtPoly22)) -> POr (get_add_poly_extra smtPoly1 smtPoly21, get_add_poly_extra smtPoly1 smtPoly22)
    
  and get_div_poly = function
    | [] -> raise (Failure "Need arguments for addition")
    | [poly] -> poly
    | poly1::poly2::remainingPolys -> get_div_poly((get_div_poly_extra poly1 poly2) :: remainingPolys)

  and get_div_poly_extra smtPoly1 smtPoly2 = match smtPoly1, smtPoly2 with
    | (SPoly(poly1), SPoly(poly2)) -> SPoly (Div(poly1, poly2, inf_I, new IA.af2 0))
    | (SPoly(poly1), Poly (boolConstraint2, poly2)) -> Poly (boolConstraint2, Div(poly1, poly2, inf_I, new IA.af2 0))
    | (Poly(boolConstraint1, poly1), SPoly(poly2)) -> Poly (boolConstraint1, Div(poly1, poly2, inf_I, new IA.af2 0))
    | (Poly(boolConstraint1, poly1), Poly (boolConstraint2, poly2)) 
      -> Poly(And(boolConstraint1, boolConstraint2), Div(poly1, poly2, inf_I, new IA.af2 0))
    | (POr(smtPoly11, smtPoly12), _) -> POr (get_div_poly_extra smtPoly11 smtPoly2, get_div_poly_extra smtPoly12 smtPoly2)
    | (_, POr(smtPoly21, smtPoly22)) -> POr (get_div_poly_extra smtPoly1 smtPoly21, get_div_poly_extra smtPoly1 smtPoly22)

  and get_minus_poly = function
    | [] -> raise (Failure "Need arguments for Subtraction") 
    | [SPoly(Real (f, _, _, _))] -> SPoly(Real ((~-.) f, false, inf_I, new IA.af2 0))
    | [SPoly(poly)] -> SPoly(Sub(Real (0., false, inf_I, new IA.af2 0), poly, inf_I, new IA.af2 0))
    | [Poly(boolConstraint, Real (f, _, _, _))] -> Poly(boolConstraint, Real ((~-.) f, false, inf_I, new IA.af2 0))
    | [Poly(boolConstraint, poly)] -> Poly(boolConstraint, Sub (Real (0., false, inf_I, new IA.af2 0), poly, inf_I, new IA.af2 0))
    | polys -> get_minus_poly_extra polys

  and get_minus_poly_extra = function 
    | [] -> raise (Failure "Need arguments for Subtraction") 
    | [poly] -> poly
    | poly1::poly2::remainingPolys -> get_minus_poly_extra((get_minus_poly_extra_extra poly1 poly2) :: remainingPolys)     

  and get_minus_poly_extra_extra smtPoly1 smtPoly2 = match smtPoly1, smtPoly2 with
    | (SPoly(poly1), SPoly(poly2)) -> SPoly (Sub(poly1, poly2, inf_I, new IA.af2 0))
    | (SPoly(poly1), Poly (boolConstraint2, poly2)) -> Poly (boolConstraint2, Sub(poly1, poly2, inf_I, new IA.af2 0))
    | (Poly(boolConstraint1, poly1), SPoly(poly2)) -> Poly (boolConstraint1, Sub(poly1, poly2, inf_I, new IA.af2 0))
    | (Poly(boolConstraint1, poly1), Poly (boolConstraint2, poly2)) 
      -> Poly(And(boolConstraint1, boolConstraint2), Sub(poly1, poly2, inf_I, new IA.af2 0))
    | (POr(smtPoly11, smtPoly12), _) 
      -> POr (get_minus_poly_extra_extra smtPoly11 smtPoly2, get_minus_poly_extra_extra smtPoly12 smtPoly2)
    | (_, POr(smtPoly21, smtPoly22)) 
      -> POr (get_minus_poly_extra_extra smtPoly1 smtPoly21, get_minus_poly_extra_extra smtPoly1 smtPoly22)

  and get_poly_term varTermMap functions variables varBindings = function
    |TermSpecConst (_ , specReal1) ->  
      get_poly_specReal specReal1
    |TermQualIdentifier (_ , qualidentifier1) -> 
      get_poly_qualidentifier varTermMap functions variables varBindings qualidentifier1
    |TermQualIdTerm (_ , qualidentifier2 , termqualidterm_term_term563) -> 
      let qualidentifier_string = get_string_qualidentifier qualidentifier2 in
      if qualidentifier_string = "*" then
        let polys = get_polys_termqualidterm_term_term56 varTermMap functions variables varBindings termqualidterm_term_term563 in
        get_mul_poly polys
      else if qualidentifier_string = "+" then
        let polys = get_polys_termqualidterm_term_term56 varTermMap functions variables varBindings termqualidterm_term_term563 in
        get_add_poly polys  
      else if qualidentifier_string = "/" then
        let polys = get_polys_termqualidterm_term_term56 varTermMap functions variables varBindings termqualidterm_term_term563 in
        get_div_poly polys
      else if qualidentifier_string = "-" then
        let polys = get_polys_termqualidterm_term_term56 varTermMap functions variables varBindings termqualidterm_term_term563 in
        get_minus_poly polys
      else if qualidentifier_string = "ite" then
        get_ite_polys_termqualidterm_term_term56 varTermMap functions variables varBindings termqualidterm_term_term563
      else if StringMap.mem qualidentifier_string functions then
        let (arguments, funcSort, funcDef) = StringMap.find qualidentifier_string functions in
        if funcSort = "Real" || funcSort = "Int" then
          let varTermMap = get_varTermMap_termqualidterm_term_term56 arguments termqualidterm_term_term563 in
          get_poly_term varTermMap functions variables varBindings funcDef
        else   
          raise (Failure ("Undefined predicate symbols: " ^ qualidentifier_string))
      else raise (Failure "Undefined Function Symbols")
    | _ -> raise (Failure "Undefined Suported Function Symbols")
     (*|TermLetTerm (_ , termletterm_term_varbinding584 , term6) ->  print_string "(";print_string " "; print_string "let";print_string " "; print_string "(";print_string " "; pp_termletterm_term_varbinding58 termletterm_term_varbinding584;print_string " "; print_string ")";print_string " "; pp_term term6;print_string " "; print_string ")"; () 
     |TermForAllTerm (_ , termforallterm_term_sortedvar604 , term6) ->  print_string "(";print_string " "; print_string "forall";print_string " "; print_string "(";print_string " "; pp_termforallterm_term_sortedvar60 termforallterm_term_sortedvar604;print_string " "; print_string ")";print_string " "; pp_term term6;print_string " "; print_string ")"; () 
     |TermExistsTerm (_ , termexiststerm_term_sortedvar624 , term6) ->  print_string "(";print_string " "; print_string "exists";print_string " "; print_string "(";print_string " "; pp_termexiststerm_term_sortedvar62 termexiststerm_term_sortedvar624;print_string " "; print_string ")";print_string " "; pp_term term6;print_string " "; print_string ")"; () 
     |TermExclimationPt (_ , term3 , termexclimationpt_term_attribute644) ->  print_string "(";print_string " "; print_string "!";print_string " "; pp_term term3;print_string " "; pp_termexclimationpt_term_attribute64 termexclimationpt_term_attribute644;print_string " "; print_string ")"; () 
     *)

  and get_ite_polys_termqualidterm_term_term56 varTermMap functions variables varBindings = function
    |(_,[]) -> raise (Failure "Wrong number of arguments for ite")
    | (d , (term1)::termqualidterm_term_term562) -> 
      let boolConstraints = get_constraint_term varTermMap functions variables varBindings term1 in
      let boolConstraint = get_boolExp_from_list boolConstraints in  
      let smtPolys = get_polys_termqualidterm_term_term56 varTermMap functions variables varBindings (d,termqualidterm_term_term562) in 
      (match smtPolys with 
        | [smtPoly1; smtPoly2] -> 
          (let rec get_ite_polys boolConstraint2 smtPoly = match smtPoly with
            | SPoly(poly) -> Poly(boolConstraint2, poly)
            | Poly(boolConstraint1, poly1) -> Poly(And (boolConstraint1, boolConstraint2), poly1)
            | POr(smtPoly11, smtPoly21) -> POr (get_ite_polys boolConstraint2 smtPoly11, get_ite_polys boolConstraint2 smtPoly21)
          in
          POr(get_ite_polys boolConstraint smtPoly1, get_ite_polys (not_of_boolCons boolConstraint) smtPoly2)
          )
        | _ -> raise (Failure "Wrong number of arguments for ite")
      )

  and get_polys_termqualidterm_term_term56 varTermMap functions variables varBindings = function
    |(_,[]) ->   [] 
    | (d , (term1)::termqualidterm_term_term562) -> 
      let poly = get_poly_term varTermMap functions variables varBindings term1 in
      let remainingPolys = get_polys_termqualidterm_term_term56 varTermMap functions variables varBindings (d,termqualidterm_term_term562) in 
      poly :: remainingPolys 

  and get_constraints_termqualidterm_term_term56 varTermMap functions variables varBindings = function
    |(_,[]) ->   [] 
    | (d , (term1)::termqualidterm_term_term562) -> 
      let constraint1 = get_constraint_term varTermMap functions variables varBindings term1 in
      let remainingConstraints = get_constraints_termqualidterm_term_term56 varTermMap functions variables varBindings (d,termqualidterm_term_term562) in 
      constraint1 @ remainingConstraints

  and get_varTermMap_termqualidterm_term_term56 arguments = function
    |(_,[]) ->  StringMap.empty 
    | (d , (term1)::termqualidterm_term_term562) -> 
      (match arguments with
        | [] -> raise (Failure "Wrong number of arguments for defined function symbol")
        | (var, _)::remainingArguments -> 
          let varTermMap = get_varTermMap_termqualidterm_term_term56 remainingArguments (d, termqualidterm_term_term562) in
          StringMap.add var term1 varTermMap
      )      

  and get_le_constraint polys = match polys with
    | [] ->  raise (Failure "Wrong Input")
    | [poly] -> []
    | poly1::poly2::remainingPolys -> (get_le_constraint_extra poly1 poly2) :: (get_le_constraint (poly2::remainingPolys))

  and get_le_constraint_extra smtPoly1 smtPoly2 = match smtPoly1, smtPoly2 with
    | (SPoly(poly1), SPoly(poly2)) -> get_le_constraint_extra_extra poly1 poly2
    | (SPoly(poly1), Poly(boolConstraint2, poly2)) -> And(boolConstraint2, get_le_constraint_extra_extra poly1 poly2)
    | (Poly(boolConstraint1, poly1), SPoly(poly2)) -> And(boolConstraint1, get_le_constraint_extra_extra poly1 poly2)
    | (_, POr(smtPoly21, smtPoly22)) -> Or(get_le_constraint_extra smtPoly1 smtPoly21, get_le_constraint_extra smtPoly1 smtPoly22)
    | (POr(smtPoly11, smtPoly12), _) -> Or(get_le_constraint_extra smtPoly11 smtPoly2, get_le_constraint_extra smtPoly12 smtPoly2)

  and get_le_constraint_extra_extra poly1 poly2 = match poly1, poly2 with
    | (_, Real (0., _, inf_I, _)) -> Single (new polynomialConstraint (Le (reduce poly1)))
    | (Real (0., _, inf_I, _), _) -> Single (new polynomialConstraint (Gr (reduce poly2)))
    | _ -> Single (new polynomialConstraint (Le (reduce (Sub(poly1, poly2, inf_I, new IA.af2 0)))))

  and get_leq_constraint polys = match polys with
    | [] ->  raise (Failure "Wrong Input") 
    | [poly] -> []
    | poly1::poly2::remainingPolys -> (get_leq_constraint_extra poly1 poly2) :: (get_leq_constraint (poly2::remainingPolys))

  and get_leq_constraint_extra smtPoly1 smtPoly2 = match smtPoly1, smtPoly2 with
    | (SPoly(poly1), SPoly(poly2)) -> get_leq_constraint_extra_extra poly1 poly2
    | (SPoly(poly1), Poly(boolConstraint2, poly2)) -> And(boolConstraint2, get_leq_constraint_extra_extra poly1 poly2)
    | (Poly(boolConstraint1, poly1), SPoly(poly2)) -> And(boolConstraint1, get_leq_constraint_extra_extra poly1 poly2)
    | (_, POr(smtPoly21, smtPoly22)) -> Or(get_leq_constraint_extra smtPoly1 smtPoly21, get_leq_constraint_extra smtPoly1 smtPoly22)
    | (POr(smtPoly11, smtPoly12), _) -> Or(get_leq_constraint_extra smtPoly11 smtPoly2, get_leq_constraint_extra smtPoly12 smtPoly2)

  and get_leq_constraint_extra_extra poly1 poly2 = match poly1, poly2 with
    | (_, Real (0., _, inf_I, _)) -> Single (new polynomialConstraint (Leq (reduce poly1)))
    | (Real (0., _, inf_I, _), _) -> Single (new polynomialConstraint (Geq (reduce poly2)))
    | _ -> Single (new polynomialConstraint (Leq (reduce (Sub(poly1, poly2, inf_I, new IA.af2 0)))))  

  and get_gr_constraint polys = match polys with
    | [] ->  raise (Failure "Wrong Input")
    | [poly] -> []
    | poly1::poly2::remainingPolys -> (get_gr_constraint_extra poly1 poly2) :: (get_gr_constraint (poly2::remainingPolys))

  and get_gr_constraint_extra smtPoly1 smtPoly2 = match smtPoly1, smtPoly2 with
    | (SPoly(poly1), SPoly(poly2)) -> get_gr_constraint_extra_extra poly1 poly2
    | (SPoly(poly1), Poly(boolConstraint2, poly2)) -> And(boolConstraint2, get_gr_constraint_extra_extra poly1 poly2)
    | (Poly(boolConstraint1, poly1), SPoly(poly2)) -> And(boolConstraint1, get_gr_constraint_extra_extra poly1 poly2)
    | (_, POr(smtPoly21, smtPoly22)) -> Or(get_gr_constraint_extra smtPoly1 smtPoly21, get_gr_constraint_extra smtPoly1 smtPoly22)
    | (POr(smtPoly11, smtPoly12), _) -> Or(get_gr_constraint_extra smtPoly11 smtPoly2, get_gr_constraint_extra smtPoly12 smtPoly2)

  and get_gr_constraint_extra_extra poly1 poly2 = match poly1, poly2 with
    | (_, Real (0., _, inf_I, _)) -> Single (new polynomialConstraint (Gr (reduce poly1)))
    | (Real (0., _, inf_I, _), _) -> Single (new polynomialConstraint (Le (reduce poly2)))
    | _ -> Single (new polynomialConstraint (Gr (reduce (Sub(poly1, poly2, inf_I, new IA.af2 0)))))    

  and get_geq_constraint polys = match polys with
    | [] ->  raise (Failure "Wrong Input")
    | [poly] ->  []
    | poly1::poly2::remainingPolys -> (get_geq_constraint_extra poly1 poly2) :: (get_geq_constraint (poly2::remainingPolys))

  and get_geq_constraint_extra smtPoly1 smtPoly2 = match smtPoly1, smtPoly2 with
    | (SPoly(poly1), SPoly(poly2)) -> get_geq_constraint_extra_extra poly1 poly2
    | (SPoly(poly1), Poly(boolConstraint2, poly2)) -> And(boolConstraint2, get_geq_constraint_extra_extra poly1 poly2)
    | (Poly(boolConstraint1, poly1), SPoly(poly2)) -> And(boolConstraint1, get_geq_constraint_extra_extra poly1 poly2)
    | (_, POr(smtPoly21, smtPoly22)) -> Or(get_geq_constraint_extra smtPoly1 smtPoly21, get_geq_constraint_extra smtPoly1 smtPoly22)
    | (POr(smtPoly11, smtPoly12), _) -> Or(get_geq_constraint_extra smtPoly11 smtPoly2, get_geq_constraint_extra smtPoly12 smtPoly2)

  and get_geq_constraint_extra_extra poly1 poly2 = match poly1, poly2 with
    | (_, Real (0., _, inf_I, _)) -> Single (new polynomialConstraint (Geq (reduce poly1)))
    | (Real (0., _, inf_I, _), _) -> Single (new polynomialConstraint (Leq (reduce poly2)))
    | _ -> Single (new polynomialConstraint (Geq (reduce (Sub(poly1, poly2, inf_I, new IA.af2 0)))))   

  and get_eq_constraint polys = match polys with
    | [] ->  raise (Failure "Wrong Input")
    | [poly] -> []
    | poly1::poly2::remainingPolys -> (get_eq_constraint_extra poly1 poly2) :: (get_eq_constraint (poly2::remainingPolys))

  and get_eq_constraint_extra smtPoly1 smtPoly2 = match smtPoly1, smtPoly2 with
    | (SPoly(poly1), SPoly(poly2)) -> get_eq_constraint_extra_extra poly1 poly2
    | (SPoly(poly1), Poly(boolConstraint2, poly2)) -> And(boolConstraint2, get_eq_constraint_extra_extra poly1 poly2)
    | (Poly(boolConstraint1, poly1), SPoly(poly2)) -> And(boolConstraint1, get_eq_constraint_extra_extra poly1 poly2)
    | (_, POr(smtPoly21, smtPoly22)) -> Or(get_eq_constraint_extra smtPoly1 smtPoly21, get_eq_constraint_extra smtPoly1 smtPoly22)
    | (POr(smtPoly11, smtPoly12), _) -> Or(get_eq_constraint_extra smtPoly11 smtPoly2, get_eq_constraint_extra smtPoly12 smtPoly2)

  and get_eq_constraint_extra_extra poly1 poly2 = match poly1, poly2 with
    | (_, Real (0., _, inf_I, _)) -> Single (new polynomialConstraint (Eq (reduce poly1)))
    | (Real (0., _, inf_I, _), _) -> Single (new polynomialConstraint (Eq (reduce poly2)))
    | _ -> Single (new polynomialConstraint (Eq (reduce (Sub(poly1, poly2, inf_I, new IA.af2 0)))))   

  and get_constraint_term varTermMap functions variables varBindings = function 
    |TermQualIdentifier (_ , qualidentifier1) -> 
      get_constraint_qualidentifier varTermMap  functions variables varBindings qualidentifier1
    |TermQualIdTerm (_ , qualidentifier2 , termqualidterm_term_term563) ->
      let qualidentifier_string = get_string_qualidentifier qualidentifier2 in
      if qualidentifier_string = "<" then 
        let polys = get_polys_termqualidterm_term_term56 varTermMap functions variables varBindings termqualidterm_term_term563 in
        get_le_constraint polys
      else if qualidentifier_string = "<=" then
        let polys = get_polys_termqualidterm_term_term56 varTermMap functions variables varBindings termqualidterm_term_term563 in
        get_leq_constraint polys 
      else if qualidentifier_string = ">" then
        let polys = get_polys_termqualidterm_term_term56 varTermMap functions variables varBindings termqualidterm_term_term563 in
        get_gr_constraint polys
      else if qualidentifier_string = ">=" then
        let polys = get_polys_termqualidterm_term_term56 varTermMap functions variables varBindings termqualidterm_term_term563 in
        get_geq_constraint polys
      else if qualidentifier_string = "=" then
        try 
          let polys = get_polys_termqualidterm_term_term56 varTermMap functions variables varBindings termqualidterm_term_term563 in
          get_eq_constraint polys
        with Failure _ -> 
          let constraints = get_constraints_termqualidterm_term_term56 varTermMap functions variables varBindings termqualidterm_term_term563 in
          let iffConstraint = get_iff_constraint constraints in
          [iffConstraint]
      else if qualidentifier_string = "and" then
        let constraints = get_constraints_termqualidterm_term_term56 varTermMap functions variables varBindings termqualidterm_term_term563 in
        let andConstraint = get_and_constraint constraints in
        [andConstraint]
      else if qualidentifier_string = "or" then
        let constraints = get_constraints_termqualidterm_term_term56 varTermMap functions variables varBindings termqualidterm_term_term563 in
        let orConstraint = get_or_constraint constraints in
        [orConstraint]
      else if qualidentifier_string = "not" then
        let constraints = get_constraints_termqualidterm_term_term56 varTermMap functions variables varBindings termqualidterm_term_term563 in
        let notConstraint = get_not_constraint constraints in
        [notConstraint]
      else if qualidentifier_string = "ite" then
        let constraints = get_constraints_termqualidterm_term_term56 varTermMap functions variables varBindings termqualidterm_term_term563 in
        let iteConstraint = get_ite_constraint constraints in
        [iteConstraint]
      else if StringMap.mem qualidentifier_string functions then
        let (arguments, funcSort, funcDef) = StringMap.find qualidentifier_string functions in
        if funcSort = "Bool" then
          let varTermMap = get_varTermMap_termqualidterm_term_term56 arguments termqualidterm_term_term563 in
          get_constraint_term varTermMap functions variables varBindings funcDef
        else   
          raise (Failure ("Undefined predicate symbols: " ^ qualidentifier_string))
      else raise (Failure ("Undefined predicate symbols: " ^ qualidentifier_string))
    |TermLetTerm (_ , termletterm_term_varbinding584 , term6) ->  
      let varBindings = get_let_termletterm_term_varbinding58 varBindings termletterm_term_varbinding584 in
      get_constraint_term varTermMap functions variables varBindings term6
    | _ -> [] (*raise (Failure "Not supported syntax line 138")*)

  and get_and_constraint = function 
    | [] -> raise (Failure "Need arguments for and") 
    | [constraint1] -> constraint1
    | constraint1 :: constraint2 :: remainingConstraints -> get_and_constraint((And(constraint1, constraint2)) :: remainingConstraints)
    
  and get_iff_constraint = function 
    | [] -> raise (Failure "Need arguments for =") 
    | [constraint1] -> raise (Failure "Need arguments for =")
    | [constraint1; constraint2] -> And(Or(not_of_boolCons constraint1, constraint2), Or(constraint1, not_of_boolCons constraint2))
    | constraint1 :: constraint2 :: remainingConstraints -> And(get_iff_constraint [constraint1; constraint2], get_iff_constraint (constraint2::remainingConstraints) )
    
  and get_or_constraint = function 
    | [] -> raise (Failure "Need arguments for or") 
    | [constraint1] -> constraint1
    | constraint1 :: constraint2 :: remainingConstraints -> get_or_constraint((Or(constraint1, constraint2)) :: remainingConstraints)  

  and get_not_constraint = function 
    | [] -> raise (Failure "Need arguments for not") 
    | [constraint1] -> 
      (*print_endline ("not of " ^ string_infix_of_constraints constraint1);
      print_endline ("is " ^ string_infix_of_constraints (not_of_boolCons constraint1));
      flush stdout;*)
      not_of_boolCons constraint1
    | _ -> raise (Failure "Extra arguments for not") 

  and get_ite_constraint = function 
    | [constraint1; constraint2; constraint3] -> And(Or(constraint1, constraint3), Or(not_of_boolCons constraint1, constraint2))
    | _ -> raise (Failure "wrong number of arguments for ite") 

  and get_let_termletterm_term_varbinding58 varBindings = function
    |(_,[]) -> varBindings
    | (d , (varbinding1)::termletterm_term_varbinding582) ->  
      let varBindings = get_let_varbinding varBindings varbinding1 in
      get_let_termletterm_term_varbinding58 varBindings (d,termletterm_term_varbinding582)
      
  and get_let_varbinding varBindings = function 
    | VarBindingSymTerm (_ , symbol2 , term3) -> 
      let var = get_string_symbol symbol2 in 
      StringMap.add var term3 varBindings
                

  and get_constraints_command varTermMap functions variables = function 
    | CommandAssert (_ , term3) -> get_constraint_term varTermMap functions variables StringMap.empty term3
    | _  -> []  

  and get_constraints_commands varTermMap functions variables = function  
    |(_,[]) ->   []
    | (d , (command1)::commands_commands_command302) -> 
      (get_constraints_command varTermMap functions variables command1) @ (get_constraints_commands varTermMap functions variables (d, commands_commands_command302))
    

  and get_constraints varTermMap functions variables = function
    |Commands (_ , commands_commands_command301) ->
      let constraints = get_constraints_commands varTermMap functions variables commands_commands_command301 in
      let mergedBoolExp = get_boolExp_from_list constraints in
      (*print_endline (string_prefix_of_constraints mergedBoolExp);
      flush stdout;*)
      [mergedBoolExp]
      
      
  and get_variables = function   
    |Commands (_ , commands_commands_command301) ->
      get_variables_commands commands_commands_command301


  and get_functions = function
    |Commands (_ , commands_commands_command301) ->
      get_functions_commands commands_commands_command301
          
  and get_variables_commands = function  
    |(_,[]) ->   StringMap.empty
    | (d , (command1)::commands_commands_command302) -> 
      let merge_variables var sort1 sort2 = match sort1, sort2 with
        | Some varSort1, _ -> Some varSort1
        | _, Some varSort2 -> Some varSort2
        | None, None -> None
      in
      StringMap.merge merge_variables (get_variables_command command1) (get_variables_commands (d, commands_commands_command302))  

  and get_functions_commands = function  
    |(_,[]) ->   StringMap.empty
    | (d , (command1)::commands_commands_command302) -> 
      let merge_functions funcSym def1 def2 = match def1, def2 with
        | Some def11, _ -> Some def11
        | _, Some def21 -> Some def21
        | None, None -> None
      in
      StringMap.merge merge_functions (get_functions_command command1) (get_functions_commands (d, commands_commands_command302))  
    
  and get_variables_command = function 
    | CommandDeclareFun (_ , symbol3 , commanddeclarefun_command_sort135 , sort7) ->  
      let variable = get_string_symbol symbol3 in
      let varSort = get_sort sort7 in 
      StringMap.singleton variable varSort
    | _  -> StringMap.empty

  and get_functions_command = function 
    |CommandDefineFun (_ , symbol3 , commanddefinefun_command_sortedvar155 , sort7 , term8) ->  
      let funcSym = get_string_symbol symbol3 in
      let arguments = get_arguments_commanddefinefun_command_sortedvar15 commanddefinefun_command_sortedvar155 in
      let funcSort = get_sort sort7 in
      StringMap.singleton funcSym (arguments, funcSort, term8)
    | _  -> StringMap.empty

  and get_arguments_commanddefinefun_command_sortedvar15 = function
    |(_,[]) -> []
    | (d , (sortedvar1)::commanddefinefun_command_sortedvar152) ->  
      (get_sortedvar sortedvar1) :: (get_arguments_commanddefinefun_command_sortedvar15 (d,commanddefinefun_command_sortedvar152))

  and get_sortedvar = function 
    |SortedVarSymSort (_ , symbol2 , sort3) -> 
      let var = get_string_symbol symbol2 in
      let varSort = get_sort sort3 in
      (var, varSort)
    
  and get_sort = function  
    |SortIdentifier (_ , identifier1) ->  get_string_identifier identifier1 
    |SortIdSortMulti (_ , identifier2 , sortidsortmulti_sort_sort443) -> raise (Failure "Wrong Input when declaring function symbols")
    
  and get_boolExp_from_list = function
    | [] -> raise (Failure "No constraint")
    | [boolExp] -> boolExp
    | boolExp :: remainingBoolExps -> And(boolExp, get_boolExp_from_list remainingBoolExps)
    
  and merge_interval var value1 value2 = match value1, value2 with
    | Some intv1, Some intv2 -> Some (inter_I_I intv1 intv2)
    | Some intv1, None -> Some intv1
    | None, Some intv2 -> Some intv2
    | None, None -> None


  (*get miniSat form of interval constraints*)
  let genSatForm fileName lb ub logic inFile =
    let ic = open_in fileName in
    let lexbuf = Lexing.from_channel ic in  
    let parsed =  Smtlib_parse.main Smtlib_lex.token lexbuf in
    let functions = match parsed with
      | None -> StringMap.empty
      | Some(x) -> get_functions x;
    in
    let variables = match parsed with
      | None -> StringMap.empty
      | Some(x) -> get_variables x;
    in
    (*print_int (StringMap.cardinal variables);
    let print_var var varSort =
      print_endline (var ^ " " ^ varSort);
      flush stdout;
    in
    StringMap.iter print_var variables;
    flush stdout;*)
    let constraints = match parsed with
      | None -> []
      | Some(x) -> get_constraints StringMap.empty functions variables x;
    in
    (*print_endline "finished getting constraints";
    flush stdout;*)
    let boolCons = List.nth constraints 0 in
    (* print_endline (string_infix_of_constraints boolCons);
    flush stdout;
    raise (Failure "Tung dep trai"); *)
    let (miniSATExpr, index, miniSATCodesConstraintsMap, ma, isEquation, isNotEquation, _) = miniSATExpr_of_constraints boolCons 1 IntMap.empty logic StringMap.empty in 
    (*print_endline "finished getting miniSAT constraints";
    flush stdout;*)
    (* miniSATExpr_of_constraints is defined in PolynomialConstraint.ml *)
    
    (* convert miniSATExpr into CNF *)
    let cnfMiniSATExpr = cnf_of_miniSATExpr miniSATExpr in
    (*print_endline "finished getting cnf constraints";
    flush stdout;*)
    let varsSet = get_varsSet_boolCons boolCons in
    let nVars = VariablesSet.cardinal varsSet in
    let totalMiniSATVars = index - 1 in
    (* convert cnfMiniSATExpr to string under the format of miniSAT input *)
    let oc = open_out inFile in
    let miniSATClauses = get_clauses cnfMiniSATExpr true in
    Printf.fprintf oc "p cnf %d %d\n" totalMiniSATVars miniSATClauses;   
    write_cnf_toFile cnfMiniSATExpr true oc; (* write_cnf_toFile is defined in ast.ml *)
    (*print_endline "finished getting cnf constraints string";
    flush stdout;*)
    (*print_endline cnfMiniSATExprString;
    flush stdout;*)
    
    (*print_int (IntMap.cardinal miniSATCodesConstraintsMap);
    flush stdout;*)
    (* get intervals from constraints *) 
    (* let varsIntvsMap = get_intervals_from_constraints boolCons in *)
     
    (* (* generate the minisat constraints for intervals *)
    let rec genMiniSATIntvString index nVars =
      if nVars = 0 then ""
      else (string_of_int index) ^ " 0\n" ^ genMiniSATIntvString (index + 1) (nVars - 1)
    in
    let miniSATIntvString = genMiniSATIntvString index nVars in
    Printf.fprintf oc "%s" miniSATIntvString; *)
    close_out oc;
    let rec generateIntvInfo var currentVarsIntvsMap =
      StringMap.add var {low=lb; high=ub} currentVarsIntvsMap
    in
    let varsIntvsMap = VariablesSet.fold generateIntvInfo varsSet StringMap.empty in
    (*Compute the total of variables for SAT encoding*)
    (*let max_bound = getMB 0.0 eIntv in
    let para = int_of_float (max_bound /. esl) in
    (*let totalVars = iLit * 4 * para in*)
    let totalVars = 
      if max_bound = infinity then 10000
      else index + 2* nVars * (sum_total_var 1 para)
    in*)
    (*let sTrivialClause = "-" ^ string_of_int totalVars ^ " " ^string_of_int totalVars^ " 0" in*)

    (nVars, "", varsIntvsMap, miniSATCodesConstraintsMap, index-1, ma, isEquation, isNotEquation)

  
  (* compute the list of chosen constraints and *)
  let rec getConsAndIntv solution miniSATCodesConstraintsMap chosenPolyConstraints = match solution with
    |[] -> chosenPolyConstraints
    |h::t ->
      let absH = abs h in
      (*print_endline ("Getting constraint number: " ^ string_of_int h);
      flush stdout;*)
      try
        let nextChosenPolyConstraint = IntMap.find absH miniSATCodesConstraintsMap in
        
        (*print_endline ("Got constraint: " ^ nextChosenPolyConstraint#to_string_infix);
        flush stdout;*)
        let newChosenPolyConstraints = (*insertion_sort_polyCons nextChosenPolyConstraint chosenPolyConstraints in (* insertion_sort_polyCons is defined in PolynomialConstraint.ml *)*)
          if h > 0 then                                
            nextChosenPolyConstraint::chosenPolyConstraints 
          else 
            (print_int h;
            print_endline "";
            flush stdout;
            let newPolyConstraint = not_of_polyConstraint nextChosenPolyConstraint#get_constraint in
            let polyCons = new polynomialConstraint(newPolyConstraint) in
            polyCons#set_miniSATCode h;
            polyCons#set_logic nextChosenPolyConstraint#get_logic;
            polyCons::chosenPolyConstraints)
          in
        (*print_endline "Finish adding constraint";
        flush stdout;*)
        getConsAndIntv t miniSATCodesConstraintsMap newChosenPolyConstraints
      with Not_found -> getConsAndIntv t miniSATCodesConstraintsMap chosenPolyConstraints
      

  let rec check_procedure varsIntvsMapPrioritiesMaps (polyConstraints:PolynomialConstraint.polynomialConstraint list) 
              unsatPolyConstraintsCodes strTestUS iaTime testingTime usTime parsingTime decompositionTime remainingTime =
    if FloatMap.is_empty varsIntvsMapPrioritiesMaps then
      let get_unsatcore miniSATCode currentUnsatCore = 
        "-" ^ string_of_int miniSATCode ^ " " ^ currentUnsatCore
      in
      let unsatCore = IntSet.fold get_unsatcore unsatPolyConstraintsCodes "0" in
      print_string "UNSAT core: ";
      print_endline unsatCore;
      flush stdout;
      (-1, unsatCore, "", "", "", "", "", "", "", iaTime, testingTime, usTime, parsingTime, decompositionTime) 
    else
      let startTime = Sys.time() in 
      let (esl, varsIntvsMaps) = FloatMap.max_binding varsIntvsMapPrioritiesMaps in
      let varsIntvsMap = List.hd varsIntvsMaps in
      (* print_endline "---------------------------NEW----------------------";
      print_endline ("esl: " ^ string_of_float esl);
      print_endline (log_intervals varsIntvsMap);
      flush stdout; *)
      let varsIntvsMapPrioritiesMaps = match List.tl varsIntvsMaps with
        | [] -> FloatMap.remove esl varsIntvsMapPrioritiesMaps
        | _ -> FloatMap.add esl (List.tl varsIntvsMaps) varsIntvsMapPrioritiesMaps
      in
      let (res, unsatPolyConstraintsCodes, uk_cl, validPolyConstraints, varsIntvsMap, iaTime, usTime) = icp unsatPolyConstraintsCodes [] [] 
                                                          polyConstraints varsIntvsMap esl iaTime usTime (remainingTime -. Sys.time() +. startTime) in
      (* print_endline ("EndICP, result: " ^ string_of_int res);
      flush stdout; *)
      if (res = -1) then 
        check_procedure varsIntvsMapPrioritiesMaps polyConstraints unsatPolyConstraintsCodes strTestUS
                                                     iaTime testingTime usTime parsingTime decompositionTime remainingTime
      else if (res = 1) then (*if all clauses are satisfiable*)
        let intvLog = log_intervals varsIntvsMap in
        let validPolyConstraints = List.rev validPolyConstraints in
        let iaLog = log_ia validPolyConstraints in
        (res, "", "", intvLog ^  iaLog, "", "", "", "", "", 
              iaTime, testingTime, usTime, parsingTime, decompositionTime)
      else (*if unknown, testing will be implemented here*)(
        (*let uk_cl = List.rev uk_cl in (* reverse the list so that the apis are sorted based on variables dependency *)*)
        (*print_endline("IA Unkown constraints: " ^ string_infix_of_polynomialConstraints uk_cl); (* string_infix_of_polynomialConstraints is defined in polynomialConstraint.ml *)
        dflush stdout;*)
        if (uk_cl = []) then (*This case will never happen*)
          (res, "", "", "", "", "", "", "", "", iaTime, testingTime, usTime, parsingTime, decompositionTime)
        else (
          (* print_endline "Start Testing";
          flush stdout; *)
          let startTestingTime = Sys.time() in
          let (tc, sTest, clTest_US, a, b) = 
            test uk_cl varsIntvsMap (remainingTime -. Sys.time() +. startTime) (* test is defined in Testing.ml *)
            (* test_icp uk_cl varsIntvsMap StringMap.empty VariablesSet.empty esl (remainingTime -. Sys.time() +. startTime) (* test is defined in Testing.ml *) *)
            
          in
          (*print_endline ("UNSAT constraints num: " ^ string_of_int b);
          flush stdout;*)
          (*print_endline ("SAT: " ^ assignments_toString tc);*)
          (*let (sTest, clTest_US, a) = evalTest assIntv uk_cl checkVarID strTestUS in*)
          (*let (tc, sTest, clTest_US, a) =  search_tc2 uk_cl assIntv strTestUS esl in *)
          (* print_endline ("End Testing, result: " ^ string_of_int sTest);
          flush stdout; *)
          let testingTime = testingTime +. Sys.time() -. startTestingTime in
          if (sTest = 1) then (
           let intvLog = log_intervals varsIntvsMap in
           let validPolyConstraints = List.rev validPolyConstraints in
           let iaLog = log_ia validPolyConstraints in

           let assignmentsLog = log_assignment a in (* log_assignment is in Assignments.ml *)
           let testLog = log_test uk_cl in
           let assignmentsString = string_of_assignment a in
           let uk_cl_string = string_postfix_of_polyConstraints uk_cl in
           (sTest, assignmentsString , uk_cl_string, intvLog ^ iaLog ^ assignmentsLog ^ testLog, "", "", "", "", 
                string_postfix_of_polyConstraints validPolyConstraints ^ " ; " ^ string_of_intervals varsIntvsMap
                , iaTime, testingTime, usTime, parsingTime, decompositionTime)
          )
          else
          (
           let startDecompositionTime = Sys.time() in
           (* If the uk_cl are equalities, then we implement some tricks for solving them. *)
           let (isEqualitiesSAT, unsatPolyConstraints) = 
             if can_apply_imvt uk_cl && is_all_equations uk_cl then (* is_all_equalities is defined in ast.ml *)
               check_equalities uk_cl varsIntvsMap VariablesSet.empty (* check_equalities is defined in Equalities_handler.ml *)
             else (false, uk_cl)
           in
             if isEqualitiesSAT then 
               let intvLog = log_intervals varsIntvsMap in
               let validPolyConstraints = List.rev validPolyConstraints in
               let iaLog = log_ia validPolyConstraints in
               let assignmentsLog = log_assignment a in (* log_assignment is in Assignments.ml *)
               let testLog = log_test uk_cl in
               (1, "", "", intvLog ^ iaLog ^ assignmentsLog ^ testLog ^ (get_allLogs uk_cl), "", 
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
                (*print_endline(bool_expr_list_to_infix_string decomposedExpr);*)
                flush stdout;*)
                (*let testUNSATPolyCons = List.hd clTest_US in
                let decomposedPolyConstraints = testUNSATPolyCons :: uk_cl in*)
                let maxDecompose = 1 in (* only $maxDecompose are allowed to decomposed, the priority is based on sensitivity *)
                let (unsatPolyConstraintsCodes, varsIntvsMapPrioritiesMaps) = 
                  (*let (newInterval, newLearn, newBumpVars, isDecomposed) = decompose_unsat_detection (List.hd decomposedExpr) assIntv dIntv checkVarID nextMiniSATCode esl in*)
                  (*let (newInterval, newLearn, newBumpVars, isDecomposed) = decompose_list_unsat_detection uk_cl assIntv dIntv checkVarID nextMiniSATCode esl in
                  if isDecomposed then 
                    (newInterval, newLearn, newBumpVars, isDecomposed)
                  else*)
                    dynamicDecom varsIntvsMap unsatPolyConstraintsCodes varsIntvsMapPrioritiesMaps (List.hd decomposedExpr) 
                                  uk_cl maxDecompose esl (remainingTime -. Sys.time() +. startTime) in

                    (* dynamicDecom_noStrategy varsIntvsMap varsIntvsMapPrioritiesMaps (List.hd decomposedExpr) 
                                  uk_cl maxDecompose esl (remainingTime -. Sys.time() +. startTime) in *)
                (*print_endline "after decomposed";
                flush stdout;*)
                (* print_endline ("New Boxes: \n" ^ string_of_varsIntvsPrioritiesMap varsIntvsMapPrioritiesMaps);
                flush stdout; *)
                (* let varsIntvsMapPrioritiesMaps = FloatMap.empty in *)
                let decompositionTime = decompositionTime +. Sys.time() -. startDecompositionTime in
                check_procedure varsIntvsMapPrioritiesMaps polyConstraints unsatPolyConstraintsCodes strTestUS
                                                            iaTime testingTime usTime parsingTime decompositionTime remainingTime                
             )
          )
        )
      )

  (*=========================== START DYNTEST =======================================*)  
  (*dynTest: Interval arithmetic, Testing and Dynamic interval decomposition*)
  let dynTest varsIntvsMap miniSATCodesConstraintsMap clausesNum strCheck ia esl strTestUS iaTime 
                                          testingTime usTime parsingTime decompositionTime remainingTime =
  (* 
    let add_intv var currentVarsIntvsMap = 
      StringMap.add var {low = neg_infinity; high = infinity} currentVarsIntvsMap
    in  
    let varsIntvsMap = VariablesSet.fold add_intv varsSet StringMap.empty in *)
    let varsIntvsMapPrioritiesMaps = FloatMap.singleton esl [varsIntvsMap] in

    (*let tmp1 = new af2 2 in
    tmp1#set_a {low= (~-.) 75.;high= (~-.) 75.};
    let nar = Array.create 2 {low=0.;high=0.} in
    Array.set nar 0 {low=(~-.)200.;high=(~-.)200.};
    Array.set nar 1 {low=(~-.)75.;high=(~-.)75.};
    tmp1#set_ar nar;
    tmp1#set_kp 50.;
    tmp1#set_kn 125.;
    tmp1#set_k 375.;
    
    let tmp2 = new af2 2 in
    tmp2#set_a {low=(~-.)3.e20;high=1.e20};
    let nar = Array.create 2 {low=0.;high=0.} in
    Array.set nar 0 {low=5.e20	;high=6.e20};
    Array.set nar 1 {low=2.e20;high=3.e20};
    tmp2#set_ar nar;
    tmp2#set_kp 3.e20;
    tmp2#set_kn 1.e20;
    tmp2#set_k 3.;
    
    let tmp = tmp1#add tmp2 in
    tmp#print_form;*)
    
    (* let tmp1 = {low= 0.;high= 0.} in
    let tmp2 = {low= 0. ;high= 10.} in
    let tmp = tmp1 /$ tmp2 in
    print_I tmp;
    flush stdout;
    raise (Failure "Tung"); *)
    
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
    let polyConstraints = getConsAndIntv solution miniSATCodesConstraintsMap [] in

    (*let polyConstraints = List.rev polyConstraints in*)
    (* print_endline(string_infix_of_polynomialConstraints polyConstraints); (* In PolynomialConstraint.ml *)
    flush stdout; *)
    (* raise (Failure "Tung dep trai"); *)
    (* print_endline ("\nIntervals: \n" ^ log_intervals varsIntvsMiniSATCodesMap); (* string_of_intervals is defined in Assignments.ml *)
    flush stdout; *)
    let parsingTime = parsingTime +. Sys.time() -. startTime in
    (*print_endline "Start IA";
    flush stdout;*)
    check_procedure varsIntvsMapPrioritiesMaps polyConstraints IntSet.empty strTestUS 
              iaTime testingTime usTime parsingTime decompositionTime remainingTime
    
  (* ================================= END OF DYNTEST ==========================================*) 
end


(*===========================================================*)
(* Export those functions to C/C++ *)
(*===========================================================*)
let _ = Callback.register "caml_genSatForm" Caml.genSatForm;;
let _ = Callback.register "caml_dynTest" Caml.dynTest;
