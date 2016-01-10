open Ast
open Smtlib_syntax
open Printf

let rec get_string_symbol = function
  |Symbol (_ , str1) -> str1
  |SymbolWithOr (_ , str1) -> str1

and get_string_identifier = function 
  |IdSymbol (_ , symbol1) ->  get_string_symbol symbol1 
  |IdUnderscoreSymNum (_ , symbol3 , idunderscoresymnum_identifier_numeral334) -> raise (Failure "Not supported syntax line 125")

and get_string_qualidentifier = function
  |QualIdentifierId (_ , identifier1) ->  get_string_identifier identifier1
  |QualIdentifierAs (_ , identifier3 , sort4) ->  raise (Failure "Not supported syntax line 129")

and get_poly_specCons = function 
  |SpecConstsDec (_ , str1) ->  
    if !theory = intTheory then
      let errorMessage = "Int theory does not accept a decimal constant" in
      print_endline errorMessage;
      flush stdout;
      raise (Failure errorMessage);
    else
      SPoly (Cons (str1, realType))
  |SpecConstNum (_ , str1) ->  
    if !theory = realTheory then
      SPoly(Cons (str1, realType))
    else 
      SPoly(Cons (str1, intType))
  |SpecConstString (_ , str1) ->  
    let errorMessage = "Int/Real theories do not accept string constants" in
    print_endline errorMessage;
    flush stdout;
    raise (Failure errorMessage)
  |SpecConstsHex (_ , str1) ->  
    let errorMessage = "Int/Real theories do not accept hex constants" in
    print_endline errorMessage;
    flush stdout;
    raise (Failure errorMessage)
  |SpecConstsBinary (_ , str1) ->  
    let errorMessage = "Int/Real theories do not accept binary constants" in
    print_endline errorMessage;
    flush stdout;
    raise (Failure errorMessage)



and get_poly_symbol varTermMap functions variables varsPolysMap varsConstraintsMap = function 
  |Symbol (_ , str1) -> 
    (try
      StringMap.find str1 varsPolysMap
      (* let term = StringMap.find str1 varBindings in
      get_poly_term varTermMap functions variables varBindings term *)
    with Not_found -> 
      (try
        let term = StringMap.find str1 varTermMap in
        get_poly_term StringMap.empty functions variables varsPolysMap varsConstraintsMap term
      with Not_found -> 
        (try
          let varSort = StringMap.find str1 variables in
          if varSort = intType || varSort = realType then SPoly(Var (str1, varSort))
          else raise (Failure "Wrong input")
        with Not_found -> raise (Failure ("Need Real/Int variable: " ^ str1)))
      )
    )
  |SymbolWithOr (_ , str1) -> 
    (try
      StringMap.find str1 varsPolysMap
    with Not_found ->  
      (try
        let term = StringMap.find str1 varTermMap in
        get_poly_term StringMap.empty functions variables varsPolysMap varsConstraintsMap term
      with Not_found -> 
        (try
          let varSort = StringMap.find str1 variables in
          if (varSort = intType || varSort = realType) then 
            SPoly(Var (str1, varSort))
          else raise (Failure "Wrong input")
        with Not_found -> raise (Failure "Need Real/Int variable 2")
        )
      )
    )

and get_constraint_symbol varTermMap functions variables varsPolysMap varsConstraintsMap = function 
  |Symbol (_ , str1) -> 
    (try
      StringMap.find str1 varsConstraintsMap
      (* let term = StringMap.find str1 varBindings in
      get_constraint_term varTermMap functions variables varBindings term *)
    with  
      Not_found -> 
        ( try
            let varSort = StringMap.find str1 variables in
            if varSort = boolType then [BVar str1]
            else raise (Failure ("Unexpected predicate symbol in get_constraint_symbol 1: " ^ str1))
          with Not_found -> 
            (try
              let term = StringMap.find str1 varTermMap in
              get_constraint_term StringMap.empty functions variables varsPolysMap varsConstraintsMap term
            with Not_found -> 
              if str1 = "true" then [True]
              else if str1 = "false" then [False]
              else raise (Failure "Wrong input  in get_constraint_symbol 2")
            )
        )
    )
  |SymbolWithOr (_ , str1) -> 
    (try
      StringMap.find str1 varsConstraintsMap
    with  
      Not_found -> 
        ( try
            let varSort = StringMap.find str1 variables in
            if varSort = boolType then [BVar str1]
            else raise (Failure "Wrong input in get_constraint_symbol 3")
          with Not_found -> 
            (try
              let term = StringMap.find str1 varTermMap in
              get_constraint_term StringMap.empty functions variables varsPolysMap varsConstraintsMap term
            with Not_found -> 
              if str1 = "true" then [True]
              else if str1 = "false" then [False]
              else raise (Failure "Wrong input in get_constraint_symbol 4")
            )
        )
    )

and get_poly_identifier varTermMap functions variables varsPolysMap varsConstraintsMap = function 
  |IdSymbol (_ , symbol1) ->  get_poly_symbol varTermMap functions variables varsPolysMap varsConstraintsMap symbol1 
  |IdUnderscoreSymNum (_ , symbol3 , idunderscoresymnum_identifier_numeral334) -> raise (Failure "Not supported syntax line 165")

and get_constraint_identifier varTermMap functions variables varsPolysMap varsConstraintsMap = function 
  |IdSymbol (_ , symbol1) ->  get_constraint_symbol varTermMap functions variables varsPolysMap varsConstraintsMap symbol1 
  |IdUnderscoreSymNum (_ , symbol3 , idunderscoresymnum_identifier_numeral334) -> raise (Failure "Not supported syntax line 200")  

and get_poly_qualidentifier varTermMap functions variables varsPolysMap varsConstraintsMap = function 
  |QualIdentifierId (_ , identifier1) ->  get_poly_identifier varTermMap functions variables varsPolysMap varsConstraintsMap identifier1
  |QualIdentifierAs (_ , identifier3 , sort4) ->  raise (Failure "Not supported syntax line 129")

and get_constraint_qualidentifier varTermMap functions variables varsPolysMap varsConstraintsMap = function 
  |QualIdentifierId (_ , identifier1) ->  get_constraint_identifier varTermMap functions variables varsPolysMap varsConstraintsMap identifier1
  |QualIdentifierAs (_ , identifier3 , sort4) ->  raise (Failure "Not supported syntax line 204")  

and get_mul_poly = function
  | [] -> raise (Failure "Need arguments for multiplication")
  | [poly] -> poly 
  | poly1::poly2::remainingPolys -> get_mul_poly ((get_mul_poly_extra poly1 poly2) :: remainingPolys)
  
and get_mul_poly_extra smtPoly1 smtPoly2 = match smtPoly1, smtPoly2 with
  | (SPoly(poly1), SPoly(poly2)) ->  
    let polType = get_type_polyExprs poly1 poly2 in
    SPoly (Mul(poly1, poly2, polType))
  | (SPoly(poly1), Poly (boolConstraint2, poly2)) -> 
    let polType = get_type_polyExprs poly1 poly2 in
    Poly (boolConstraint2, Mul(poly1, poly2, polType))
  | (Poly(boolConstraint1, poly1), SPoly(poly2)) -> 
    let polType = get_type_polyExprs poly1 poly2 in
    Poly (boolConstraint1, Mul(poly1, poly2, polType))
  | (Poly(boolConstraint1, poly1), Poly (boolConstraint2, poly2)) -> 
    let polType = get_type_polyExprs poly1 poly2 in
    Poly(And(boolConstraint1, boolConstraint2), Mul(poly1, poly2, polType))
  | (POr(smtPoly11, smtPoly12), _) -> 
    POr (get_mul_poly_extra smtPoly11 smtPoly2, get_mul_poly_extra smtPoly12 smtPoly2)
  | (_, POr(smtPoly21, smtPoly22)) -> 
    POr (get_mul_poly_extra smtPoly1 smtPoly21, get_mul_poly_extra smtPoly1 smtPoly22)

and get_add_poly = function
  | [] -> raise (Failure "Need arguments for addition") 
  | [poly] -> poly
  | poly1::poly2::remainingPolys -> get_add_poly((get_add_poly_extra poly1 poly2) :: remainingPolys)

and get_add_poly_extra smtPoly1 smtPoly2 = match smtPoly1, smtPoly2 with
  | (SPoly(poly1), SPoly(poly2)) -> 
    let polType = get_type_polyExprs poly1 poly2 in
    SPoly (Add(poly1, poly2, polType))
  | (SPoly(poly1), Poly (boolConstraint2, poly2)) -> 
    let polType = get_type_polyExprs poly1 poly2 in
    Poly (boolConstraint2, Add(poly1, poly2, polType))
  | (Poly(boolConstraint1, poly1), SPoly(poly2)) -> 
    let polType = get_type_polyExprs poly1 poly2 in
    Poly (boolConstraint1, Add(poly1, poly2, polType))
  | (Poly(boolConstraint1, poly1), Poly (boolConstraint2, poly2)) -> 
    let polType = get_type_polyExprs poly1 poly2 in
    Poly(And(boolConstraint1, boolConstraint2), Add(poly1, poly2, polType))
  | (POr(smtPoly11, smtPoly12), _) -> 
    POr (get_add_poly_extra smtPoly11 smtPoly2, get_add_poly_extra smtPoly12 smtPoly2)
  | (_, POr(smtPoly21, smtPoly22)) -> 
    POr (get_add_poly_extra smtPoly1 smtPoly21, get_add_poly_extra smtPoly1 smtPoly22)

and get_mod_poly = function
  | [poly1; poly2] -> get_mod_poly_extra poly1 poly2
  | _ -> raise (Failure "Wrong number of arguments for Modulo") 

and get_mod_poly_extra smtPoly1 smtPoly2 = match smtPoly1, smtPoly2 with
  | (SPoly(poly1), SPoly(poly2)) -> 
    SPoly (Mod(poly1, poly2))
  | (SPoly(poly1), Poly (boolConstraint2, poly2)) -> 
    Poly (boolConstraint2, Mod(poly1, poly2))
  | (Poly(boolConstraint1, poly1), SPoly(poly2)) -> 
    Poly (boolConstraint1, Mod(poly1, poly2))
  | (Poly(boolConstraint1, poly1), Poly (boolConstraint2, poly2)) -> 
    Poly(And(boolConstraint1, boolConstraint2), Mod(poly1, poly2))
  | (POr(smtPoly11, smtPoly12), _) -> 
    POr (get_mod_poly_extra smtPoly11 smtPoly2, get_mod_poly_extra smtPoly12 smtPoly2)
  | (_, POr(smtPoly21, smtPoly22)) -> 
    POr (get_mod_poly_extra smtPoly1 smtPoly21, get_mod_poly_extra smtPoly1 smtPoly22)
  
and get_div_poly = function
  | [] -> raise (Failure "Need arguments for addition")
  | [poly] -> poly
  | poly1::poly2::remainingPolys -> get_div_poly((get_div_poly_extra poly1 poly2) :: remainingPolys)

and get_div_poly_extra smtPoly1 smtPoly2 = match smtPoly1, smtPoly2 with
  | (SPoly(poly1), SPoly(poly2)) -> 
    let polType = get_type_polyExprs poly1 poly2 in
    SPoly (Div(poly1, poly2, polType))
  | (SPoly(poly1), Poly (boolConstraint2, poly2)) -> 
    let polType = get_type_polyExprs poly1 poly2 in
    Poly (boolConstraint2, Div(poly1, poly2, polType))
  | (Poly(boolConstraint1, poly1), SPoly(poly2)) -> 
    let polType = get_type_polyExprs poly1 poly2 in
    Poly (boolConstraint1, Div(poly1, poly2, polType))
  | (Poly(boolConstraint1, poly1), Poly (boolConstraint2, poly2)) -> 
    let polType = get_type_polyExprs poly1 poly2 in
    Poly(And(boolConstraint1, boolConstraint2), Div(poly1, poly2, polType))
  | (POr(smtPoly11, smtPoly12), _) -> 
    POr (get_div_poly_extra smtPoly11 smtPoly2, get_div_poly_extra smtPoly12 smtPoly2)
  | (_, POr(smtPoly21, smtPoly22)) -> 
    POr (get_div_poly_extra smtPoly1 smtPoly21, get_div_poly_extra smtPoly1 smtPoly22)

and get_minus_poly = function
  | [] -> raise (Failure "Need arguments for Subtraction") 
  | [poly] -> get_minus_single_poly poly
  | polys -> get_minus_poly_extra polys

and get_minus_single_poly = function
  | SPoly(poly) -> 
    let polType = get_type_polyExpr poly in 
    SPoly(SSub(poly, polType))
  | Poly(boolConstraint, poly) -> 
    let polType = get_type_polyExpr poly in
    Poly(boolConstraint, SSub (poly, polType))
  | POr(smtPoly1, smtPoly2) -> POr(get_minus_single_poly smtPoly1, 
                                   get_minus_single_poly smtPoly2)

and get_minus_poly_extra = function 
  | [] -> raise (Failure "Need arguments for Subtraction") 
  | [poly] -> poly
  | poly1::poly2::remainingPolys -> get_minus_poly_extra(
                                                  (get_minus_poly_extra_extra poly1 poly2) 
                                                   :: remainingPolys)       

and get_minus_poly_extra_extra smtPoly1 smtPoly2 = match smtPoly1, smtPoly2 with
  | (SPoly(poly1), SPoly(poly2)) -> 
    let polType = get_type_polyExprs poly1 poly2 in
    SPoly (Sub(poly1, poly2, polType))
  | (SPoly(poly1), Poly (boolConstraint2, poly2)) -> 
    let polType = get_type_polyExprs poly1 poly2 in
    Poly (boolConstraint2, Sub(poly1, poly2, polType))
  | (Poly(boolConstraint1, poly1), SPoly(poly2)) -> 
    let polType = get_type_polyExprs poly1 poly2 in
    Poly (boolConstraint1, Sub(poly1, poly2, polType))
  | (Poly(boolConstraint1, poly1), Poly (boolConstraint2, poly2)) -> 
    let polType = get_type_polyExprs poly1 poly2 in
    Poly(And(boolConstraint1, boolConstraint2), Sub(poly1, poly2, polType))
  | (POr(smtPoly11, smtPoly12), _) 
    -> POr (get_minus_poly_extra_extra smtPoly11 smtPoly2, get_minus_poly_extra_extra smtPoly12 smtPoly2)
  | (_, POr(smtPoly21, smtPoly22)) 
    -> POr (get_minus_poly_extra_extra smtPoly1 smtPoly21, get_minus_poly_extra_extra smtPoly1 smtPoly22)

and get_poly_term varTermMap functions variables varsPolysMap varsConstraintsMap = function
  |TermSpecConst (_ , specCons1) ->  
    get_poly_specCons specCons1
  |TermQualIdentifier (_ , qualidentifier1) -> 
    get_poly_qualidentifier varTermMap functions variables varsPolysMap varsConstraintsMap qualidentifier1
  |TermQualIdTerm (_ , qualidentifier2 , termqualidterm_term_term563) -> 
    let qualidentifier_string = get_string_qualidentifier qualidentifier2 in
    if qualidentifier_string = "*" then
      let polys = get_polys_termqualidterm_term_term56 varTermMap functions variables 
                          varsPolysMap varsConstraintsMap termqualidterm_term_term563 in
      get_mul_poly polys
    else if qualidentifier_string = "+" then
      let polys = get_polys_termqualidterm_term_term56 varTermMap functions variables 
                          varsPolysMap varsConstraintsMap termqualidterm_term_term563 in
      get_add_poly polys  
    else if qualidentifier_string = "/" then
      let polys = get_polys_termqualidterm_term_term56 varTermMap functions variables 
                          varsPolysMap varsConstraintsMap termqualidterm_term_term563 in
      get_div_poly polys
    else if qualidentifier_string = "-" then
      let polys = get_polys_termqualidterm_term_term56 varTermMap functions variables 
                          varsPolysMap varsConstraintsMap termqualidterm_term_term563 in
      get_minus_poly polys
    else if qualidentifier_string = "mod" then
      let polys = get_polys_termqualidterm_term_term56 varTermMap functions variables 
                          varsPolysMap varsConstraintsMap termqualidterm_term_term563 in
      get_mod_poly polys
    else if qualidentifier_string = "ite" then
      get_ite_polys_termqualidterm_term_term56 varTermMap functions variables 
                          varsPolysMap varsConstraintsMap termqualidterm_term_term563
    else if StringMap.mem qualidentifier_string functions then
      let (arguments, funcSort, funcDef) = StringMap.find qualidentifier_string functions in
      if funcSort = "Real" || funcSort = "Int" then
        let varTermMap = get_varTermMap_termqualidterm_term_term56 arguments termqualidterm_term_term563 in
        get_poly_term varTermMap functions variables varsPolysMap varsConstraintsMap funcDef
      else   
        raise (Failure ("Undefined predicate symbols: " ^ qualidentifier_string))
    else raise (Failure "Undefined Function Symbols")
  | _ -> raise (Failure "Undefined Suported Function Symbols")
   (*|TermLetTerm (_ , termletterm_term_varbinding584 , term6) ->  print_string "(";print_string " "; print_string "let";print_string " "; print_string "(";print_string " "; pp_termletterm_term_varbinding58 termletterm_term_varbinding584;print_string " "; print_string ")";print_string " "; pp_term term6;print_string " "; print_string ")"; () 
   |TermForAllTerm (_ , termforallterm_term_sortedvar604 , term6) ->  print_string "(";print_string " "; print_string "forall";print_string " "; print_string "(";print_string " "; pp_termforallterm_term_sortedvar60 termforallterm_term_sortedvar604;print_string " "; print_string ")";print_string " "; pp_term term6;print_string " "; print_string ")"; () 
   |TermExistsTerm (_ , termexiststerm_term_sortedvar624 , term6) ->  print_string "(";print_string " "; print_string "exists";print_string " "; print_string "(";print_string " "; pp_termexiststerm_term_sortedvar62 termexiststerm_term_sortedvar624;print_string " "; print_string ")";print_string " "; pp_term term6;print_string " "; print_string ")"; () 
   |TermExclimationPt (_ , term3 , termexclimationpt_term_attribute644) ->  print_string "(";print_string " "; print_string "!";print_string " "; pp_term term3;print_string " "; pp_termexclimationpt_term_attribute64 termexclimationpt_term_attribute644;print_string " "; print_string ")"; () 
   *)

and get_ite_polys_termqualidterm_term_term56 varTermMap functions (variables:(int Ast.StringMap.t)) 
             varsPolysMap varsConstraintsMap = function
  |(_,[]) -> raise (Failure "Wrong number of arguments for ite")
  | (d , (term1)::termqualidterm_term_term562) -> 
    let boolConstraints = get_constraint_term varTermMap functions variables 
                                       varsPolysMap varsConstraintsMap term1 in
    let boolConstraint = get_boolExp_from_list boolConstraints in  
    let smtPolys = get_polys_termqualidterm_term_term56 varTermMap functions variables 
             varsPolysMap varsConstraintsMap (d,termqualidterm_term_term562) in 
    (match smtPolys with 
      | [smtPoly1; smtPoly2] -> 
        (let rec get_ite_polys boolConstraint2 smtPoly = match smtPoly with
          | SPoly(poly) -> Poly(boolConstraint2, poly)
          | Poly(boolConstraint1, poly1) -> Poly(And (boolConstraint1, boolConstraint2), poly1)
          | POr(smtPoly11, smtPoly21) -> POr (get_ite_polys boolConstraint2 smtPoly11, 
                                      get_ite_polys boolConstraint2 smtPoly21)
        in

        POr(get_ite_polys boolConstraint smtPoly1, 
              get_ite_polys (Not(boolConstraint)) smtPoly2)
        )
      | _ -> raise (Failure "Wrong number of arguments for ite")
    )

and get_polys_termqualidterm_term_term56 varTermMap functions variables varsPolysMap varsConstraintsMap = function
  |(_,[]) ->   [] 
  | (d , (term1)::termqualidterm_term_term562) -> 
    let poly = get_poly_term varTermMap functions variables varsPolysMap varsConstraintsMap term1 in
    let remainingPolys = get_polys_termqualidterm_term_term56 varTermMap functions variables 
                                  varsPolysMap varsConstraintsMap (d,termqualidterm_term_term562) in 
    poly :: remainingPolys 

and get_constraints_termqualidterm_term_term56 varTermMap functions variables varsPolysMap varsConstraintsMap = function
  |(_,[]) ->   [] 
  | (d , (term1)::termqualidterm_term_term562) -> 
    let constraint1 = get_constraint_term varTermMap functions variables varsPolysMap varsConstraintsMap term1 in
    let remainingConstraints = get_constraints_termqualidterm_term_term56 varTermMap functions variables 
                                               varsPolysMap varsConstraintsMap (d,termqualidterm_term_term562) in 
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

and get_le_constraint polys variables = match polys with
  | [] ->  raise (Failure "Wrong Input")
  | [poly] -> []
  | poly1::poly2::remainingPolys -> (get_le_constraint_extra poly1 poly2 variables) :: (get_le_constraint (poly2::remainingPolys) variables)

and get_le_constraint_extra smtPoly1 smtPoly2 variables = match smtPoly1, smtPoly2 with
  | (SPoly(poly1), SPoly(poly2)) -> get_le_constraint_extra_extra poly1 poly2  variables
  | (SPoly(poly1), Poly(boolConstraint2, poly2)) -> And(boolConstraint2, get_le_constraint_extra_extra poly1 poly2 variables)
  | (Poly(boolConstraint1, poly1), SPoly(poly2)) -> And(boolConstraint1, get_le_constraint_extra_extra poly1 poly2  variables)
  | (Poly(boolConstraint1, poly1), Poly(boolConstraint2, poly2)) -> 
    And(And(boolConstraint1, boolConstraint2), get_le_constraint_extra_extra poly1 poly2  variables)
  | (_, POr(smtPoly21, smtPoly22)) -> Or(get_le_constraint_extra smtPoly1 smtPoly21 variables, get_le_constraint_extra smtPoly1 smtPoly22 variables)
  | (POr(smtPoly11, smtPoly12), _) -> Or(get_le_constraint_extra smtPoly11 smtPoly2 variables, get_le_constraint_extra smtPoly12 smtPoly2 variables)

and get_le_constraint_extra_extra poly1 poly2 variables = 
  Le (poly1, poly2)

and get_leq_constraint polys variables = match polys with
  | [] ->  raise (Failure "Wrong Input") 
  | [poly] -> []
  | poly1::poly2::remainingPolys -> (get_leq_constraint_extra poly1 poly2 variables) :: (get_leq_constraint (poly2::remainingPolys) variables)

and get_leq_constraint_extra smtPoly1 smtPoly2 variables = match smtPoly1, smtPoly2 with
  | (SPoly(poly1), SPoly(poly2)) -> get_leq_constraint_extra_extra poly1 poly2  variables
  | (SPoly(poly1), Poly(boolConstraint2, poly2)) -> And(boolConstraint2, get_leq_constraint_extra_extra poly1 poly2 variables)
  | (Poly(boolConstraint1, poly1), SPoly(poly2)) -> And(boolConstraint1, get_leq_constraint_extra_extra poly1 poly2 variables)
  | (Poly(boolConstraint1, poly1), Poly(boolConstraint2, poly2)) -> 
    And(And(boolConstraint1, boolConstraint2), get_leq_constraint_extra_extra poly1 poly2 variables)
  | (_, POr(smtPoly21, smtPoly22)) -> Or(get_leq_constraint_extra smtPoly1 smtPoly21 variables, get_leq_constraint_extra smtPoly1 smtPoly22 variables)
  | (POr(smtPoly11, smtPoly12), _) -> Or(get_leq_constraint_extra smtPoly11 smtPoly2 variables, get_leq_constraint_extra smtPoly12 smtPoly2 variables)

and get_leq_constraint_extra_extra poly1 poly2 variables = 
  Leq (poly1, poly2)
  

and get_gr_constraint polys variables = match polys with
  | [] ->  raise (Failure "Wrong Input")
  | [poly] -> []
  | poly1::poly2::remainingPolys -> (get_gr_constraint_extra poly1 poly2 variables) :: (get_gr_constraint (poly2::remainingPolys) variables)

and get_gr_constraint_extra smtPoly1 smtPoly2 variables = match smtPoly1, smtPoly2 with
  | (SPoly(poly1), SPoly(poly2)) -> get_gr_constraint_extra_extra poly1 poly2 variables
  | (SPoly(poly1), Poly(boolConstraint2, poly2)) -> And(boolConstraint2, get_gr_constraint_extra_extra poly1 poly2 variables)
  | (Poly(boolConstraint1, poly1), SPoly(poly2)) -> And(boolConstraint1, get_gr_constraint_extra_extra poly1 poly2 variables)
  | (Poly(boolConstraint1, poly1), Poly(boolConstraint2, poly2)) -> 
    And(And (boolConstraint1, boolConstraint2), get_gr_constraint_extra_extra poly1 poly2 variables)
  | (_, POr(smtPoly21, smtPoly22)) -> Or(get_gr_constraint_extra smtPoly1 smtPoly21 variables, get_gr_constraint_extra smtPoly1 smtPoly22 variables)
  | (POr(smtPoly11, smtPoly12), _) -> Or(get_gr_constraint_extra smtPoly11 smtPoly2 variables, get_gr_constraint_extra smtPoly12 smtPoly2 variables)

and get_gr_constraint_extra_extra poly1 poly2 variables = 
  Gr (poly1, poly2)
  

and get_geq_constraint polys variables = match polys with
  | [] ->  raise (Failure "Wrong Input")
  | [poly] ->  []
  | poly1::poly2::remainingPolys -> (get_geq_constraint_extra poly1 poly2 variables) :: (get_geq_constraint (poly2::remainingPolys) variables)

and get_geq_constraint_extra smtPoly1 smtPoly2 variables = match smtPoly1, smtPoly2 with
  | (SPoly(poly1), SPoly(poly2)) -> get_geq_constraint_extra_extra poly1 poly2 variables
  | (SPoly(poly1), Poly(boolConstraint2, poly2)) -> And(boolConstraint2, get_geq_constraint_extra_extra poly1 poly2 variables)
  | (Poly(boolConstraint1, poly1), SPoly(poly2)) -> And(boolConstraint1, get_geq_constraint_extra_extra poly1 poly2 variables)
  | (Poly(boolConstraint1, poly1), Poly(boolConstraint2, poly2)) -> 
    And(And(boolConstraint1, boolConstraint2), get_geq_constraint_extra_extra poly1 poly2 variables)
  | (_, POr(smtPoly21, smtPoly22)) -> Or(get_geq_constraint_extra smtPoly1 smtPoly21 variables, get_geq_constraint_extra smtPoly1 smtPoly22 variables)
  | (POr(smtPoly11, smtPoly12), _) -> Or(get_geq_constraint_extra smtPoly11 smtPoly2 variables, get_geq_constraint_extra smtPoly12 smtPoly2 variables)

and get_geq_constraint_extra_extra poly1 poly2 variables = 
  Geq (poly1, poly2)
  
and get_eq_constraint polys variables = match polys with
  | [] ->  raise (Failure "Wrong Input")
  | [poly] -> []
  | poly1::poly2::remainingPolys -> (get_eq_constraint_extra poly1 poly2 variables) :: (get_eq_constraint (poly2::remainingPolys) variables)

and get_eq_constraint_extra smtPoly1 smtPoly2 variables = match smtPoly1, smtPoly2 with
  | (SPoly(poly1), SPoly(poly2)) -> get_eq_constraint_extra_extra poly1 poly2 variables
  | (SPoly(poly1), Poly(boolConstraint2, poly2)) -> 
    
    (* let (miniSATExpr, _, _, _, _, _, _) = 
      miniSATExpr_of_constraints boolConstraint2 0 IntMap.empty "logic" StringMap.empty  
    in
    print_string (string_infix_of_miniSATExpr miniSATExpr ^ " -> ");
    flush stdout; *)

    And(boolConstraint2, get_eq_constraint_extra_extra poly1 poly2 variables)
  | (Poly(boolConstraint1, poly1), SPoly(poly2)) -> 

    (* let (miniSATExpr, _, _, _, _, _, _) = 
      miniSATExpr_of_constraints boolConstraint1 0 IntMap.empty "logic" StringMap.empty  
    in
    print_string (string_infix_of_miniSATExpr miniSATExpr ^ " -> ");
    flush stdout; *)

    And(boolConstraint1, get_eq_constraint_extra_extra poly1 poly2 variables)

  | (Poly(boolConstraint1, poly1), Poly(boolConstraint2, poly2)) -> 

    (* let (miniSATExpr, _, _, _, _, _, _) = 
      miniSATExpr_of_constraints (And (boolConstraint1, boolConstraint2)) 0 IntMap.empty "logic" StringMap.empty  
    in
    print_string (string_infix_of_miniSATExpr miniSATExpr ^ " -> ");
    flush stdout; *)

    And(And(boolConstraint1, boolConstraint2), get_eq_constraint_extra_extra poly1 poly2 variables)
  | (_, POr(smtPoly21, smtPoly22)) -> 
    Or(get_eq_constraint_extra smtPoly1 smtPoly21 variables, get_eq_constraint_extra smtPoly1 smtPoly22 variables)
  | (POr(smtPoly11, smtPoly12), _) -> 
    Or(get_eq_constraint_extra smtPoly11 smtPoly2 variables, get_eq_constraint_extra smtPoly12 smtPoly2 variables)

and get_eq_constraint_extra_extra poly1 poly2 variables = 
  Eq (poly1, poly2)


and get_constraint_term varTermMap functions (variables:(int StringMap.t))
                                                    varsPolysMap varsConstraintsMap = function 
  |TermQualIdentifier (_ , qualidentifier1) -> 
    get_constraint_qualidentifier varTermMap  functions variables varsPolysMap varsConstraintsMap qualidentifier1
  |TermQualIdTerm (_ , qualidentifier2, termqualidterm_term_term563) ->
    let qualidentifier_string = get_string_qualidentifier qualidentifier2 in
    if qualidentifier_string = "<" then 
      let polys = get_polys_termqualidterm_term_term56 varTermMap functions variables varsPolysMap varsConstraintsMap termqualidterm_term_term563 in
      get_le_constraint polys variables
    else if qualidentifier_string = "<=" then
      let polys = get_polys_termqualidterm_term_term56 varTermMap functions variables varsPolysMap varsConstraintsMap termqualidterm_term_term563 in
      get_leq_constraint polys variables
    else if qualidentifier_string = ">" then
      let polys = get_polys_termqualidterm_term_term56 varTermMap functions variables varsPolysMap varsConstraintsMap termqualidterm_term_term563 in
      get_gr_constraint polys variables
    else if qualidentifier_string = ">=" then
      let polys = get_polys_termqualidterm_term_term56 varTermMap functions variables varsPolysMap varsConstraintsMap termqualidterm_term_term563 in
      get_geq_constraint polys variables
    else if qualidentifier_string = "=" then
      try 
        let polys = get_polys_termqualidterm_term_term56 varTermMap functions variables varsPolysMap varsConstraintsMap termqualidterm_term_term563 in
        get_eq_constraint polys variables
      with Failure _ -> 
        let constraints = get_constraints_termqualidterm_term_term56 varTermMap functions variables varsPolysMap varsConstraintsMap termqualidterm_term_term563 in
        let iffConstraint = get_iff_constraint constraints in
        [iffConstraint]
    else if qualidentifier_string = "and" then
      let constraints = get_constraints_termqualidterm_term_term56 varTermMap functions variables varsPolysMap varsConstraintsMap termqualidterm_term_term563 in
      let andConstraint = get_and_constraint constraints in
      [andConstraint]
    else if qualidentifier_string = "or" then
      let constraints = get_constraints_termqualidterm_term_term56 varTermMap functions variables varsPolysMap varsConstraintsMap termqualidterm_term_term563 in
      let orConstraint = get_or_constraint constraints in
      [orConstraint]
    else if qualidentifier_string = "not" then
      let constraints = get_constraints_termqualidterm_term_term56 varTermMap functions variables varsPolysMap varsConstraintsMap termqualidterm_term_term563 in
      let notConstraint = get_not_constraint constraints in
      [notConstraint]
    else if qualidentifier_string = "ite" then
      let constraints = get_constraints_termqualidterm_term_term56 varTermMap functions variables varsPolysMap varsConstraintsMap termqualidterm_term_term563 in
      let iteConstraint = get_ite_constraint constraints in
      [iteConstraint]
    else if qualidentifier_string = "xor" then
      let constraints = get_constraints_termqualidterm_term_term56 varTermMap functions variables varsPolysMap varsConstraintsMap termqualidterm_term_term563 in
      let iteConstraint = get_xor_constraint constraints in
      [iteConstraint]
    else if StringMap.mem qualidentifier_string functions then
      let (arguments, funcSort, funcDef) = StringMap.find qualidentifier_string functions in
      if funcSort = "Bool" then
        let varTermMap = get_varTermMap_termqualidterm_term_term56 arguments termqualidterm_term_term563 in
        get_constraint_term varTermMap functions variables varsPolysMap varsConstraintsMap funcDef
      else   
        raise (Failure ("Undefined predicate symbols: " ^ qualidentifier_string))
    else raise (Failure ("Undefined predicate symbols: " ^ qualidentifier_string))
  |TermLetTerm (_ , termletterm_term_varbinding584 , term6) ->  
    let (varsPolysMap, varsConstraintsMap) = get_let_termletterm_term_varbinding58 varTermMap functions variables varsPolysMap varsConstraintsMap termletterm_term_varbinding584 in
    get_constraint_term varTermMap functions variables varsPolysMap varsConstraintsMap term6
  | _ -> [] (*raise (Failure "Not supported syntax line 138")*)

and get_and_constraint = function 
  | [] -> raise (Failure "Need arguments for and") 
  | [constraint1] -> constraint1
  | constraint1 :: constraint2 :: remainingConstraints -> get_and_constraint((And(constraint1, constraint2)) :: remainingConstraints)
  
and get_iff_constraint = function 
  | [] -> raise (Failure "Need arguments for =") 
  | [constraint1] -> raise (Failure "Need arguments for =")
  | [constraint1; constraint2] -> And(Or(Not constraint1, constraint2), Or(constraint1, Not constraint2))
  | constraint1 :: constraint2 :: remainingConstraints -> And(get_iff_constraint [constraint1; constraint2], get_iff_constraint (constraint2::remainingConstraints) )
  
and get_or_constraint = function 
  | [] -> raise (Failure "Need arguments for or") 
  | [constraint1] -> constraint1
  | constraint1 :: constraint2 :: remainingConstraints -> 
    get_or_constraint((Or(constraint1, constraint2)) :: remainingConstraints)  

and get_not_constraint = function 
  | [] -> raise (Failure "Need arguments for not") 
  | [constraint1] -> 
    
    (* print_endline ("not of " ^ string_infix_of_constraints constraint1);
    print_endline ("is " ^ string_infix_of_constraints (Not constraint1));
    flush stdout; *)
    

    Not constraint1
  | _ -> raise (Failure "Extra arguments for not") 

and get_ite_constraint = function 
  | [constraint1; constraint2; constraint3] -> And(Or(constraint1, constraint3), Or(Not constraint1, constraint2))
  | _ -> raise (Failure "wrong number of arguments for ite") 

and get_xor_constraint = function 
  | [] -> raise (Failure "Need arguments for xor")
  | [constraint1] -> constraint1
  | constraint1:: constraint2:: remainingConstraints -> 
    let notOfConstraint1 = Not constraint1 in
    let notOfConstraint2 = Not constraint2 in
    get_xor_constraint (And(Or(constraint1, constraint2), Or(notOfConstraint1, notOfConstraint2))::remainingConstraints)

and get_let_termletterm_term_varbinding58 varTermMap functions variables varsPolysMap varsConstraintsMap = function
  |(_,[]) -> (varsPolysMap, varsConstraintsMap)
  | (d , (varbinding1)::termletterm_term_varbinding582) ->  
    let (varsPolysMap, varsConstraintsMap) = get_let_varbinding varTermMap functions variables varsPolysMap varsConstraintsMap varbinding1 in
    get_let_termletterm_term_varbinding58 varTermMap functions variables varsPolysMap varsConstraintsMap (d,termletterm_term_varbinding582)
    
and get_let_varbinding varTermMap functions variables varsPolysMap varsConstraintsMap = function 
  | VarBindingSymTerm (_ , symbol2 , term3) -> 
    let var = get_string_symbol symbol2 in 
    try
      let poly = get_poly_term varTermMap functions variables varsPolysMap varsConstraintsMap term3 in
      (StringMap.add var poly varsPolysMap, varsConstraintsMap)
    with Failure _ ->
      try
        let polyCons = get_constraint_term varTermMap functions variables varsPolysMap varsConstraintsMap term3 in
        (varsPolysMap, StringMap.add var polyCons varsConstraintsMap)
      with Failure errorMessage -> 
        (* print_endline errorMessage;
        flush stdout; *)
        raise (Failure "Unexpected expression")
      

and get_constraints_command varTermMap functions variables = function 
  | CommandAssert (_ , term3) -> get_constraint_term varTermMap functions variables StringMap.empty StringMap.empty term3
  | _  -> []  

and get_constraints_commands varTermMap functions variables = function  
  |(_,[]) ->   []
  | (d , (command1)::commands_commands_command302) -> 
    (get_constraints_command varTermMap functions variables command1) @ (get_constraints_commands varTermMap functions variables (d, commands_commands_command302))
  

and get_constraints varTermMap functions variables = function
  |Commands (_ , commands_commands_command301) ->
    let constraints = get_constraints_commands varTermMap functions variables commands_commands_command301 in

    (* let constraints = List.rev constraints in *)

    (* let mergedBoolExp = get_boolExp_from_list constraints in
    print_endline (string_prefix_of_constraints mergedBoolExp);
    flush stdout;
    [mergedBoolExp] *)

    constraints
    
    
and get_variables = function   
  |Commands (_ , commands_commands_command301) ->
    get_variables_commands commands_commands_command301

and set_theory = function   
  |Commands (_ , commands_commands_command301) ->
    set_theory_commands commands_commands_command301


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

and set_theory_commands = function  
  |(_,[]) ->   ()
  | (d , (command1)::commands_commands_command302) -> 
    if not (set_theory_command command1) then 
      set_theory_commands (d, commands_commands_command302)    

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
    let varType = match varSort with 
      | "Int" -> intType
      | "Real" -> realType
      | "Bool" -> boolType
    in
    StringMap.singleton variable varType
  | _  -> StringMap.empty

and set_theory_command = function 
  | CommandSetLogic (_ , symbol3) ->
    theory := get_string_symbol symbol3;
    true

  | _ -> false

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
  

and output_hys variables constraints fileName lb ub = 
  (* Convert to .hys file *)
  let oc = open_out (Filename.chop_extension fileName ^ ".hys") in
  fprintf oc "-- translated from %s\n" fileName;
  (* Output variables *)
  fprintf oc "DECL\n    -- the variables\n";
  let gen_hys_variables var varType (floatString, isFisrtFloat, intString, isFirstInt, 
                                     boolString, isFirstBool) = 
    if varType = realType then
      if isFisrtFloat then
        (floatString ^ " " ^ var, false, intString, isFirstInt, boolString, isFirstBool)
      else
        (floatString ^ ", " ^ var, false, intString, isFirstInt, boolString, isFirstBool)
    else if varType = intType then
      if isFisrtFloat then
        (floatString, isFisrtFloat, intString ^ " " ^ var, false, boolString, isFirstBool)
      else
        (floatString, isFisrtFloat, intString ^ ", " ^ var, false, boolString, isFirstBool)
    else if varType = boolType then
      if isFirstBool then
        (floatString, isFisrtFloat, intString, isFirstInt, boolString ^ " " ^ var, false)
      else
        (floatString, isFisrtFloat, intString, isFirstInt, boolString ^ ", " ^ var, false)
    else  
      (floatString, isFisrtFloat, intString, isFirstInt, boolString, isFirstBool)
  in
  let initialBounds = sprintf "[%s, %s]" lb ub in
  let (floatVariables,isFisrtFloat,intVariables,isFirstInt,boolVariables,isFirstBool) = 
    StringMap.fold gen_hys_variables variables (sprintf "float %s" initialBounds, true, 
                                                sprintf "int %s" initialBounds, true,
                                                "boole", true) 
  in
  if not isFisrtFloat then fprintf oc "    %s;\n" floatVariables;
  if not isFirstInt then fprintf oc "    %s;\n" intVariables;
  if not isFirstBool then fprintf oc "    %s;\n" boolVariables;

  fprintf oc "EXPR\n    -- the constraints to be solved\n";

  (* Output constraints: *)
  let gen_hys_constraint boolConstraint currentString = 
    currentString ^ "    " ^ string_infix_of_boolExpr boolConstraint ^ ";\n"
  in
  let constraintsString = List.fold_right gen_hys_constraint constraints "" in
  fprintf oc "%s\n" constraintsString;    
  close_out  oc;;

(*get miniSat form of interval constraints*)
let genSatForm fileName lb ub =
  let ic = open_in fileName in
  let lexbuf = Lexing.from_channel ic in  
  let parsed =  Smtlib_parse.main Smtlib_lex.token lexbuf in

  (match parsed with
      | None -> ()
      | Some(x) -> set_theory x;);

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

  output_hys variables constraints fileName lb ub;;

genSatForm Sys.argv.(1) Sys.argv.(2) Sys.argv.(3)