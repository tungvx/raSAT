module VariablesSet = Set.Make(String);;
module IntSet = Set.Make(struct type t = int let compare = compare end);;
module IntMap = Map.Make(struct type t = int let compare = compare end);;
module StringMap = Map.Make(String);;
module FloatMap = Map.Make(struct type t = float let compare = compare end);;

let theory = ref "QF_NRA";;
let intTheory = "QF_NIA";;
let realTheory = "QF_NRA";;

let intType = 1;;
let realType = 2;;
let boolType = 3;;

(*raSAT expression*)
type poly_expr = 
  | Add of poly_expr * poly_expr * int
  | Sub of poly_expr * poly_expr * int
  | SSub of poly_expr * int
  | Mul of poly_expr * poly_expr * int
  | Div of poly_expr * poly_expr * int
  | Mod of poly_expr * poly_expr
  | Cons of string * int
  | Var of string * int
  | Pow of string * int * int

type bool_expr = 
  | Eq of poly_expr * poly_expr
  | Neq of poly_expr * poly_expr
  | Geq of poly_expr * poly_expr
  | Leq of poly_expr * poly_expr
  | Gr of poly_expr * poly_expr
  | Le of poly_expr * poly_expr
  | BVar of string
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr
  | True
  | False
  | Not of bool_expr


type smt_poly_expr =
  | SPoly of poly_expr
  | Poly of bool_expr * poly_expr
  | POr of smt_poly_expr * smt_poly_expr
	

let rec remove_div_polyExpr_extra = function
  | Add(Div(e1, e2, _), Div(e3,e4, _), polType) -> 
    Div(Add(Mul(e1, e4, polType), Mul(e2, e3, polType), polType), Mul(e2, e4, polType), polType)
  | Add(Div(e1, e2, _), e3, polType) -> 
    Div(Add(e1, Mul(e2, e3, polType), polType), e2, polType)
  | Add(e1, Div(e3, e4, _), polType) -> 
    Div(Add(Mul(e1, e4, polType), e3, polType), e4, polType)
  | Add(e1, e3, polType) -> Add(e1, e3, polType)

  | Sub(Div(e1, e2, _), Div(e3,e4, _), polType) -> 
    Div(Sub(Mul(e1, e4, polType), Mul(e2, e3, polType), polType), Mul(e2, e4, polType), polType)
  | Sub(Div(e1, e2, _), e3, polType) -> 
    Div(Sub(e1, Mul(e2, e3, polType), polType), e2, polType)
  | Sub(e1, Div(e3, e4, _), polType) -> 
    Div(Sub(Mul(e1, e4, polType), e3, polType), e4, polType)
  | Sub(e1, e3, polType) -> Sub(e1, e3, polType)

  | SSub(Div(e1, e2, _), polType) ->
    Div(SSub(e1, polType),e2, polType)
  | SSub (e1, polType) -> SSub (e1, polType)

  | Mul(Div(e1, e2, _), Div(e3, e4, _), polType) ->
    Div(Mul(e1, e3, polType), Mul(e2,e4,polType), polType)
  | Mul(Div(e1, e2,_), e3, polType) ->
    Div(Mul(e1, e3, polType), e2, polType)
  | Mul(e1, Div(e3, e4, _), polType) ->
    Div(Mul(e1, e3, polType), e4, polType)
  | Mul(e1, e3, polType) -> Mul(e1, e3, polType)

  | Div(Div(e1, e2, _), Div(e3, e4, _), polType) ->
    Div(Mul(e1, e4, polType), Mul(e2,e3,polType), polType)
  | Div(Div(e1, e2,_), e3, polType) ->
    Div(e1, Mul(e2, e3, polType), polType)
  | Div(e1, Div(e3, e4, _), polType) ->
    Div(Mul(e1, e4, polType), e3, polType)
  | Div(e1, e3, polType) -> Div(e1, e3, polType)

  | Mod(e1, e2) -> Mod(e1, e2)

  | Cons(c, polType) -> Cons(c, polType)

  | Var (var, polType) -> Var (var, polType)
  
  | Pow(var, multiplicity, polType) -> Pow(var, multiplicity, polType)



let rec remove_div_polyExpr = function
  | Add (e1, e2, polType) ->
    let removedDivE1 = remove_div_polyExpr e1 in
    let removedDivE2 = remove_div_polyExpr e2 in
    remove_div_polyExpr_extra (Add(removedDivE1, removedDivE2, polType))
  | Sub (e1, e2, polType) ->
    let removedDivE1 = remove_div_polyExpr e1 in
    let removedDivE2 = remove_div_polyExpr e2 in
    remove_div_polyExpr_extra (Sub(removedDivE1, removedDivE2, polType))
  | SSub (e, polType) ->
    let removedDivE = remove_div_polyExpr e in
    remove_div_polyExpr_extra (SSub(removedDivE, polType))
  | Mul (e1, e2, polType) ->
    let removedDivE1 = remove_div_polyExpr e1 in
    let removedDivE2 = remove_div_polyExpr e2 in
    remove_div_polyExpr_extra (Mul(removedDivE1, removedDivE2, polType))
  | Div (e1, e2, polType) ->
    let removedDivE1 = remove_div_polyExpr e1 in
    let removedDivE2 = remove_div_polyExpr e2 in
    remove_div_polyExpr_extra (Div(removedDivE1, removedDivE2, polType))
  | Mod (e1, e2) ->
    let removedDivE1 = remove_div_polyExpr e1 in
    let removedDivE2 = remove_div_polyExpr e2 in
    remove_div_polyExpr_extra (Mod(removedDivE1, removedDivE2))
  | Cons(c, polType) -> Cons(c, polType)
  | Var (var, polType) -> Var (var, polType)
  | Pow(var, multiplicity, polType) -> Pow(var, multiplicity, polType)

let rec remove_div_boolExpr_extra = function
  | Eq(Div(e1, e2, _), Div(e3, e4, polType)) ->
    let nonZeroE2 = Not(Eq(e2, Cons("0", polType))) in
    let nonZeroE4 = Not(Eq(e4, Cons("0", polType))) in
    let eqCons = Eq(Mul(e1, e4, polType), Mul(e2, e3, polType)) in
    And(And(nonZeroE2, nonZeroE4), eqCons)
  | Eq(Div(e1, e2, polType), e3) ->
    let nonZeroE2 = Not(Eq(e2, Cons("0", polType))) in
    let eqCons = Eq(e1, Mul(e2, e3, polType)) in
    And(nonZeroE2, eqCons)
  | Eq(e1, Div(e3, e4, polType)) ->
    let nonZeroE4 = Not(Eq(e4, Cons("0", polType))) in
    let eqCons = Eq(Mul(e1, e4, polType), e3) in
    And(nonZeroE4, eqCons)
  | Eq(e1, e3) -> Eq(e1, e3)

  | Neq (Div(e1, e2, _), Div(e3, e4, polType)) ->
    let nonZeroE2 = Not(Eq(e2, Cons("0", polType))) in
    let nonZeroE4 = Not(Eq(e4, Cons("0", polType))) in
    let neqCons = Not(Eq(Mul(e1, e4, polType), Mul(e2, e3, polType))) in
    And(And(nonZeroE2, nonZeroE4), neqCons)
  | Neq (Div(e1, e2, polType), e3) ->
    let nonZeroE2 = Not(Eq(e2, Cons("0", polType))) in
    let neqCons = Not(Eq(e1, Mul(e2, e3, polType))) in
    And(nonZeroE2, neqCons)
  | Neq(e1, Div(e3, e4, polType)) ->
    let nonZeroE4 = Not(Eq(e4, Cons("0", polType))) in
    let neqCons = Not(Eq(Mul(e1, e4, polType), e3)) in
    And(nonZeroE4, neqCons)
  | Neq(e1, e3) -> Neq(e1, e3)

  | Geq (Div(e1, e2, _), Div(e3, e4, polType)) ->
    let denumMul = Mul(e2, e4, polType) in

    (* If denumMul > 0 *)
    let posDenumMul = Gr(denumMul, Cons("0", polType)) in
    let geqCons = Geq(Mul(e1, e4, polType), Mul(e2, e3, polType)) in
    let posGeqCons = And(posDenumMul, geqCons) in

    (* If denumMul < 0 *)
    let negDenumMul = Le(denumMul, Cons("0", polType)) in
    let geqCons = Leq(Mul(e1, e4, polType), Mul(e2, e3, polType)) in
    let negGeqCons = And(negDenumMul, geqCons) in

    Or(posGeqCons, negGeqCons)
  | Geq(Div(e1, e2, polType), e3) ->
    (* if e2 > 0 *)
    let posE2 = Gr(e2, Cons("0", polType)) in
    let geqCons = Geq(e1, Mul(e2, e3, polType)) in
    let posGeqCons = And(posE2, geqCons) in

    (* if e2 < 0 *)
    let negE2 = Le(e2, Cons("0", polType)) in
    let geqCons = Leq(e1, Mul(e2, e3, polType)) in
    let negGeqCons = And(negE2, geqCons) in

    Or(posGeqCons, negGeqCons)
  | Geq(e1, Div(e3, e4, polType)) ->
    (* If e4 > 0 *)
    let posE4 = Gr(e4, Cons("0", polType)) in
    let geqCons = Geq(Mul(e1, e4, polType), e3) in
    let posGeqCons = And(posE4, geqCons) in

    (* If e4 < 0 *)
    let negE4 = Le(e4, Cons("0", polType)) in
    let geqCons = Leq(Mul(e1, e4, polType), e3) in
    let negGeqCons = And(negE4, geqCons) in

    Or(posGeqCons, negGeqCons)
  | Geq(e1, e3) -> Geq(e1, e3)

  | Leq(Div(e1, e2, polType), Div(e3, e4, _)) ->
    let denumMul = Mul(e2, e4, polType) in

    (* if denumMul > 0 *)
    let posDenumMul = Gr(denumMul, Cons("0", polType)) in
    let leqCons = Leq(Mul(e1, e4, polType), Mul(e2, e3, polType)) in
    let posLeqCons = And(posDenumMul, leqCons) in

    (* if denumMul < 0 *)
    let negDenumMul = Le(denumMul, Cons("0", polType)) in
    let leqCons = Geq(Mul(e1, e4, polType), Mul(e2, e3, polType)) in
    let negLeqCons = And(negDenumMul, leqCons) in

    Or(posLeqCons, negLeqCons)
  | Leq(Div(e1, e2, polType), e3) ->
    (* if e2 > 0 *)
    let posE2 = Gr(e2, Cons("0", polType)) in
    let leqCons = Leq(e1, Mul(e2, e3, polType)) in
    let posLeqCons = And(posE2, leqCons) in

    (* if e2 < 0 *)
    let negE2 = Le(e2, Cons("0", polType)) in
    let leqCons = Geq(e1, Mul(e2, e3, polType)) in
    let negLeqCons = And(negE2, leqCons) in

    Or(posLeqCons, negLeqCons)
  | Leq(e1, Div(e3, e4, polType)) ->
    (* if e4 > 0 *)
    let posE4 = Gr(e4, Cons("0", polType)) in
    let leqCons = Leq(Mul(e1, e4, polType), e3) in
    let posLeqCons = And(posE4, leqCons) in

    (* if e4 < 0 *)
    let negE4 = Le(e4, Cons("0", polType)) in
    let leqCons = Geq(Mul(e1, e4, polType), e3) in
    let negLeqCons = And(negE4, leqCons) in

    Or(posLeqCons, negLeqCons)
  | Leq(e1, e2) -> Leq(e1, e2)

  | Gr(Div(e1, e2, polType), Div(e3, e4, _)) ->
    let denumMul = Mul(e2, e4, polType) in

    (* if denumMul > 0 *)
    let posDenumMul = Gr(denumMul, Cons("0", polType)) in
    let grCons = Gr(Mul(e1, e4, polType), Mul(e2, e3, polType)) in
    let posGrCons = And(posDenumMul, grCons) in

    (* if denumMul < 0 *)
    let negDenumMul = Le(denumMul, Cons("0", polType)) in
    let grCons = Le(Mul(e1, e4, polType), Mul(e2, e3, polType)) in
    let negGrCons = And(negDenumMul, grCons) in

    Or(posGrCons, negGrCons)
  | Gr(Div(e1, e2, polType), e3) ->
    (* if e2 > 0 *)
    let posE2 = Gr(e2, Cons("0", polType)) in
    let grCons = Gr(e1, Mul(e2, e3, polType)) in
    let posGrCons = And(posE2, grCons) in

    (* if e2 < 0 *)
    let negE2 = Le(e2, Cons("0", polType)) in
    let grCons = Le(e1, Mul(e2, e3, polType)) in
    let negGrCons = And(negE2, grCons) in

    Or(posGrCons, negGrCons)
  | Gr(e1, Div(e3, e4, polType)) ->
    (* If e4 > 0 *)
    let posE4 = Gr(e4, Cons("0", polType)) in
    let grCons = Gr(Mul(e1, e4, polType), e3) in
    let posGrCons = And(posE4, grCons) in

    (* if e4 < 0 *)
    let negE4 = Le(e4, Cons("0", polType)) in
    let grCons = Le(Mul(e1, e4, polType), e3) in
    let negGrCons = And(negE4, grCons) in

    Or(posGrCons, negGrCons)
  | Gr(e1, e2) -> Gr(e1, e2)

  | Le(Div(e1, e2, polType), Div(e3, e4, _)) ->
    let denumMul = Mul(e2, e4, polType) in

    (* if denumMul > 0 *)
    let posDenumMul = Gr(denumMul, Cons("0", polType)) in
    let leCons = Le(Mul(e1, e4, polType), Mul(e2, e3, polType)) in
    let posLeCons = And(posDenumMul, leCons) in

    (* if denumMul < 0 *)
    let negDenumMul = Le(denumMul, Cons("0", polType)) in
    let leCons = Gr(Mul(e1, e4, polType), Mul(e2, e3, polType)) in
    let negLeCons = And(negDenumMul, leCons) in

    Or(posLeCons, negLeCons)
  | Le(Div(e1, e2, polType), e3) ->
    (* if e2 > 0 *)
    let posE2 = Gr(e2, Cons("0", polType)) in
    let leCons = Le(e1, Mul(e2, e3, polType)) in
    let posLeCons = And(posE2, leCons) in

    (* if e2 < 0 *)
    let negE2 = Le(e2, Cons("0", polType)) in
    let leCons = Gr(e1, Mul(e2, e3, polType)) in
    let negLeCons = And(negE2, leCons) in

    Or(posLeCons, negLeCons)
  | Le(e1, Div(e3, e4, polType)) ->
    (* if e4 > 0 *)
    let posE4 = Gr(e4, Cons("0", polType)) in
    let leCons = Le(Mul(e1, e4, polType), e3) in
    let posLeCons = And(posE4, leCons) in

    (* if e4 < 0 *)
    let negE4 = Le(e4, Cons("0", polType)) in
    let leCons = Gr(Mul(e1, e4, polType), e3) in
    let negLeCons = And(negE4, leCons) in

    Or(posLeCons, negLeCons)
  | Le(e1, e2) -> Le(e1, e2)

  | BVar (var) -> BVar (var)

  | And (b1, b2) -> And(remove_div_boolExpr_extra b1, remove_div_boolExpr_extra b2)

  | Or (b1, b2) -> Or(remove_div_boolExpr_extra b1, remove_div_boolExpr_extra b2)

  | True -> True

  | False -> False

  | Not b ->  Not (remove_div_boolExpr_extra b)

let rec remove_div_boolExpr = function 
  | Eq(e1, e2) ->
    let removedDivE1 = remove_div_polyExpr e1 in
    let removedDivE2 = remove_div_polyExpr e2 in
    remove_div_boolExpr_extra (Eq(removedDivE1, removedDivE2))
  | Neq(e1, e2) ->
    let removedDivE1 = remove_div_polyExpr e1 in
    let removedDivE2 = remove_div_polyExpr e2 in
    remove_div_boolExpr_extra (Neq(removedDivE1, removedDivE2))
  | Geq(e1, e2) ->
    let removedDivE1 = remove_div_polyExpr e1 in
    let removedDivE2 = remove_div_polyExpr e2 in
    remove_div_boolExpr_extra (Geq(removedDivE1, removedDivE2))
  | Leq(e1, e2) ->
    let removedDivE1 = remove_div_polyExpr e1 in
    let removedDivE2 = remove_div_polyExpr e2 in
    remove_div_boolExpr_extra (Leq(removedDivE1, removedDivE2))
  | Gr(e1, e2) ->
    let removedDivE1 = remove_div_polyExpr e1 in
    let removedDivE2 = remove_div_polyExpr e2 in
    remove_div_boolExpr_extra (Gr(removedDivE1, removedDivE2))
  | Le(e1, e2) ->
    let removedDivE1 = remove_div_polyExpr e1 in
    let removedDivE2 = remove_div_polyExpr e2 in
    remove_div_boolExpr_extra (Le(removedDivE1, removedDivE2))
  | BVar (var) -> BVar (var)
  | And(b1, b2) -> And(remove_div_boolExpr b1, remove_div_boolExpr b2)
  | Or(b1, b2) -> Or(remove_div_boolExpr b1, remove_div_boolExpr b2)
  | True -> True
  | False -> False
  | Not (b) -> Not(remove_div_boolExpr b)


let rec remove_div_boolExpr_list = function 
  | [] -> []
  | h :: t ->  (remove_div_boolExpr h) :: (remove_div_boolExpr_list t)


let remove_forbidden_char var = Str.global_replace (Str.regexp "\\.") "____" var  

(*==================== START string_infix_of_polyExpr ==============================*)	
(* This function converts a polynomial expression into infix string form *)
let rec string_infix_of_polyExpr = function
  | Var (x, _) -> remove_forbidden_char x (* "(" ^ x ^ ", " ^ string_of_bool changed ^ ")" *)
	| Add(e1, e2, _) -> "(" ^ (string_infix_of_polyExpr e1) ^ " + " ^ (string_infix_of_polyExpr e2) ^ ")" 
	| Sub(e1, e2, _) -> "(" ^ (string_infix_of_polyExpr e1) ^ " - " ^ (string_infix_of_polyExpr e2) ^ ")"
  | SSub(e, _) -> "(- " ^ (string_infix_of_polyExpr e) ^ ")"
	| Mul(e1, e2, _) -> "(" ^ (string_infix_of_polyExpr e1) ^ " * " ^ (string_infix_of_polyExpr e2) ^ ")"
	| Div(e1, e2, _) -> "(" ^ (string_infix_of_polyExpr e1) ^ " / " ^ (string_infix_of_polyExpr e2) ^ ")"
	| Cons (c, _) -> c
  | Pow (var, multiplicity, _) -> "(pow " ^ remove_forbidden_char var ^ " " ^ string_of_int multiplicity ^ ")"
  | Mod (e1, e2) -> "(" ^ (string_infix_of_polyExpr e1) ^ " mod " ^ (string_infix_of_polyExpr e2) ^ ")"
(*==================== END string_infix_of_polyExpr ==============================*)	


	
(*==================== START bool_expr_to_infix_string ==============================*)	
(* This function converts a bool expression into the string of infix form *)
let rec string_infix_of_boolExpr boolExpr =
  match boolExpr with
  |Eq (e1, e2) -> 
		"(" ^ (string_infix_of_polyExpr e1) ^ " = " ^ (string_infix_of_polyExpr e2) ^ ")"
	|Neq (e1, e2) -> 
		"(" ^ (string_infix_of_polyExpr e1) ^ " != " ^ (string_infix_of_polyExpr e2) ^ ")"
	|Leq (e1, e2) -> 
		"(" ^ (string_infix_of_polyExpr e1) ^ " <= " ^ (string_infix_of_polyExpr e2) ^ ")"
	|Le (e1, e2) -> 
    "(" ^ (string_infix_of_polyExpr e1) ^ " < " ^ (string_infix_of_polyExpr e2) ^ ")"
	|Geq (e1, e2) -> 
		"(" ^ (string_infix_of_polyExpr e1) ^ " >= " ^ (string_infix_of_polyExpr e2) ^ ")"
	|Gr (e1, e2) -> 
		"(" ^ (string_infix_of_polyExpr e1) ^ " > " ^ (string_infix_of_polyExpr e2) ^ ")"
  | And(c1, c2)  -> 
    "(" ^ string_infix_of_boolExpr c1 ^ " and " ^ string_infix_of_boolExpr c2 ^ ")"
  | Or(c1, c2) -> 
    "(" ^ string_infix_of_boolExpr c1 ^ " or " ^ string_infix_of_boolExpr c2 ^ ")"
  | BVar var -> var
  | Not c -> "(not " ^ string_infix_of_boolExpr c ^ ")"
  | True -> "(0 = 0)"
  | False -> "(0 != 0)"
(*==================== END bool_expr_to_infix_string ==============================*)

let get_type_polyExpr = function
  | Add (_,_, polType) -> polType
  | Sub (_,_, polType) -> polType
  | SSub (_,polType) -> polType
  | Mul (_,_, polType) -> polType
  | Div (_,_, polType) -> polType
  | Cons (_, polType) -> polType
  | Var (_, polType) -> polType
  | Pow (_,_, polType) -> polType 
  | Mod (_) -> intType

let get_type_polyExprs poly1 poly2 = 
  let polType1 = get_type_polyExpr poly1 in
  let polType2 = get_type_polyExpr poly2 in
  if polType1 != polType2 then
    let errorMessage = "Type mismatched" in
    print_endline errorMessage;
    flush stdout;
    raise (Failure errorMessage);
  else 
    polType1	
