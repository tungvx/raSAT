

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
	

(*==================== START string_infix_of_polyExpr ==============================*)	
(* This function converts a polynomial expression into infix string form *)
let rec string_infix_of_polyExpr = function
  | Var (x, _) -> x (* "(" ^ x ^ ", " ^ string_of_bool changed ^ ")" *)
	| Add(e1, e2, _) -> "(" ^ (string_infix_of_polyExpr e1) ^ ") + (" ^ (string_infix_of_polyExpr e2) ^ ")" 
	| Sub(e1, e2, _) -> "(" ^ (string_infix_of_polyExpr e1) ^ ") - (" ^ (string_infix_of_polyExpr e2) ^ ")"
  | SSub(e, _) -> "(- " ^ (string_infix_of_polyExpr e) ^ ")"
	| Mul(e1, e2, _) -> "(" ^ (string_infix_of_polyExpr e1) ^ ") * (" ^ (string_infix_of_polyExpr e2) ^ ")"
	| Div(e1, e2, _) -> "(" ^ (string_infix_of_polyExpr e1) ^ ") / (" ^ (string_infix_of_polyExpr e2) ^ ")"
	| Cons (c, _) -> c
  | Pow (var, multiplicity, _) -> "(pow " ^ var ^ " " ^ string_of_int multiplicity ^ ")"
  | Mod (e1, e2) -> "(" ^ (string_infix_of_polyExpr e1) ^ ") mod (" ^ (string_infix_of_polyExpr e2) ^ ")"
(*==================== END string_infix_of_polyExpr ==============================*)	


	
(*==================== START bool_expr_to_infix_string ==============================*)	
(* This function converts a bool expression into the string of infix form *)
let rec string_infix_of_boolExpr boolExpr =
  match boolExpr with
  |Eq (e1, e2) -> 
		(string_infix_of_polyExpr e1) ^ " = " ^ (string_infix_of_polyExpr e2)
	|Neq (e1, e2) -> 
		(string_infix_of_polyExpr e1) ^ " != " ^ (string_infix_of_polyExpr e2)
	|Leq (e1, e2) -> 
		(string_infix_of_polyExpr e1) ^ " <= " ^ (string_infix_of_polyExpr e2)
	|Le (e1, e2) -> 
    (string_infix_of_polyExpr e1) ^ " < " ^ (string_infix_of_polyExpr e2)
	|Geq (e1, e2) -> 
		(string_infix_of_polyExpr e1) ^ " >= " ^ (string_infix_of_polyExpr e2)
	|Gr (e1, e2) -> 
		(string_infix_of_polyExpr e1) ^ " > " ^ (string_infix_of_polyExpr e2)
  | And(c1, c2)  -> 
    "( " ^ string_infix_of_boolExpr c1 ^ " and " ^ string_infix_of_boolExpr c2 ^ ")"
  | Or(c1, c2) -> 
    "( " ^ string_infix_of_boolExpr c1 ^ " or " ^ string_infix_of_boolExpr c2 ^ ")"
  | BVar var -> var
  | Not c -> "(not " ^ string_infix_of_boolExpr c ^ ")"
  | True -> "(0 = 0)"
  | False -> "(0 != 0)"
(*==================== END bool_expr_to_infix_string ==============================*)

let get_type_polyExpr = function
  | Add (_,_, polType) -> polType
  | Sub (_,_, polType) -> polType
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
