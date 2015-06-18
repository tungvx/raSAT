open Ast
open Multipleset

type var = string 
module MultiVar = struct 
    (* multisets naively implemented as sorted lists *) 
    type t = var list 
    let compare = Pervasives.compare 
end 

module Poly = Map.Make(MultiVar) 

type poly = 
  | Monomials of float Poly.t
  | MonoAdd of poly * poly 
  | MonoSub of poly * poly 
  | MonoMul of poly * poly 
  | MonoDiv of poly * poly 

let sort vars = List.sort String.compare vars  

(* evaluate expressions into values *) 
let rec eval = function 
  | Real c -> Monomials (Poly.singleton [] c)
  | Var v  -> Monomials (Poly.singleton [v] 1.)
  | Add(e1, e2) -> 
    let newE1 = eval e1 in
    let newE2 = eval e2 in
    (
    match (newE1, newE2) with
      | (Monomials m1, Monomials m2) -> Monomials (plus m1 m2)
      | _ -> MonoAdd (newE1, newE2)
    )
  | Sub(e1, e2) -> 
    let newE1 = eval e1 in
    let newE2 = eval e2 in
    (
    match (newE1, newE2) with
      | (Monomials m1, Monomials m2) -> Monomials (minus m1 m2)
      | _ -> MonoSub (newE1, newE2)
    )
  | Mul(e1, e2) -> 
    let newE1 = eval e1 in
    let newE2 = eval e2 in
    (
    match (newE1, newE2) with
      | (Monomials m1, Monomials m2) -> Monomials (times m1 m2)
      | _ -> MonoMul (newE1, newE2)
    )
  | Div(e1, e2) -> MonoDiv ((eval e1), (eval e2))

(* BatOption.default *) 
and default d = function 
  | None -> d 
  | Some x -> x 

and plus p1 p2 =
  let add_opt _vars c1 c2 = 
    Some (default 0. c1 +. default 0. c2) in
  Poly.merge add_opt p1 p2 

and minus p1 p2 =
  let minus_opt _vars c1 c2 = 
    Some (default 0. c1 -. default 0. c2) in
  Poly.merge minus_opt p1 p2 

and times p1 p2 = (* naive implementation *) 
  let p2_times_monome vars coeff acc = 
    let add_monome v c acc = 
      let monome = Poly.singleton (sort (vars @ v)) (c *. coeff) in 
      plus monome acc in 
    Poly.fold add_monome p2 acc in 
  Poly.fold p2_times_monome p1 Poly.empty 


let show p = Poly.fold (fun vars coeff acc -> (vars, coeff)::acc) p [] 

(* translate values back into expressions *) 
let rec reify = function
  | Monomials p ->
    let addMonomial vars coefficient currentPolyExpr =
      if coefficient = 0. then currentPolyExpr
      else 
        let createMulExpr currentMulExpr var = 
          (match currentMulExpr with
            | Real 1. -> Var var
            | _ -> Mul(currentMulExpr, Var var) 
          )
        in
        let newPolyExpr = List.fold_left createMulExpr (Real coefficient) vars in
        (
        match currentPolyExpr with 
          | Real 0. -> newPolyExpr
          | _ -> Add (currentPolyExpr, newPolyExpr)
        )
    in
    Poly.fold addMonomial p (Real 0.)
  | MonoAdd (poly1, poly2) -> 
    let polyExpr1 = reify poly1 in
    let polyExpr2 = reify poly2 in
    Add (polyExpr1, polyExpr2)
  | MonoSub (poly1, poly2) -> 
    let polyExpr1 = reify poly1 in
    let polyExpr2 = reify poly2 in
    Sub (polyExpr1, polyExpr2)
  | MonoMul (poly1, poly2) -> 
    let polyExpr1 = reify poly1 in
    let polyExpr2 = reify poly2 in
    Mul (polyExpr1, polyExpr2)
  | MonoDiv (poly1, poly2) -> 
    let polyExpr1 = reify poly1 in
    let polyExpr2 = reify poly2 in
    Div (polyExpr1, polyExpr2)
(*Simplify an expression*)  
let reduce e = reify (eval e)

