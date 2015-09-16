open Ast
open Multiset
open Interval
open Util


module MultiVar = struct 
    (* multisets naively implemented as sorted lists  *)
    type t = string 
    let compare = Pervasives.compare 
    let to_string s = s
end 

module MultiVarSet = Multiset.Make(MultiVar)
module Poly = Map.Make(MultiVarSet) 

type poly = 
  | Monomials of float Poly.t
  | MonoAdd of poly * poly 
  | MonoSub of poly * poly 
  | MonoMul of poly * poly 
  | MonoDiv of poly * poly  



let print_monomials p =
  let print_monomial vars coefficient =
    MultiVarSet.print_tree print_string vars;
    print_string (": " ^ string_of_float coefficient);
    print_endline "";
    flush stdout;
  in
  Poly.iter print_monomial p


(* evaluate expressions into values *) 
let rec eval = function 
  | Real (c, _, _, _) -> Monomials (Poly.singleton MultiVarSet.empty c)
  | Var (v, _, _, _)  -> Monomials (Poly.singleton (MultiVarSet.add v MultiVarSet.empty)  1.)
  | Add(e1, e2, _, _) -> 
    let newE1 = eval e1 in
    let newE2 = eval e2 in
    (
    match (newE1, newE2) with
      | (Monomials m1, Monomials m2) -> Monomials (plus m1 m2)
      | _ -> MonoAdd (newE1, newE2)
    )
  | Sub(e1, e2, _, _) -> 
    let newE1 = eval e1 in
    let newE2 = eval e2 in
    (
    match (newE1, newE2) with
      | (Monomials m1, Monomials m2) -> Monomials (minus m1 m2)
      | _ -> MonoSub (newE1, newE2)
    )
  | Mul(e1, e2, _, _) -> 
    let newE1 = eval e1 in
    let newE2 = eval e2 in
    (
    match (newE1, newE2) with
      | (Monomials m1, Monomials m2) -> Monomials (times m1 m2)
      | _ -> MonoMul (newE1, newE2)
    )
  | Div(e1, e2, _, _) -> MonoDiv ((eval e1), (eval e2))
  | _ -> raise (Failure "Unsupported SMT2 function symbols")

(* BatOption.default *) 
and default d = function 
  | None -> d 
  | Some x -> x 

and plus p1 p2 =
  let add_opt _vars c1 c2 = 
    (* MultiVarSet.print_tree print_string _vars;
    print_endline "";
    flush stdout; *)
    Some (default 0. c1 +. default 0. c2) 
  in
  (* print_endline "\n\nAdding ";
  print_monomials p1;
  print_endline "\nwith ";
  print_monomials p2;
  print_endline "\nResult ";
  print_monomials (Poly.merge add_opt p1 p2 ); *)
  Poly.merge add_opt p1 p2 

and minus p1 p2 =
  let minus_opt _vars c1 c2 = 
    Some (default 0. c1 -. default 0. c2) in
  Poly.merge minus_opt p1 p2 

and times p1 p2 = (* naive implementation *) 
  let p2_times_monome vars coeff acc = 
    let add_monome v c acc = 
      let monome = Poly.singleton (MultiVarSet.add_sets v vars) (c *. coeff) in 
      (* print_endline "Multiplying";
      MultiVarSet.print_tree print_string v;
      print_endline "";
      MultiVarSet.print_tree print_string vars;
      print_endline "";
      print_endline "Result:";
      MultiVarSet.print_tree print_string (MultiVarSet.add_sets v vars);
      print_endline "";
      flush stdout; *)
      plus monome acc 
    in 
    Poly.fold add_monome p2 acc in 
  Poly.fold p2_times_monome p1 Poly.empty 


let show p = Poly.fold (fun vars coeff acc -> (vars, coeff)::acc) p [] 

(* translate values back into expressions *) 
let rec reify = function
  | Monomials p ->
    let addMonomial vars coefficient currentPolyExpr =
      if coefficient = 0. then currentPolyExpr
      else 
        let createMulExpr currentMulExpr (var, multiplicity) = 
          let tmpVarExpr = 
            if multiplicity = 1 then Var (var, {low=infinity;high=neg_infinity}, new IA.af2 0 , false)
            else Pow(var, multiplicity, {low=infinity;high=neg_infinity}, false, {low=neg_infinity;high=infinity}, new IA.af2 0) 
          in
          (match currentMulExpr with
            | Real (1., _, _, _) -> tmpVarExpr
            | _ -> Mul(currentMulExpr, tmpVarExpr, {low=neg_infinity;high=infinity}, new IA.af2 0) 
          )
        in
        (* MultiVarSet.print_tree print_string vars;
        print_endline "";
        flush stdout; *)
        let cIntv = {low=coefficient;high=coefficient} in
        let newPolyExpr = List.fold_left createMulExpr (Real (coefficient, false, cIntv, new IA.af2 0)) 
                                                (MultiVarSet.elements_packed vars) in
        (
        match currentPolyExpr with 
          | Real (0., _, _, _) -> newPolyExpr
          | _ -> Add (currentPolyExpr, newPolyExpr, {low=neg_infinity;high=infinity}, new IA.af2 0)
        )
    in
    Poly.fold addMonomial p (Real (0., false, {low=0.;high=0.}, new IA.af2 0))
  | MonoAdd (poly1, poly2) -> 
    let polyExpr1 = reify poly1 in
    let polyExpr2 = reify poly2 in
    Add (polyExpr1, polyExpr2, {low=neg_infinity;high=infinity}, new IA.af2 0)
  | MonoSub (poly1, poly2) -> 
    let polyExpr1 = reify poly1 in
    let polyExpr2 = reify poly2 in
    Sub (polyExpr1, polyExpr2, {low=neg_infinity;high=infinity}, new IA.af2 0)
  | MonoMul (poly1, poly2) -> 
    let polyExpr1 = reify poly1 in
    let polyExpr2 = reify poly2 in
    Mul (polyExpr1, polyExpr2, {low=neg_infinity;high=infinity}, new IA.af2 0)
  | MonoDiv (poly1, poly2) -> 
    let polyExpr1 = reify poly1 in
    let polyExpr2 = reify poly2 in
    Div (polyExpr1, polyExpr2, {low=neg_infinity;high=infinity}, new IA.af2 0)
(*Simplify an expression*)  
let reduce e = 
  let newE = reify (eval e) in 
  (* print_endline ("Simplified " ^ string_infix_of_polyExpr e ^ " to " ^ string_infix_of_polyExpr newE);
  flush stdout; *)
  newE

