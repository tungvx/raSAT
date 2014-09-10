open Exp

type var = string 
module MultiVar = struct 
    (* multisets naively implemented as sorted lists *) 
    type t = var list 
    let compare = Pervasives.compare 
end 

module Poly = Map.Make(MultiVar) 

(*This module is used for simplification of expression*)
module Expr = struct
    
  type value = float Poly.t 

  let sort vars = List.sort String.compare vars 

  let constant x = Poly.singleton [] x 
  let variable v = Poly.singleton [v] 1. 

  (* BatOption.default *) 
  let default d = function 
    | None -> d 
    | Some x -> x 
  
  let plus p1 p2 =
    let add_opt _vars c1 c2 = 
      Some (default 0. c1 +. default 0. c2) in
    Poly.merge add_opt p1 p2 

  let minus p1 p2 =
    let minus_opt _vars c1 c2 = 
      Some (default 0. c1 -. default 0. c2) in
    Poly.merge minus_opt p1 p2 

  let times p1 p2 = (* naive implementation *) 
    let p2_times_monome vars coeff acc = 
      let add_monome v c acc = 
        let monome = Poly.singleton (sort (vars @ v)) (c *. coeff) in 
        plus monome acc in 
      Poly.fold add_monome p2 acc in 
    Poly.fold p2_times_monome p1 Poly.empty 
  

  let rec power p (n: int) = match n with
     |1 -> p
     |_ -> times p (power p (n-1))

  (* evaluate expressions into values *) 
  let rec eval (exp: Exp.smt_poly_expr) = match exp with 
    | Real c -> constant c 
    | Var v  -> variable v
    | SubVar u -> variable u
    | Add(e1, e2) -> plus  (eval e1) (eval e2) 
    | Sub(e1, e2) -> minus (eval e1) (eval e2)   
    | Mul(e1, e2) -> times (eval e1) (eval e2) 
    | Pow(e, n)  -> power (eval e) n
    | _ -> constant 1. (*This will never happen*)
(*    | Div(e1, e2) -> Div (eval e1) (eval e2) *) 
	

  let show p = Poly.fold (fun vars coeff acc -> (vars, coeff)::acc) p [] 

  (* translate values back into expressions *) 
  let reify p = 
    let monome vars coeff = 
      if (coeff <> 0.) then
      begin
        let times_var acc var = Mul (acc, Var var) in 
        List.fold_left times_var (Real coeff) vars  
      end 
      else Real 0.  in
    (* extract the first elem before summing, 
       to avoid a dummy 0. initial accumulator *) 
    if Poly.is_empty p then Real 0. 
    else 
      begin
        let (v,c) = Poly.min_binding p in 
        let p' = Poly.remove v p in 
        let f va co acc = 
	  if (co <> 0.) then Add (monome va co , acc)
	  else acc in
        Poly.fold f p' (monome v c)       
      end

  (*Simplify an expression*)  
  let reduce (e: Exp.smt_poly_expr) = reify (eval e)

end 
(*End of module Expr*)
