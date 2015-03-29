(*Infinite float domains*)
open Interval

type bound = Float of float | Neg_inf | Pos_inf
let eq a b = match a with
  |Float f1 -> (match b with
    |Float f2-> if (f1 == f2) then true
          else false
    |_ -> false
    )
  |Neg_inf -> if (b==Neg_inf) then true
        else false
  |Pos_inf -> if (b==Pos_inf) then true
        else false
let leq a b = match a with
  |Float f1 -> (match b with
    |Float f2-> if (f1 <= f2) then true
          else false
    |Neg_inf -> false
    |Pos_inf -> true
    )
  |Neg_inf -> true
  |Pos_inf -> if (b==Pos_inf) then true
        else false
let le a b = match a with
  |Float f1 -> (match b with
    |Float f2-> if (f1 < f2) then true
          else false
    |Neg_inf -> false	          
    |Pos_inf -> true	          
    )
  |Neg_inf -> if (b==Neg_inf) then false
        else true
  |Pos_inf -> false
let geq a b = match a with
  |Float f1 -> (match b with
    |Float f2-> if (f1 >= f2) then true
          else false
    |Neg_inf -> true
    |Pos_inf -> false
    )
  |Neg_inf -> if (b==Neg_inf) then true
        else false
  |Pos_inf -> true
let ge a b = match a with
  |Float f1 -> (match b with
    |Float f2-> if (f1 > f2) then true
          else false
    |Neg_inf -> true	          
    |Pos_inf -> false	          
    )
  |Pos_inf -> if (b==Pos_inf) then false
        else true
  |Neg_inf -> false


(*Multiple in infinite domain*)
let inf_min a b = 
  match a with
  |Float f1 -> (match b with
    |Float f2 -> Float (min f1 f2)
    |Neg_inf -> Neg_inf
    |Pos_inf -> Float f1
  )
  |Neg_inf -> Neg_inf
  |Pos_inf -> b

let inf_max a b = 
  match a with
  |Float f1 -> (match b with
    |Float f2 -> Float (max f1 f2)
    |Pos_inf -> Pos_inf
    |Neg_inf -> Float f1
  )
  |Pos_inf -> Pos_inf
  |Neg_inf -> b

let mul (a:bound) (b:bound) =  
  match a with
  | Float f1 -> (
match b with 
	|Float f2 -> Float (f1*.f2)
	|Neg_inf -> (if (f1 < 0.) then Pos_inf  	
		    else if (f1 > 0.) then  Neg_inf
            else Float 0.)
	|Pos_inf -> (if (f1 > 0.) then Pos_inf  	
		    else if (f1 < 0.) then Neg_inf
            else Float 0.)
     )
  | Neg_inf -> (
match b with 
	|Float f2 ->(if (f2 < 0.) then Pos_inf  	
		    else if (f2 > 0.) then  Neg_inf
            else Float 0.)
	|Neg_inf -> Pos_inf    		    
	|Pos_inf -> Neg_inf
)
  | Pos_inf -> (
match b with 
	|Float f2 ->(if (f2 > 0.) then Pos_inf  	
		    else if (f2 < 0.) then Neg_inf
            else Float 0.)
	|Neg_inf -> Neg_inf  
	|Pos_inf -> Pos_inf 
     )

let float_of_bound f = match f with
  | Pos_inf -> infinity
  | Neg_inf -> neg_infinity
  | Float c -> c


(* This function convert a float number into bound value *)
let bound_of_float f = 
  if f = infinity then Pos_inf
  else if f = neg_infinity then Neg_inf
  else Float f 
  
(*Interval class denoted interval*)  
class interval lo hi = object (self)
    val mutable l = (lo: float)
    val mutable h = (hi: float)
  
    (*get interface*)
    method l = l
    method h = h
    
    (*set interface*)
    method set_l l1 = l<-l1

    method set_h h1 = h<-h1

    (*addition operator*)
    method add (other: interval)= 
      let result = new interval (self#l+.other#l) (self#h+.other#h) in
      result;
        
    (*addition by a coeff*)
    method add2 (c: float)= 
      let result = new interval (self#l+.c) (self#h+.c) in
      result;

    (*subtraction operator*)
    method sub (other: interval)= 
      let result = new interval (self#l-.other#h) (self#h-.other#l) in
      result;

    (*subtraction by a coeff*)
    method sub2 (c: float)= 
      let result = new interval (self#l-.c) (self#h-.c) in
      result;

    (*multiplication operator*)
    method mul (other: interval)= 
      let mi = min (min (self#l*.other#l) (self#l*.other#h)) (min (self#h*.other#l) (self#h*.other#h)) in 
      let ma = max (max (self#l*.other#l) (self#l*.other#h)) (max (self#h*.other#l) (self#h*.other#h)) in 
      let result = new interval mi ma in
      result;     
    (*multiple an interval with a coeff*)
     method mul2 (other: float) = 
      let result = 
        new interval (self#l*.other) (self#h*.other) in
      result; 

     (*meet operator for interval*)
     method meet (other: interval) =
       let result =
         new interval (min self#l other#l) (max self#h other#h) in
       result;

     (*power function*)
     method pow (n: int) = 
       let mi = ref self#l in
       let ma = ref self#h in	 
       for i = 1 to n-1 do
       	   let lo = min (min (!mi*.self#l) (!mi*.self#h)) (min (!ma*.self#l) (!ma*.self#h)) in
         let hi = max (max (!mi*.self#l) (!mi*.self#h)) (max (!ma*.self#l) (!ma*.self#h)) in
         mi := lo;
         ma := hi;
       done;
       let result = new interval !mi !ma in
       result;
     
     method to_string = "[" ^ string_of_float l ^ ", " ^ string_of_float h ^ "]"
       
     method printForm =
      Printf.printf "[%f,%f]\n" self#l self#h;  

end



(*Interval class with infinite bounds*)  
class inf_interval lo hi = object (self)
    val mutable l = (lo: bound)
    val mutable h = (hi: bound)
  
    (*get interface*)
    method l = l
    method h = h
    
    (*set interface*)
    method set_l l1 = l<-l1
    method set_h h1 = h<-h1

    (*addition operator*)
    method add (other: inf_interval)= 
let val_lo = 
match self#l with
  | Float s -> (match other#l with
    | Float o -> Float (s+.o) 
    | Neg_inf -> Neg_inf
    | Pos_inf -> Pos_inf)
  | Neg_inf -> Neg_inf
  | Pos_inf -> Pos_inf
in
let val_hi = 
match self#h with
  | Float s -> (match other#h with
    | Float o -> Float (s+.o) 
    | Pos_inf -> Pos_inf
    | Neg_inf -> Neg_inf)
  | Neg_inf -> Neg_inf
  | Pos_inf -> Pos_inf
in
let result = new inf_interval val_lo val_hi in
result;
     
    (*addition by a coeff*)
    method add2 (c: float)= 
let val_lo = 
match self#l with
  | Float s -> Float (s+.c) 
  | Neg_inf -> Neg_inf
  | Pos_inf -> Pos_inf
in
let val_hi = 
match self#h with
  | Float s -> Float (s+.c) 
  | Neg_inf -> Neg_inf
  | Pos_inf -> Pos_inf
in
let result = new inf_interval val_lo val_hi in
result;

    (*subtraction operator*)
    method sub (other: inf_interval)= 
let val_lo = 
match self#l with
  | Float s -> (match other#h with
    | Float o -> Float (s-.o) 
    | Neg_inf -> Pos_inf
    | Pos_inf -> Neg_inf)
  | Neg_inf -> Neg_inf
  | Pos_inf -> Pos_inf
in
let val_hi = 
match self#h with
  | Float s -> (match other#l with
    | Float o -> Float (s-.o) 
    | Neg_inf -> Pos_inf
    | Pos_inf -> Neg_inf)
  | Neg_inf -> Neg_inf
  | Pos_inf -> Pos_inf
in
let result = new inf_interval val_lo val_hi in
result;

    (*subtraction by a coeff*)
    method sub2 (c: float)= 
let val_lo = 
match self#l with
  | Float s -> Float (s-.c) 
  | Neg_inf -> Neg_inf
  | Pos_inf -> Pos_inf
in
let val_hi = 
match self#h with
  | Float s -> Float (s-.c) 
  | Neg_inf -> Neg_inf
  | Pos_inf -> Pos_inf
in
let result = new inf_interval val_lo val_hi in
result;	

    (*multiplication operator*)
    method mul (other: inf_interval)= 
let mi = inf_min (inf_min (mul self#l other#l) (mul self#l other#h)) 
         (inf_min (mul self#h other#l) (mul self#h other#h)) in 
let ma = inf_max (inf_max (mul self#l other#l) (mul self#l other#h)) 
         (inf_max (mul self#h other#l) (mul self#h other#h)) in 
let result = new inf_interval mi ma in
result;       

    (*multiple an interval with a coeff*)
     method mul2 (c: float) = 
let result = new inf_interval (mul self#l (Float c)) (mul self#h (Float c)) in
result; 

     (*meet operator for interval*)
     method meet (other: inf_interval) =
 let result = new inf_interval (inf_min self#l other#l) (inf_max self#h other#h) in
 result;

     (*power function*)
     method pow (n: int) = 
 let mi = ref self#l in
 let ma = ref self#h in	 
 for i = 1 to n-1 do
 	   let lo = inf_min (inf_min (mul !mi self#l) (mul !mi self#h)) (inf_min (mul !ma self#l) (mul !ma self#h)) in
   let hi = inf_max (inf_max (mul !mi self#l) (mul !mi self#h)) (inf_max (mul !ma self#l) (mul !ma self#h)) in
   mi := lo;
   ma := hi;
 done;
 let result = new inf_interval !mi !ma in
 result;
       
     method printForm =
 let sl = match self#l with
   |Float f -> string_of_float f
   |Neg_inf -> "-w"
   |Pos_inf -> "+w" in
 let su = match self#h with
   |Float f -> string_of_float f
   |Neg_inf -> "-w"
   |Pos_inf -> "+w" in
 Printf.printf "[%s, %s]" sl su;

     method to_interval = 
       let floatL = float_of_bound l in
       let floatH = float_of_bound h in
       new interval floatL floatH	
end
(*Define inf_interval arithmetic operators*)
module ICI = struct
  let ( ^)  (i: inf_interval)  (n: int)           = i #pow n    
  let ( * ) (i1: inf_interval) (i2: inf_interval) = i1 #mul i2 
  let ( *@) (c: float)         (i: inf_interval)  = i #mul2 c
  let ( + ) (i1: inf_interval) (i2: inf_interval) = i1 #add i2
  let ( +@) (i: inf_interval)  (c: float)         = i #add2 c
  let ( - ) (i1: inf_interval) (i2: inf_interval) = i1 #sub i2   
  let ( -@) (i: inf_interval)  (c: float)         = i #sub2 c  

  let ( == )(i1: bound) (i2: bound) = eq i1 i2  
  let ( >= )(i1: bound) (i2: bound) = geq i1 i2  
  let ( > ) (i1: bound) (i2: bound) = ge i1 i2  
  let ( <= )(i1: bound) (i2: bound) = leq i1 i2 
  let ( < ) (i1: bound) (i2: bound) = le i1 i2  
end


(*Define arithmetic operators*)
module CI = struct
  let ( ^)  (i: interval)  (n: int)       = i #pow n    
  let ( * ) (i1: interval) (i2: interval) = i1 #mul i2 
  let ( *@) (c: float)     (i: interval)  = i #mul2 c
  let ( + ) (i1: interval) (i2: interval) = i1 #add i2
  let ( +@) (i: interval)  (c: float)     = i #add2 c
  let ( - ) (i1: interval) (i2: interval) = i1 #sub i2   
  let ( -@) (i: interval)  (c: float)     = i #sub2 c   
end

(*Some intermediate functions on interval*)
(*evaluate result of a*\epsilon + b*|\epsilon|*)
let eval (a: interval) (b: interval) = 
   let e = new interval 0. 1. in
   let e_ = new interval (-1.) 0. in
   let i1 = CI.((new interval (a#l+.b#l) (a#h+.b#h))*e) in
   let i2 = CI.((new interval (a#l-.b#h) (a#h-.b#l))*e_) in
   let result = i1#meet i2 in
   result;

(*-------------------------------*) 
(*The first affine form class denoted af1*)
class af1 size = object (self)
    val mutable a = (0.0: float)  (*coeff of free var*)
    val mutable k = (0.0: float)  (*coeff of epsilon+-*)
    val mutable ar = (Array.create size 0.00: float array)  (*array of coeff of epsilon i*)

    (*Get interface*)
    method a = a
    method k = k
    method ar = ar

    (*Set interface*)
    method set_a a1 = a<-a1
    method set_k k1 = k<- abs_float (k1) 
    method set_ar ar1 = ar<-ar1

    (*addition operator*)
    method add (other: af1) = 
let size1 = Array.length self#ar in
let result = new af1 size1 in
result#set_a (self#a +. other#a);
result#set_k (self#k +. other#k);
let nar = Array.create size1 0.0 in
for i = 0 to size1 - 1 do
  Array.set nar i (self#ar.(i) +. other#ar.(i));
done;
result#set_ar nar;
result;
    
    (*addition by a coeff*)
    method add2 (c: float) = 
let result = new af1 size in
result#set_a (self#a+.c);
result#set_k self#k;
let nar = Array.create size 0.0 in
for i = 0 to size - 1 do
  Array.set nar i self#ar.(i);
done;
result#set_ar nar;
result;

    (*subtraction operator*)
    method sub (other: af1) = 
let size1 = Array.length self#ar in
let result = new af1 size1 in
result#set_a (self#a -. other#a);
result#set_k (self#k +. other#k);
let nar = Array.create size1 0.0 in
for i = 0 to size1 - 1 do
  Array.set nar i (self#ar.(i) -. other#ar.(i));
done;
result#set_ar nar;
result;

    (*subtraction by a coeff*)
    method sub2 (c: float) = 
let result = new af1 size in
result#set_a (self#a -.c);
result#set_k self#k;
let nar = Array.create size 0.0 in
for i = 0 to size - 1 do
  Array.set nar i self#ar.(i);
done;
result#set_ar nar;
result;

    (*multiplication operator*)
    method mul (other: af1) = 
let size1 = Array.length self#ar in
let result = new af1 size1 in
result#set_a (self#a *. other#a);

let nar = Array.create size1 0.0 in
for i = 0 to size1 - 1 do
  Array.set nar i (self#ar.(i)*.other#a +. other#ar.(i)*.self#a);
done;
result#set_ar nar;

let k1 = ref 0.0 in 
k1 := abs_float (self#a)*.other#k +. abs_float (other#a)*.self#k +. self#k*.other#k;
for i = 0 to size1 - 1 do
  k1 := !k1 +. abs_float(other#ar.(i))*.self#k;
  k1 := !k1 +. abs_float(self#ar.(i))*.other#k;
  for j = 0 to size1 -1 do
    k1 := !k1 +.  abs_float (self#ar.(i)) *. abs_float (other#ar.(j));	    
  done;
done;
result#set_k !k1;

result;

   (*multiplication with a coeff*)
   method mul2 (c: float)=
     let size1 = Array.length self#ar in
     let result = new af1 size1 in
     result#set_a (c*.self#a);
     result#set_k (abs_float(c)*.self#k);
     let ar1 = Array.create size1 0.0 in
     for i = 0 to Array.length ar1 - 1 do
 Array.set ar1 i (c*.ar.(i));
     done;
     result#set_ar ar1;
     result;

   (*power function, assume from a + be*)
   (* method pow (n: int) = 
(*let a1 = Math.pow self#a n in*)
let size1 = Array.length self#ar in
let ar1 = Array.create size1 0.0 in
let k1 = ref 0.0 in
for i = 0 to size1 -1 do
  if self#ar.(i)<> 0.0 then
  k1 := Math.prop self#a self#ar.(i) n; 
        Array.set ar1 i (float_of_int(n)*.(Math.pow self#a (n-1))*.self#ar.(i));
done;
let result = new af1 size1 in
result#set_a (Math.pow self#a n);
result#set_k !k1;
result#set_ar ar1;
result;     
   *)     

   (*Evaluate the bound*)
    method evaluate =        
     let lo = ref (self#a -. abs_float(self#k)) in
     let hi = ref (self#a +. abs_float(self#k)) in

     for i = 0 to Array.length self#ar - 1 do
        lo := !lo -. abs_float(ar.(i));
        hi := !hi +. abs_float(ar.(i));
     done;

     let result = new interval !lo !hi in
     result;
    
    method printForm = 
      Printf.printf "%f " a;
for i = 1 to Array.length ar do
 	    Printf.printf "%fe%d " ar.(i-1) i
done;
Printf.printf "%fe+-\n" k;
end 

module AF1 = struct
  let rec pow (t: af1) (n: int)= 
    match n with
    | 1 -> t
    | _ -> t#mul (pow t (n-1))

  let ( ^ ) (t: af1 ) (n: int)  = pow t n    
  let ( * ) (t1: af1) (t2: af1) = t1 #mul t2
  let ( *@ )(c: float)(t: af1)  = t #mul2 c
  let ( + ) (t1: af1) (t2: af1) = t1 #add t2
  let ( +@) (t: af1)  (c: float)= t #add2 c
  let ( - ) (t1: af1) (t2: af1) = t1 #sub t2
  let ( -@ )(t: af1)  (c: float)= t #sub2 c
end

(*---------------------------------------*)   
(*The second affine form class denoted af2*)
class af2 size = object (self)
    val mutable a = {low = 0.; high = 0.}   (*coeff of free var*)
    val mutable kp = 0.  (*coeff of epsilon+*)
    val mutable kn = 0.  (*coeff of epsilon-*)
    val mutable k = 0.   (*coeff of epsilon+-*)
    val mutable ar = (Array.create size {low = 0.; high = 0.})  (*array of coeff of epsilon i*)

    (*Get interface*)
    method a = a
    method kp = kp
    method kn = kn
    method k = k
    method ar = ar

    (*Set interface*)
    method set_a a1 = a<-a1
    method set_kp kp1 = kp<- abs_float (kp1) 
    method set_kn kn1 = kn<- abs_float (kn1) 
    method set_k k1 = k<- abs_float (k1) 
    method set_ar ar1 = ar<-ar1

    (*addition operator*)
    method add (other: af2) = 
      let size1 = Array.length self#ar in
      let result = new af2 size1 in
      result#set_a (self#a +$ other#a);
      
      let newKp = self#kp +.$ {low=0.;high=other#kp} in
      result#set_kp newKp.high;
      
      let newKn = self#kn +.$ {low=0.;high=other#kn} in
      result#set_kn newKn.high;
      
      let newK = self#k +.$ {low=0.;high=other#k} in
      result#set_k newK.high;
      
      let nar = Array.create size1 {low=0.;high=0.} in
      for i = 0 to size1 - 1 do
        Array.set nar i (self#ar.(i) +$ other#ar.(i));

      done;
      result#set_ar nar;
      
      result;

    (*subtraction operator*)
    method sub (other: af2) = 
      let size1 = Array.length self#ar in
      let result = new af2 size1 in
      
      result#set_a (self#a -$ other#a);
      
      let newKp = self#kp +.$ {low=0.;high=other#kn} in
      result#set_kp newKp.high;
      
      let newKn = self#kn +.$ {low=0.; high=other#kp} in
      result#set_kn newKn.high;
      
      let newK = self#k +.$ {low=0.; high=other#k} in
      result#set_k newK.high;
      
      let nar = Array.create size1 {low=0.;high=0.} in
      for i = 0 to size1 - 1 do
        Array.set nar i (self#ar.(i) -$ other#ar.(i));
      done;
      result#set_ar nar;
      
      result;

    (*multiplication operator*)
    method mul (other: af2) = 
      let size1 = Array.length self#ar in
      let result = new af2 size1 in
      result#set_a (self#a *$ other#a);

      let nar = Array.create size1 {low=0.;high=0.} in
      for i = 0 to size1 - 1 do
        Array.set nar i (self#ar.(i) *$ other#a +$ other#ar.(i) *$ self#a);
      done;
      result#set_ar nar;

      let k1 = ref 0.0 in 
      let k2 = ref 0.0 in
      let k3 = ref 0.0 in

      let tmpK2 = (self#kp *.$ {low=0.; high=other#kp}) +$ (self#kn *.$ {low=0.;high=other#kn}) in
      k2 := tmpK2.high;
      
      let tmpK3 = (self#kp *.$ {low=0.;high=other#kn}) +$ (self#kn *.$ {low=0.;high=other#kp}) in
      k3 := tmpK3.high;

      let tmpK1 = (abs_I (self#a) *$. other#k) +$ (abs_I (other#a) *$. self#k) +$ (self#k *.$ {low=0.;high=other#k}) in
      k1 := tmpK1.high;

      if self#a.low > 0.0 then
         (k2 := self#a*.other#kp;
         k3 := self#a*.other#kn)
      else if self#a.high < 0.0 then
         (k2 := self#a*.other#kn;
         k3 := self#a*.other#kp);
      else 
      ./raSAT Test/smtlib-20140121/QF_NRA/meti-tarski/Chua/1/VC1/L/Chua-1-VC1-L-chunk-0088.smt2 lb="-inf inf" sbox=0.0000000000000001
         
      if other#a > 0.0 then
         (k2 := !k2+.other#a*.self#kp;
         k3 := !k3+.other#a*.self#kn)
      else
         (k2 := !k2+.other#a*.self#kn;
         k3 := !k3+.other#a*.self#kp);        

for i = 0 to size1 - 1 do
  k1 := !k1 +. abs_float(other#ar.(i))*.(self#k+.self#kp+.self#kn);
  k1 := !k1 +. abs_float(self#ar.(i))*.(other#k+.other#kp+.other#kn);
  for j = 0 to size1 -1 do
    let tmp = self#ar.(i)*.other#ar.(j) in
    if i<>j then
             k1 := !k1 +.  abs_float tmp	    
    else if tmp > 0.0 then
       k2 := !k2 +. tmp
          else 
             k3 := !k3 -. tmp
  done;
done;
result#set_kp !k2;

result#set_kn !k3;
result#set_k !k1;

result;

   (*multiplication with a coeff*)
   method mul2 (c: float)=
     let size1 = Array.length self#ar in
     let result = new af2 size1 in
     result#set_a (c*.self#a);
     result#set_kp (abs_float(c)*.self#kp);
     result#set_kn (abs_float(c)*.self#kn);
     result#set_k (abs_float(c)*.self#k);
     let ar1 = Array.create size1 0.0 in
     for i = 0 to Array.length ar1 - 1 do
      Array.set ar1 i (c*.ar.(i));
     done;
     result#set_ar ar1;
     result;        
   
   (*Evaluate the bound*)
    method evaluate =        
     let lo = ref (self#a-.self#k-.self#kn) in
     let hi = ref (self#a+.self#k+.self#kp) in

     for i = 0 to Array.length self#ar - 1 do
        lo := !lo -. abs_float(ar.(i));
        hi := !hi +. abs_float(ar.(i));
     done;

     let result = new interval !lo !hi in
     result;
    
    method extract_sortedVarsSens varsIndicesList = 
      let rec insert_sort_varSen sortedVarsSensList (var, sen, isPositiveSen) =
        match sortedVarsSensList with 
          | [] -> [(var, sen, isPositiveSen)]
          | (otherVar, otherSen, otherIsPositiveSen) :: remainings -> 
            if sen > otherSen then (var, sen, isPositiveSen) :: sortedVarsSensList
            else if sen < otherSen then (otherVar, otherSen, otherIsPositiveSen) :: (insert_sort_varSen remainings (var, sen, isPositiveSen))
            else (* randomly sort them *)
              if Random.bool () then (var, sen, isPositiveSen) :: sortedVarsSensList
              else (otherVar, otherSen, otherIsPositiveSen) :: (insert_sort_varSen remainings (var, sen, isPositiveSen))
      in
      let rec rec_extract_varsSen varsIndicesList sortedSensVarsList = 
        match varsIndicesList with
          | [] -> sortedSensVarsList
          | ((var:string), index)::remaining ->
            (*print_endline ("Start getting sen at: " ^ string_of_int index);

            flush stdout;*)
            let varSen = Array.get ar index in
            (*print_string (var ^ ": " ^ string_of_float varSen ^ " ");
            flush stdout;*)
            let positiveVarSen = abs_float varSen in
            let isPositiveSen = varSen > 0. in
            (*print_string (string_of_bool isPositiveSen);
            flush stdout;*)
            let newSortedVarsSensList = insert_sort_varSen sortedSensVarsList (var, positiveVarSen, isPositiveSen) in
            rec_extract_varsSen remaining newSortedVarsSensList
      in
      (*let add_string_of_varSen oldString (var, sen) = 
        oldString ^ " " ^ var ^ ": " ^ string_of_float sen

      in
      print_endline ("VarsSens: " ^ List.fold_left add_string_of_varSen "" (rec_extract_varsSen varsIndicesList []));
      flush stdout;*)
      rec_extract_varsSen varsIndicesList []
             
    
    method printForm = 
      Printf.printf "%f " a;
for i = 1 to Array.length ar do
 	    Printf.printf "%fe%d " ar.(i-1) i
done;
Printf.printf "%fe+\n" kp;
Printf.printf "%fe-\n" kn;
Printf.printf "%fe+-\n" k;

end 
module AF2 = struct
 let rec pow (t: af2) (n: int)= 
    match n with
    | 1 -> t
    | _ -> t#mul (pow t (n-1))

  let ( * ) (t1: af2) (t2: af2) = t1 #mul t2
  let ( *@) (c: float)(t: af2)  = t #mul2 c
  let ( + ) (t1: af2) (t2: af2) = t1 #add t2
  let ( +@) (t: af2)  (c: float)= t #add2 c
  let ( - ) (t1: af2) (t2: af2) = t1 #sub t2
  let ( -@) (t: af2)  (c: float)= t #sub2 c
  let ( ^ ) (t: af2)  (n: int)  = pow t n
end
(*class af2 size = object (self)
    val mutable a = (0.0: float)   (*coeff of free var*)
    val mutable kp = (0.0: float)  (*coeff of epsilon+*)
    val mutable kn = (0.0: float)  (*coeff of epsilon-*)
    val mutable k = (0.0: float)   (*coeff of epsilon+-*)
    val mutable ar = (Array.create size 0.00: float array)  (*array of coeff of epsilon i*)

    (*Get interface*)
    method a = a
    method kp = kp
    method kn = kn
    method k = k
    method ar = ar

    (*Set interface*)
    method set_a a1 = a<-a1
    method set_kp kp1 = kp<- abs_float (kp1) 
    method set_kn kn1 = kn<- abs_float (kn1) 
    method set_k k1 = k<- abs_float (k1) 
    method set_ar ar1 = ar<-ar1

    (*addition operator*)
    method add (other: af2) = 
      let size1 = Array.length self#ar in
      let result = new af2 size1 in
      result#set_a (self#a +. other#a);
      result#set_kp (self#kp +. other#kp);
      result#set_kn (self#kn +. other#kn);
      result#set_k (self#k +. other#k);
      let nar = Array.create size1 0.0 in
      for i = 0 to size1 - 1 do
        Array.set nar i (self#ar.(i) +. other#ar.(i));

      done;
      result#set_ar nar;
      result;
    
    (*addition by a coeff*)
    method add2 (c: float) = 
      let result = new af2 size in
      result#set_a (self#a+.c);
      result#set_kp self#kp;
      result#set_kn self#kn;
      result#set_k self#k;
      let nar = Array.create size 0.0 in
      for i = 0 to size - 1 do
        Array.set nar i self#ar.(i);
      done;
      result#set_ar nar;
      result;

    (*subtraction operator*)
    method sub (other: af2) = 
      let size1 = Array.length self#ar in
      let result = new af2 size1 in
      result#set_a (self#a -. other#a);
      result#set_kp (self#kp +. other#kn);
      result#set_kn (self#kn +. other#kp);
      result#set_k (self#k +. other#k);
      let nar = Array.create size1 0.0 in
      for i = 0 to size1 - 1 do
        Array.set nar i (self#ar.(i) -. other#ar.(i));
      done;
      result#set_ar nar;
      result;

    (*subtraction by a coeff*)
    method sub2 (c: float) = 
      let result = new af2 size in
      result#set_a (self#a-.c);
      result#set_kp self#kn;
      result#set_kn self#kp;
      result#set_k self#k;
      let nar = Array.create size 0.0 in
      for i = 0 to size - 1 do
        Array.set nar i self#ar.(i);
      done;
      result#set_ar nar;
      result;

    (*multiplication operator*)
    method mul (other: af2) = 
let size1 = Array.length self#ar in
let result = new af2 size1 in
result#set_a (self#a *. other#a);

let nar = Array.create size1 0.0 in
for i = 0 to size1 - 1 do
  Array.set nar i (self#ar.(i)*.other#a +. other#ar.(i)*.self#a);
done;
result#set_ar nar;

let k1 = ref 0.0 in 
let k2 = ref 0.0 in
let k3 = ref 0.0 in

      k2 := self#kp*.other#kp+.self#kn*.other#kn;
      k3 := self#kp*.other#kn+.self#kn*.other#kp;

k1 := abs_float (self#a)*.other#k +. abs_float (other#a)*.self#k +. self#k*.other#k;

      if self#a > 0.0 then
         (k2 := self#a*.other#kp;
         k3 := self#a*.other#kn)
      else
         (k2 := self#a*.other#kn;
         k3 := self#a*.other#kp);
      if other#a > 0.0 then
         (k2 := !k2+.other#a*.self#kp;
         k3 := !k3+.other#a*.self#kn)
      else
         (k2 := !k2+.other#a*.self#kn;
         k3 := !k3+.other#a*.self#kp);        

for i = 0 to size1 - 1 do
  k1 := !k1 +. abs_float(other#ar.(i))*.(self#k+.self#kp+.self#kn);
  k1 := !k1 +. abs_float(self#ar.(i))*.(other#k+.other#kp+.other#kn);
  for j = 0 to size1 -1 do
    let tmp = self#ar.(i)*.other#ar.(j) in
    if i<>j then
             k1 := !k1 +.  abs_float tmp	    
    else if tmp > 0.0 then
       k2 := !k2 +. tmp
          else 
             k3 := !k3 -. tmp
  done;
done;
result#set_kp !k2;
result#set_kn !k3;
result#set_k !k1;

result;

   (*multiplication with a coeff*)
   method mul2 (c: float)=
     let size1 = Array.length self#ar in
     let result = new af2 size1 in
     result#set_a (c*.self#a);
     result#set_kp (abs_float(c)*.self#kp);
     result#set_kn (abs_float(c)*.self#kn);
     result#set_k (abs_float(c)*.self#k);
     let ar1 = Array.create size1 0.0 in
     for i = 0 to Array.length ar1 - 1 do
      Array.set ar1 i (c*.ar.(i));
     done;
     result#set_ar ar1;
     result;        
   
   (*Evaluate the bound*)
    method evaluate =        
     let lo = ref (self#a-.self#k-.self#kn) in
     let hi = ref (self#a+.self#k+.self#kp) in

     for i = 0 to Array.length self#ar - 1 do
        lo := !lo -. abs_float(ar.(i));
        hi := !hi +. abs_float(ar.(i));
     done;

     let result = new interval !lo !hi in
     result;
    
    method extract_sortedVarsSens varsIndicesList = 
      let rec insert_sort_varSen sortedVarsSensList (var, sen, isPositiveSen) =
        match sortedVarsSensList with 
          | [] -> [(var, sen, isPositiveSen)]
          | (otherVar, otherSen, otherIsPositiveSen) :: remainings -> 
            if sen > otherSen then (var, sen, isPositiveSen) :: sortedVarsSensList
            else if sen < otherSen then (otherVar, otherSen, otherIsPositiveSen) :: (insert_sort_varSen remainings (var, sen, isPositiveSen))
            else (* randomly sort them *)
              if Random.bool () then (var, sen, isPositiveSen) :: sortedVarsSensList
              else (otherVar, otherSen, otherIsPositiveSen) :: (insert_sort_varSen remainings (var, sen, isPositiveSen))
      in
      let rec rec_extract_varsSen varsIndicesList sortedSensVarsList = 
        match varsIndicesList with
          | [] -> sortedSensVarsList
          | ((var:string), index)::remaining ->
            (*print_endline ("Start getting sen at: " ^ string_of_int index);
            flush stdout;*)
            let varSen = Array.get ar index in
            (*print_string (var ^ ": " ^ string_of_float varSen ^ " ");
            flush stdout;*)
            let positiveVarSen = abs_float varSen in
            let isPositiveSen = varSen > 0. in
            (*print_string (string_of_bool isPositiveSen);
            flush stdout;*)
            let newSortedVarsSensList = insert_sort_varSen sortedSensVarsList (var, positiveVarSen, isPositiveSen) in
            rec_extract_varsSen remaining newSortedVarsSensList
      in
      (*let add_string_of_varSen oldString (var, sen) = 
        oldString ^ " " ^ var ^ ": " ^ string_of_float sen
      in
      print_endline ("VarsSens: " ^ List.fold_left add_string_of_varSen "" (rec_extract_varsSen varsIndicesList []));
      flush stdout;*)
      rec_extract_varsSen varsIndicesList []
             
    
    method printForm = 
      Printf.printf "%f " a;
for i = 1 to Array.length ar do
 	    Printf.printf "%fe%d " ar.(i-1) i
done;
Printf.printf "%fe+\n" kp;
Printf.printf "%fe-\n" kn;
Printf.printf "%fe+-\n" k;

end 
module AF2 = struct
 let rec pow (t: af2) (n: int)= 
    match n with
    | 1 -> t
    | _ -> t#mul (pow t (n-1))

  let ( * ) (t1: af2) (t2: af2) = t1 #mul t2
  let ( *@) (c: float)(t: af2)  = t #mul2 c
  let ( + ) (t1: af2) (t2: af2) = t1 #add t2
  let ( +@) (t: af2)  (c: float)= t #add2 c
  let ( - ) (t1: af2) (t2: af2) = t1 #sub t2
  let ( -@) (t: af2)  (c: float)= t #sub2 c
  let ( ^ ) (t: af2)  (n: int)  = pow t n
end
*)


(*---------------------------------------*)   
(*The Chebyshev Approximation Form class denoted CAI1*)
class cai1 (size: int) = object (self)
    val mutable a  = (new interval 0.0 0.0: interval)   (*coeff of free var*)
    val mutable k  = (new interval 0.0 0.0: interval)   (*coeff of epsilon+*)
    val mutable ar = (Array.create (2*size) (new interval 0.0 0.0): interval array)  (*array of coeff of epsilon i*)

    (*Get interface*)
    method a = a
    method k = k
    method ar = ar

    (*Set interface*)
    method set_a a1 = a<-a1
    method set_k k1 = k<- k1 
    method set_ar ar1 = ar<-ar1
    
    method printForm = 
      Printf.printf "[%f,%f] " a#l a#h;
for i = 1 to Array.length ar do
 	    Printf.printf "[%f,%f]e%d " ar.(i-1)#l ar.(i-1)#h i
done;
Printf.printf "[%f,%f]e+-\n" k#l k#h;

    (* CAF Arithmetic *)

    (* Additional operator *)
    method add (other: cai1) = 	
let result = new cai1 size in
result#set_a CI.(self#a + other#a);

let cons = new interval (-1.) 1. in
result#set_k CI.((self#k*cons) + (other#k*cons));

let nar = Array.create (2*size) (new interval 0.0 0.0) in
for i = 0 to 2*size - 1 do
  Array.set nar i CI.(self#ar.(i) + other#ar.(i));
done;
result#set_ar nar;
result;     

    (*addition by a coeff*)
    method add2 (c: float) = 
let result = new cai1 size in
result#set_a CI.(self#a +@ c);
result#set_k self#k;
let nar = Array.create (2*size) (new interval 0.0 0.0) in
for i = 0 to 2*size - 1 do
  Array.set nar i self#ar.(i);
done;
result#set_ar nar;
result;

    (*subtraction operator*)
    method sub (other: cai1) = 
let result = new cai1 size in
result#set_a CI.(self#a - other#a);

let cons = new interval (-1.) 1. in
result#set_k CI.((self#k*cons) + (other#k*cons));

let nar = Array.create (2*size) (new interval 0.0 0.0) in
for i = 0 to 2*size - 1 do
  Array.set nar i CI.(self#ar.(i) - other#ar.(i));
done;
result#set_ar nar;
result;

    (*subtraction by a coeff*)
    method sub2 (c: float) = 
let result = new cai1 size in
result#set_a CI.(self#a -@ c);
result#set_k self#k;
let nar = Array.create (2*size) (new interval 0.0 0.0) in
for i = 0 to 2*size - 1 do
  Array.set nar i self#ar.(i);
done;
result#set_ar nar;
result;

    (*multiplication operator*)
    method mul (other: cai1) = 
let result = new cai1 size in

(*Compute a0*)
let a0 = ref (new interval 0.0 0.0) in
a0 := CI.(self#a * other#a);
let q = new interval (-0.25) 0.0 in
let f = new interval (-0.25) (0.25) in
for i = 0 to size - 1 do
  let idx = i + size in
  a0 := CI.(!a0 + (q*(self#ar.(i)*other#ar.(i))));
  a0 := CI.(!a0 + (q*(self#ar.(idx)*other#ar.(idx))));
	  a0 := CI.(!a0 + (f*(self#ar.(i)*other#ar.(idx))));
  a0 := CI.(!a0 + (f*(self#ar.(idx)*other#ar.(i))));
done;
result#set_a !a0;

(*Compute coeff for epsilon i=1 to size*)
let nar = Array.create (2*size) (new interval 0.0 0.0) in
for i = 0 to size - 1 do
  let idx = i + size in
  Array.set nar i CI.((self#ar.(i)*other#a)+(other#ar.(i)*self#a)+(self#ar.(i)*other#ar.(idx))+(self#ar.(idx)*other#ar.(i)));
  Array.set nar (idx) CI.((self#ar.(idx)*other#a)+(other#ar.(idx)*self#a)+(self#ar.(i)*other#ar.(i))+(self#ar.(idx)*other#ar.(idx)));
done;
result#set_ar nar;

(*Compute coeff K for epsilon +-*)	
let cons = new interval (-1.) 1. in
let k = ref (new interval 0.0 0.0) in (*refer to e+*)

k := CI.((self#a*other#k*cons) + (self#k * other#a * cons));
k := CI.(!k + (self#k*other#k*cons));

for i = 0 to size - 1 do
  let idx = i + size in
  k := CI.(!k + (self#ar.(i)*other#k*cons) + (other#ar.(i)*self#k*cons));
  k := CI.(!k + (self#ar.(idx)*other#k*cons) + (other#ar.(idx)*self#k*cons));

  for j = 0 to size -1 do
    if i<>j then
       let jdx = j + size in
             k := CI.(!k + (self#ar.(i)*other#ar.(j)*cons));
       k := CI.(!k + (self#ar.(i)*other#ar.(jdx)*cons));
       k := CI.(!k + (self#ar.(idx)*other#ar.(j)*cons));
       k := CI.(!k + (self#ar.(idx)*other#ar.(jdx)*cons));
  done;
done;

result#set_k !k;

result;

   (*multiplication with a coeff*)
   method mul2 (c: float)=
     let result = new cai1 size in
     result#set_a CI.(c*@self#a);
     result#set_k CI.(c*@self#k);
     let ar1 = Array.create (2*size) (new interval 0.0 0.0) in
     for i = 0 to 2*size - 1 do
 Array.set ar1 i CI.(c*@ar.(i));
     done;
     result#set_ar ar1;
     result;        
   
   (*Evaluate the bound*)
    method evaluate =        
     let e = new interval (-1.0) 1. in 
     (*let e_ = new interval 0. 1. in*)
     
     let result = ref (new interval 0.0 0.0) in
     
     result := self#a;

     for i = 0 to size - 1 do
  let idx = i + size in
  result := CI.(!result + (eval self#ar.(i) self#ar.(idx)));
  
        (*result := CI.(!result + (self#ar.(i)*e));
        result := CI.(!result + (self#ar.(idx)*e_));*)
     done;

     result := CI.(!result + (self#k*e));
     !result;
end 
module CAI1 = struct
  let rec pow (t: cai1) (n: int)= 
    match n with
    | 1 -> t
    | _ -> t#mul  (pow t (n-1))

  let ( * ) (t1: cai1) (t2: cai1) = t1 #mul t2
  let ( *@) (c: float)(t: cai1)  = t #mul2 c
  let ( + ) (t1: cai1) (t2: cai1) = t1 #add t2
  let ( +@) (t: cai1)  (c: float)= t #add2 c
  let ( - ) (t1: cai1) (t2: cai1) = t1 #sub t2
  let ( -@ )(t: cai1)  (c: float)= t #sub2 c
  let ( ^ ) (t: cai1)  (n: int)  = pow t n
end

(*---------------------------------------*)   
(*The Chebyshev Approximation Form class denoted CAF_2, the form with epsilon = [0,1]*)
class caf2 (size: int) = object (self)
    val mutable a  = (new interval 0.0 0.0: interval)   (*coeff of free var*)
    val mutable k  = (new interval 0.0 0.0: interval)   (*coeff of epsilon+*)
    val mutable ar = (Array.create size (new interval 0.0 0.0): interval array)  (*array of coeff of epsilon i*)

    (*Get interface*)
    method a = a
    method k = k
    method ar = ar

    (*Set interface*)
    method set_a a1 = a<-a1
    method set_k k1 = k<- k1 
    method set_ar ar1 = ar<-ar1
    
    method printForm = 
      Printf.printf "[%f,%f] " a#l a#h;
for i = 1 to Array.length ar do
 	    Printf.printf "[%f,%f]e%d " ar.(i-1)#l ar.(i-1)#h i
done;
Printf.printf "[%f,%f]e+\n" k#l k#h;

    (* CAF Arithmetic *)

    (* Additional operator *)
    method add (other: caf2) = 	
let result = new caf2 size in
result#set_a CI.(self#a + other#a);

let cons = new interval 0. 1. in
result#set_k CI.((self#k*cons) + (other#k*cons));

let nar = Array.create size (new interval 0.0 0.0) in
for i = 0 to size - 1 do
  Array.set nar i CI.(self#ar.(i) + other#ar.(i));
done;
result#set_ar nar;
result;     

    (*addition by a coeff*)
    method add2 (c: float) = 
let result = new caf2 size in
result#set_a CI.(self#a +@ c);
result#set_k self#k;
let nar = Array.create size (new interval 0.0 0.0) in
for i = 0 to size - 1 do
  Array.set nar i self#ar.(i);
done;
result#set_ar nar;
result;

    (*subtraction operator*)
    method sub (other: caf2) = 
let result = new caf2 size in
result#set_a CI.(self#a - other#a);

let cons = new interval 0. 1. in
result#set_k CI.((self#k*cons) - (other#k*cons));

let nar = Array.create size (new interval 0.0 0.0) in
for i = 0 to size - 1 do
  Array.set nar i CI.(self#ar.(i) - other#ar.(i));
done;
result#set_ar nar;
result;

    (*subtraction by a coeff*)
    method sub2 (c: float) = 
let result = new caf2 size in
result#set_a CI.(self#a -@ c);
result#set_k self#k;
let nar = Array.create size (new interval 0.0 0.0) in
for i = 0 to size - 1 do
  Array.set nar i self#ar.(i);
done;
result#set_ar nar;
result;

    (*multiplication operator*)
    method mul (other: caf2) = 
let result = new caf2 size in

let a0 = ref (new interval 0.0 0.0) in
a0 := CI.(self#a * other#a);
let q = new interval (-0.25) 0.0 in
for i = 0 to size - 1 do
  a0 := CI.(!a0 + (q*(self#ar.(i)*other#ar.(i))));
done;
result#set_a !a0;

let nar = Array.create size (new interval 0.0 0.0) in
for i = 0 to size - 1 do
  Array.set nar i CI.((self#ar.(i)*other#a) + (other#ar.(i)*self#a) + (self#ar.(i)*other#ar.(i)));
done;
result#set_ar nar;

let cons = new interval 0. 1. in
let k = ref (new interval 0.0 0.0) in (*refer to e+*)

k := CI.((self#a*other#k*cons) + (self#k * other#a * cons));

for i = 0 to size - 1 do
  k := CI.(!k + (self#ar.(i)*other#k*cons) + (other#ar.(i)*self#k*cons));

  for j = 0 to size -1 do
    if i<>j then
             k := CI.(!k + (self#ar.(i)*other#ar.(j)*cons));
  done;
done;

result#set_k !k;

result;

   (*multiplication with a coeff*)
   method mul2 (c: float)=
     let result = new caf2 size in
     result#set_a CI.(c*@self#a);
     result#set_k CI.(c*@self#k);
     let ar1 = Array.create size (new interval 0.0 0.0) in
     for i = 0 to size - 1 do
 Array.set ar1 i CI.(c*@ar.(i));
     done;
     result#set_ar ar1;
     result;        
   
   (*Evaluate the bound*)
    method evaluate =        
     let cons = new interval 0. 1. in
     let result = ref (new interval 0.0 0.0) in
     
     result := self#a;

     for i = 0 to size - 1 do
        result := CI.(!result + (self#ar.(i)*cons));
     done;

     result := CI.(!result + (self#k*cons));
     !result;
end 
module CAF2 = struct
  let rec pow (t: caf2) (n: int)= 
    match n with
    | 1 -> t
    | _ -> t#mul  (pow t (n-1))

  let ( * ) (t1: caf2) (t2: caf2) = t1 #mul t2
  let ( *@) (c: float)(t: caf2)  = t #mul2 c
  let ( + ) (t1: caf2) (t2: caf2) = t1 #add t2
  let ( +@) (t: caf2)  (c: float)= t #add2 c
  let ( - ) (t1: caf2) (t2: caf2) = t1 #sub t2
  let ( -@) (t: caf2)  (c: float)= t #sub2 c
  let ( ^ ) (t: caf2)  (n: int)  = pow t n
end

(*---------------------------------------*)   
(*The Extended Chebyshev Approximation Form class denoted CF*)
class cai2 (size: int) = object (self)
    val mutable a  = (new interval 0.0 0.0: interval)   (*coeff of free var*)
    val mutable k  = (new interval 0.0 0.0: interval)   (*coeff of epsilon+*)
    val mutable ar = (Array.create (2*size) (new interval 0.0 0.0): interval array)  (*array of coeff of epsilon i*)
    val mutable m1 = (Array.make_matrix size size (new interval 0.0 0.0))  (*for coeff: a_i*a_j *)
    val mutable m2 = (Array.make_matrix size size (new interval 0.0 0.0))  (*for coeff: a_i*a_j *)
    val mutable m3 = (Array.make_matrix size size (new interval 0.0 0.0))  (*for coeff: a_i*a_j *)

    (*Get interface*)
    method a = a
    method k = k
    method ar = ar
    method m1 = m1
    method m2 = m2
    method m3 = m3

    (*Set interface*)
    method set_a a1 = a<-a1
    method set_k k1 = k<- k1 
    method set_ar ar1 = ar<-ar1
    method set_m1 ma1 = m1<-ma1
    method set_m2 ma2 = m2<-ma2
    method set_m3 ma3 = m3<-ma3
    
    method printForm = 
      Printf.printf "[%f,%f] " a#l a#h;
for i = 1 to Array.length ar do
 	    Printf.printf "[%f,%f]e%d " ar.(i-1)#l ar.(i-1)#h i
done;
Printf.printf "\n";
for i=1 to size-1 do
  for j=i+1 to size do
    Printf.printf "[%f,%f]e%d*e%d " m1.(i-1).(j-1)#l m1.(i-1).(j-1)#h i j
  done;
done;
Printf.printf "\n";
for i=1 to size do

  for j=1 to size do
    Printf.printf "[%f,%f]e%d*e%d " m2.(i-1).(j-1)#l m2.(i-1).(j-1)#h i (j+size)
  done;
done;
Printf.printf "\n";
for i=1 to size-1 do
  for j=i+1 to size do
    Printf.printf "[%f,%f]e%d*e%d " m3.(i-1).(j-1)#l m3.(i-1).(j-1)#h (i+size) (j+size)

  done;
done;
Printf.printf "\n";
Printf.printf "[%f,%f]e+-\n" k#l k#h;

    (* CF Arithmetic *)

    (* Additional operator *)
    method add (other: cai2) = 	
let result = new cai2 size in
result#set_a CI.(self#a + other#a);

let cons = new interval (-1.) 1. in
result#set_k CI.((self#k*cons) + (other#k*cons));

let nar = Array.create (2*size) (new interval 0.0 0.0) in
for i = 0 to 2*size - 1 do
  nar.(i) <- CI.(self#ar.(i) + other#ar.(i));
done;
result#set_ar nar;

let ma1 = (Array.make_matrix size size (new interval 0.0 0.0)) in
for i=0 to size-2 do
  for j=i+1 to size-1 do
    ma1.(i).(j) <- CI.(self#m1.(i).(j) + other#m1.(i).(j))
  done;
done;
result#set_m1 ma1;

let ma2 = (Array.make_matrix size size (new interval 0.0 0.0)) in
for i=0 to size-2 do
  for j=i+1 to size-1 do
      ma2.(i).(j) <- CI.(self#m2.(i).(j) + other#m2.(i).(j));
      ma2.(j).(i) <- CI.(self#m2.(j).(i) + other#m2.(j).(i));
  done;
done;
result#set_m2 ma2;

let ma3 = (Array.make_matrix size size (new interval 0.0 0.0)) in
for i=0 to size-2 do
  for j=i+1 to size-1 do
    ma3.(i).(j) <- CI.(self#m3.(i).(j) + other#m3.(i).(j))
  done;
done;
result#set_m3 ma3;

result;     

    (*addition by a coeff*)
    method add2 (c: float) = 
let result = new cai2 size in
result#set_a CI.(self#a +@ c);
result#set_k self#k;

let nar = Array.create (2*size) (new interval 0.0 0.0) in
for i = 0 to 2*size - 1 do
  nar.(i) <- self#ar.(i);
done;
result#set_ar nar;

let ma1 = (Array.make_matrix size size (new interval 0.0 0.0)) in
for i=0 to size-2 do
  for j=i+1 to size-1 do
    ma1.(i).(j) <- self#m1.(i).(j)
  done;
done;
result#set_m1 ma1;

let ma2 = (Array.make_matrix size size (new interval 0.0 0.0)) in
for i=0 to size-2 do
  for j=i+1 to size-1 do
    ma2.(i).(j) <- self#m2.(i).(j);
    ma2.(j).(i) <- self#m2.(j).(i);
  done;
done;
result#set_m2 ma2;

let ma3 = (Array.make_matrix size size (new interval 0.0 0.0)) in
for i=0 to size-2 do
  for j=i+1 to size-1 do
    ma3.(i).(j) <- self#m3.(i).(j)
  done;
done;
result#set_m3 ma3;

result;

   (* Substractional operator *)
    method sub (other: cai2) = 	
let result = new cai2 size in
result#set_a CI.(self#a - other#a);

let cons = new interval (-1.) 1. in
result#set_k CI.((self#k*cons) + (other#k*cons));

let nar = Array.create (2*size) (new interval 0.0 0.0) in
for i = 0 to 2*size - 1 do
  nar.(i) <- CI.(self#ar.(i) - other#ar.(i));
done;
result#set_ar nar;

let ma1 = (Array.make_matrix size size (new interval 0.0 0.0)) in
for i=0 to size-2 do
  for j=i+1 to size-1 do
    ma1.(i).(j) <- CI.(self#m1.(i).(j) - other#m1.(i).(j))
  done;
done;
result#set_m1 ma1;

let ma2 = (Array.make_matrix size size (new interval 0.0 0.0)) in
for i=0 to size-2 do
  for j=i+1 to size-1 do
    ma2.(i).(j) <- CI.(self#m2.(i).(j) - other#m2.(i).(j));
    ma2.(j).(i) <- CI.(self#m2.(j).(i) - other#m2.(j).(i));
  done;
done;
result#set_m2 ma2;

let ma3 = (Array.make_matrix size size (new interval 0.0 0.0)) in
for i=0 to size-2 do
  for j=i+1 to size-1 do
    ma3.(i).(j) <- CI.(self#m3.(i).(j) - other#m3.(i).(j))
  done;
done;
result#set_m3 ma3;

result;     

    (*substraction by a coeff*)
    method sub2 (c: float) = 
let result = new cai2 size in
result#set_a CI.(self#a -@ c);
result#set_k self#k;

let nar = Array.create (2*size) (new interval 0.0 0.0) in
for i = 0 to 2*size - 1 do
  nar.(i) <- self#ar.(i);
done;
result#set_ar nar;

let ma1 = (Array.make_matrix size size (new interval 0.0 0.0)) in
for i=0 to size-2 do
  for j=i+1 to size-1 do
    ma1.(i).(j) <- self#m1.(i).(j)
  done;
done;
result#set_m1 ma1;

let ma2 = (Array.make_matrix size size (new interval 0.0 0.0)) in
for i=0 to size-2 do
  for j=i+1 to size-1 do
    ma2.(i).(j) <- self#m2.(i).(j);
    ma2.(j).(i) <- self#m2.(j).(i);
  done;
done;
result#set_m2 ma2;

let ma3 = (Array.make_matrix size size (new interval 0.0 0.0)) in
for i=0 to size-2 do
  for j=i+1 to size-1 do
    ma3.(i).(j) <- self#m3.(i).(j)
  done;
done;
result#set_m3 ma3;

result;

    (*multiplication with: a0 *)
    method mul_cons(c: interval) = 
let result = new cai2 size in
result#set_a CI.(self#a * c);
let cons = new interval (-1.) 1. in
result#set_k CI.(self#k * c * cons);

let nar = Array.create (2*size) (new interval 0.0 0.0) in
for i = 0 to 2*size - 1 do
  nar.(i) <- CI.(self#ar.(i) * c);
done;
result#set_ar nar;

let ma1 = (Array.make_matrix size size (new interval 0.0 0.0)) in
for i=0 to size-2 do
  for j=i+1 to size-1 do
    ma1.(i).(j) <- CI.(self#m1.(i).(j) * c)
  done;
done;
result#set_m1 ma1;

let ma2 = (Array.make_matrix size size (new interval 0.0 0.0)) in
for i=0 to size-2 do
  for j=i+1 to size-1 do
    ma2.(i).(j) <- CI.(self#m2.(i).(j) * c);
    ma2.(j).(i) <- CI.(self#m2.(j).(i) * c)
  done;
done;
result#set_m2 ma2;

let ma3 = (Array.make_matrix size size (new interval 0.0 0.0)) in
for i=0 to size-2 do
  for j=i+1 to size-1 do
    ma3.(i).(j) <- CI.(self#m3.(i).(j) * c)
  done;
done;
result#set_m3 ma3;

result;

    (*multiplication with: a_i*e_i*)
    method mul_ei arr = 
let result = new cai2 size in

let q = new interval (-0.25) 0.0 in
let f = new interval (-0.25) (0.25) in

(*for dependent coeff*)
let a0 = ref (new interval 0.0 0.0) in	
for i=0 to size-1 do
  let idx = i+size in
  a0 := CI.(!a0 + (q*(self#ar.(i)*arr.(i))));
	  a0 := CI.(!a0 + (f*(self#ar.(idx)*arr.(i))));
done;  	  
result#set_a !a0;

      (*for epsilon i part*)	
let nar = Array.create (2*size) (new interval 0.0 0.0) in
for i=0 to size-1 do
  let idx = i + size in
  nar.(i) <- CI.(nar.(i)+self#a*arr.(i)+self#ar.(idx)*arr.(i))
done;

for i=0 to size-2 do
for j=i+1 to size-1 do
    nar.(i) <- CI.(nar.(i) + arr.(j)*self#m1.(i).(j)*q);
    nar.(j) <- CI.(nar.(j) + arr.(i)*self#m1.(i).(j)*q);
    nar.(i) <- CI.(nar.(i) + arr.(j)*self#m2.(i).(j)*f);
    nar.(j) <- CI.(nar.(j) + arr.(i)*self#m2.(j).(i)*f);
done;
done;

      (*for epsilon i+n part*)	
for i=0 to size-1 do
  let idx = i+size in
  nar.(idx) <- CI. (nar.(idx) + arr.(i)*self#ar.(i));
done;

for i=0 to size-2 do
  let idx = i + size in
for j=i+1 to size-1 do
  let jdx = j+size in
	    nar.(jdx) <- CI.(nar.(jdx)+arr.(i)*self#m2.(i).(j)*q);
    nar.(idx) <- CI.(nar.(idx)+arr.(j)*self#m2.(j).(i)*q);
    nar.(idx) <- CI.(nar.(idx)+arr.(j)*self#m3.(i).(j)*f);
    nar.(jdx) <- CI.(nar.(jdx)+arr.(i)*self#m3.(i).(j)*f);
done;
done;
result#set_ar nar;

(*for e_i*e_j part*)
let ma1 = (Array.make_matrix size size (new interval 0.0 0.0)) in

for i=0 to size-2 do
  for j=i+1 to size-1 do
    ma1.(i).(j) <- CI.(ma1.(i).(j)+arr.(i)*self#ar.(j)+
		   arr.(j)*self#ar.(i)+arr.(j)*self#m2.(i).(j)+arr.(i)*self#m2.(j).(i));
  done;
done;
result#set_m1 ma1;


(*for e_i*e_{j+n} part*)
let ma2 = (Array.make_matrix size size (new interval 0.0 0.0)) in
for i=0 to size-2 do
    let idx = i+size in
for j=i+1 to size-1 do
    let jdx = j+size in
    ma2.(i).(j) <- CI.(ma2.(i).(j)+arr.(i)*self#ar.(jdx));
    ma2.(j).(i) <- CI.(ma2.(j).(i)+arr.(j)*self#ar.(idx));

    ma2.(i).(j) <- CI.(ma2.(i).(j)+arr.(j)*self#m1.(i).(j));
    ma2.(j).(i) <- CI.(ma2.(j).(i)+arr.(i)*self#m1.(i).(j));

    ma2.(i).(j) <- CI.(ma2.(i).(j)+arr.(i)*self#m3.(i).(j));
    ma2.(j).(i) <- CI.(ma2.(j).(i)+arr.(j)*self#m3.(i).(j));
done;	
done;	
result#set_m2 ma2;

(*for e_{i+n}*e_{j+n} part*)
let ma3 = (Array.make_matrix size size (new interval 0.0 0.0)) in
for i=0 to size-2 do
for j=i+1 to size-1 do
    ma3.(i).(j) <- CI.(ma3.(i).(j)+arr.(i)*self#m2.(i).(j) + arr.(j)*self#m2.(j).(i));
done;
done;	
result#set_m3 ma3;

(*for e_{+-} part*)
let c = new interval (-1.) 1. in
let _k = ref (new interval 0.0 0.0) in
for i=0 to size-1 do
  _k := CI.(!_k+c*arr.(i)*self#k);
  for j=0 to size-2 do
    for k=j+1 to size-1 do
      if (i<>j)&&(i<>k) then
	_k := CI.(!_k + c*arr.(i)*self#m1.(j).(k)+c*arr.(i)*self#m2.(j).(k)+c*arr.(i)*self#m2.(k).(j)+
		    c*arr.(i)*self#m3.(j).(k));
    done;
  done;
done;
result#set_k !_k;

result;

    (*multiplication: a_i*e_{i+n}*)
    method mul_ein arr = 
let result = new cai2 size in

let q = new interval (-0.25) 0.0 in
let f = new interval (-0.25) (0.25) in

(*for dependent coeff*)
let a0 = ref (new interval 0.0 0.0) in	
for i=0 to size-1 do
  let idx = i+size in
  a0 := CI.(!a0 + (f*(self#ar.(i)*arr.(idx))));
	  a0 := CI.(!a0 + (q*(self#ar.(idx)*arr.(idx))));
done;  	  
result#set_a !a0;

      (*for epsilon i part*)	
let nar = Array.create (2*size) (new interval 0.0 0.0) in
for i=0 to size-1 do
  let idx = i+size in
  nar.(i) <- CI.(nar.(i)+self#ar.(i)*arr.(idx))
done;

for i=0 to size-2 do
	  let idx = i+size in
for j=i+1 to size-1 do
  let jdx = j+size in
    nar.(i) <- CI.(nar.(i) + arr.(jdx)*self#m1.(i).(j)*f);
    nar.(j) <- CI.(nar.(j) + arr.(idx)*self#m1.(i).(j)*f);
    nar.(i) <- CI.(nar.(i) + arr.(jdx)*self#m2.(i).(j)*q);	
    nar.(j) <- CI.(nar.(j) + arr.(idx)*self#m2.(j).(i)*q);	
done;
done;

      (*for epsilon i+n part*)	
for i=0 to size-1 do
  let idx = i + size in
  nar.(idx) <- CI.(nar.(idx)+self#a*arr.(idx)+self#ar.(idx)*arr.(idx))
done;

for i=0 to size-2 do
  let idx = i + size in
for j=i+1 to size-1 do
  let jdx = j + size in
    nar.(jdx) <- CI.(nar.(jdx)+arr.(idx)*self#m2.(i).(j)*f);
    nar.(idx) <- CI.(nar.(idx)+arr.(jdx)*self#m2.(j).(i)*f);
    nar.(idx) <- CI.(nar.(idx)+arr.(jdx)*self#m3.(i).(j)*q);
    nar.(jdx) <- CI.(nar.(jdx)+arr.(idx)*self#m3.(i).(j)*q);
done;
done;
result#set_ar nar;

(*for e_i*e_j part*)
let ma1 = (Array.make_matrix size size (new interval 0.0 0.0)) in

for i=0 to size-2 do
  let idx = i+size in
  for j=i+1 to size-1 do
    let jdx = i+size in
    ma1.(i).(j) <- CI.(ma1.(i).(j)+arr.(idx)*self#m1.(i).(j)+arr.(jdx)*self#m1.(i).(j));
  done;
done;
result#set_m1 ma1;

(*for e_i*e_{j+n} part*)
let ma2 = (Array.make_matrix size size (new interval 0.0 0.0)) in

for i=0 to size-2 do
	  let idx = i+size in
for j=i+1 to size-1 do
	  let jdx = j+size in
    ma2.(i).(j) <- CI.(ma2.(i).(j)+arr.(jdx)*self#ar.(i));
    ma2.(j).(i) <- CI.(ma2.(j).(i)+arr.(idx)*self#ar.(j));
    ma2.(i).(j) <- CI.(ma2.(i).(j)+arr.(jdx)*self#m2.(i).(j)+arr.(idx)*self#m2.(i).(j));
    ma2.(j).(i) <- CI.(ma2.(j).(i)+arr.(idx)*self#m2.(j).(i)+arr.(jdx)*self#m2.(j).(i));
done;
done;	
result#set_m2 ma2;

(*for e_{i+n}*e_{j+n} part*)
let ma3 = (Array.make_matrix size size (new interval 0.0 0.0)) in

for i=0 to size-2 do
  let idx = i+size in
  for j=i+1 to size-1 do
    let jdx = j+size in
    ma3.(i).(j) <- CI.(ma3.(i).(j)+arr.(idx)*self#ar.(jdx)+
		   arr.(jdx)*self#ar.(idx)+arr.(idx)*self#m3.(i).(j)+arr.(jdx)*self#m3.(i).(j));
  done;
done;
result#set_m3 ma3;

(*for e_{+-} part*)
let c = new interval (-1.) 1. in
let _k = ref (new interval 0.0 0.0) in
for i=0 to size-1 do
  let idx = i+size in
  _k := CI.(!_k + c*arr.(idx)*self#k);
  for j=0 to size-2 do	  
    for k=j+1 to size-1 do
      if (i<>j)&&(i<>k) then
	_k := CI.(!_k + c*arr.(idx)*self#m1.(j).(k)+c*arr.(idx)*self#m2.(j).(k)
		   +c*arr.(idx)*self#m2.(k).(j)+c*arr.(idx)*self#m3.(j).(k));
    done;
  done;
done;
result#set_k !_k;

result;

    (*multiplication: a_i*e_i*e_j*)
    method mul_eij ma = 
let result = new cai2 size in

let q = new interval (-0.25) 0.0 in
let f = new interval (-0.25) (0.25) in

(*for dependent coeff*)
let a0 = ref (new interval 0.0 0.0) in	
for i=0 to size-2 do
  for j=i+1 to size-1 do
  	    a0 := CI.(!a0+ma.(i).(j)*self#m1.(i).(j)*q*q);
    a0 := CI.(!a0+ma.(i).(j)*self#m2.(i).(j)*q*f);
  	    a0 := CI.(!a0+ma.(i).(j)*self#m2.(j).(i)*q*f);
     	    a0 := CI.(!a0+ma.(i).(j)*self#m3.(i).(j)*f*f);
  done;
done;

result#set_a !a0;

      (*for epsilon i part*)	
let nar = Array.create (2*size) (new interval 0.0 0.0) in

for i=0 to size-2 do
  let idx = i+size in
for j=i+1 to size-1 do
  let jdx = j+size in
  	    nar.(i) <- CI.(nar.(i) + ma.(i).(j)*self#ar.(j)*q);
  	    nar.(j) <- CI.(nar.(j) + ma.(i).(j)*self#ar.(i)*q);

    nar.(i) <- CI.(nar.(i) + ma.(i).(j)*self#ar.(jdx)*f);
    nar.(j) <- CI.(nar.(j) + ma.(i).(j)*self#ar.(idx)*f);

    nar.(i) <- CI.(nar.(i) + ma.(i).(j)*self#m2.(j).(i)*q);
    nar.(j) <- CI.(nar.(j) + ma.(i).(j)*self#m2.(i).(j)*q);

    nar.(i) <- CI.(nar.(i) + ma.(i).(j)*self#m3.(i).(j)*f);
    nar.(j) <- CI.(nar.(j) + ma.(i).(j)*self#m3.(i).(j)*f);
done;
done;

      (*for epsilon i+n part*)
for i=0 to size-2 do
  let idx = i + size in
for j=i+1 to size-1 do
  let jdx = j + size in
    nar.(idx) <- CI.(nar.(idx)+ma.(i).(j)*self#m1.(i).(j)*q);
  	    nar.(jdx) <- CI.(nar.(jdx)+ma.(i).(j)*self#m1.(i).(j)*q);
    	    nar.(idx) <- CI.(nar.(idx)+ma.(i).(j)*self#m2.(i).(j)*f);
     	    nar.(jdx) <- CI.(nar.(jdx)+ma.(i).(j)*self#m2.(j).(i)*f);
done;
done;
result#set_ar nar;

(*for e_i*e_j part*)
let ma1 = (Array.make_matrix size size (new interval 0.0 0.0)) in

for i=0 to size-2 do
  let idx = i+size in
  for j=i+1 to size-1 do
    let jdx = j+size in
    ma1.(i).(j) <- CI.(ma1.(i).(j)+ma.(i).(j)*self#a+ma.(i).(j)*self#ar.(jdx)+
			 ma.(i).(j)*self#ar.(idx)+ma.(i).(j)*self#m3.(i).(j));
  done;
done;
result#set_m1 ma1;

(*for e_i*e_{j+n} part*)
let ma2 = (Array.make_matrix size size (new interval 0.0 0.0)) in

for i=0 to size-2 do
for j=i+1 to size-1 do
    ma2.(i).(j) <- CI.(ma2.(i).(j)+ma.(i).(j)*self#ar.(j));
	    ma2.(j).(i) <- CI.(ma2.(j).(i)+ma.(i).(j)*self#ar.(i));
  	    ma2.(i).(j) <- CI.(ma2.(i).(j)+ma.(i).(j)*self#m2.(j).(i));
     	    ma2.(j).(i) <- CI.(ma2.(i).(j)+ma.(i).(j)*self#m2.(i).(j));
done;
done;	
result#set_m2 ma2;

(*for e_{i+n}*e_{j+n} part*)
let ma3 = (Array.make_matrix size size (new interval 0.0 0.0)) in

for i=0 to size-2 do
  for j=i+1 to size-1 do
    ma3.(i).(j) <- CI.(ma3.(i).(j)+ma.(i).(j)*self#m1.(i).(j));
  done;
done;
result#set_m3 ma3;

(*for e_{+-} part*)
let c = new interval (-1.) 1. in
let _k = ref (new interval 0.0 0.0) in
for i=0 to size-2 do	  	  
  for j=i+1 to size-1 do	    
   	    _k := CI.(!_k+c*ma.(i).(j)*self#k);	    
    (if (j<>size-1) then 
        let jdx1 = size-1 in
	let jdx2 = 2*size-1 in
	_k := CI.(!_k+c*ma.(i).(j)*self#ar.(jdx1)+c*ma.(i).(j)*self#ar.(jdx2));	    
    );
    for k=0 to size-2 do	      
      (if (k<>i)&&(k<>j) then 
	let kdx = k+size in
	_k := CI.(!_k+c*ma.(i).(j)*self#ar.(k)+c*ma.(i).(j)*self#ar.(kdx));	      
      );
      for l=k+1 to size-1 do
	if (i<>k) || (j<>l) then
	  (_k := CI.(!_k + c*ma.(i).(j)*self#m1.(k).(l)+c*ma.(i).(j)*self#m2.(k).(l)+c*ma.(i).(j)*self#m2.(l).(k)
		     +c*ma.(i).(j)*self#m3.(k).(l)));
      done;
    done;
  done;
done;
result#set_k !_k;

result;

    (*multiplication: a_i*e_i*e_{j+n}*)
    method mul_eijn ma = 
let result = new cai2 size in

let q = new interval (-0.25) 0.0 in
let f = new interval (-0.25) (0.25) in

(*for dependent coeff*)
let a0 = ref (new interval 0.0 0.0) in	
for i=0 to size-1 do
  for j=0 to size-1 do
    if (i<>j) then(
  	    a0 := CI.(!a0+ma.(i).(j)*self#m1.(i).(j)*q*f+ma.(i).(j)*self#m1.(j).(i)*q*f);
    a0 := CI.(!a0+ma.(i).(j)*self#m2.(i).(j)*q*q);
  	    a0 := CI.(!a0+ma.(i).(j)*self#m2.(j).(i)*f*f);
     	    a0 := CI.(!a0+ma.(i).(j)*self#m3.(i).(j)*q*f+ma.(i).(j)*self#m3.(j).(i)*q*f)
   );
  done;
done;

result#set_a !a0;

      (*for epsilon i part*)	
let nar = Array.create (2*size) (new interval 0.0 0.0) in

for i=0 to size-1 do
for j=0 to size-1 do
  let jdx = j+size in
  if (i<>j) then(
  	    nar.(i) <- CI.(nar.(i) + ma.(i).(j)*self#ar.(j)*f);
    nar.(i) <- CI.(nar.(i) + ma.(i).(j)*self#ar.(jdx)*q);
  	    nar.(j) <- CI.(nar.(j) + ma.(i).(j)*self#m1.(i).(j)*q + ma.(i).(j)*self#m1.(j).(i)*q);
  	    nar.(i) <- CI.(nar.(i) + ma.(i).(j)*self#m2.(j).(i)*f);
    nar.(j) <- CI.(nar.(j) + ma.(i).(j)*self#m2.(j).(i)*f);
    nar.(i) <- CI.(nar.(i) + ma.(i).(j)*self#m3.(i).(j)*q + ma.(i).(j)*self#m3.(j).(i)*q)
    );
done;
done;

      (*for epsilon i+n part*)
for i=0 to size-2 do
  let idx = i + size in
for j=i+1 to size-1 do
  let jdx = j + size in
	    nar.(jdx) <- CI.(nar.(jdx)+ma.(i).(j)*self#ar.(i)*q);
	    nar.(jdx) <- CI.(nar.(jdx)+ma.(i).(j)*self#ar.(idx)*f);
    nar.(idx) <- CI.(nar.(idx)+ma.(i).(j)*self#m1.(i).(j)*f);
    	    nar.(idx) <- CI.(nar.(idx)+ma.(i).(j)*self#m2.(i).(j)*q);
     	    nar.(jdx) <- CI.(nar.(jdx)+ma.(i).(j)*self#m3.(i).(j)*f+ma.(i).(j)*self#m3.(j).(i)*f);
done;
done;
result#set_ar nar;

(*for e_i*e_j part*)
let ma1 = (Array.make_matrix size size (new interval 0.0 0.0)) in

for i=0 to size-2 do
  for j=i+1 to size-1 do
      ma1.(i).(j) <- CI.(ma1.(i).(j)+ma.(i).(j)*self#ar.(j)+ma.(j).(i)*self#ar.(i));
	      ma1.(i).(j) <- CI.(ma1.(i).(j)+ma.(i).(j)*self#m2.(j).(i)+ma.(j).(i)*self#m2.(i).(j));
  done;
done;
result#set_m1 ma1;

(*for e_i*e_{j+n} part*)
let ma2 = (Array.make_matrix size size (new interval 0.0 0.0)) in

for i=0 to size-1 do
  let idx = i+size in
for j=0 to size-1 do
  let jdx = j+size in
  if (j<>i) then(
   	    ma2.(i).(j) <- CI.(ma2.(i).(j)+ma.(i).(j)*self#a);
    ma2.(i).(j) <- CI.(ma2.(i).(j)+ma.(i).(j)*self#ar.(idx)+ma.(i).(j)*self#ar.(jdx));
  	    ma2.(j).(i) <- CI.(ma2.(j).(i)+ma.(i).(j)*self#m1.(i).(j)+ma.(i).(j)*self#m1.(j).(i));
     	    ma2.(i).(j) <- CI.(ma2.(i).(j)+ma.(i).(j)*self#m3.(i).(j)+ma.(i).(j)*self#m3.(j).(i));
   );
done;
done;	
result#set_m2 ma2;

(*for e_{i+n}*e_{j+n} part*)
let ma3 = (Array.make_matrix size size (new interval 0.0 0.0)) in

for i=0 to size-2 do
  for j=i+1 to size-1 do
    ma3.(i).(j) <- CI.(ma3.(i).(j)+ma.(i).(j)*self#ar.(i)+ma.(j).(i)*self#ar.(j));
  	    ma3.(i).(j) <- CI.(ma3.(i).(j)+ma.(i).(j)*self#m2.(i).(j)+ma.(j).(i)*self#m2.(j).(i));
  done;
done;
result#set_m3 ma3;

(*for e_{+-} part*)
let c = new interval (-1.) 1. in
let _k = ref (new interval 0.0 0.0) in
for i=0 to size-2 do	  
  for j= i+1 to size-1 do	    
   	    _k := CI.(!_k+c*ma.(i).(j)*self#k+c*ma.(j).(i)*self#k);
    (if (j<>size-1) then 
    	        let jdx1 = size-1 in
	let jdx2 = 2*size-1 in
	_k := CI.(!_k+c*ma.(i).(j)*self#ar.(jdx1)+c*ma.(i).(j)*self#ar.(jdx2));
	_k := CI.(!_k+c*ma.(j).(i)*self#ar.(jdx1)+c*ma.(j).(i)*self#ar.(jdx2));
    );
    for k=0 to size-2 do
      (if (k<>i) && (k<>j) then 
	let kdx = k+size in
	_k := CI.(!_k+c*ma.(i).(j)*self#ar.(k)+c*ma.(i).(j)*self#ar.(kdx));
	_k := CI.(!_k+c*ma.(j).(i)*self#ar.(k)+c*ma.(j).(i)*self#ar.(kdx));
      );
      for l=k+1 to size-1 do
      if (i<>k) || (j<>l) then
	(_k := CI.(!_k + c*ma.(i).(j)*self#m1.(k).(l)+c*ma.(i).(j)*self#m2.(k).(l)+c*ma.(i).(j)*self#m2.(l).(k)+
		     c*ma.(i).(j)*self#m3.(k).(l)));
	(_k := CI.(!_k + c*ma.(j).(i)*self#m1.(k).(l)+c*ma.(j).(i)*self#m2.(k).(l)+c*ma.(j).(i)*self#m2.(l).(k)+
		     c*ma.(j).(i)*self#m3.(k).(l)));
      done;
    done;
  done;
done;
result#set_k !_k;

result;

    (*multiplication: a_i*e_{i+n}*e_{j+n}*)
    method mul_einjn ma = 
let result = new cai2 size in

let q = new interval (-0.25) 0.0 in
let f = new interval (-0.25) (0.25) in

(*for dependent coeff*)
let a0 = ref (new interval 0.0 0.0) in	
for i=0 to size-2 do
  for j=i+1 to size-1 do
  	    a0 := CI.(!a0+ma.(i).(j)*self#m1.(i).(j)*f*f);
    a0 := CI.(!a0+ma.(i).(j)*self#m2.(i).(j)*q*f+ma.(i).(j)*self#m2.(j).(i)*q*f);
     	    a0 := CI.(!a0+ma.(i).(j)*self#m3.(i).(j)*q*q);
  done;
done;

result#set_a !a0;

      (*for epsilon i part*)	
let nar = Array.create (2*size) (new interval 0.0 0.0) in

for i=0 to size-2 do
for j=i+1 to size-1 do
  	    nar.(i) <- CI.(nar.(i) + ma.(i).(j)*self#m1.(i).(j)*f);
     	    nar.(j) <- CI.(nar.(j) + ma.(i).(j)*self#m1.(i).(j)*f);
    nar.(i) <- CI.(nar.(j) + ma.(i).(j)*self#m2.(i).(j)*q);
  	    nar.(j) <- CI.(nar.(j) + ma.(i).(j)*self#m2.(j).(i)*q);
done;
done;

      (*for epsilon i+n part*)
for i=0 to size-2 do
  let idx = i + size in
for j=i+1 to size-1 do
  let jdx = j + size in
  	    nar.(idx) <- CI.(nar.(idx)+ma.(i).(j)*self#ar.(j)*f);
  	    nar.(jdx) <- CI.(nar.(jdx)+ma.(i).(j)*self#ar.(i)*f);

  	    nar.(idx) <- CI.(nar.(idx)+ma.(i).(j)*self#ar.(jdx)*q);
  	    nar.(jdx) <- CI.(nar.(jdx)+ma.(i).(j)*self#ar.(idx)*q);  

    	    nar.(jdx) <- CI.(nar.(jdx)+ma.(i).(j)*self#m2.(i).(j)*f);
    	    nar.(idx) <- CI.(nar.(idx)+ma.(i).(j)*self#m2.(j).(i)*f);

     	    nar.(idx) <- CI.(nar.(idx)+ma.(i).(j)*self#m3.(i).(j)*q);
     	    nar.(jdx) <- CI.(nar.(jdx)+ma.(i).(j)*self#m3.(i).(j)*q);
done;
done;
result#set_ar nar;

(*for e_i*e_j part*)
let ma1 = (Array.make_matrix size size (new interval 0.0 0.0)) in

for i=0 to size-2 do
  for j=i+1 to size-1 do
      ma1.(i).(j) <- CI.(ma1.(i).(j)+ma.(i).(j)*self#m1.(i).(j));
  done;
done;
result#set_m1 ma1;

(*for e_i*e_{j+n} part*)
let ma2 = (Array.make_matrix size size (new interval 0.0 0.0)) in

for i=1 to size-2 do
for j=i+1 to size-1 do
    ma2.(i).(j) <- CI.(ma2.(i).(j)+ma.(i).(j)*self#ar.(i));
    ma2.(j).(i) <- CI.(ma2.(j).(i)+ma.(i).(j)*self#ar.(j));

  	    ma2.(i).(j) <- CI.(ma2.(i).(j)+ma.(i).(j)*self#m2.(i).(j));
  	    ma2.(j).(i) <- CI.(ma2.(j).(i)+ma.(i).(j)*self#m2.(j).(i));
done;
done;	
result#set_m2 ma2;

(*for e_{i+n}*e_{j+n} part*)
let ma3 = (Array.make_matrix size size (new interval 0.0 0.0)) in

for i=0 to size-2 do
  let idx = i+size in
  for j=i+1 to size-1 do
    let jdx = j+size in
    ma3.(i).(j) <- CI.(ma3.(i).(j)+ma.(i).(j)*self#a);
    ma3.(i).(j) <- CI.(ma3.(i).(j)+ma.(i).(j)*self#ar.(jdx)+ma.(i).(j)*self#ar.(idx));
  	    ma3.(i).(j) <- CI.(ma3.(i).(j)+ma.(i).(j)*self#m3.(i).(j));
  done;
done;
result#set_m3 ma3;

(*for e_{+-} part*)
let c = new interval (-1.) 1. in
let _k = ref (new interval 0.0 0.0) in
for i=0 to size-2 do	  
  for j= i+1 to size-1 do	    
   	    _k := CI.(!_k+c*ma.(i).(j)*self#k);
    (if (j<>size-1) then 
    	        let jdx1 = size-1 in
	let jdx2 = 2*size-1 in
	_k := CI.(!_k+c*ma.(i).(j)*self#ar.(jdx1)+c*ma.(i).(j)*self#ar.(jdx2));
    );
    for k=0 to size-2 do
      (if (k<>i) && (k<>j) then 
	let kdx = k+size in
	_k := CI.(!_k+c*ma.(i).(j)*self#ar.(k)+c*ma.(i).(j)*self#ar.(kdx));
      );
      for l=k+1 to size-1 do
      if (i<>k) || (j<>l) then
	(_k := CI.(!_k + c*ma.(i).(j)*self#m1.(k).(l)+c*ma.(i).(j)*self#m2.(k).(l)+
		     c*ma.(i).(j)*self#m2.(l).(k)+c*ma.(i).(j)*self#m3.(k).(l)));
      done;
    done;
  done;
done;
result#set_k !_k;

result;

    (*multiplication: k *)
    method mul_k (k1:interval) = 
let result = new cai2 size in

let a0 = new interval 0.0 0.0 in	
result#set_a a0;

let nar = Array.create (2*size) (new interval 0.0 0.0) in
result#set_ar nar;

let ma1 = (Array.make_matrix size size (new interval 0.0 0.0)) in
result#set_m1 ma1;

let ma2 = (Array.make_matrix size size (new interval 0.0 0.0)) in
result#set_m2 ma2;

let ma3 = (Array.make_matrix size size (new interval 0.0 0.0)) in
result#set_m3 ma3;

let c = new interval (-1.0) 1.0 in
let _k = ref (new interval 0.0 0.0) in
_k := CI.(!_k + c*k1*self#a + c*k1*self#k);

for i=0 to size-1 do
  let idx = i+size in
	    _k := CI.(!_k + c*k1*self#ar.(i)+c*k1*self#ar.(idx));	  
done;

for i=0 to size-2 do
for j=i+1 to size-1 do
	    _k := CI.(!_k + c*k1*self#m1.(i).(j)+c*k1*self#m2.(i).(j)+c*k1*self#m2.(j).(i)+c*k1*self#m3.(i).(j));	  	   done;
done;
result#set_k !_k;

result;

    (*Exact multiplication operator*)
    method mul (other: cai2) = 
let result = ref (new cai2 size) in
result := !result#add (self#mul_cons other#a);
result := !result#add (self#mul_ei other#ar);
result := !result#add (self#mul_ein other#ar);
result := !result#add (self#mul_eij other#m1);
result := !result#add (self#mul_eijn other#m2);
result := !result#add (self#mul_einjn other#m3);
result := !result#add (self#mul_k other#k);
!result;

    (*Evaluate the bound*)
    method evaluate =        
     let e = new interval (-1.0) 1. in
     (*let e_ = new interval 0. 1. in*)
     let result = ref (new interval 0.0 0.0) in
     
     result := self#a;


     for i = 0 to size - 1 do
  let idx = i + size in
  result := CI.(!result + (eval self#ar.(i) self#ar.(idx)));
     done;

     (*can be optimize in this step*)       
     for i=0 to size-2 do
 for j=i+1 to size-1 do
   (*result:= CI.(!result + self#m1.(i).(j)*e + self#m3.(i).(j)*e_);*)
   result:= CI.(!result + (eval self#m1.(i).(j) self#m3.(i).(j)));
   result:= CI.(!result + self#m2.(i).(j)*e + self#m2.(j).(i)*e);
 done;
     done;

     result := CI.(!result + (self#k*e));
     !result;

end

module CAI2 = struct
  let rec pow (t: cai2) (n: int)= 
    match n with
    | 1 -> t
    | _ -> t#mul  (pow t (n-1))

  let ( * ) (t1: cai2) (t2: cai2)   = t1 #mul t2
  let ( *@) (c:float)(t: cai2)    = t #mul_cons (new interval c c)
  let ( + ) (t1: cai2) (t2: cai2)   = t1 #add t2
  let ( +@) (t: cai2)  (c: float) = t #add2 c
  let ( - ) (t1: cai2) (t2: cai2)   = t1 #sub t2
  let ( -@ )(t: cai2)  (c: float) = t #sub2 c
  let ( ^ ) (t: cai2)  (n: int)   = pow t n
end

(*---------------------------------------*)   
(*Optimization of the Chebyshev Approximation Form class denoted CAI3*)
class cai3 (size: int) = object (self)
    val mutable a  = (new interval 0.0 0.0: interval)   (*coeff of free var*)
    val mutable k  = (new interval 0.0 0.0: interval)   (*coeff of epsilon+*)
    val mutable ar = (Array.create (2*size) (new interval 0.0 0.0): interval array)  (*array of coeff of epsilon i*)
    val mutable m  = (Array.make_matrix size size (new interval 0.0 0.0))  (*for coeff: a_i*a_j *)

    (*Get interface*)
    method a = a
    method k = k
    method ar = ar
    method m = m

    (*Set interface*)
    method set_a a1 = a<-a1
    method set_k k1 = k<- k1 
    method set_ar ar1 = ar<-ar1
    method set_m m1 = m<-m1
    
    method printForm = 
      Printf.printf "[%f,%f] " a#l a#h;
for i = 1 to Array.length ar do
 	    Printf.printf "[%f,%f]e%d " ar.(i-1)#l ar.(i-1)#h i
done;
Printf.printf "\n";
for i=1 to size-1 do
  for j=i+1 to size do
    Printf.printf "[%f,%f]e%d*e%d " m.(i-1).(j-1)#l m.(i-1).(j-1)#h i j
  done;
done;
Printf.printf "\n";
Printf.printf "[%f,%f]e+-\n" k#l k#h;

    (* CAI3 Arithmetic *)

    (* Additional operator *)
    method add (other: cai3) = 	
let result = new cai3 size in
result#set_a CI.(self#a + other#a);

let cons = new interval (-1.) 1. in
result#set_k CI.((self#k*cons) + (other#k*cons));

let nar = Array.create (2*size) (new interval 0.0 0.0) in
for i = 0 to 2*size - 1 do
  Array.set nar i CI.(self#ar.(i) + other#ar.(i));
done;
result#set_ar nar;

let ma = (Array.make_matrix size size (new interval 0.0 0.0)) in
for i=0 to size-2 do
  for j=i+1 to size-1 do
    ma.(i).(j) <- CI.(self#m.(i).(j) + other#m.(i).(j))
  done;
done;
result#set_m ma;
result;     

    (*addition by a coeff*)
    method add2 (c: float) = 
let result = new cai3 size in
result#set_a CI.(self#a +@ c);
result#set_k self#k;
let nar = Array.create (2*size) (new interval 0.0 0.0) in
for i = 0 to 2*size - 1 do
  Array.set nar i self#ar.(i);
done;
result#set_ar nar;

let ma = (Array.make_matrix size size (new interval 0.0 0.0)) in
for i=0 to size-2 do
  for j=i+1 to size-1 do
    ma.(i).(j) <- self#m.(i).(j)
  done;
done;
result#set_m ma;
result;

    (*subtraction operator*)
    method sub (other: cai3) = 
let result = new cai3 size in
result#set_a CI.(self#a - other#a);

let cons = new interval (-1.) 1. in
result#set_k CI.((self#k*cons) + (other#k*cons));

let nar = Array.create (2*size) (new interval 0.0 0.0) in
for i = 0 to 2*size - 1 do
  Array.set nar i CI.(self#ar.(i) - other#ar.(i));
done;
result#set_ar nar;

let ma = (Array.make_matrix size size (new interval 0.0 0.0)) in
for i=0 to size-2 do
  for j=i+1 to size-1 do
    ma.(i).(j) <- CI.(self#m.(i).(j) - other#m.(i).(j))
  done;
done;
result#set_m ma;
result;

    (*subtraction by a coeff*)
    method sub2 (c: float) = 
let result = new cai3 size in
result#set_a CI.(self#a -@ c);
result#set_k self#k;
let nar = Array.create (2*size) (new interval 0.0 0.0) in
for i = 0 to 2*size - 1 do
  Array.set nar i self#ar.(i);
done;
result#set_ar nar;

let ma = (Array.make_matrix size size (new interval 0.0 0.0)) in
for i=0 to size-2 do
  for j=i+1 to size-1 do
    ma.(i).(j) <- self#m.(i).(j)
  done;
done;
result#set_m ma;
result;

    (*multiplication operator*)
    method mul (other: cai3) = 
let result = new cai3 size in

(*Compute a0*)
let a0 = ref (new interval 0.0 0.0) in
a0 := CI.(self#a * other#a);
let q = new interval (-0.25) 0.0 in
let f = new interval (-0.25) (0.25) in
for i = 0 to size - 1 do
  let idx = i + size in
  a0 := CI.(!a0 + (q*(self#ar.(i)*other#ar.(i))));
  a0 := CI.(!a0 + (q*(self#ar.(idx)*other#ar.(idx))));
	  a0 := CI.(!a0 + (f*(self#ar.(i)*other#ar.(idx))));
  a0 := CI.(!a0 + (f*(self#ar.(idx)*other#ar.(i))));
done;
result#set_a !a0;

(*Compute coeff for epsilon i=1 to size*)
let nar = Array.create (2*size) (new interval 0.0 0.0) in
for i = 0 to size - 1 do
  let idx = i + size in
  Array.set nar i CI.((self#ar.(i)*other#a)+(other#ar.(i)*self#a)+(self#ar.(i)*other#ar.(idx))+(self#ar.(idx)*other#ar.(i)));
  Array.set nar (idx) CI.((self#ar.(idx)*other#a)+(other#ar.(idx)*self#a)+(self#ar.(i)*other#ar.(i))+(self#ar.(idx)*other#ar.(idx)));
done;

(*Compute coeff for epsilon_i*epsilon_j*)
let ma = (Array.make_matrix size size (new interval 0.0 0.0)) in
for i=0 to size-2 do
  let idx = i + size in
  for j=i+1 to size-1 do	    
    let jdx = j + size in
    ma.(i).(j) <- CI.(self#a*other#m.(i).(j) + other#a*self#m.(i).(j) + 
		      other#ar.(i)*self#ar.(j) + other#ar.(j)*self#ar.(i) +
		      self#ar.(idx)*other#m.(i).(j)+ self#ar.(jdx)*other#m.(i).(j) + 
		      other#ar.(idx)*self#m.(i).(j)+ other#ar.(jdx)*self#m.(i).(j));
    nar.(i) <- CI.(nar.(i)+f*self#ar.(jdx)*other#m.(i).(j)+ f*other#ar.(jdx)*self#m.(i).(j));
    nar.(j) <- CI.(nar.(j)+f*self#ar.(idx)*other#m.(i).(j)+ f*other#ar.(idx)*self#m.(i).(j));
  done;
done;
result#set_ar nar;
result#set_m ma;

(*Compute coeff K for epsilon +-*)	
let cons = new interval (-1.) 1. in
let k = ref (new interval 0.0 0.0) in (*refer to e+*)

k := CI.((self#a*other#k*cons) + (self#k * other#a * cons));
k := CI.(!k + (self#k*other#k*cons));

for i = 0 to size - 1 do
  let idx = i + size in
  k := CI.(!k + (self#ar.(i)*other#k*cons) + (other#ar.(i)*self#k*cons));
  k := CI.(!k + (self#ar.(idx)*other#k*cons) + (other#ar.(idx)*self#k*cons));

  for j = 0 to size -1 do
    if i<>j then
       let jdx = j + size in
       k := CI.(!k + (self#ar.(i)*other#ar.(jdx)*cons));
       k := CI.(!k + (self#ar.(idx)*other#ar.(j)*cons));
       k := CI.(!k + (self#ar.(idx)*other#ar.(jdx)*cons));
       if i < j then (
       k := CI.(!k + (self#k*other#m.(i).(j)*cons)+(other#k*self#m.(i).(j)*cons));
       for t = 0 to size -1 do
	 let tdx = t+size in
	 k := CI.(!k + self#m.(i).(j)*other#ar.(t)*cons + other#m.(i).(j)*self#ar.(t)*cons);
	 if (t <> i)&&(t<>j) then
	   k := CI.(!k + self#m.(i).(j)*other#ar.(tdx)*cons + other#m.(i).(j)*self#ar.(tdx)*cons);
       done;
       )
  done;
done;

result#set_k !k;

result;

   (*multiplication with a coeff*)
   method mul2 (c: float)=
     let result = new cai3 size in
     result#set_a CI.(c*@self#a);
     result#set_k CI.(c*@self#k);
     let ar1 = Array.create (2*size) (new interval 0.0 0.0) in
     for i = 0 to 2*size - 1 do
 Array.set ar1 i CI.(c*@ar.(i));
     done;
     result#set_ar ar1;

     let ma = (Array.make_matrix size size (new interval 0.0 0.0)) in
     for i=0 to size-2 do
 for j=i+1 to size-1 do
   ma.(i).(j) <- CI.(c*@self#m.(i).(j));
 done;
     done;
     result#set_m ma;

     result;        
   
   (*Evaluate the bound*)
    method evaluate =        
     let e = new interval (-1.0) 1. in 
     (*let e_ = new interval 0. 1. in*)
     
     let result = ref (new interval 0.0 0.0) in
     
     result := self#a;

     for i = 0 to size - 1 do
  let idx = i + size in
  result := CI.(!result + (eval self#ar.(i) self#ar.(idx)));
  
        (*result := CI.(!result + (self#ar.(i)*e));
        result := CI.(!result + (self#ar.(idx)*e_));*)
     done;

     for i=0 to size-2 do
 for j=i+1 to size-1 do
   result:= CI.(!result + self#m.(i).(j)*e);
 done;
     done;

     result := CI.(!result + (self#k*e));
     !result;
end 
module CAI3 = struct
  let rec pow (t: cai3) (n: int)= 
    match n with
    | 1 -> t
    | _ -> t#mul  (pow t (n-1))

  let ( * ) (t1: cai3) (t2: cai3) = t1 #mul t2
  let ( *@) (c: float) (t: cai3)  = t #mul2 c
  let ( + ) (t1: cai3) (t2: cai3) = t1 #add t2
  let ( +@) (t: cai3)  (c: float) = t #add2 c
  let ( - ) (t1: cai3) (t2: cai3)  = t1 #sub t2
  let ( -@) (t: cai3)  (c: float) = t #sub2 c
  let ( ^ ) (t: cai3)  (n: int)   = pow t n
end

