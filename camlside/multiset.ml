(***********************************************************************)
(* Signed Multisets over ordered types                                 *)
(* MultiSets over ordered types       Haakon Nilsen <haakon@ii.uib.no> *)
(* This code is an adaptation of the set module of the standard library*)
(* (rev 1.17) including code from the original Multiset module by      *)
(* Sébastien Briais'.                                                  *)
(*                                                                     *)
(*  This file is distributed under the terms of the GNU Library General*)
(*  Public License, with the special exception on linking described    *)
(*  LICENSE file which comes with OCaml distribution.                  *)
(***********************************************************************)


module type OrderedType =
sig
  type t
  val compare: t -> t -> int
  val to_string: t -> string
end
  
module type S =
sig
  type elt
  type t
    val empty: t
    val is_empty: t -> bool
    val mem: elt -> t -> bool
    val add: elt -> ?n:int -> t -> t
    val add_sets: t -> t -> t
    val negate: t -> t
    val minus: t -> t -> t
    val union: t -> t -> t
    val inter: t -> t -> t
    val compare: t -> t -> int
    val equal: t -> t -> bool
    val subset: t -> t -> bool
    val iter: (elt -> int -> unit) -> t -> unit
    val fold: (elt -> int -> 'a -> 'a) -> t -> 'a -> 'a
    val mul: int -> t -> t
    val question: t -> t -> t -> t
    val multifold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val map: (elt -> elt) -> t -> t
    val for_all: (elt -> int -> bool) -> t -> bool
    val exists: (elt -> int -> bool) -> t -> bool
    val filter: (elt -> int -> bool) -> t -> t
    val partition: (elt -> int -> bool) -> t -> t * t
    val cardinal: t -> int
    val elements: t -> ( elt * int ) list
    val elements_packed: t -> (elt * int) list
    val min_elt: t -> elt
(*    val max_elt: t -> elt*)
    val choose: t -> elt
    val split: elt -> t -> t * int * t
    val of_list: (elt * int) list -> t
    val print_tree: (elt ->  unit) -> t -> unit
  end

module Make(Ord: OrderedType) =
  struct
    type elt = Ord.t
    type t = Empty | Node of t * (elt * int) * t * int

   (* Sets are represented by balanced binary trees (the heights of the
     children differ by at most 2 *)
    let height = function
        Empty -> 0
      | Node(_, _, _, h) -> h

   (* Creates a new node with left son l, value v and right son r.
      We must have all elements of l < v < all elements of r.
      l and r must be balanced and | height l - height r | <= 2.
      Inline expansion of height for better speed. *)
    let create l v r =
      let hl = 
  match l with 
    Empty -> 0 
  | Node(_,_,_,h) -> h 
      in
      let hr = match r with 
  Empty -> 0 
      | Node(_,_,_,h) -> h 
      in
      Node(l, v, r, ((max hl hr) + 1))

   (* Same as create, but performs one step of rebalancing if necessary.
      Assumes l and r balanced and | height l - height r | <= 3.
      Inline expansion of create for better speed in the most frequent case
      where no rebalancing is required. *)
    let bal l v r =
      let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
      let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
      if hl > hr + 2 then begin
        match l with
          Empty -> invalid_arg "Multiset.bal"
        | Node(ll, lv, lr, _) ->
            if height ll >= height lr then
              create ll lv (create lr v r)
            else begin
              match lr with
                Empty -> invalid_arg "Multiset.bal"
              | Node(lrl, lrv, lrr, _)->
                  create (create ll lv lrl) lrv (create lrr v r)
            end
      end else if hr > hl + 2 then begin
        match r with
          Empty -> invalid_arg "Multiset.bal"
        | Node(rl, rv, rr, _) ->
            if height rr >= height rl then
              create (create l v rl) rv rr
            else begin
              match rl with
                Empty -> invalid_arg "Multiset.bal"
              | Node(rll, rlv, rlr, _) ->
                  create (create l v rll) rlv (create rlr rv rr)
            end
      end else
        Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))

    (** value x t returs n iff t(x) = n **)
    let rec value x t =
      match t with 
    Empty -> 0
  | Node(l, (v,m), r,_) ->
      let c = Ord.compare x v in
        if c = 0 then m
        else if c < 0 then value x l
        else value x r
        
   
    let elements_packed s =
      let rec multielements_aux accu = function
          Empty ->
            accu
        | Node(l, (v,n), r, _) ->
            multielements_aux ((v,n):: multielements_aux accu r) l
      in
      multielements_aux [] s

    (* Smallest and greatest element of a set *)
    let rec min_elt_aux = function
        Empty -> raise Not_found
      | Node(Empty, v, r, _) -> v
      | Node(l, v, r, _) -> min_elt_aux l

    let rec min_elt t =
      fst (min_elt_aux t)

    let rec max_elt t =
      let rec max_elt_aux = function
          Empty -> raise Not_found
        | Node(l, v, Empty, _) -> v
        | Node(l, v, r, _) -> max_elt_aux r
      in
  fst (max_elt_aux t)


   (* Remove the smallest element of the given set *)
    let rec remove_min_elt = function
        Empty -> invalid_arg "Set.remove_min_elt"
      | Node(Empty, v, r, _) -> r
      | Node(l, v, r, _) -> bal (remove_min_elt l) v r





   (* Merge two trees l and r into one.
      All elements of l must precede the elements of r.
      Assume | height l - height r | <= 2. *)
    let merge t1 t2 =
      match (t1, t2) with
          (Empty, t) 
  | (t, Empty) -> t
  | (_, _) -> bal t1 (min_elt_aux t2) (remove_min_elt t2)


    
    (** 
  inserts [x -> n] into s. Assumes x not bound 
    **)
    let rec insert x ?(n=1) s = 
      assert ((value x s) = 0);
      match s with 
    Empty -> 
      begin
        match n with
    | 0 -> Empty
    | num -> Node(Empty, (x, num ), Empty, 1)
      end
  | Node(l, (v, m), r, h)  ->
      let c = Ord.compare x v in
        if c = 0 then 
    failwith "Canot insert existing value\n"
        else
    begin
      if c < 0 then
        bal (insert x ~n:n l) (v, m) r
      else
        bal l (v, m) (insert x ~n:n r)
    end
      
      


 (* Same as create and bal, but no assumptions are made on the
       relative heights of l and r. *)
    let rec join l v r =
      match (l, v, r) with
          (Empty, (v', m), _) -> assert ((value v' r) = 0); insert v' ~n:m r
  | (_, (v', m), Empty) -> assert ((value v' l) = 0); insert v' ~n:m l
  | (Node(ll, lv, lr, lh), (v', m), Node(rl, rv, rr, rh)) ->
            if lh > rh + 2 then bal ll lv (join lr v r) else
              if rh > lh + 2 then bal (join l v rl) rv rr else
    create l (v', m) r
      

   (* Merge two trees l and r into one.
      All elements of l must precede the elements of r.
      No assumption on the heights of l and r. *)
    let concat t1 t2 =
      match (t1, t2) with
          (Empty, t) 
  | (t, Empty) -> t
  | (_, _) -> join t1 (min_elt_aux t2) (remove_min_elt t2)


      
      
    (* Insertion of element(s) *)
    let rec add x ?(n=1) s = 
      if n=0 then s else
  match s with
            Empty -> Node(Empty, (x, n), Empty, 1)
    | Node(l, (v, m), r, h) ->
              let c = Ord.compare x v in
    if c = 0 then 
      match (n+m) with
          0 -> concat l r 
        | num -> (Node (l, (v, num), r, h)) else
      if c < 0 then bal (add x ~n:n l) (v, m) r else bal l (v, m) (add x ~n:n r)
        
        
    (* Splitting.  split x s returns a triple (l, value, r) where
       - l is the set of elements of s that are < x
       - r is the set of elements of s that are > x
       - present is 0 if s contains no element equal to x,
       or the value if s contains an element equal to x. *)
    let rec split x = function
        Empty ->
          (Empty, 0, Empty)
      | Node(l, (v, m), r, _) ->
          let c = Ord.compare x v in
          if c = 0 then (l, m, r)
          else if c < 0 then
            let (ll, pres, rl) = split x l in (ll, pres, join rl (v, m) r)
          else
            let (lr, pres, rr) = split x r in (join l (v, m) lr, pres, rr)
               
               
    (* Implementation of the set operations *)
               
    let empty = Empty
      
    let is_empty = function Empty -> true | _ -> false
      
    let rec mem x = function
        Empty -> false
      | Node(l, (v, m), r, _) ->
          let c = Ord.compare x v in
      c = 0 || mem x (if c < 0 then l else r)
        
        

    let rec negate s1 =
      match s1 with
    Empty -> Empty
  | Node(l, (v, n), r, h) -> Node (negate l, ( v, -n), negate r, h)
      
  
        
    (* Adds two signed multisets *)
    let rec add_sets s1 s2 =
      match (s1, s2) with
        | (Empty, t2) -> t2
  | (t1, Empty) -> t1
  | (Node(l1, (v1, m1), r1, h1), Node(l2, (v2, m2), r2, h2)) ->
            let ((l_1,m_1,r_1),(l_2,m_2,r_2), v_) =
        if h1 >= h2 then
    ((l1,m1,r1),split v1 s2, v1) 
    else
    (split v2 s1, (l2,m2,r2), v2) 
      in
      let num = m_1 + m_2 in
        if num = 0 then concat (add_sets l_1 l_2) (add_sets r_1 r_2)
        else join (add_sets l_1 l_2) (v_, num) (add_sets r_1 r_2)
       
       
       
    (* Subtracts the multiset s2 from s1. Returns a signed multiset *) 
    let rec minus s1 s2 =
      add_sets s1 (negate s2)
  
    let rec union s1 s2 =
      match (s1, s2) with
    (Empty, Empty) -> Empty
  | (Empty, Node(l, (v,n),r,h))
  | (Node(l, (v,n),r,h), Empty) -> 
    let l1 = union Empty l 
    and r1 = union Empty r in
      if n > 0 then
        Node( l1 , (v,n), r1, h )
      else
        concat l1 r1
  | (Node(l1, (v1, m1), r1, h1), Node(l2, (v2, m2), r2, h2)) ->
      let ( (l_1, m_1, r_1), (l_2, m_2, r_2), v_) =
        if h1 >= h2 then
    ( (l1, m1, r1), split v1 s2, v1) 
        else
    (split v2 s1, (l2, m2, r2), v2) 
      in
        match (max m_2 m_1) with
    | 0 -> concat (union l_1 l_2) (union r_1 r_2)
    | num -> assert (num != 0); join (union l_1 l_2) (v_, num) (union r_1 r_2)
        
        
    let rec inter s1 s2 =
      match (s1, s2) with
    (Empty, Empty) -> Empty
  | (Empty, Node (l, (v , n) , r , h)) 
  | (Node (l , (v , n) , r , h) , Empty) -> 
      if(n >= 0) then concat (inter Empty l) (inter Empty r)
      else Node(inter Empty l, (v, n), inter Empty r , h )
  | (Node(l1, (v1, m1), r1, _) , t2) ->
            let (l2,m2,r2) = split v1 t2 in
      let min_m = min m1 m2 in
        if (min_m) = 0 then
    concat (inter l1 l2) (inter r1 r2)
              else
    join (inter l1 l2) (v1, min_m) (inter r1 r2)
      
    let rec diff s1 s2 =
      match (s1, s2) with
          (Empty, t2) -> Empty
  | (t1, Empty) -> t1
  | (Node(l1, (v1, m1), r1, _), t2) ->
            match split v1 t2 with
    (l2, 0, r2) ->
      join (diff l1 l2) (v1, m1) (diff r1 r2)
              | (l2, m2, r2) ->
      if m1 > m2 then
                    join (diff l1 l2) (v1, m1 - m2) (diff r1 r2)
      else
                    concat (diff l1 l2) (diff r1 r2)
          
    let rec compare_aux l1 l2 =
      match (l1, l2) with
        ([], []) -> 0
        | ([], _)  -> -1
        | (_, []) -> 1
        | (Empty :: t1, Empty :: t2) -> compare_aux t1 t2
        | (Node(Empty, (v1, m1), r1, _) :: t1, Node(Empty, (v2, m2), r2, _) :: t2) ->
            let c = Ord.compare v1 v2 in
            if c <> 0 then c 
            else if m1 <> m2 then compare m1 m2
            else compare_aux (r1::t1) (r2::t2)
        | (Node(l1, v1, r1, _) :: t1, t2) ->
            compare_aux (l1 :: Node(Empty, v1, r1, 0) :: t1) t2
        | (t1, Node(l2, v2, r2, _) :: t2) ->
            compare_aux t1 (l2 :: Node(Empty, v2, r2, 0) :: t2)
      
    let compare s1 s2 =
      compare_aux [s1] [s2]

    let equal s1 s2 =
      compare s1 s2 = 0

    let rec subset s1 s2 =
      match (s1, s2) with
  Empty, Empty -> true
      | Empty, Node(l,(v,n),r,_) ->
    0 <= n &&  
      ( subset Empty l ) && 
      ( subset Empty r )
      | Node(l,(v,n),r,_), Empty ->
          n <= 0 && 
      ( subset l Empty ) && 
      ( subset r Empty )
      | Node (l1, (v1, m1), r1, h1), (Node (l2, (v2, m2), r2, h2)) ->
    let ((l_1, m_1, r_1), (l_2, m_2, r_2), v_) =
      if h1 >= h2 then
        ((l1, m1, r1), split v1 s2, v1)
      else
        (split v2 s1, (l2, m2, r2), v2)
    in
      ( m_1 <= m_2 ) &&
        ( subset l_1 l_2 ) &&
        ( subset r_1 r_2 )
    


    let rec iter f = function
      Empty -> ()
    | Node(l, (v, n), r, _) -> iter f l; f v n; iter f r

    let rec fold f s accu =
      match s with
        Empty -> accu
      | Node(l, (v, n), r, _) -> fold f l (f v n (fold f r accu))


    let rec mul n t =
      match t with
  | Empty -> Empty
  | Node ( l, (v, x), r, h) -> 
      let ml = mul n l
      and mr = mul n r
      in Node(ml, (v, n*x), mr, h) 

    (** Implements s1 ? s2 : s3 as needed for loop
  (s1 ? s2 : s3) (x) = s2(x) if s1(x) > 0 and s3(x) otherwise **)
    let question s1 s2 s3 =
      let list_helper el _ li = el::li in
      let names3 = fold list_helper s3 [] 
      in let rec question_h s1 s2 s3 =
  match s1 with
    | Node(l,(v,n),r,_) -> (
        let ql = question_h l s2 s3
        and qr = question_h r s2 s3
        in let sval = if n > 0 then 
    value v s2 
    else
      value v s3
        in 
       if sval = 0 then concat ql qr else
         join ql (v, sval) qr
      )
    | Empty -> Empty
        
      in let res1 = question_h s1 s2 s3 
      in let rec tester names3 res1 =
  match names3 with
    | [] -> res1
    | el :: list -> 
        (let res2 =
    match value el s1 with
      | 0 -> add el ~n:(value el s3) res1 
      | _ -> res1
    in tester list res2
        )
      in tester names3 res1
  
  
    let rec multifold f s accu =
      let rec repeat_fold f x accu = function
          0 -> accu
        | n -> repeat_fold f x (f x accu) (n-1)
      in
      match s with
        Empty -> accu
      | Node(l, (v, n), r, _) ->
          multifold f l (repeat_fold f v (multifold f r accu) n)

    let map f s = fold (fun x n t -> add (f x) ~n:n t) s empty

    let rec for_all p = function
        Empty -> true
      | Node(l, (v, n), r, _) -> p v n && for_all p l && for_all p r

    let rec exists p = function
        Empty -> false
      | Node(l, (v, n), r, _) -> p v n || exists p l || exists p r

    let filter p s =
      let rec filt accu = function
        | Empty -> accu
        | Node(l, (v, n), r, _) ->
            filt (filt (if p v n then add v ~n:n accu else accu) l) r in
      filt Empty s

    let partition p s =
      let rec part (t, f as accu) = function
        | Empty -> accu
        | Node(l, (v,n), r, _) ->
            part (part (if p v n then (add v ~n:n t, f) else (t, add v ~n:n f)) l) r
      in
      part (Empty, Empty) s
        
    let rec cardinal = function
        Empty -> 0
      | Node(l, v, r, _) -> cardinal l + 1 + cardinal r

    let rec elements_aux accu = function
        Empty -> accu
      | Node(l, v, r, _) -> elements_aux ((v,1) :: ( elements_aux accu r) ) l

    let rec repeat_elt x accu = function
        0 -> accu
      | n -> repeat_elt x (x::accu) (n-1)

    let rec elements_aux accu = function
        Empty -> accu
      | Node(l, (v, n), r, _) -> elements_aux ((v, n)
            :: (elements_aux accu r) ) l

    let elements s =
      elements_aux [] s

    let choose = min_elt


    let of_list l =
      let rec of_list_aux accu = function
          [] -> accu
        | (x, n) :: xs -> of_list_aux (add x ~n:n accu) xs
      in
      of_list_aux empty l

    let rec print_tree f = function
        Empty -> print_string "Empty"
      | Node (l, (v, n), r, h) ->
          print_string "Node (";
          print_tree f l;
          print_string ", (";
          f v;
          print_int n;
          print_string "), ";
          print_tree f r;
          print_string ", ";
          print_int h;
          print_string ")"

  end