type 'a infList = Nil | Cons of 'a * (unit -> 'a infList);;

let get_first aInfList = match aInfList with
  | Nil -> failwith "get_first"
  | Cons(h, t) -> (h, t())

let rec (@@) infList1 infList2 = match infList1 with
  | Nil -> infList2
  | Cons(head, tail) -> Cons(head, fun() -> tail() @@ infList2);;

    
let rec append_first element aInfList = 
  match aInfList with 
  | Nil -> Nil
  | Cons(head, tail) -> Cons(element::head, fun() -> (append_first element (tail())))
  
let rec convert_toInfList aList = match aList with
  | [] -> Nil
  | h :: t -> Cons([h], fun() -> (convert_toInfList t))  
