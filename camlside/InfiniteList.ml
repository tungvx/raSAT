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

  
let rec gen_power_set_of_length aList currentLength = 
  if currentLength > List.length aList then Nil
  else if currentLength = 1 then convert_toInfList aList
  else
    let tail = List.tl aList in
    let subPowerSet = gen_power_set_of_length tail (currentLength - 1) in
    let currentPowerSet = append_first (List.hd aList) subPowerSet in 
      let nextPowerSet = gen_power_set_of_length tail currentLength in
      currentPowerSet @@ nextPowerSet

let rec gen_power_set aList currentLength maxLength =
  if (currentLength <= 0 || currentLength > maxLength) then 
    Nil
  else 
    let currentPowerSet = gen_power_set_of_length aList currentLength in
    let remainingPowerSet = gen_power_set aList (currentLength + 1) maxLength in
    currentPowerSet @@ remainingPowerSet

let rec power_set aList =
  gen_power_set aList 1 (List.length aList);;  
