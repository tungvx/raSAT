module VariablesSet = Set.Make(String);;
module IntMap = Map.Make(struct type t = int let compare = compare end);;
module StringMap = Map.Make(String);;

let varsSet_to_string varsSet = 
  let varsList = VariablesSet.elements varsSet in
  String.concat " " varsList
  
let print_varsSet varsSet = 
  let varsString = varsSet_to_string varsSet in
  print_endline varsString  
