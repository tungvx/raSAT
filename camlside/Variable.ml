module VariablesSet = Set.Make(String);;

let varsSet_to_string varsSet = 
  let varsList = VariablesSet.elements varsSet in
  String.concat " " varsList
  
let print_varsSet varsSet = 
  let varsString = varsSet_to_string varsSet in
  print_endline varsString  
