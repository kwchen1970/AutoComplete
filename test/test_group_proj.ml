open OUnit2
open Group_proj.Trie

let tree = empty
let tree = insert (to_char_list "apple") tree
let tree = insert (to_char_list "dog") tree
let tree = insert (to_char_list "triangle") tree
let tree = insert (to_char_list "arson") tree
let sear = search (to_char_list "a") tree
let fold lst = List.fold_left (fun acc elem -> acc ^ elem) "" lst
let _ = print_endline (fold sear)
