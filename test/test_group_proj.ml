open OUnit2
open Group_proj.Trie

let tree = empty
let tree = insert (to_char_list "apple") tree
let tree = insert (to_char_list "aprle") tree
let tree = insert (to_char_list "appol") tree
let tree = insert (to_char_list "appla") tree
let tree = insert (to_char_list "apole") tree
let sear = search (to_char_list "") tree
let fold lst = List.fold_left (fun acc elem -> elem ^ " " ^ acc) "" lst
let _ = print_endline (fold sear)
let _ = print_endline (string_of_int (List.length sear))
