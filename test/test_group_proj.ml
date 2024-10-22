open OUnit2
open Group_proj.Trie

let tree = empty
let tree = insert (to_char_list "apple") tree
let tree = insert (to_char_list "aprle") tree
let tree = insert (to_char_list "appol") tree
let tree = insert (to_char_list "appla") tree
let tree = insert (to_char_list "apole") tree
let tree = insert (to_char_list "bapple") tree
let tree = insert (to_char_list "barple") tree
let tree = insert (to_char_list "triangle") tree
let all = all_words tree
let fold lst = List.fold_left (fun acc elem -> elem ^ " " ^ acc) "" lst
let _ = print_endline (fold all)
let _ = print_endline (string_of_int (List.length all))
let app = search (to_char_list "app") tree
let fold lst = List.fold_left (fun acc elem -> elem ^ " " ^ acc) "" lst
let _ = print_endline (fold app)
let _ = print_endline (string_of_int (List.length app))
let app = search (to_char_list "a") tree
let fold lst = List.fold_left (fun acc elem -> elem ^ " " ^ acc) "" lst
let _ = print_endline (fold app)
let _ = print_endline (string_of_int (List.length app))
let app = search (to_char_list "b") tree
let fold lst = List.fold_left (fun acc elem -> elem ^ " " ^ acc) "" lst
let _ = print_endline (fold app)
let _ = print_endline (string_of_int (List.length app))
