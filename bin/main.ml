open Group_proj
open Group_proj.View
include Tree.Trie
module Tr = Trie

type t = Tr.t

let rec insert_all word_list tree =
  match word_list with
  | [] -> tree
  | h :: t ->
      let new_tree = Tr.insert (Tr.to_char_list h) tree in
      insert_all t new_tree

let tree =
  insert_all
    [
      "apple";
      "aprle";
      "appol";
      "appla";
      "apole";
      "bapple";
      "barple";
      "triangle";
    ]
    Tr.empty

let () =
  basic_window ();
  start_text ();
  return_suggest 5 tree 80 540;
  print_to_screen ()
