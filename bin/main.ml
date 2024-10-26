open Group_proj
open Group_proj.View
include Tree.Trie
module Tr = Trie

type t = Tr.t

let () =
  basic_window ();
  start_text ();
  print_to_screen "" 120 540 120
