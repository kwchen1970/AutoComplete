open Group_proj
open Group_proj.View
include Tree.Trie
module Tr = Trie

type t = Tr.t

(**[safe_exit ] exits the GUI safely*)
let safe_exit () =
  ();
  exit 0 (* Exit the program cleanly *)

(**This launches the GUI and the operations it can do.*)
let () =
  try
    basic_window ();
    (* load_ppm "data/test.ppm"; *)
    start_text ();
    no_suggest ();
    print_to_screen "" 135 540 120
  with
  | Graphics.Graphic_failure _ ->
      (* Catch the fatal I/O error and exit cleanly *)
      safe_exit ()
  | Sys_error msg -> Printf.printf "Error: %s\n" msg
  | Failure msg -> Printf.printf "Error: %s\n" msg
