open Graphics
include Tree.Trie
module Tr = Trie

type t = Tr.t

let string_to_char_list str = List.of_seq (String.to_seq str)

let rec bubble (st : int) (ed : int) () =
  set_color (rgb 229 228 226);

  fill_circle st 540 20;
  fill_circle ed 540 20;
  fill_rect st 520 (ed - st) 40

let rec print_to_screen () =
  (* let x = 100 in let y = 540 in moveto x y; *)
  let c = read_key () in
  set_color black;
  draw_string (String.make 1 c);
  print_to_screen ()

let rec turn_char_to_string acc =
  let c = read_key () in
  if c = ' ' then ()
  else
    let accum = acc ^ String.make 1 c in
    turn_char_to_string accum

let helper_text x y =
  let title = "" in
  set_text_size 100;
  set_color black;
  moveto x y;
  draw_string title

let string_lis_to_string lis = String.concat "" lis

let turn_char_to_string_lis trie =
  let c = read_key () in
  let char_lis = c :: [] in
  Tr.search char_lis trie

let print_suggestions lis x y =
  let text = String.concat "" lis in
  set_text_size 100;
  set_color black;
  moveto x y;
  draw_string text

let return_suggest incr trie x y =
  let lis = turn_char_to_string_lis trie in
  print_suggestions lis x (y + incr)

let basic_window () =
  open_graph " 1920x1080";
  bubble 80 1280 ();
  set_color white;
  set_window_title "Auto_Complete";

  set_text_size 500;

  let title = "AutoComplete :)" in
  set_text_size 100;
  set_color blue;
  let x = (size_x () - (String.length title * 8)) / 2 in
  let y = 9 * size_y () / 10 in
  moveto x y;

  draw_string title

(**[start_text] prompts where the typing will appear*)
let start_text () =
  let title = "Type Here:  " in
  set_text_size 100;
  set_color red;
  let x = 80 in
  let y = 540 in
  moveto x y;
  draw_string title
