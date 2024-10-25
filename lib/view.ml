open Graphics
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

let string_to_char_list str = List.of_seq (String.to_seq str)

let rec bubble (st : int) (ed : int) () =
  set_color (rgb 229 228 226);

  fill_circle st 540 20;
  fill_circle ed 540 20;
  fill_rect st 520 (ed - st) 40

(* let rec print_to_screen () = (* let x = 100 in let y = 540 in moveto x y; *)
   let c = read_key () in if c = '\027' then () else ( set_color black;
   draw_string (String.make 1 c); flush (); print_to_screen ()) *)

let rec words pos = function
  | [] -> ()
  | h :: t ->
      set_color black;
      moveto (size_x () / 3) pos;
      draw_string h;
      words (pos - 40) t

let no_suggest () =
  set_color (rgb 229 228 226);
  fill_rect ((size_x () / 3) - 125) 520 250 40

let print_suggestions1 lst =
  if lst = [] then begin
    no_suggest ()
  end
  else begin
    set_color (rgb 229 228 226);
    fill_rect
      ((size_x () / 3) - 125)
      (520 - (List.length lst * 40))
      800
      ((List.length lst * 40) + 20);
    words 500 lst
  end

let continue lst =
  let event = wait_next_event [ Key_pressed ] in
  if
    (not (List.is_empty lst))
    && ((event.key >= 'a' && event.key <= 'z')
       || (event.key >= 'A' && event.key <= 'Z'))
  then print_suggestions1 lst
  else no_suggest ();
  moveto (size_x () / 3) 540

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

let return_suggest incr x y =
  let lis = turn_char_to_string_lis tree in
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

let rec accumulate_and_display acc =
  let c = read_key () in
  let updated_acc = acc ^ String.make 1 c in

  let suggestions = Tr.search (Tr.to_char_list updated_acc) tree in
  if c = ' ' then (
    print_suggestions1 suggestions;
    accumulate_and_display "")
  else (
    print_suggestions1 suggestions;
    accumulate_and_display updated_acc)

let rec print_to_screen accum =
  (* Get the current character input *)
  let event = wait_next_event [ Key_pressed ] in
  let c = event.key in

  if c = 'q' then ()
  else
    (* Append the character to the accumulator if it's not a space *)
    let new_accum = if c = ' ' then accum else accum ^ String.make 1 c in
    no_suggest ();

    let suggestions = Tr.search (string_to_char_list new_accum) tree in
    if suggestions = [] then
      no_suggest () (* Call no_suggest if there are no suggestions *)
    else print_suggestions1 suggestions;

    (* Display the current typed characters *)
    set_color black;
    (* Set the color for the typed characters *)
    let x_offset = 100 in
    (* Starting x position *)
    let y_offset = 540 in
    (* y position for the text *)
    let current_x = x_offset + (String.length new_accum * 7) in
    (* Calculate new x position based on character width *)
    moveto current_x y_offset;
    draw_string new_accum;

    (* Call the function recursively with the new accumulator *)
    print_to_screen new_accum
