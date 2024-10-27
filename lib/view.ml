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

let no_suggest () =
  set_color (rgb 229 228 0);
  (* Pure white color *)
  fill_rect
    ((size_x () / 3) - 125) (* Same X-offset as print_suggestions1 *)
    (490 - 200) (* Ensure it covers the max vertical space *)
    800 (* Width matching print_suggestions1 *)
    240

let rec words pos_y = function
  | [] -> ()
  | h :: t ->
      set_color black;
      moveto (size_x () / 3) pos_y;
      draw_string h;
      words (pos_y - 40) t

let print_suggestions1 lst =
  let x_offset = (size_x () / 3) - 125 in
  let y_offset = 500 in
  let box_height = (List.length lst * 40) + 20 in
  set_color (rgb 229 228 226);
  fill_rect x_offset (y_offset - box_height) 800 box_height;
  words (y_offset - 40) lst;
  synchronize ()

let is_inside (x, y) (rect_x, rect_y) width height =
  x >= rect_x && x <= rect_x + width && y >= rect_y && y <= rect_y + height

let find_chosen_suggestion lst =
  let event = wait_next_event [ Button_down; Key_pressed ] in
  let click_x = event.mouse_x in
  let click_y = event.mouse_y in
  let menu_x = (size_x () / 3) - 125 in
  let menu_y = 520 - (List.length lst * 40) in
  let width = 800 in
  let height = (List.length lst * 40) + 20 in
  if is_inside (click_x, click_y) (menu_x, menu_y) width height then
    List.find_map
      (fun (option, index) ->
        let option_y = menu_y + (index * 40) in
        if is_inside (click_x, click_y) (menu_x, option_y) width 40 then
          Some option (* Return the clicked option *)
        else None)
      (List.mapi (fun i option -> (option, i)) lst)
  else None

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

let autofill word_accum suggestions =
  let suggestion = List.nth suggestions 0 in
  String.sub suggestion (String.length word_accum)
    (String.length suggestion - String.length word_accum)

let rec print_autofill rest_of_word x_int y_int color =
  let count = x_int + 7 in
  set_color color;
  moveto count y_int;
  draw_string (String.make 1 rest_of_word.[0]);
  if String.length rest_of_word > 1 then
    print_autofill
      (String.sub rest_of_word 1 (String.length rest_of_word - 1))
      count y_int color
  else ()

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

(* let rec print_to_screen accum = (* Get the current character input *) let
   event = wait_next_event [ Key_pressed ] in let c = event.key in

   if c = '\027' then () else (* Append the character to the accumulator if it's
   not a space *) let new_accum = if c = ' ' then "" else accum ^ String.make 1
   c in let total_accum = accum ^ String.make 1 c in no_suggest ();

   let suggestions = Tr.search (string_to_char_list new_accum) tree in let
   choice, chose_word = if suggestions = [] then (* Call no_suggest if there are
   no suggestions *) let () = no_suggest () in ("", false) else let () =
   print_suggestions1 suggestions in let user_choice = find_chosen_suggestion
   suggestions in match user_choice with | None -> ("", false) | Some s -> (s,
   true) in

   (* Display the current typed characters *) set_color black; let x_offset =
   140 in let y_offset = 540 in let current_x = x_offset + (String.length
   total_accum * 7) in let total_accum = if chose_word then total_accum ^ " " ^
   choice else total_accum in moveto current_x y_offset; if String.length
   new_accum > 0 then draw_string (String.make 1 total_accum.[String.length
   total_accum - 1]) else if chose_word then draw_string (" " ^ choice) else
   draw_string " "; (* Call the function recursively with the new accumulator *)
   print_to_screen total_accum *)

let print_word word x_int y_int =
  let len = String.length word in
  let nex_x = x_int + len + 1 in
  moveto nex_x y_int;
  draw_string word

let rec print_to_screen accum x_int y_int counter =
  synchronize ();
  (* Get the current character input *)
  let old_suggestions = Tr.search (string_to_char_list accum) tree in
  let event = wait_next_event [ Key_pressed ] in
  let c = event.key in
  if c = '\027' then ()
  else if c = '\t' && String.length accum > 0 then
    if List.length old_suggestions > 0 then (
      let rest_of_word = autofill accum old_suggestions in
      print_autofill rest_of_word x_int y_int black;
      let count = x_int + (7 * String.length rest_of_word) in
      print_to_screen "" count y_int (count + 4))
    else ()
  else if List.length old_suggestions > 0 then
    let rest_of_word = autofill accum old_suggestions in
    print_autofill rest_of_word x_int y_int (rgb 229 228 226)
  else ();
  (* Append the character to the accumulator if it's not a space *)
  let new_accum = if c = ' ' then "" else accum ^ String.make 1 c in
  let suggestions = Tr.search (string_to_char_list new_accum) tree in
  if c = ' ' then no_suggest () else print_suggestions1 suggestions;
  print_endline new_accum;

  if new_accum = "" then no_suggest () else print_suggestions1 suggestions;

  (* Display the current typed characters *)
  set_color black;
  let x_offset = x_int in
  let y_offset = y_int in

  let count = x_offset + 7 in
  moveto count y_offset;
  if String.length new_accum > 0 then
    let () =
      draw_string (String.make 1 new_accum.[String.length new_accum - 1])
    in
    if List.length suggestions > 0 then
      let rest_of_word = autofill new_accum suggestions in
      print_autofill rest_of_word count y_offset red
    else ()
  else draw_string " ";

  (* Call the function recursively with the new accumulator *)
  print_to_screen new_accum count y_offset (count + 4)
