open Graphics
include Tree.Trie
module Tr = Trie
include Tree.Dict
open Array

type t = Tr.t

let rec insert_all word_list tree =
  match word_list with
  | [] -> tree
  | h :: t ->
      let new_tree = Trie.insert (Trie.to_char_list h) tree in
      insert_all t new_tree

let word_dict = create_dict "data/COMMON.TXT" []
let word_tree = insert_all word_dict Tr.empty

(**[insert_all lis t] inserts all words in lis into a t*)
let rec insert_all word_list tree =
  match word_list with
  | [] -> tree
  | h :: t ->
      let new_tree = Tr.insert (Tr.to_char_list h) tree in
      insert_all t new_tree

(*test tree I am using for the prototype*)
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

(**[string_to_char_list str] is the char list form a string*)
let string_to_char_list str = List.of_seq (String.to_seq str)

(**[blend_color alpha bg fg] blends together color bg and fg according to alpha*)
let blend_color alpha bg fg =
  let r1 = (bg lsr 16) land 0xFF in
  (* Extract red component from bg *)
  let g1 = (bg lsr 8) land 0xFF in
  (* Extract green component from bg *)
  let b1 = bg land 0xFF in

  (* Extract blue component from bg *)
  let r2 = (fg lsr 16) land 0xFF in
  (* Extract red component from fg *)
  let g2 = (fg lsr 8) land 0xFF in
  (* Extract green component from fg *)
  let b2 = fg land 0xFF in

  (* Extract blue component from fg *)
  let blend a c1 c2 =
    int_of_float ((a *. float c2) +. ((1. -. a) *. float c1))
  in

  rgb (blend alpha r1 r2) (blend alpha g1 g2) (blend alpha b1 b2)

(**[bubble st ed] draws the search bar bubble*)
let rec bubble (st : int) (ed : int) () =
  set_color (rgb 229 228 226);
  fill_circle st 540 20;
  fill_circle ed 540 20;
  fill_rect st 520 (ed - st) 40

let center_pad width height =
  let x = (1920 - width) / 2 in
  let y = (1080 - height) / 2 in
  set_color (rgb 229 228 226);
  (* set_color (rgb 200 28 226); *)
  fill_rect x y width height

(**[no_suggest] clears the suggestions on the GUI*)
let no_suggest x_off y_off =
  (* set_color (rgb 0 228 0); *)
  set_color (rgb 0 228 0);
  (* Pure white color *)
  fill_rect
    (x_off - 15) (* Same X-offset as print_suggestions1 *)
    (y_off - 250) (* Ensure it covers the max vertical space *)
    250 (* Width matching print_suggestions1 *)
    240

(**[words pos_y] prints the words in a vertical list*)
let rec words x_off pos_y = function
  | [] -> ()
  | h :: t ->
      set_color blue;
      moveto x_off pos_y;
      draw_string h;
      words x_off (pos_y - 40) t

(**[print_suggestions1 lst] words from lst onto a grey suggestion box*)
let print_suggestions1 lst x_off y_off x_off_word =
  set_color (rgb 229 0 0);
  (* set_color (rgb 229 228 226); *)
  (* Pure white color *)
  fill_rect x_off_word (* Same X-offset as print_suggestions1 *)
    (y_off - 250) (* Ensure it covers the max vertical space *)
    250 (* Width matching print_suggestions1 *)
    240;
  words (x_off_word + 11) (y_off - 25) lst;
  synchronize ()

(**[is_inside (x,y) (rect_x, rect_y) width height] is whether x,y is in
   rect_x,rect_y*)
let is_inside (x, y) (rect_x, rect_y) width height =
  x >= rect_x && x <= rect_x + width && y >= rect_y && y <= rect_y + height

(**[find_chosen_suggestion lst] is the suggestion that is clicked on*)
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

(**[turn_char_to_string acc] string of a char that is read *)
let rec turn_char_to_string acc =
  let c = read_key () in
  if c = ' ' then ()
  else
    let accum = acc ^ String.make 1 c in
    turn_char_to_string accum

(**[string_lis_to_string lis] is a string of a string list*)
let string_lis_to_string lis = String.concat "" lis

(**[turn_char_to_string_lis trie] is a list of the suggestions for a key pressed *)
let turn_char_to_string_lis trie =
  let c = read_key () in
  let char_lis = c :: [] in
  Tr.search char_lis trie

(**[autofill word_accum suggestions] rest of the word to be filled by the
   suggestion*)
let autofill word_accum suggestions =
  let suggestion = List.nth suggestions 0 in
  String.sub suggestion (String.length word_accum)
    (String.length suggestion - String.length word_accum)

(** [print_autofill rest_of_word x_int y_int color] prints the autofilled word
    from suggestions*)
let rec print_autofill rest_of_word x_int y_int color =
  let bg = rgb 229 228 226 in
  let fg = color in
  let col = blend_color 0.4 bg fg in
  let count = x_int + 7 in
  set_color col;
  moveto count y_int;
  draw_string (String.make 1 rest_of_word.[0]);
  if String.length rest_of_word > 1 then
    print_autofill
      (String.sub rest_of_word 1 (String.length rest_of_word - 1))
      count y_int color
  else ()

(**[basic_window ()] creates a blank GUI*)
let basic_window () =
  open_graph " 1920x1080";
  center_pad 800 800;
  set_color white;
  set_window_title "Auto_Complete";

  set_text_size 500;

  let title = "" in
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
  let x = 570 in
  let y = 875 in
  moveto x y;
  draw_string title

let print_array arr = Array.iter (fun x -> print_endline x) arr

(**[accumulate_and_display acc] accumulates keys pressed and prints suggestions
   to screen*)
let rec accumulate_and_display acc x_off y_off x_off_word =
  let c = read_key () in
  let updated_acc = acc ^ String.make 1 c in

  let suggestions = Tr.search (Tr.to_char_list updated_acc) tree in
  if c = ' ' then (
    print_suggestions1 suggestions x_off y_off x_off_word;
    accumulate_and_display "" x_off y_off x_off_word)
  else (
    print_suggestions1 suggestions x_off y_off x_off_word;
    accumulate_and_display updated_acc x_off y_off x_off_word)

let hashtable_to_string table =
  (* Convert the hashtable to a list of key-value pairs *)
  let pairs =
    Hashtbl.fold (fun key value acc -> (key, value) :: acc) table []
  in
  (* Sort the pairs by the key *)
  let sorted_pairs =
    List.sort (fun (key1, _) (key2, _) -> compare key1 key2) pairs
  in
  (* Join all the formatted key-value pairs into a single string, separated by
     commas *)
  String.concat ", "
    (List.map
       (fun (key, value) -> Printf.sprintf "%d:%s" key value)
       sorted_pairs)

let hashtable_to_text table : string =
  let pairs =
    Hashtbl.fold (fun key value acc -> (key, value) :: acc) table []
  in
  (* Sort the pairs by the key *)
  let sorted_pairs =
    List.sort (fun (key1, _) (key2, _) -> compare key1 key2) pairs
  in
  (* let char_lst = Hashtbl.fold (fun k v acc -> v :: acc) sorted_pairs [] in *)
  List.fold_left (fun acc (key, elem) -> acc ^ elem) "" sorted_pairs

let save_text_to_file filename text =
  let oc = open_out filename in
  output_string oc text;
  close_out oc;
  print_endline ("Text saved to " ^ filename)

let command_pressed = ref false

(**[print_to_screen accum x_int y_int counter] prints the suggestions and
   present typing to screen*)
let rec print_to_screen accum x_int y_int counter x_off_word accum_sent
    word_index =
  let min_x_bound = 569 in
  let max_x_bound = 1300 in
  let line_height = 20 in
  synchronize ();
  (* Get the current character input *)
  let old_suggestions = Tr.search (string_to_char_list accum) tree in
  let event = wait_next_event [ Key_pressed ] in
  let c = event.key in
  if c = 's' && !command_pressed then begin
    (* Save the text content to a file *)
    (* save_text_to_file "output.txt" !text; *)
    print_endline (hashtable_to_text accum_sent);
    save_text_to_file "output.txt" (hashtable_to_text accum_sent)
  end
  else if c = '\027' then begin
    (* Detect Command key press (Escape is used here for simplicity) *)
    command_pressed := true
  end
  else if
    (* If any key is pressed, add to the buffer *)
    (* command_pressed := false; *)
    c = '\008'
  then begin
    set_color (rgb 229 228 226);
    fill_rect x_int y_int 14 (line_height - 5);
    let new_accum =
      if accum = "" then accum else String.sub accum 0 (String.length accum - 1)
    in
    print_to_screen new_accum (x_int - 7) y_int counter x_off_word accum_sent
      word_index
  end
  else if c = '\t' && String.length accum > 0 then
    if List.length old_suggestions > 0 then (
      let rest_of_word = autofill accum old_suggestions in
      print_autofill rest_of_word x_int y_int black;
      let count = x_int + (7 * String.length rest_of_word) in
      print_to_screen "" count y_int (count + 4) x_off_word accum_sent
        word_index)
    else ()
  else if List.length old_suggestions > 0 then
    let rest_of_word = autofill accum old_suggestions in
    print_autofill rest_of_word x_int y_int black
  else ();
  if c <> '\x08' && c <> '\027' then
    Hashtbl.add accum_sent (word_index + 1) (String.make 1 c)
  else ();
  print_endline (hashtable_to_string accum_sent);

  (* Append the character to the accumulator if it's not a space *)
  let new_accum = if c = ' ' then "" else accum ^ String.make 1 c in
  let suggestions = Tr.search (string_to_char_list new_accum) tree in
  if c = ' ' then
    if c = ' ' then
      if x_int > max_x_bound - 190 then no_suggest (max_x_bound - 190) y_int
      else if x_int - 50 < min_x_bound then no_suggest (min_x_bound + 8) y_int
      else no_suggest (x_int - 50) y_int
    else print_suggestions1 suggestions x_int y_int x_off_word;
  if (580 < x_int && x_int < 590) && y_int < 855 then (
    (* set_color (rgb 0 0 224); *)
    set_color (rgb 229 228 226);
    fill_rect
      (max_x_bound - 190) (* Same X-offset as print_suggestions1 *)
      (y_int - 230) (* Ensure it covers the max vertical space *)
      250 (* Width matching print_suggestions1 *)
      240);
  let count, y_offset =
    if x_int >= max_x_bound then (580, y_int - line_height)
    else (x_int + 7, y_int)
  in
  (* Display the current typed characters *)
  (* print_endline new_accum; *)
  set_color black;
  moveto count y_offset;
  if String.length new_accum > 0 then
    let () =
      draw_string (String.make 1 new_accum.[String.length new_accum - 1])
    in
    if List.length suggestions > 0 then (
      let rest_of_word = autofill new_accum suggestions in
      print_autofill rest_of_word count y_offset (rgb 120 99 97);
      print_autofill rest_of_word count y_offset (rgb 120 99 97))
    else ()
  else draw_string " ";

  (* Call the function recursively with the new accumulator *)
  if c = ' ' then
    let x_off_word =
      if x_int > max_x_bound - 190 then max_x_bound - 190 else x_int
    in
    print_to_screen new_accum count y_offset (count + 4) x_off_word accum_sent
      (word_index + 1)
  else
    print_to_screen new_accum count y_offset (count + 4) x_off_word accum_sent
      (word_index + 1)

(**functions to read a ppm file to display a image*)

(**[skip_comments ic] doesn't read comments in a ppm file in the returned string*)
let skip_comments ic =
  let rec aux () =
    let line = input_line ic in
    if String.length line > 0 && String.get line 0 = '#' then
      aux () (* Skip comment lines *)
    else line
  in
  aux ()

(**[load_ppm filename] turns the ppm into a image on screen*)
let load_ppm filename =
  let ic = open_in filename in
  (* Read the magic number (P3) *)
  let magic_number = input_line ic in
  if magic_number <> "P3" then failwith "Unsupported PPM format";

  (* Skip comments and read width, height *)
  let line = skip_comments ic in
  let width, height = Scanf.sscanf line "%d %d" (fun w h -> (w, h)) in

  (* Read the max color value (usually 255) *)
  let _ = input_line ic in

  (* We can ignore it *)

  (* Read all pixel data *)
  let pixel_data = ref [] in
  try
    while true do
      let value = input_line ic in
      pixel_data := value :: !pixel_data
    done
  with End_of_file ->
    ();

    (* Check if we have enough values *)
    let total_pixels = width * height * 3 in
    if List.length !pixel_data < total_pixels then
      failwith "Not enough pixel values in the file";

    (* Read pixel data and plot each pixel *)
    let rec read_pixel index =
      if index >= total_pixels then () (* End of pixel data *)
      else
        let r = int_of_string (List.nth !pixel_data index) in
        let g = int_of_string (List.nth !pixel_data (index + 1)) in
        let b = int_of_string (List.nth !pixel_data (index + 2)) in
        let color = Graphics.rgb r g b in
        Graphics.set_color color;
        (* Set the color first *)
        Graphics.plot (index mod width) (height - (index / width) - 1);
        read_pixel (index + 3)
    in

    read_pixel 0;

    close_in ic
