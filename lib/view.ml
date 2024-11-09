open Graphics
include Tree.Trie
module Tr = Trie

type t = Tr.t
(**Functions taken from dict.ml*)
exception File_not_found of string
let create_lines_list file_name =
  try
    let lines_enum = BatFile.lines_of file_name in
    let lines_list = BatList.of_enum lines_enum in
    lines_list
  with Sys_error _ ->
    raise (File_not_found ("[" ^ file_name ^ "] does not exist."))

let rec fill_dictionary lines_list dict =
  match lines_list with
  | [] -> dict
  | h :: t -> h :: fill_dictionary t dict

let create_dict file_name dict =
  fill_dictionary (create_lines_list file_name) dict
let dict_list = create_dict "data/COMMON.TXT" []

(**[insert_all lis t] inserts all words in lis into a t*)
let rec insert_all word_list tree =
  match word_list with
  | [] -> tree
  | h :: t ->
      let new_tree = Tr.insert (Tr.to_char_list h) tree in
      insert_all t new_tree
let tree2 = insert_all dict_list Tr.empty
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
(**Functions taken from dict.ml*)
exception File_not_found of string
let create_lines_list file_name =
  try
    let lines_enum = BatFile.lines_of file_name in
    let lines_list = BatList.of_enum lines_enum in
    lines_list
  with Sys_error _ ->
    raise (File_not_found ("[" ^ file_name ^ "] does not exist."))

let rec fill_dictionary lines_list dict =
  match lines_list with
  | [] -> dict
  | h :: t -> h :: fill_dictionary t dict

let create_dict file_name dict =
  fill_dictionary (create_lines_list file_name) dict
let dict_list = create_dict "data/COMMON.TXT" []

(**[insert_all lis t] inserts all words in lis into a t*)
let rec insert_all word_list tree =
  match word_list with
  | [] -> tree
  | h :: t ->
      let new_tree = Tr.insert (Tr.to_char_list h) tree in
      insert_all t new_tree
let tree2 = insert_all dict_list Tr.empty
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

let char_array : string array = Array.make 0 ""

(**[no_suggest] clears the suggestions on the GUI*)
let no_suggest () =
  (* set_color (rgb 229 228 226); *)
  set_color white;
  (* Pure white color *)
  fill_rect
    ((size_x () / 3) - 125) (* Same X-offset as print_suggestions1 *)
    (475 - 200) (* Ensure it covers the max vertical space *)
    800 (* Width matching print_suggestions1 *)
    245

(**[words pos_y] prints the words in a vertical list*)
let rec words pos_y = function
  | [] -> ()
  | h :: t ->
      set_color black;
      moveto (size_x () / 3) pos_y;
      draw_string h;
      words (pos_y - 40) t

(**[print_suggestions1 lst] words from lst onto a grey suggestion box*)
let print_suggestions1 lst =
  set_color (rgb 229 228 226);
  if not (List.is_empty lst) then begin
    let x_offset = (size_x () / 3) - 125 in
    let y_offset = 520 in
    let box_height = (List.length lst * 40) in
    set_color (rgb 229 228 226);
    fill_rect x_offset (y_offset - box_height) 800 (box_height - 20); (* large textbox*)
    fill_circle (x_offset + 20) (y_offset - 20) 20;
    fill_circle (x_offset + 800 - 20) (y_offset - 20) 20; 
    fill_rect (x_offset + 20) (y_offset - 40) 760 40;   (* top rounded corners*)
    fill_circle (x_offset + 20) (y_offset - box_height) 20;
    fill_circle (x_offset + 800 - 20) (y_offset - box_height) 20;
    fill_rect (x_offset + 20) (y_offset - box_height - 20) 760 40; (* bottom rounded corners*)
    words (y_offset - 40) lst;
    synchronize ()
  end else ()

(**[is_inside (x,y) (rect_x, rect_y) width height] is whether x,y is in rect_x,rect_y*)
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
(**[string_lis_to_string lis] is a string of a string list*)
let string_lis_to_string lis = String.concat "" lis

(**[turn_char_to_string_lis trie] is a list of the suggestions for a key pressed *)
(**[turn_char_to_string_lis trie] is a list of the suggestions for a key pressed *)
let turn_char_to_string_lis trie =
  let c = read_key () in
  let char_lis = c :: [] in
  Tr.search char_lis trie

  (**[autofill word_accum suggestions] rest of the word to be filled by the suggestion*)
let autofill word_accum suggestions =
  let suggestion = List.nth suggestions 0 in
  String.sub suggestion (String.length word_accum)
    (String.length suggestion - String.length word_accum)

(** [print_autofill rest_of_word x_int y_int color] prints the autofilled word from suggestions*)
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
  (**[autofill word_accum suggestions] rest of the word to be filled by the suggestion*)
let autofill word_accum suggestions =
  let suggestion = List.nth suggestions 0 in
  String.sub suggestion (String.length word_accum)
    (String.length suggestion - String.length word_accum)

(** [print_autofill rest_of_word x_int y_int color] prints the autofilled word from suggestions*)
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
  (* bubble 80 1280 (); *)
  bubble ((size_x () / 3) - 175) ((size_x () / 3) + 585) ();
  set_color white;
  set_window_title "Auto_Complete";

  set_text_size 500;

  let title = "" in
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
  let x = 440 - 80 in
  let y = 540 in
  moveto x y;
  draw_string title

(**[accumulate_and_display acc] accumulates keys pressed and prints suggestions to screen*)
let rec accumulate_and_display acc =
  let c = read_key () in
  let updated_acc = acc ^ String.make 1 c in

  let suggestions = Tr.search (Tr.to_char_list updated_acc) tree2 in
  if c = ' ' then (
    (* print_suggestions1 suggestions; *)
    accumulate_and_display "")
  else (
    print_suggestions1 suggestions;
    accumulate_and_display updated_acc)

(**[print_word word x_int y_int] prints each word to a coordinate*)
let print_word word x_int y_int =
  let len = String.length word in
  let nex_x = x_int + len + 1 in
  moveto nex_x y_int;
  draw_string word

(**[print_to_screen accum x_int y_int counter] prints the suggestions and present typing to screen*)
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

  (**functions to read a ppm file to display a image*)

  (**[skip_comments ic] doesn't read comments in a ppm file in the returned string*)
  let skip_comments ic =
    let rec aux () =
      let line = input_line ic in
      if String.length line > 0 && String.get line 0 = '#' then
        aux ()  (* Skip comment lines *)
      else
        line
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
      let (width, height) = Scanf.sscanf line "%d %d" (fun w h -> (w, h)) in
    
      (* Read the max color value (usually 255) *)
      let _ = input_line ic in  (* We can ignore it *)
    
      (* Read all pixel data *)
      let pixel_data = ref [] in
        try while true do
          let value = input_line ic in
          pixel_data := value :: !pixel_data
        done with End_of_file -> ();
     
    
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
          Graphics.set_color color;            (* Set the color first *)
          Graphics.plot (index mod width) (height - (index / width) - 1);
          read_pixel (index + 3)
      in
    
      read_pixel 0;
    
      close_in ic