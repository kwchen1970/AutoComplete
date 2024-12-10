open Graphics
include Trie
module Tr = Trie
include Dict
open Group_proj.Inference
include Lwt.Infix

type t = Tr.t

let empt = Tr.empty
let string_list_to_string lst =
  String.concat "" lst (* Concatenates the list with no separator *)

let rec insert_all word_list tree =
  match word_list with
  | [] -> tree
  | h :: t ->
      let new_tree = Trie.insert (Trie.to_char_list h) tree in
      insert_all t new_tree

let word_dict = create_dict "data/COMMON.TXT" []
let word_tree = insert_all word_dict Tr.empty

let full_tree = insert_all (create_dict "data/COMMON.TXT" []) Tr.empty

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
  fill_rect x y width height

(**[no_suggest] clears the suggestions on the GUI*)
let no_suggest x_off y_off =
  (* set_color (rgb 0 228 0); *)
  set_color (rgb 229 228 226);
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
  set_color (rgb 229 228 226);
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

let rec print_autofill_sentence sent x_int y_int color =
  complete_sentence sent >>= fun rest_sent ->
  if String.length rest_sent = 0 then Lwt.return ()
  else
    let count = x_int + 7 in
    set_color color;
    moveto count y_int;
    draw_string (String.make 1 rest_sent.[0]);
    if String.length rest_sent > 1 then
      print_autofill_sentence
        (String.sub rest_sent 1 (String.length rest_sent - 1))
        count y_int color
    else Lwt.return ()
let is_printable c =
      let code = Char.code c in
      code >= 32 && code <= 126
let remove_wd_characters str =
        String.to_seq str
        |> Seq.filter is_printable
        |> String.of_seq

(* let print_autofill_sentence_blocking sent x_int y_int color = Lwt_main.run
   (print_autofill_sentence sent x_int y_int color) *)

let print_autofill_sentence_blocking sent x_int y_int color =
  let s = Lwt_main.run (complete_sentence sent) in
  let rest_sent = remove_wd_characters s in
  if String.length rest_sent > 0 then (
    set_color color;
    moveto x_int y_int;
    draw_string rest_sent)

(* let test_sentence_auto sent x_int y_int = print_autofill_sentence_blocking
   sent x_int y_int blue *)
(**[autofill word_accum suggestions] rest of the word to be filled by the
   suggestion*)

(**suggestions can be empty but it breaks it when it is empty*)
let autofill word_accum suggestions =
  if List.length suggestions = 0 then ""
  else
    let suggestion = List.nth suggestions 0 in
    String.sub suggestion (String.length word_accum)
      (String.length suggestion - String.length word_accum)

(** [print_autofill rest_of_word x_int y_int color] prints the autofilled word
    from suggestions*)
let rec print_autofill rest_of_word x_int y_int color =
  if String.length rest_of_word = 0 then ()
  else
    let count = x_int + 7 in
    set_color color;
    moveto count y_int;
    draw_string (String.make 1 rest_of_word.[0]);
    if String.length rest_of_word > 1 then
      print_autofill
        (String.sub rest_of_word 1 (String.length rest_of_word - 1))
        count y_int color
    else ()

let rec print_sent_autofill rest_of_sent x_int y_int color = 
  if String.length rest_of_sent = 0 then ()
  else
    let count = x_int in
    set_color color;
    moveto count y_int;
    draw_string (String.make 1 rest_of_sent.[0]);
    if String.length rest_of_sent > 1 then
      print_sent_autofill
        (String.sub rest_of_sent 1 (String.length rest_of_sent - 1))
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

let draw_buttons () =
  moveto 338 720;
  set_color (rgb 70 183 224);
  fill_rect 300 700 100 50;
  set_color black;
  draw_string "Save";
  moveto 328 520;
  set_color (rgb 121 217 65);
  fill_rect 300 500 100 50;
  set_color black;
  draw_string "Retrieve";
  moveto 328 320;
  set_color (rgb 240 70 58);
  fill_rect 300 300 100 50;
  set_color black;
  draw_string "Button 3"

(**[start_text] prompts where the typing will appear*)
let start_text () =
  let title = "Type Here:  " in
  set_text_size 100;
  set_color red;
  let x = 570 in
  let y = 875 in
  moveto x y;
  draw_string title
  let string_to_char_lis (s : string) : char list =
    List.of_seq (String.to_seq s)
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

(**[print_to_screen accum x_int y_int counter] prints the suggestions and
   present typing to screen*)
let rec print_to_screen accum x_int y_int counter x_off_word accum_sent
    accum_sentence word_index sent tree=
  print_endline ("sentence is " ^ sent);
  print_endline ("accum is [" ^ accum ^ "]");
  let min_x_bound = 569 in
  let max_x_bound = 1300 in
  let line_height = 20 in
  synchronize ();
  print_endline ("length of accum is " ^ string_of_int (String.length accum));

  (* print_endline("old_suggestions is "^string_lis_to_string (Tr.search
     (string_to_char_list accum) tree)); *)
  (* Get the current character input *)
  let event = wait_next_event [ Key_pressed ] in
  let c = event.key in

  (* if you press ctrl c you should inesrt what you typed so far into the trie tree*)
  let tree = if c = '\003' then Tr.insert_new (string_to_char_lis accum) tree else tree in 
  let c = if c = '\003' then  ' ' else c in 
  let old_suggestions =
    if String.length accum > 0 then
      Tr.search (string_to_char_list accum) tree
    else []
  in
  if c = '.' || c = '!' || c = '?' then
    Hashtbl.add accum_sentence word_index sent
  else if c = '\027' then begin close_graph (); 
    exit 0;  end
  else if c = '\t' && String.length accum > 0 then
    if List.length old_suggestions > 0 then (
      let rest_of_word = autofill accum old_suggestions in
      print_autofill rest_of_word x_int y_int black;
      let count = x_int + (7 * String.length rest_of_word) in
      print_to_screen "" count y_int (count + 4) x_off_word accum_sent
        accum_sentence word_index sent tree)
    else ()
  else if c = '\x08' then begin
    set_color (rgb 229 228 226);
    fill_rect x_int y_int (max_x_bound + 56 - x_int) (line_height - 5);
    if Hashtbl.mem accum_sent word_index then begin
      Hashtbl.remove accum_sent word_index
    end
  else();
    let new_accum = 
      if accum = "" then accum else String.sub accum 0 (String.length accum - 1)
    in let suggestions =
      if c <> ' ' then Tr.search (string_to_char_list new_accum) tree else []
    in
    if c = ' '|| new_accum = "" then
      if x_int > max_x_bound - 190 then no_suggest (max_x_bound - 190) y_int
      else if x_int - 50 < min_x_bound then no_suggest (min_x_bound + 8) y_int
      else no_suggest (x_int - 50) y_int
    else print_suggestions1 suggestions x_int y_int x_off_word;
    print_to_screen new_accum (x_int - 7) y_int counter x_off_word accum_sent
      accum_sentence (word_index-1) sent tree
  end
  else if List.length old_suggestions > 0 then (
    let rest_of_word = autofill accum old_suggestions in
    print_endline ("rest of word is " ^ rest_of_word);
    print_autofill rest_of_word x_int y_int (rgb 229 228 226);
    print_endline "autofilled already")
  else ();
  print_endline ("old_suggestions are " ^ string_lis_to_string old_suggestions);
  (* Add word to accum_sentence if it is complete.*)
  if c <> '\x08' && c <> '\027' then
    Hashtbl.add accum_sent (word_index + 1) (String.make 1 c)
  else ();
  print_endline (hashtable_to_string accum_sent);
  (* Append the character to the accumulator if it's not a space *)
  let new_sent =
    if c = '.' || c = '!' || c = '?' then "" else sent ^ String.make 1 c
  in
  (* Append the character to the accumulator if it's not a space *)
  let new_accum = if c = ' ' then "" else accum ^ String.make 1 c in
  let suggestions =
    if c <> ' ' then Tr.search (string_to_char_list new_accum) tree else []
  in
  if c = ' ' then
    if x_int > max_x_bound - 190 then no_suggest (max_x_bound - 190) y_int
    else if x_int - 50 < min_x_bound then no_suggest (min_x_bound + 8) y_int
    else no_suggest (x_int - 50) y_int
  else print_suggestions1 suggestions x_int y_int x_off_word;
  print_endline ("suggestions are " ^ string_lis_to_string suggestions);
  if (580 < x_int && x_int < 590) && y_int < 855 then (
    (* set_color (rgb 0 0 224); *)
    set_color (rgb 255 182 193);
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
  set_color black;
  moveto count y_offset;
  print_endline "before the draw_string";
  print_endline ("new_accum is " ^ new_accum);
  if String.length new_accum > 0 then
    let () =
      draw_string (String.make 1 new_accum.[String.length new_accum - 1])
    in
    if List.length suggestions > 0 then
      let rest_of_word = autofill new_accum suggestions in
      print_autofill rest_of_word count y_offset red
    else ()
  else draw_string " ";

  print_endline "end of autofill part";

  (* Call the function recursively with the new accumulator *)
  if c = ' ' then
    let x_off_word =
      if x_int > max_x_bound - 190 then max_x_bound - 190 else x_int
    in
    print_to_screen new_accum count y_offset (count + 4) x_off_word accum_sent
      accum_sentence (word_index + 1) new_sent tree
  else
    print_to_screen new_accum count y_offset (count + 4) x_off_word accum_sent
      accum_sentence (word_index + 1) new_sent tree

let print_to_screen_1 accum x_int y_int counter x_off_word accum_sent
accum_sentence word_index sent = print_to_screen accum x_int y_int counter x_off_word accum_sent
accum_sentence word_index sent full_tree

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
      if index >= total_pixels then ()
      else
        let r = int_of_string (List.nth !pixel_data index) in
        let g = int_of_string (List.nth !pixel_data (index + 1)) in
        let b = int_of_string (List.nth !pixel_data (index + 2)) in
        let color = Graphics.rgb r g b in
        Graphics.set_color color;
        Graphics.plot (index mod width) (height - (index / width) - 1);
        read_pixel (index + 3)
    in

    read_pixel 0;

    close_in ic

let overflow_rectangle () =
  set_color white;
  fill_rect (((1920 - 800) / 2) + 800) ((1080 - 800) / 2) 200 750

let rec print_to_screen_sentence accum x_int y_int counter x_off_word accum_sent
    accum_sentence word_index sent last_sent_suggest =
  print_endline ("sentence is " ^ sent);
  print_endline ("accum is [" ^ accum ^ "]");
  print_endline ("accum_sentence is "^hashtable_to_string accum_sentence);
  print_endline("accum_sent is "^ hashtable_to_string accum_sent);
  let max_x_bound = 1300 in
  let line_height = 20 in
  synchronize ();
  let old_suggestions =
    if String.length accum > 0 then
      Tr.search (string_to_char_list accum) (full_tree)
    else []
  in
  let event = wait_next_event [ Key_pressed ] in
  let c = event.key in
  if c = ' ' then
    overflow_rectangle ();

  if c = '.' || c = '!' || c = '?' then
    Hashtbl.add accum_sentence word_index sent
  else if c = '\027' then begin close_graph (); 
  exit 0;  end
  else if c = '\t' then ()
    (*begin if Hashtbl.find accum_sent word_index = " " then begin
    print_endline("here in the conditional");
    let old_suggest = last_sent_suggest in
    print_sent_autofill old_suggest x_int y_int black;
    set_color (rgb 0 228 226); 
  end *)
  else if c = '\x08' then begin
    set_color (rgb 229 228 226);
    fill_rect x_int y_int (max_x_bound + 56 - x_int) (line_height - 5);
    let sent =
      if String.length sent = 0 then
        ""
      else
        String.sub sent 0 (String.length sent -1)
      in 
    print_to_screen_sentence accum (x_int - 7) y_int counter x_off_word
      accum_sent accum_sentence word_index sent ""
  end
  else if c = ' ' then begin
      if Hashtbl.find accum_sent word_index = " " then begin
        print_endline("here in the conditional");
        set_color (rgb 229 228 226);
        fill_rect (x_int+5) y_int (max_x_bound + 56 - x_int-5) (line_height - 5);
        print_endline("prompt is " ^sent);
      print_autofill_sentence_blocking sent (x_int + 7) y_int red; 
        let old_suggest = last_sent_suggest in
      print_to_screen_sentence accum x_int y_int counter x_off_word
      accum_sent accum_sentence (word_index) sent old_suggest
      end
      else begin
      set_color (rgb 229 228 226);
      fill_rect (x_int+5) y_int (max_x_bound + 56 - x_int-5) (line_height - 5);
      print_autofill_sentence_blocking sent (x_int+7) y_int red; 
      end
  end
  else ();
  print_endline ("old_suggestions are " ^ string_lis_to_string old_suggestions);
  (* Add word to accum_sent if it is complete.*)
  if c <> '\x08' && c <> '\027' then
    Hashtbl.add accum_sent (word_index + 1) (String.make 1 c)
  else ();
  (* Append the character to the accumulator if it's not a space *)
  let new_sent =
    if c = '.' || c = '!' || c = '?' then "" else sent ^ String.make 1 c
  in
  let new_accum = if c = ' ' then "" else accum ^ String.make 1 c in
  if String.length new_accum == 1 then begin
    (* Set color to white to clear the rectangle *)
    overflow_rectangle ();
    set_color (rgb 229 228 226);

    (* Fill the rectangle with white to clear it *)
    fill_rect (x_int + 2) y_int (max_x_bound + 56 - x_int) (line_height - 3)
  end;
  set_color black;
  moveto (x_int + 2) y_int;
  (* if (580 < x_int && x_int < 590) && y_int < 855 then ( (* set_color (rgb 0 0
     224); *) set_color (rgb 255 182 193); fill_rect (max_x_bound - 190) (* Same
     X-offset as print_suggestions1 *) (y_int - 230) (* Ensure it covers the max
     vertical space *) 250 (* Width matching print_suggestions1 *) 240); *)
  let count, y_offset =
    if x_int >= max_x_bound then (580, y_int - line_height)
    else (x_int + 7, y_int)
  in
  (* Display the current typed characters *)
  set_color black;
  moveto count y_offset;
  if String.length new_accum > 0 then
    draw_string (String.make 1 new_accum.[String.length new_accum - 1])
  else draw_string " ";

  (* print_autofill_sentence_blocking new_sent count y_offset red; *)

  (* Call the function recursively with the new accumulator *)
    print_to_screen_sentence new_accum count y_offset (count + 4) x_off_word
      accum_sent accum_sentence (word_index + 1) new_sent last_sent_suggest

let rec print_to_screen_both accum x_int y_int counter x_off_word accum_sent accum_sentence
    word_index sent tree =
  print_endline("sentence is "^ sent);
  print_endline ("accum is ["^ accum ^ "]");
  let min_x_bound = 569 in 
  let max_x_bound = 1300 in
  let line_height = 20 in
  synchronize ();
  print_endline("length of accum is "^string_of_int (String.length accum));

  (* print_endline("old_suggestions is "^string_lis_to_string (Tr.search (string_to_char_list accum) tree)); *)
  (* Get the current character input *)
  let old_suggestions = 
  if String.length accum > 0 then 
 Tr.search (string_to_char_list accum) tree else [] in 
  let event = wait_next_event [ Key_pressed ] in
  let c = event.key in
  if (c = '.' || c ='!' || c = '?') then Hashtbl.add accum_sentence word_index (sent)
  else if c = '\027' then begin close_graph (); 
  exit 0;  end
  else if c = '\t' && String.length accum > 0 then
    if List.length old_suggestions > 0 then (
      let rest_of_word = autofill accum old_suggestions in
      print_autofill rest_of_word x_int y_int black;
      let count = x_int + (7 * String.length rest_of_word) in
      print_to_screen_both "" count y_int (count + 4) x_off_word accum_sent accum_sentence
        word_index sent tree)
    else ()
  else if c = '\x08' then
    ()
  (** where the traingle problem starts*)
  else if List.length old_suggestions > 0 then
    let rest_of_word = autofill accum old_suggestions in
    print_endline("rest of word is "^rest_of_word);
    print_autofill rest_of_word x_int y_int (rgb 229 228 226);
    print_endline("autofilled already");
  else ();
  print_endline("old_suggestions are "^ string_lis_to_string old_suggestions);
  (**Add word to accum_sentence if it is complete.*)
  if (c <> '\x08' && c <> '\027') then Hashtbl.add accum_sent (word_index+1) (String.make 1 c)
  else (); 
  print_endline(hashtable_to_string accum_sentence);
  (* Append the character to the accumulator if it's not a space *)
  let new_sent = if (c = '.' || c ='!' || c = '?') then "" else sent^ String.make 1 c  in
  (* Append the character to the accumulator if it's not a space *)
  let new_accum = if c = ' ' then "" else accum ^ String.make 1 c in
  let suggestions = 
  if c <> ' ' then 
Tr.search (string_to_char_list new_accum) (full_tree) else [] in 
  if c = ' ' then
    if x_int > max_x_bound - 190 then no_suggest (max_x_bound - 190) y_int
    else if (x_int-50) < min_x_bound then no_suggest (min_x_bound+8) y_int
    else no_suggest (x_int-50) y_int
  else print_suggestions1 suggestions x_int y_int x_off_word;
  print_endline("suggestions are "^string_lis_to_string suggestions);
  if (580 < x_int && x_int < 590) && y_int < 855 then (
    (* set_color (rgb 0 0 224); *)
    set_color (rgb 255 182 193);
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
  set_color black;
  moveto count y_offset;
  print_endline("before the draw_string");
  print_endline("new_accum is "^new_accum);
  if String.length new_accum > 0 then
    let () =
    draw_string (String.make 1 new_accum.[String.length new_accum - 1])
  in
  if List.length suggestions > 0 then
    let rest_of_word = autofill new_accum suggestions in
    print_autofill rest_of_word count y_offset red
  else ()
else draw_string " ";

  print_endline("end of autofill part");

  (* Call the function recursively with the new accumulator *)
  if c = ' ' then
    let x_off_word =
      if x_int > max_x_bound - 190 then max_x_bound - 190 else x_int
    in
    print_to_screen_both new_accum count y_offset (count + 4) x_off_word
      accum_sent accum_sentence (word_index +1) new_sent tree
  else
    print_to_screen_both new_accum count y_offset (count + 4) x_off_word
      accum_sent accum_sentence (word_index + 1) new_sent tree

      let print_to_screen_both_1 accum x_int y_int counter x_off_word accum_sent accum_sentence
      word_index sent  = print_to_screen_both accum x_int y_int counter x_off_word accum_sent accum_sentence
      word_index sent full_tree