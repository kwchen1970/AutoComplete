open Group_proj
open Graphics
open Group_proj.Button
module Tr = Trie
include Group_proj.Dict
module D = Group_proj.Dict
open Group_proj.Inference
include Lwt.Infix

(*type for the tree*)
type t = Tr.t

(*a empty tree*)
let empt = Tr.empty

(*type for the button*)
type button = {
  name : string;
  action : string -> string;
  enabled : bool;
}

(*enables the button*)
let enabled b = b.enabled

(**[string_list_to_string lst] is the string of a list*)
let string_list_to_string lst =
  String.concat "" lst 

(**[insert_all world_list tree ]*)
let rec insert_all word_list tree =
  match word_list with
  | [] -> tree
  | h :: t ->
      let new_tree = Tr.insert (Tr.to_char_list h) tree in
      insert_all t new_tree

(*full dictionary from data folder*)
let word_dict = create_dict "data/COMMON.TXT" empty
let word_tree = insert_all word_dict Tr.empty
let full_tree = insert_all (create_dict "data/COMMON.TXT" empty) Tr.empty

(**[insert_all lis t] inserts all words in lis into a t*)
let rec insert_all word_list tree =
  match word_list with
  | [] -> tree
  | h :: t ->
      let new_tree = Tr.insert (Tr.to_char_list h) tree in
      insert_all t new_tree

(**[string_to_char_list str] is the char list form a string*)
let string_to_char_list str = List.of_seq (String.to_seq str)

(**[blend_color alpha bg fg] blends together color bg and fg according to alpha*)
let blend_color alpha bg fg =
  let r1 = (bg lsr 16) land 0xFF in
  let g1 = (bg lsr 8) land 0xFF in
  let b1 = bg land 0xFF in
  let r2 = (fg lsr 16) land 0xFF in
  let g2 = (fg lsr 8) land 0xFF in
  let b2 = fg land 0xFF in
  let blend a c1 c2 =
    int_of_float ((a *. float c2) +. ((1. -. a) *. float c1))
  in
  rgb (blend alpha r1 r2) (blend alpha g1 g2) (blend alpha b1 b2)

(**[center_pad width height] creates the notepad in the center with size dimensions as inputted *)
let center_pad width height =
  let x = (1920 - width) / 2 in
  let y = (1080 - height) / 2 in
  set_color (rgb 229 228 226);
  fill_rect x y width height

(**[no_suggest] clears the suggestions on the GUI*)
let no_suggest x_off y_off =
  set_color (rgb 229 228 226);
  fill_rect 569 
    (y_off - 250) 
    ((1920 - 340) / 2) 
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
  fill_rect x_off_word 
    (y_off - 250) 
    250 
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
          Some option 
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

(**[print_autofill_sentence sent x_int y_int color] is the *)
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

(**[is_printable c] checks if c is a printable character*)
let is_printable c =
  let code = Char.code c in
  code >= 32 && code <= 126

(**[remove_wd_chars str] is removing the weird characters from str*)
let remove_wd_chars str =
  let sequence = String.to_seq str in
  let filt_seq = Seq.filter is_printable sequence in
  String.of_seq filt_seq

  (**[print_autofill_sentence_blocking sent x_int y_int color] is autofilling the sentence in the color color*)
let print_autofill_sentence_blocking sent x_int y_int color =
  let s = Lwt_main.run (complete_sentence sent) in
  let rest_sent = remove_wd_chars s in
  if String.length rest_sent > 0 then (
    set_color color;
    moveto x_int y_int;
    draw_string rest_sent);
  rest_sent

(**[autofill word_accum suggestions] rest of the word to be filled by the
   suggestion*)
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

  (**[print_sent_autofill rest_of_sent x_int y_int color x_max x_min line_height] is *)
let rec print_sent_autofill rest_of_sent x_int y_int color x_max x_min
    line_height =
  if String.length rest_of_sent = 0 then (x_int, y_int)
  else
    let count = x_int + 6 in
    let count2, y_int =
      if count > x_max then (x_min, y_int - line_height) else (count, y_int)
    in
    set_color color;
    moveto count2 y_int;
    if String.length rest_of_sent > 0 then (
      draw_string (String.make 1 rest_of_sent.[0]);
      print_sent_autofill
        (String.sub rest_of_sent 1 (String.length rest_of_sent - 1))
        count2 y_int color x_max x_min line_height)
    else (count, y_int)

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
(**[draw_buttons] draws the buttons on the side of the GUI*)
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
  draw_string "Press Me"

(**[start_text] prompts where the typing will appear*)
let start_text () =
  let title = "Type Here:  " in
  set_text_size 100;
  set_color red;
  let x = 570 in
  let y = 875 in
  moveto x y;
  draw_string title

(**[string_to_char_lis s] is the char list of a string*)
let string_to_char_lis (s : string) : char list = List.of_seq (String.to_seq s)

(**[hashtable_to_string2 table] is the string of a hashtable*)
let hashtable_to_string2 table =
  let included_keys = ref [] in
  let pairs =
    Hashtbl.fold
      (fun key value acc ->
        if List.mem key !included_keys then acc
        else
          let () = included_keys := key :: !included_keys in
          (key, value) :: acc)
      table []
  in
  let sorted_pairs =
    List.sort (fun (key1, _) (key2, _) -> compare key1 key2) pairs
  in
  let value_strings =
    List.map (fun (key, value) -> Printf.sprintf "%s" value) sorted_pairs
  in
  String.concat "" value_strings

(**[save_text_to_file filename text] is saving the text to a file named filename*)
let save_text_to_file filename text =
  let oc = open_out filename in
  output_string oc text;
  close_out oc;
  let title = "Text saved to: " ^ filename in
  set_text_size 100;
  set_color red;
  moveto 280 780;
  draw_string title

(**functions to read a ppm file to display a image*)
(**[skip_comments ic] doesn't read comments in a ppm file in the returned string*)
let skip_comments ic =
  let rec aux () =
    let line = input_line ic in
    if String.length line > 0 && String.get line 0 = '#' then
      aux () 
    else line
  in
  aux ()

(**[load_ppm filename] turns the ppm into a image on screen*)
let load_ppm filename =
  let ic = open_in filename in
  let magic_number = input_line ic in
  if magic_number <> "P3" then failwith "Unsupported PPM format";
  let line = skip_comments ic in
  let width, height = Scanf.sscanf line "%d %d" (fun w h -> (w, h)) in
  let _ = input_line ic in
  let pixel_data = ref [] in
  try
    while true do
      let value = input_line ic in
      pixel_data := value :: !pixel_data
    done
  with End_of_file ->
    ();
    let total_pixels = width * height * 3 in
    if List.length !pixel_data < total_pixels then
      failwith "Not enough pixel values in the file";
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

(**overflow_rectangle is a rectangle that covers up the overflow of the autocomplete*)
let overflow_rectangle () =
  set_color white;
  fill_rect (((1920 - 800) / 2) + 800) (((1080 - 800) / 2) + 50 - 60) 500 750

(*is a reference to the autofilled sentence part*)
let sent_comp = ref ""

(*is a reference to x_int from teh tab*)
let x_int_from_tab = ref 0

(*is a reference to y_int from teh tab*)
let y_int_from_tab = ref 0
(**[first_tup (x,_)] is the first argument of a tuple*)
let first_tup (x, _) = x

(**[sec_tup (_,x)] is the second argument of a tuple*)
let sec_tup (_, x) = x

(**[insert_string_to_hash s x hash] is inserting key s and value x into hashtable hash*)
let insert_string_to_hash s x hash =
  let _ = x + 1 in
  let len = String.length s in
  for i = 0 to len - 1 do
    Hashtbl.replace hash (x + i) (String.make 1 s.[i])
  done

(**[read_file filename] reads the file filename*)
let read_file filename =
  let channel = open_in filename in
  try
    let length = in_channel_length channel in
    let content = really_input_string channel length in
    close_in channel;
    content
  with e ->
    close_in_noerr channel;
    raise e

(**[load_file text] is loading the text back to the GUI*)
let load_file text =
  set_color black;
  let rec aux x y i word_i accum accum_sent =
    if i = String.length text - 1 then (x, y)
    else begin
      let c = String.get text i in
      if x >= 1360 then begin
        Hashtbl.add accum_sent i c;
        moveto 569 (y + 5);
        draw_char c;
        if c = ' ' then begin
          let () = Hashtbl.clear accum in
          aux 569 (y + 5) (i + 1) 0 accum accum_sent
        end
        else
          let () = Hashtbl.add accum word_i c in
          aux 569 (y + 5) (i + 1) (word_i + 1) accum accum_sent
      end
      else begin
        Hashtbl.add accum_sent i c;
        moveto (x + 5) y;
        draw_char c;
        if c = ' ' then begin
          Hashtbl.clear accum;
          aux (x + 6) y (i + 1) 0 accum accum_sent
        end
        else
          let () = Hashtbl.add accum word_i c in
          aux (x + 6) y (i + 1) (word_i + 1) accum accum_sent
      end
    end
  in
  aux 580 855 0 0 (Hashtbl.create 5) (Hashtbl.create 5)

(**draws text on screen for retrieve*)
let draw_str text x_int y_int max_x_bound line_height =
  let x = ref x_int in
  let y = ref y_int in
  set_color black;
  let seq = String.to_seq text in  
  Seq.iter (fun char -> 
    let count, y_offset =
      if !x >= max_x_bound then
        begin
          y := !y - line_height;
          x := 580;
        (580, !y) 
        end 
      else begin
        x := !x + 7;
        (!x, !y)
      end       
    in
    moveto count y_offset;        
    draw_char char;                
  ) seq;
  (!x, !y)

let first_element (seq : int Seq.t) =
  let c = Seq.take 1 seq in
  Seq.fold_left (fun acc elem -> elem + acc) 0 c

(**after_tab_pos is a reference for the tab position*)
let after_tab_pos = ref 0

(**[rgb24_to_color_array img] writes out the img rgb to a color array*)
let rgb24_to_color_array (img : Rgb24.t) : Graphics.color array array =
  let w, h = (img.Rgb24.width, img.Rgb24.height) in
  let color_matrix = Array.make_matrix h w Graphics.black in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let { Color.r; g; b } = Rgb24.get img x y in
      color_matrix.(y).(x) <- Graphics.rgb r g b
    done
  done;
  color_matrix

(*load_options is the image loading options*)
let load_options =
  [
    Images.Load_Progress
      (fun progress ->
        Printf.printf "Loading progress: %.2f%%\n" (progress *. 100.0));
    Images.Load_Resolution (300.0, 300.0);
  ]

(* Load a PPM file and convert it to a color array *)
let load_ppm_as_color_array filename : Graphics.color array array =
  match Ppm.load filename load_options with
  | Rgb24 img -> rgb24_to_color_array img
  | _ -> failwith "Error: Unexpected image format or corrupted file."

(**[i_width img] gets the width of the image*)
let i_width img = first_tup (Images.size img)

(**[i_length img] gets the length of the image*)
let i_length img = sec_tup (Images.size img)

(**[load_imag image x y] laods the image from a ppm array*)
let load_imag image x y =
  let col_arr = load_ppm_as_color_array image in
  let img = Graphics.make_image col_arr in
  Graphics.draw_image img x y

(**[animate_jelly time] is animating the jelly for time seconds*)
let animate_jelly time =
  let x = 1400 in
  let y = 500 in
  let images =
    [
      "data/jelly_1.ppm";
      "data/jelly_2.ppm";
      "data/jelly_3.ppm";
      "data/jelly_4.ppm";
    ]
  in
  let start_t = Unix.gettimeofday () in
  let rec loop_ani fr =
    let curr_time = Unix.gettimeofday () in
    if curr_time -. start_t < time then (
      let image_p = List.nth images (fr mod 4) in
      load_imag image_p x y;
      Unix.sleepf 0.2;
      loop_ani (fr + 1))
    else ()
  in
  loop_ani 0

(*some references for tab position*)
let tab_reference = ref 0
let tab_pos = ref 0
let space_before = ref 0
let after_backspace_pos = ref 0
let just_backspace_ref = ref 0
let backspace_pos_use = ref 0

(**[get_last_char_strin s] gets the last character of a string as a string*)
let get_last_char_strin s =
  let l = String.length s in
  if l > 0 then
    String.sub s (l - 1) 1  
  else
    ""

(**[print_to_screen_sentence accum x_int y_int counter x_off_word accum_sent
    accum_sentence word_index sent last_sent_suggest tab_before tree] is the function responsiuble for updating the GUI and 
    running all the button, autofill, typing funcitonality.*)
let rec print_to_screen_sentence accum x_int y_int counter x_off_word accum_sent
    accum_sentence word_index sent last_sent_suggest tab_before tree =
  if !just_backspace_ref = 1 then backspace_pos_use := !after_backspace_pos else backspace_pos_use := x_int;
  let min_x_bound = 569 in
  let max_x_bound = 1300 in
  let line_height = 20 in
  if !space_before = 1 then begin
    set_color (rgb 229 228 226);
    fill_rect (x_int) y_int (4) (line_height-2);  
  end
  else ();
  let event = wait_next_event [ Button_down; Key_pressed ] in
  if event.button then begin
    let click_x = event.mouse_x in
    let click_y = event.mouse_y in
    if is_inside (click_x, click_y) (300, 700) 100 50 then (
      (* save button *)
      let str = hashtable_to_string2 accum_sent in
      if String.length str > 0 then
        let () =
          save_text_to_file "data/output.txt"
            (String.sub str 0 (String.length str ))
        in
        print_to_screen_sentence accum x_int y_int counter x_off_word accum_sent
          accum_sentence word_index sent last_sent_suggest 0 tree
      else save_text_to_file "data/output.txt" "";
      print_to_screen_sentence accum x_int y_int counter x_off_word accum_sent
        accum_sentence word_index sent last_sent_suggest 0 tree)
    else if is_inside (click_x, click_y) (300, 500) 100 50 
    then
      let text = read_file "data/output.txt" in
      let new_x, new_y = 
      draw_str text x_int y_int max_x_bound line_height in
        print_to_screen_sentence accum new_x new_y counter x_off_word accum_sent
        accum_sentence word_index sent last_sent_suggest 0 tree
    else if is_inside (click_x, click_y) (300, 300) 100 50 (* button 3 *) then begin
      animate_jelly 2.;
      print_to_screen_sentence accum x_int y_int counter x_off_word accum_sent
        accum_sentence word_index sent last_sent_suggest 0 tree
    end
    else
      print_to_screen_sentence accum x_int y_int counter x_off_word accum_sent
        accum_sentence word_index sent last_sent_suggest 0 tree
  end
  else begin
    let c = event.key in
    just_backspace_ref := 0;
    space_before := 0;
    if tab_before = 1 && c = ' ' then
      print_to_screen_sentence accum x_int y_int counter x_off_word accum_sent
        accum_sentence word_index sent last_sent_suggest tab_before tree;
    let tree =
      if c = '\003' then Tr.insert_new (string_to_char_lis accum) tree else tree
    in
    let c = if c = '\003' then ' ' else c in
    if c = ' ' then overflow_rectangle ();
    let old_suggestions =
      if String.length accum > 0 then Tr.search (string_to_char_list accum) tree
      else []
    in
    if c = '.' || c = '!' || c = '?' then begin
      Hashtbl.add accum_sentence word_index sent
    end
    else if c = '\027' then begin
      close_graph ();
      exit 0
    end
    else if c = '\x13' then begin
      let str = hashtable_to_string2 accum_sent in
      if String.length str > 0 then
        save_text_to_file "data/output.txt"
          (String.sub str 0 (String.length str - 1))
      else save_text_to_file "data/output.txt" ""
    end
    else if c = '\t' then begin
      if
        (first_element (Hashtbl.to_seq_keys accum_sent)) > 0 && 
        Hashtbl.find accum_sent word_index <> " "
        && String.length accum > 0
        && tab_before = 0
      then begin
        if List.length old_suggestions > 0 then (
          let rest_of_word = autofill accum old_suggestions in
          insert_string_to_hash rest_of_word (word_index+1) accum_sent;
          insert_string_to_hash (get_last_char_strin rest_of_word) (word_index + 1 +String.length(rest_of_word )-1) accum_sent; 
          insert_string_to_hash (" ") (word_index + 1 +String.length(rest_of_word )) accum_sent; 
          print_autofill rest_of_word x_int y_int black;
          let count = x_int + (7 * String.length rest_of_word)+ 5 in
          print_to_screen_sentence "" count y_int (count + 4) count
            accum_sent accum_sentence (word_index + 1 +String.length(rest_of_word )+1) sent last_sent_suggest 1 tree)
        else ()
      end
      else if tab_before = 1 then begin
        print_to_screen_sentence accum x_int y_int counter x_off_word accum_sent
          accum_sentence word_index sent last_sent_suggest 0 tree
      end
      else begin
        set_color (rgb 229 228 226);
        fill_rect (x_int + 2) y_int (max_x_bound + 56 - x_int) (line_height - 3);
        if (first_element (Hashtbl.to_seq_keys accum_sent)) > 0 && 
          Hashtbl.find accum_sent word_index = " " then (
          let old_suggest = last_sent_suggest in
          x_int_from_tab :=
            first_tup
              (print_sent_autofill old_suggest (x_int - 7) y_int black
                 max_x_bound 580 line_height);
          y_int_from_tab :=
            sec_tup
              (print_sent_autofill old_suggest (x_int - 7) y_int black
                 max_x_bound 580 line_height);
          tab_reference := 1;
          tab_pos := !x_int_from_tab;
          let old_suggest = last_sent_suggest ^ " " in
          insert_string_to_hash old_suggest word_index accum_sent;
          after_tab_pos := !x_int_from_tab;
          let new_sent = sent ^ " " ^ old_suggest in
          let count, y_offset =
            if !x_int_from_tab >= max_x_bound then (580, !y_int_from_tab)
            else (!x_int_from_tab + 7, !y_int_from_tab)
          in
          let last_sent_sugg = old_suggest in
          print_to_screen_sentence "" count y_offset (count + 4) x_off_word
            accum_sent accum_sentence
            (word_index + String.length old_suggest)
            new_sent last_sent_sugg 1 tree)
      end
    end
  else if c = '\x08' then begin
    just_backspace_ref := 1;
    set_color (rgb 229 228 226);
    let width = max_x_bound + 56 - x_int in
    let height = line_height - 5 in
    if width > 0 && height > 0 then fill_rect x_int y_int width height else ();
    if Hashtbl.mem accum_sent word_index then begin
      Hashtbl.remove accum_sent word_index
    end
    else ();
    let sent =
      if String.length sent = 0 then ""
      else String.sub sent 0 (String.length sent - 1)
    in
    let new_accum =
      if String.length accum > 0 then
        String.sub accum 0 (String.length accum - 1)
      else ""
    in
    let suggestions =
      if c <> ' ' then Tr.search (string_to_char_list new_accum) tree else []
    in
    if c = ' ' || new_accum = "" then
      if x_int > max_x_bound - 190 then no_suggest (max_x_bound - 190) y_int
      else if x_int - 50 < min_x_bound then no_suggest (min_x_bound + 8) y_int
      else no_suggest (x_int - 50) y_int
    else print_suggestions1 suggestions x_int y_int x_int;
    after_backspace_pos := x_int - 7;
    print_to_screen_sentence new_accum (x_int - 7) y_int counter x_off_word
      accum_sent accum_sentence (word_index - 1) sent "" 0 tree
  end
    else if c = ' ' then begin
      space_before := 1;
      tab_reference := 0;
      if tab_before = 1 then
        print_to_screen_sentence accum x_int y_int counter x_off_word accum_sent
          accum_sentence word_index sent "" 0 tree
      else begin
        if (first_element (Hashtbl.to_seq_keys accum_sent)) > 0 && 
          Hashtbl.find accum_sent word_index = " " then begin
          set_color (rgb 229 228 226);
          fill_rect (x_int + 5) y_int
            (max_x_bound + 56 - x_int - 5)
            (line_height - 5);
          sent_comp :=
            print_autofill_sentence_blocking sent (x_int + 7) y_int red;
          print_to_screen_sentence accum x_int y_int counter x_off_word
            accum_sent accum_sentence word_index sent !sent_comp 0 tree
        end
        else begin
          set_color (rgb 229 228 226);
          fill_rect (x_int + 5) y_int
            (max_x_bound + 56 - x_int - 5)
            (line_height - 5);
          sent_comp :=
            print_autofill_sentence_blocking sent (x_int + 7) y_int red
        end
      end
    end
    else if List.length old_suggestions > 0 then
      let rest_of_word = autofill accum old_suggestions in
      print_autofill rest_of_word x_int y_int (rgb 229 228 226)
    else ();
    if c <> '\x08' && c <> '\027' then
      Hashtbl.add accum_sent (word_index + 1) (String.make 1 c)
    else ();

    let new_sent =
      if c = '.' || c = '!' || c = '?' then "" else sent ^ String.make 1 c
    in
    let new_accum = if c = ' ' then "" else accum ^ String.make 1 c in
    if String.length new_accum == 1 then begin
      overflow_rectangle ();
      set_color (rgb 229 228 226);
      fill_rect (x_int + 6) y_int (max_x_bound + 53 - x_int) (line_height - 3)
    end;
    (* autofill stuff begin*)
    let suggestions =
      if c <> ' ' then Tr.search (string_to_char_list new_accum) tree else []
    in
    if c = ' ' || tab_before = 1 then
      if x_int > max_x_bound - 190 then no_suggest (max_x_bound - 190) y_int
      else if x_int - 50 < min_x_bound then no_suggest (min_x_bound + 8) y_int
      else no_suggest (x_int - 50) y_int 
    else begin
      if !tab_reference = 1 then begin
        let new_x_off = x_off_word + (6 * String.length !sent_comp) in
        let corr_x_off =
          if new_x_off > max_x_bound then
            min_x_bound + (3 * (new_x_off - max_x_bound))
          else new_x_off
        in
        print_suggestions1 suggestions x_int y_int corr_x_off
      end
      else if !tab_reference = 0 then begin
        print_suggestions1 suggestions x_int y_int x_off_word
      end
    end;

    if (580 < x_int && x_int < 590) && y_int < 855 then (
      (* set_color (rgb 0 0 224); *)
      set_color (rgb 229 228 226);
      fill_rect
        (max_x_bound - 190) 
        (y_int - 230) 
        250 
        240);

    set_color black;
    moveto (x_int + 2) y_int;
    let count, y_offset =
      if !backspace_pos_use >= max_x_bound then (580, y_int - line_height)
      else (!backspace_pos_use + 7, y_int)
    in
    set_color black;
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

    if c = ' ' then
      let x_off_word =
        if x_int > max_x_bound - 190 then max_x_bound - 190 else x_int
      in
      print_to_screen_sentence new_accum count y_offset (count + 4) x_off_word
        accum_sent accum_sentence (word_index + 1) new_sent !sent_comp 0 tree
    else
      print_to_screen_sentence new_accum count y_offset (count + 4) x_off_word
        accum_sent accum_sentence (word_index + 1) new_sent !sent_comp 0 tree
  end

(**[print_to_screen_sentence_1  accum x_int y_int counter x_off_word accum_sent
    accum_sentence word_index sent last_sent_suggest tab_before] is the same function as 
    print_to_screen_sentence except it uses the full_tree in place of tree parameter*)
let print_to_screen_sentence_1 accum x_int y_int counter x_off_word accum_sent
    accum_sentence word_index sent last_sent_suggest tab_before =
  print_to_screen_sentence accum x_int y_int counter x_off_word accum_sent
    accum_sentence word_index sent last_sent_suggest tab_before full_tree

(**[safe_exit ] exits the GUI safely*)
let safe_exit () =
  ();
  exit 0 

(**This launches the GUI and the operations it can do.*)
let () =
  Random.self_init ();
  try
    basic_window ();
    draw_buttons ();
    start_text ();
    let color_array = load_ppm_as_color_array "data/actual_sugar_title.ppm" in
    let img = Graphics.make_image color_array in
    Graphics.draw_image img 520 900;
    print_to_screen_sentence_1 "" 580 855 120 580 (Hashtbl.create 5) (Hashtbl.create 5) 0 "" "" 0 
  with
  | Graphics.Graphic_failure _ ->
      safe_exit ()
  | Sys_error msg -> Printf.printf "Error: %s\n" msg
  | Failure msg -> Printf.printf "Error: %s\n" msg