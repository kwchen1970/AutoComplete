open Graphics

let basic_window () =
  open_graph " 1920x1080";
  set_color white;
  set_window_title "Auto_Complete";

  set_text_size 500;

  fill_rect 0 0 800 600;
  let title = "AutoComplete :)" in
  set_text_size 100;
  set_color blue;
  let x = (size_x () - (String.length title * 8)) / 2 in
  let y = 9 * size_y () / 10 in
  moveto x y;
  draw_string title;

  (* Wait for a key press to keep the window open *)
  ignore (read_key ());
  close_graph ()
