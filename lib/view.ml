open Graphics

let basic_window () =
  open_graph " 1920x1080";
  set_window_title "Auto_Complete";

  set_color white;
  fill_rect 0 0 800 600;

  (* Wait for a key press to keep the window open *)
  ignore (read_key ());

  set_color blue;
  let title = "AutoComplete :)" in
  let x = (size_x () - (String.length title * 8)) / 2 in
  let y = size_y () / 2 in
  moveto x y;
  draw_string title;

  (* Close the graphics window after key press *)
  close_graph ()
