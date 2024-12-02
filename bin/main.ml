open Group_proj
open Group_proj.View
include Tree.Trie
module Tr = Trie

type t = Tr.t
(** writing the image out functions*)
let rgb24_to_color_array (img : Rgb24.t) : Graphics.color array array =
  let w, h = (img.Rgb24.width, img.Rgb24.height) in
  let color_matrix = Array.make_matrix h w Graphics.black in
  (* Initialize with black *)
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let { Color.r; g; b } = Rgb24.get img x y in
      color_matrix.(y).(x) <- Graphics.rgb r g b
    done
  done;
  color_matrix

let load_options =
  [
    Images.Load_Progress
      (fun progress ->
        Printf.printf "Loading progress: %.2f%%\n" (progress *. 100.0));
    Images.Load_Resolution (300.0, 300.0)
    (* Setting resolution to 300 DPI *)
    (* Load_only_the_first_frame could also be included here if needed *);
  ]

(* Load a PPM file and convert it to a color array *)
let load_ppm_as_color_array filename : Graphics.color array array =
  match Ppm.load filename load_options with
  | Rgb24 img -> rgb24_to_color_array img
  | _ -> failwith "Error: Unexpected image format or corrupted file."

(**[safe_exit ] exits the GUI safely*)
let safe_exit () =
  ();
  exit 0 (* Exit the program cleanly *)

let first_tup (x, _) = x
let sec_tup (_, x) = x
let i_width img = first_tup (Images.size img)
let i_length img = sec_tup (Images.size img)

(**This launches the GUI and the operations it can do.*)
(** writing the image out functions*)
let rgb24_to_color_array (img : Rgb24.t) : Graphics.color array array =
  let w, h = (img.Rgb24.width, img.Rgb24.height) in
  let color_matrix = Array.make_matrix h w Graphics.black in
  (* Initialize with black *)
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let { Color.r; g; b } = Rgb24.get img x y in
      color_matrix.(y).(x) <- Graphics.rgb r g b
    done
  done;
  color_matrix

let load_options =
  [
    Images.Load_Progress
      (fun progress ->
        Printf.printf "Loading progress: %.2f%%\n" (progress *. 100.0));
    Images.Load_Resolution (300.0, 300.0)
    (* Setting resolution to 300 DPI *)
    (* Load_only_the_first_frame could also be included here if needed *);
  ]

(* Load a PPM file and convert it to a color array *)
let load_ppm_as_color_array filename : Graphics.color array array =
  match Ppm.load filename load_options with
  | Rgb24 img -> rgb24_to_color_array img
  | _ -> failwith "Error: Unexpected image format or corrupted file."

(**[safe_exit ] exits the GUI safely*)
let safe_exit () =
  ();
  exit 0 (* Exit the program cleanly *)

let first_tup (x, _) = x
let sec_tup (_, x) = x
let i_width img = first_tup (Images.size img)
let i_length img = sec_tup (Images.size img)

(**This launches the GUI and the operations it can do.*)
let () =
  try
    basic_window ();
    draw_buttons ();
    start_text ();
    let color_array = load_ppm_as_color_array "data/actual_sugar_title.ppm" in
    let img = Graphics.make_image color_array in
    Graphics.draw_image img 550 900;
    if Sys.argv.(1) = "autofill" then 
    print_to_screen "" 580 855 120 580 (Hashtbl.create 5) (Hashtbl.create 5) 0 ""
  else if Sys.argv.(1) = "sentence" then print_to_screen_sentence "" 580 855 120 580 (Hashtbl.create 5) (Hashtbl.create 5) 0 ""
  with
  | Graphics.Graphic_failure _ ->
      (* Catch the fatal I/O error and exit cleanly *)
      safe_exit ()
  | Sys_error msg -> Printf.printf "Error: %s\n" msg
  | Failure msg -> Printf.printf "Error: %s\n" msg