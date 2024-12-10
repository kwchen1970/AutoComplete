include Batteries

exception File_not_found of string

type t = string BatSet.t

let empty = BatSet.empty
let fold lst = List.fold_left (fun acc elem -> acc ^ " [" ^ elem ^ "]") "" lst

(* Convert text file [file_name] into a string list. *)
let create_lines_list file_name =
  try
    let lines_enum = BatFile.lines_of file_name in
    let lines_list = BatList.of_enum lines_enum in
    lines_list
  with Sys_error _ ->
    raise (File_not_found ("[" ^ file_name ^ "] does not exist."))

(* Dedups the string list text file and returns a dictionary with no
   duplicates. *)
let rec create_dict file_name dict =
  let deduped_dict =
    BatSet.to_list (fill_dictionary (create_lines_list file_name) dict)
  in
  deduped_dict

and fill_dictionary lines_list dict =
  match lines_list with
  | [] -> dict
  | line :: t -> BatSet.add line (fill_dictionary t dict)
