include Batteries

exception File_not_found of string

type 'a t = string list

let empty = []
let fold lst = List.fold_left (fun acc elem -> acc ^ elem) "" lst

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
