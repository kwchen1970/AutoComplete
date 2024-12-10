exception File_not_found of string

type 'a t
(** ['a t] is List. *)

val empty : 'a t
(** [empty] is the representation of an empty dictionary.*)

val create_lines_list : string -> string list
(** [create_lines_list file_name] is a function that takes in the text file to
    be spellchecked [file_name] and returns a string list where each element is
    a seperate line in [file_name].*)

val create_dict : string -> string list -> string list
val fold : string list -> string
