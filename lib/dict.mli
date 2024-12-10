exception File_not_found of string

type 'a t
(** ['a t] is List. *)

val empty : 'a t
(** [empty] is the representation of an empty dictionary.*)

val create_lines_list : string -> string list
(** [create_lines_list file_name] is a function that takes in the text file to
    be spellchecked [file_name] and returns a string list where each element is
    a seperate line in [file_name].*)

val fill_dictionary : 'a list -> 'a list -> 'a list
(** [fill_dictionary dict] takes in a string list [lines_list] containing all
    lines from the text file to be spellchecked, where each line element
    corresponds to a word in either the system or user dictionary text files,
    and the combined system-user dictionary [dict] and adds all of these words
    into dictionary [dict]. Duplicate words are ignored. Returns a new
    dictionary containing all words in [lines_list] and [dict].*)

val create_dict : string -> string list -> string list
val fold : string list -> string
