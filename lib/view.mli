type t

val insert_all : string list -> t -> t
val string_to_char_list : string -> char list
val bubble : int -> int -> unit -> unit
val words : int -> string list -> unit
val no_suggest : unit -> unit
val print_suggestions1 : string list -> unit

val is_inside : int * int -> int * int -> int -> int -> bool
(** returns whether the user's click is within a certain rectangle on the screen *)

val find_chosen_suggestion : string list -> string option
(** finds and draws the suggestion that the user clicked on *)

val print_to_screen : string -> int -> int -> int -> unit
val basic_window : unit -> unit
val start_text : unit -> unit
val return_suggest : int -> int -> int -> unit
val tree : t
val accumulate_and_display : string -> 'a
