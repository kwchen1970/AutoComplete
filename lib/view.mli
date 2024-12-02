type t

val insert_all : string list -> t -> t
(**[insert_all lis t] inserts all words in lis into a t*)
val string_to_char_list : string -> char list
val start_text: unit -> unit
(**prints the type here text to screen*)
val bubble : int -> int -> unit -> unit
(**draws the grey bubbles*)
(* val words : int -> string list -> unit *)
(* val no_suggest : int -> int -> unit *)
(**clears the suggestions*)
val is_inside : int * int -> int * int -> int -> int -> bool
(** returns whether the user's click is within a certain rectangle on the screen *)

val find_chosen_suggestion : string list -> string option
(** finds and draws the suggestion that the user clicked on *)

val print_to_screen : string -> int -> int -> int -> unit
(**prints the typing and suggestions to screen*)
val insert_all : string list -> t -> t
(**[insert_all lis t] inserts all words in lis into a t*)
val string_to_char_list : string -> char list
val start_text: unit -> unit
(**prints the type here text to screen*)
val bubble : int -> int -> unit -> unit
(**draws the grey bubbles*)
(* val words : int -> string list -> unit *)
(* val print_suggestions1 : string list -> unit *)
(**draws suggestions in a vertical list*)

val is_inside : int * int -> int * int -> int -> int -> bool
(** returns whether the user's click is within a certain rectangle on the screen *)

val find_chosen_suggestion : string list -> string option
(** finds and draws the suggestion that the user clicked on *)

val print_to_screen : string ->
  int -> int -> int -> int -> (int, string) Hashtbl.t -> (int, string) Hashtbl.t -> int -> string-> unit
val print_to_screen_sentence : string ->
    int -> int -> int -> int -> (int, string) Hashtbl.t -> (int, string) Hashtbl.t -> int -> string-> unit
(**prints the typing and suggestions to screen*)
val basic_window : unit -> unit
(**generates blank window*)
val tree : t
(**tree used for testing*)
(* val accumulate_and_display : string -> 'a *)
(**accumulates and generates suggestions*)
val load_ppm: string -> unit
(**loads a ppm file into a image*)
(* 
val test_sentence_auto : string -> int -> int -> unit  *)