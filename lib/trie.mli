type t
(** The type of a Trie tree. *)

exception Empty
(** Raised when an operation cannot be performed because a Trie tree is empty. *)

val empty : t
(** [empty] is the empty Trie tree *)

val to_char_list : string -> char list
(** [is_empty q] is [true] when [q] is the empty Trie tree and [false]
    otherwise. *)
(* val is_empty :'a t-> bool *)

val insert : char list -> t -> t
(** [insert word tree] is the Trie tree that results from inserting [word] into
    [tree]. *)

val prepend : string -> string list -> string list
(** [to_word_list tree] returns a list containing all the word leaves in the
    Trie tree [tree]. *)

val search : char list -> t -> string list
(* val to_word_list :'a t-> word list *)

val all_words : t -> string list
