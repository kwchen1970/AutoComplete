type t
(** AF: [t] is a Trie tree that represents a tree.

    RI: Every [Leaf] of the Trie tree must be a word that is inputted by the
    user. For all [prefixes] in a StringMap node of the Trie tree that have
    [is_word = true], there are no duplicate words among these [prefixes].*)

exception Empty
(** Raised when an operation cannot be performed because a Trie tree is empty. *)

val empty : t
(** [empty] is the empty Trie tree *)
val refresh_priorities : unit
val return_pqueue : unit -> (string * int) Rbtree.t
val pqueue_to_string : (string * int) Rbtree.t -> string
val last_visited : (string * t) ref
val last_prefix : string ref
val to_char_list : string -> char list

val is_empty : t -> bool
(** [is_empty q] is [true] when [q] is the empty Trie tree and [false]
    otherwise. *)

val insert : char list -> t -> t
(** [insert word tree prefix] is the Trie tree that results from inserting
    [word] into [tree]. *)

val prepend : string -> string list -> string list -> string list
(** [to_word_list tree] returns a list containing all the word leaves in the
    Trie tree [tree]. *)

val search : char list -> t -> string list
(* val to_word_list :'a t-> word list *)

val all_words : t -> string list
val remove : string -> t -> t
val to_string : t -> string
val insert_new : char list -> t -> t
