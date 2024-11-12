type t
(** The type of an ngram collection. *)

exception NotFound of string

type ngram
(** The type of a node of ngram. *)

type s_word
(** The type of a node of unigram. *)

val empty : unit -> t
(** [empty] is the empty ngram collection. *)

val new_ngram : t -> string -> string -> unit
(** [new_ngram coll str c] creates a new ngram node in [coll] with content [str]
    and initializes its children with [c]. *)

val search_ngram : t -> string -> ngram
(** [search_ngram coll str] returns the ngram node with content [str] in the
    ngram collection [coll]. Raises: [NotFound] if such ngram node cannot be
    found. *)

val add_ngram : t -> string -> string -> unit
(** [add_ngram coll str c] adds word [c] to the suggestion for the ngram node
    with content [str] in [coll]. *)

val get_occ : t -> string -> int
(** [get_occ coll str] returns the occurences of the ngram [str] in [coll]. *)

val get_s_occ : t -> string -> string -> int
(** [get_s_occ coll str c] returns the occurences of the word [c] as a successor
    of the ngram [str] in [coll]. *)

val get_suggestion : t -> string -> string list
(** [get_suggestion coll str] returns the list of suggested word (sorted by
    suggestion priority) following after [str] in collection [coll]. *)

val get_top_suggestion : t -> string -> string
(** [get_top_suggestion coll str] returns the top suggested word (sorted by
    suggestion priority) following after [str] in collection [coll]. *)

val to_str_list : t -> string list
(** [to_str_list coll] is the list of ngrams in [coll]. *)

val test_mutability : int * string list
