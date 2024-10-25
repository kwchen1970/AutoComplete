type t
(** The type of an ngram collection. *)

exception NotFound

type elt
(** The type of a node of ngram. *)

type s_elt
(** The type of a node of unigram. *)

val empty : t
(** [empty] is the empty ngram collection. *)

val add_ngram : t -> string -> string -> t
(** [new_ngram coll str c] creates a new ngram node in [coll] with content [str]
    and initializes its children with [c]. *)

val search_ngram : t -> string -> elt
(** [search_ngram coll str] returns the ngram node with content [str] in the
    ngram collection [coll]. Raises: [NotFound] if such ngram node cannot be
    found. *)

val update_ngram : t -> string -> string -> t
(** [update_ngram coll str c] adds word [c] to the suggestion for the ngram node
    with content [str] in [coll]. *)

val get_occ : t -> string -> int
(** [get_occ coll str] returns the occurences of the ngram node [ngram] in
    [coll]. *)

val get_suggestion : t -> string -> string list
(** [get_suggestion coll str] returns the list of suggested word (sorted by
    suggestion priority) following after [str] from collection [coll]. *)

val to_str_list : t -> string list
(** [to_str_list coll] is the list of ngrams in [coll]. *)
