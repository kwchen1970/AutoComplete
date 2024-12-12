type t
(** AF: [t] is the representation type for a Trie tree, where a node containing
    data is
    [Node of (is_word, priority, [tree = map of words with current node as prefix])].
    Each node contains a StringMap with Node elements that have the current
    node's word as its prefix. [Node (false, 0, StringMap.empty)] represents an
    empty Trie tree. *)

(** RI: Every Node of the Trie tree without any children, meaning that
    [tree = StringMap.empty], must be a word [is_word = true] that is inputted
    or inserted by the user. For all [prefixes] in a StringMap node of the Trie
    tree that have [is_word = true], there are no duplicate words among these
    [prefixes]. All children of a parent Node must have the parent Node's
    associated string as its prefix. Word Nodes, where [is_word = true], can
    also have children or non-empty [tree].*)

exception Empty
(** The [Empty] exception is raised when an operation cannot be performed
    because a Trie tree is empty. *)

val empty : t
(** [empty] is the empty Trie tree *)

val return_pqueue : unit -> (string * int) Rbtree.t
(** [return_pqueue ()] is a function that returns a priority queue implemented
    by a red-black tree containing certain words in the Trie tree ['a t].
    Elements of the priority queue are the tuple (word : string, priority :
    int). *)

val pqueue_to_string : (string * int) Rbtree.t -> string
(** [pqueue_to_string pqueue] is a function that returns a string representation
    the priority queue [pqueue] implemented by a red-black tree.*)

val last_visited : (string * t) ref
(** [last_visited] is a ref that contains the Node : t and corresponding prefix
    : string last accessed by some traversal through the Trie tree. *)

val last_prefix : string ref
(** [last_prefix] is a ref that contains the prefix : string last accessed by
    some traversal through the Trie tree. *)

val to_char_list : string -> char list
(** [to_char_list word] is a function that returns a character list containing
    all the characters of the given word [word], in the order that these
    characters appear in [word]. *)

val is_empty : t -> bool
(** [is_empty (Node (_, _, tree))] is a function that checks whether a Trie tree
    [(Node (_, _, tree))] is empty, returning [true] when [(Node (_, _, tree))]
    is the empty Trie tree and [false] otherwise. *)

val insert : char list -> t -> t
(** [insert word tree] is a function that inserts [word] into a Trie tree [tree]
    if [word] was previously not an element of the tree. If not already
    existing, the [insert] function will create a path containing Nodes where
    the parent Node's string is the prefix of its childrens' strings from the
    root of [tree] to a word leaf with [word] as its corresponding string. If
    [word] was previously not an element of the tree, [word] will be added to a
    hashtable of priorities [priorities] with starting priority of 1. If [word]
    was already an element of tree, [insert] will update [word]'s priority in
    [priorities] by incrementing the value of element with key = [word] by 1. *)

val insert_new : char list -> t -> t
(** [insert_new char_list tree] is a helper function to [insert] that, given the
    word [word] represented by [char_list] was previously not an element of
    tree, inserts the word into the Trie tree [tree] and updates the priorities
    hashtable [priorities] to include a new element with key = [word] and value
    = 1. If [word] is already in [tree], [insert_new] updates the element with
    key = [word] in [priorities] by incrementing its value priority by 1 (value
    = value + 1). *)

val search : char list -> t -> string list
(** [search prefix_list (Node (is_word, priority, tree))] is a function that
    traverses through a Trie tree with root node
    [(Node (is_word, priority, tree))] and returns the top five highest priority
    elements in the tree with prefix represented by the character list
    [prefix_list]. Priority of words is dependent on the number of times the
    user inputs that word (calls [insert] on [word]). *)

val all_words : t -> string list
(** [all_words (Node (is_word, priority, tree))] is a function with the same
    functionality as [search], except that [all_words] returns the top five
    highest priority elements in the Trie tree, with no constaints on the prefix
    of the returned words. *)

val remove : string -> t -> t
(** [remove word (Node (is_word, priority, tree) as trie)] is a function that
    removes word [word] from the Trie tree [trie], along with parent nodes that
    are substring or prefixes of [word], until a parent node either is a word
    [is_word = true] or has multiple children other than [word]. [remove]
    returns an updated Trie tree without [word] if [word] was an element of
    [trie], else [remove] returns the original, unmodified tree [trie]. If
    [word] is an element of [trie], [remove] also removes the element associated
    with [word] from the priorities hashtable [priorities] and the priority
    queue [pqueue]. *)

val to_string : t -> string
(** [to_string (Node (is_word, priority, tree))] is a function that returns the
    string representation of the Trie tree [(Node (is_word, priority, tree))].
    This specific string representation differentiates between whether the
    string corresponding to a Node is a word (has "WORD:" prefix) or not. *)
