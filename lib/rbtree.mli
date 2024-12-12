type color =
  | Red
  | Black

(** [color] is the representation type for the color of a node in the red-black
    tree. [color] can either be [Red] or [Black]*)

type 'a t =
  | Leaf
  | Node of color * 'a list * 'a t * 'a t

(* AF: ['a t] is the representation type for the red-black tree, where a node
   containing data is [Node (color, [values], left, right), with [values] being
   a list with elements of the same value (in the case of the priority queue
   implementation, all elements in the same [values] list has the same
   priority). [Leaf] represents an empty red-black tree. *)

(* RI: All elements in the tree share the same type ['a] and for any elements
   [e1], [e2] in the same [values] list, the comparison function [cmp] should
   return [cmp e1 e2] = 0. The following invariants must be preserved:

   BST invariant: elements in the left subtree have values smaller than elements
   in the parent node, and elements in the right subtree have larger values.

   Local invariant: No red node [color = Red] has a red child.

   Global invariant: Every path from the root to a leaf has the same number of
   black nodes [color = Black].*)

val empty : 'a t
(** [empty] is an empty red-black tree. *)

val is_empty : 'a t -> bool
(** [is_empty tree] checks whether a red-black tree [tree] is empty.*)

val mem : 'a -> 'a t -> ('a -> 'a -> int) -> bool
(** [mem x tree cmp] is a function that checks whether [x] is an element in the
    red-black tree [tree], traversing [tree] based off of the given comparison
    function [cmp]. *)

val insert : 'a -> 'a t -> ('a -> 'a -> int) -> 'a t
(** [insert x tree cmp] is a function that inserts [x] into the red-black tree
    [tree], preserving order of all values of the tree as defined by the
    comparison function [cmp]. Duplicates are allowed: duplicate values will be
    added to the [values] list with same values (when cmp v1 v2 returns 0).

    Requires: that the BST, local, and global invariants be preserved in the
    possibly updated red-black tree. *)

val remove : 'a -> 'a t -> ('a -> 'a -> int) -> 'a t
(** [remove x tree cmp] is a function that removes [x] from the red-black tree
    [tree], returning a tree without [x] if [x] is a value in the tree and
    returning the unchanged [tree] if [x] is not in [tree].

    Requires: that the BST, local, and global invariants be preserved in the
    possibly updated red-black tree. *)

val preorder_traversal : 'a t -> 'a list
(** [preorder_traversal tree] is a function that returns a list of all values in
    non-empty nodes [Node] of [tree] in the order of preorder traversal. *)

val inorder_traversal : 'a t -> 'a list
(** [inorder_traversal tree] is a function that returns a list of all values in
    non-empty nodes [Node] of [tree] in the order of inorder traversal. *)

val postorder_traversal : 'a t -> 'a list
(** [postorder_traversal tree] is a function that returns a list of all values
    in non-empty nodes [Node] of [tree] in the order of postorder traversal. *)

val to_string : ('a list -> string) -> 'a t -> int -> string
(** [to_string f tree depth] is a function that returns a string representation
    of a red-black tree [tree], where values in [values] of [tree] are converted
    to string using a given function [f] and depth [depth] of each node is also
    used. *)
