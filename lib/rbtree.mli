type color =
  | Red
  | Black

type 'a t =
  | Leaf
  | Node of color * 'a * 'a t * 'a t

val empty : 'a t
val is_empty : 'a t -> bool
val mem : 'a -> 'a t -> ('a -> 'a -> int) -> bool
val insert : 'a -> 'a t -> ('a -> 'a -> int) -> 'a t
val remove : 'a -> 'a t -> ('a -> 'a -> int) -> 'a t
val preorder_traversal : 'a t -> 'a list
val inorder_traversal : 'a t -> 'a list
val postorder_traversal : 'a t -> 'a list
val to_string : ('a -> string) -> 'a t -> int -> string
