(* Implementation of Trie Trees, due 11/5.*)
open Str

module type TRIE = sig
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
  (** [insert word tree] is the Trie tree that results from inserting [word]
      into [tree]. *)

  val prepend : string -> string list -> string list
  (** [to_word_list tree] returns a list containing all the word leaves in the
      Trie tree [tree]. *)

  val search : char list -> t -> string list
  (* val to_word_list :'a t-> word list *)

  val all_words : t -> string list
end

module Trie : TRIE = struct
  module CharMap = Map.Make (Char)

  (* Define the type of CharMap where the values are of type value_type *)
  type t = Node of bool * int * t CharMap.t

  (* [t] is the representation type for the Trie tree, the tree is a CharMap
     with keys being the characters 'a' though 'z' and each key character
     pointing to CharMap values. [int] is the length of the tree.*)

  exception Empty

  let empty = Node (false, 0, CharMap.empty)

  let is_empty tree =
    match CharMap.cardinal tree with
    | 0 -> true
    | _ -> false
  (* [empty] is the empty Trie tree, represented by an array containing 26 Nodes
     with elements [word] = "a" to "z" and both [t] = Leaf. *)

  let get_tree (Node (_, _, tree)) = tree

  let to_char_list word =
    List.of_seq (String.to_seq (String.lowercase_ascii word))

  (* [insert char_list tree] is a function that inserts the word represented by
     [char_list] into the Trie tree [tree]. *)
  let rec insert char_list (Node (is_word, priority, tree)) =
    match char_list with
    | [] -> Node (true, 1, tree)
    | h :: t ->
        let new_tree =
          (* If [h] is a key in [tree], then traverse the tree that [h] points
             to. *)
          if CharMap.mem h tree then insert t (CharMap.find h tree)
            (* Else add [h] to [tree] as initially an empty tree that will
               eventually have other keys after recursing through the rest of
               char_list. *)
          else insert t empty
        in
        Node (is_word, priority, CharMap.add h new_tree tree)

  let rec prepend prefix lst =
    match lst with
    | [] -> []
    | h :: t -> (prefix ^ h) :: prepend prefix t

  (* For all keys in tree, continuting traversing through value pointed to by
     key if value != empty, else return key. *)
  let rec search_all (Node (is_word, priority, tree)) =
    let pairs = CharMap.bindings tree in
    let rec search_pairs pairs =
      match pairs with
      | [] -> []
      | (key, _) :: t ->
          let (Node (is_word, priority, _)) = CharMap.find key tree in
          let a = search_pairs t in
          if CharMap.cardinal (get_tree (CharMap.find key tree)) != 0 then
            if is_word then
              (a @ [ String.make 1 key ^ " " ^ string_of_int priority ])
              @ a
              @ prepend (String.make 1 key) (search_all (CharMap.find key tree))
            else
              a
              @ prepend (String.make 1 key) (search_all (CharMap.find key tree))
          else a @ [ String.make 1 key ^ " " ^ string_of_int priority ]
    in
    search_pairs pairs

  let rec search prefix_list (Node (is_word, priority, tree)) =
    match prefix_list with
    | [] ->
        if is_empty tree then [ "" ]
        else search_all (Node (is_word, priority, tree))
    | h :: t -> prepend (String.make 1 h) (search t (CharMap.find h tree))

  let all_words (Node (is_word, priority, tree)) =
    search (to_char_list "") (Node (is_word, priority, tree))
end
