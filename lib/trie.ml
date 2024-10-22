(* Implementation of Trie Trees, due 11/5.*)
open Str

(* module MakeTrieTree (TR : TrieTree) = struct *)
(* [elt] is the type of an element in the priority queue. *)

module CharMap = Map.Make (Char)

(* Define the type of CharMap where the values are of type value_type *)
type t = Node of t CharMap.t

(* [t] is the representation type for the Trie tree, the tree is a CharMap with
   keys being the characters 'a' though 'z' and each key character pointing to
   CharMap values. [int] is the length of the tree.*)

exception Empty

let empty = Node CharMap.empty

let is_empty tree =
  match CharMap.cardinal tree with
  | 0 -> true
  | _ -> false
(* [empty] is the empty Trie tree, represented by an array containing 26 Nodes
   with elements [word] = "a" to "z" and both [t] = Leaf. *)

let get_tree (Node tree) = tree

let to_char_list word =
  List.of_seq (String.to_seq (String.lowercase_ascii word))

(* [insert char_list tree] is a function that inserts the word represented by
   [char_list] into the Trie tree [tree]. *)
let rec insert char_list (Node tree) =
  match char_list with
  | [] -> Node tree
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
      Node (CharMap.add h new_tree tree)

let rec prepend prefix lst =
  match lst with
  | [] -> []
  | h :: t -> (prefix ^ h) :: prepend prefix t

(* For all keys in tree, continuting traversing through value pointed to by key
   if value != empty, else return key. *)
let rec search_all (Node tree) =
  let pairs = CharMap.bindings tree in
  print_endline (string_of_int (List.length pairs));
  let rec search_pairs pairs =
    match pairs with
    | [] -> []
    | (key, _) :: t ->
        print_endline ("at " ^ String.make 1 key);
        (* (String.make 1key :: *)
        let a = search_pairs t in
        if CharMap.cardinal (get_tree (CharMap.find key tree)) != 0 then
          a @ prepend (String.make 1 key) (search_all (CharMap.find key tree))
        else a @ [ String.make 1 key ]
  in
  search_pairs pairs

let rec search prefix_list (Node tree) =
  match prefix_list with
  | [] -> if is_empty tree then [] else search_all (Node tree)
  | h :: t -> prepend (String.make 1 h) (search t (CharMap.find h tree))

(* let search prefix tree = let prefix_list = to_char_list prefix in search_help
   prefix_list tree *)
