(* Implementation of Trie Trees, due 11/5.*)
open Str

module type TRIE = sig
  type t
  (** The type of a Trie tree. *)

  exception Empty
  (** Raised when an operation cannot be performed because a Trie tree is empty. *)

  val empty : t
  (** [empty] is the empty Trie tree *)

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
end

(* Implemenation of Trie Trees using CharMap. *)
module DepreciatedTrie : TRIE = struct
  module CharMap = Map.Make (Char)
  module StringMap = Map.Make (String)

  (* Define the type of CharMap where the values are of type value_type *)
  type t = Node of bool * int * t CharMap.t

  (* [t] is the representation type for the Trie tree, the tree is a CharMap
     with keys being the characters 'a' though 'z' and each key character
     pointing to CharMap values. [int] is the length of the tree.*)

  (* When the user is typing the same word, return the Node where the search
     function ends up at the last prefix, and then start searching from that
     Node. let last_node = ... *)
  exception Empty

  let empty = Node (false, 0, CharMap.empty)

  (* Map word keys to int priorities. *)
  let priorities = StringMap.empty
  let last_visited = ref ("", empty)
  let last_prefix = ref ""

  let is_empty (Node (_, _, tree)) =
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

  let rec prepend prefix lst acc =
    match lst with
    | [] -> acc
    | h :: t -> prepend prefix t ((prefix ^ h) :: acc)

  (* For all keys in tree, continuting traversing through value pointed to by
     key if value != empty, else return key. *)

  (* TODO: implement a StringMap that maps words to the Node containing info on
     that word. Key = word, value = Node which search "word" would end up. *)

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
              a
              @ [ String.make 1 key ]
              @ List.flatten
                  (a
                  :: [
                       prepend (String.make 1 key)
                         (search_all (CharMap.find key tree))
                         [];
                     ])
            else
              List.flatten
                (a
                :: [
                     prepend (String.make 1 key)
                       (search_all (CharMap.find key tree))
                       [];
                   ])
          else a @ [ String.make 1 key ]
    in
    search_pairs pairs

  let rec search prefix_list (Node (is_word, priority, tree)) =
    try
      match prefix_list with
      | [] ->
          if is_empty (Node (is_word, priority, tree)) then []
            (* SAVE THIS NODE SOMEWHERE!!!!!!!! *)
          else search_all (Node (is_word, priority, tree))
      | h :: t -> prepend (String.make 1 h) (search t (CharMap.find h tree)) []
    with Not_found -> []

  let all_words (Node (is_word, priority, tree)) =
    search (to_char_list "") (Node (is_word, priority, tree))
end

(* Implementation of Trie Trees using StringMap. The more efficient Trie Tree,
   replaceing DepreciatedTrie. *)
module Trie : TRIE = struct
  module StringMap = Map.Make (String)

  (* Define the type of CharMap where the values are of type value_type *)
  type t = Node of bool * int * t StringMap.t

  (* [t] is the representation type for the Trie tree, the tree is a CharMap
     with keys being the characters 'a' though 'z' and each key character
     pointing to CharMap values. [int] is the length of the tree.*)

  (* When the user is typing the same word, return the Node where the search
     function ends up at the last prefix, and then start searching from that
     Node. let last_node = ... Implement something that limits searched list to
     max 5 or smth.*)
  exception Empty

  let empty = Node (false, 0, StringMap.empty)

  (* Map word keys to int priorities. *)
  let priorities = StringMap.empty
  let last_visited = ref ("", empty)
  let last_prefix = ref ""

  let is_empty (Node (_, _, tree)) =
    match StringMap.cardinal tree with
    | 0 -> true
    | _ -> false
  (* [empty] is the empty Trie tree, represented by an array containing 26 Nodes
     with elements [word] = "a" to "z" and both [t] = Leaf. *)

  let get_tree (Node (_, _, tree)) = tree

  let to_char_list word =
    List.of_seq (String.to_seq (String.lowercase_ascii word))

  (* [insert char_list tree] is a function that inserts the word represented by
     [char_list] into the Trie tree [tree]. *)
  let insert char_list (Node (is_word, priority, tree)) =
    let rec insert_p char_list (Node (is_word, priority, tree)) prefix =
      match char_list with
      | [] -> Node (true, 1, tree)
      | h :: t ->
          let prefix = prefix ^ String.make 1 h in
          let new_tree =
            (* If [h] is a key in [tree], then traverse the tree that [h] points
               to. *)
            if StringMap.mem prefix tree then
              insert_p t (StringMap.find prefix tree) prefix
              (* Else add [h] to [tree] as initially an empty tree that will
                 eventually have other keys after recursing through the rest of
                 char_list. *)
            else insert_p t empty prefix
          in
          Node (is_word, priority, StringMap.add prefix new_tree tree)
    in
    insert_p char_list (Node (is_word, priority, tree)) ""

  let rec prepend prefix lst acc =
    match lst with
    | [] -> acc
    | h :: t -> prepend prefix t ((prefix ^ h) :: acc)

  (* For all keys in tree, continuting traversing through value pointed to by
     key if value != empty, else return key. *)

  (* TODO: implement a StringMap that maps words to the Node containing info on
     that word. Key = word, value = Node which search "word" would end up. *)

  let rec search_all (Node (is_word, priority, tree)) =
    let pairs = StringMap.bindings tree in
    let rec search_pairs pairs =
      match pairs with
      | [] -> []
      | (key, _) :: t ->
          let (Node (is_word, priority, _)) = StringMap.find key tree in
          let a = search_pairs t in
          if StringMap.cardinal (get_tree (StringMap.find key tree)) != 0 then
            if is_word then (key :: a) @ search_all (StringMap.find key tree)
            else a @ search_all (StringMap.find key tree)
          else key :: a
    in
    search_pairs pairs

  let search prefix_list (Node (is_word, priority, tree)) =
    print_endline ("last visited was " ^ fst !last_visited);
    last_prefix :=
      List.fold_left (fun acc elem -> acc ^ String.make 1 elem) "" prefix_list;
    try
      let rec search_p prefix_list (Node (is_word, priority, tree)) prefix =
        print_endline ("prefix is " ^ !last_prefix);
        match prefix_list with
        | [] ->
            (* [last_visited] is the Node where key = last character in
               [prefix_list] *)
            (* print_endline "ends here"; *)
            last_visited := (!last_prefix, Node (is_word, priority, tree));
            if is_empty (Node (is_word, priority, tree)) then []
            else search_all (Node (is_word, priority, tree))
        | h :: t ->
            let prefix = prefix ^ String.make 1 h in
            (* print_endline ("at " ^ prefix ^ string_of_bool (is_empty (Node
               (is_word, priority, tree)))); *)
            search_p t (StringMap.find prefix tree) prefix
      in
      print_endline (string_of_bool (is_empty (Node (is_word, priority, tree))));
      search_p prefix_list (Node (is_word, priority, tree)) ""
    with Not_found -> []

  let all_words (Node (is_word, priority, tree)) =
    search (to_char_list "") (Node (is_word, priority, tree))
end
