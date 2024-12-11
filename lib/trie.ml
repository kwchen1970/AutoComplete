(* Implementation of Trie Trees, due 11/5.*)
open Str
open Lwt.Infix
module StringMap = Map.Make (String)

(* Define the type of CharMap where the values are of type value_type *)
type t = Node of bool * int * t StringMap.t
(* [t] is the representation type for the Trie tree, the tree is a CharMap with
   keys being the characters 'a' though 'z' and each key character pointing to
   CharMap values. [int] is the length of the tree.*)

exception Empty

let empty = Node (false, 0, StringMap.empty)

(** ('k, 'v) hashtable represent a priority queue of all the searched words.. If
    priorities of a word is updated, as in it is inserted multiple times:
    replace with new priority value. Key = word : 'k, value = priority : int.
    Everytime we [search] a prefix, given the list of returned words, get the
    bindings of each word, then return in order of priority. *)
let priorities = ref (Hashtbl.create 20)

(* Red-black tree that stores (priority, word) in order of smallest to greatest
   priority. *)
let refresh_priorities = priorities := Hashtbl.create 20
let pqueue = ref Rbtree.empty
let return_pqueue () = !pqueue

(* Data of the rbtree are lists containing values of the same priority. *)
let pqueue_to_string pqueue =
  Rbtree.to_string
    (List.fold_left
       (fun acc (k, p) ->
         if acc <> "" then acc ^ ", " ^ k ^ "[" ^ string_of_int p ^ "]"
         else k ^ "[" ^ string_of_int p ^ "]")
       "")
    pqueue 0

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
let insert_new char_list (Node (is_word, priority, tree)) =
  let rec insert_p char_list (Node (is_word, priority, tree)) prefix =
    match char_list with
    | [] ->
        let _ =
          match Hashtbl.find_opt !priorities prefix with
          | Some p -> Hashtbl.replace !priorities prefix (p + 1)
          | None -> Hashtbl.add !priorities prefix 1
        in
        Node (true, 1, tree)
    | h :: t ->
        let prefix = prefix ^ String.make 1 h in
        let new_tree =
          (* If [h] is a key in [tree], then traverse the tree that [h] points
             to. *)
          if StringMap.mem prefix tree then
            insert_p t (StringMap.find prefix tree) prefix
          else insert_p t empty prefix
        in
        Node (is_word, priority, StringMap.add prefix new_tree tree)
  in
  insert_p char_list (Node (is_word, priority, tree)) ""

let insert char_list (Node (is_word, priority, tree) as trie) =
  let new_tree = insert_new char_list trie in
  new_tree

(* let rec prepend prefix lst acc = match lst with | [] -> acc | h :: t ->
   prepend prefix t ((prefix ^ h) :: acc) *)

(* For all keys in tree, continuting traversing through value pointed to by key
   if value != empty, else return key. *)

let rec search_all (Node (is_word, priority, tree)) =
  let pairs = StringMap.bindings tree in
  let rec search_pairs pairs =
    match pairs with
    | [] -> ()
    | (key, _) :: t ->
        let (Node (is_word, priority, _)) = StringMap.find key tree in
        let cmp (k1, p1) (k2, p2) = p1 - p2 in
        let _ = search_pairs t in
        if StringMap.cardinal (get_tree (StringMap.find key tree)) != 0 then
          if is_word then (
            pqueue :=
              Rbtree.insert (key, Hashtbl.find !priorities key) !pqueue cmp;
            search_all (StringMap.find key tree))
          else search_all (StringMap.find key tree)
        else
          (* Make larger priority on the left side of the tree. *)
          pqueue :=
            Rbtree.insert (key, Hashtbl.find !priorities key) !pqueue cmp
  in
  search_pairs pairs

let search prefix_list (Node (is_word, priority, tree)) =
  pqueue := Rbtree.empty;
  last_prefix :=
    List.fold_left (fun acc elem -> acc ^ String.make 1 elem) "" prefix_list;
  try
    let _ =
      let rec search_p prefix_list (Node (is_word, priority, tree)) prefix =
        match prefix_list with
        | [] ->
            (* [last_visited] is the Node where key = last character in
               [prefix_list] *)
            let cmp (k1, p1) (k2, p2) = p1 - p2 in
            last_visited := (!last_prefix, Node (is_word, priority, tree));
            if is_word then
              if is_empty (Node (is_word, priority, tree)) then
                pqueue :=
                  Rbtree.insert
                    (prefix, Hashtbl.find !priorities prefix)
                    !pqueue cmp
              else (
                pqueue :=
                  Rbtree.insert
                    (prefix, Hashtbl.find !priorities prefix)
                    !pqueue cmp;
                search_all (Node (is_word, priority, tree)))
            else search_all (Node (is_word, priority, tree))
        | h :: t ->
            let prefix = prefix ^ String.make 1 h in
            search_p t (StringMap.find prefix tree) prefix
      in
      search_p prefix_list (Node (is_word, priority, tree)) ""
    in
    let full_list =
      List.map (fun (k, p) -> k) (Rbtree.inorder_traversal !pqueue)
    in
    let rec sub_list full_list n =
      match (n, full_list) with
      | 0, _ -> []
      | _, [] -> []
      | n, h :: t -> h :: sub_list t (n - 1)
    in
    sub_list (List.rev full_list) 5
  with Not_found -> []

(* Recurse to the bottom of the tree, then continue making the nodes Empty until
   meet the next [is_word = true]. *)
let remove word (Node (is_word, priority, tree) as trie) =
  try
    let cmp (k1, p1) (k2, p2) = p1 - p2 in
    pqueue := Rbtree.remove (word, Hashtbl.find !priorities word) !pqueue cmp;
    Hashtbl.remove !priorities word;
    let prefix_list = to_char_list word in
    let rec remove_p prefix_list (Node (is_word, priority, tree)) prefix =
      match prefix_list with
      | [] ->
          (* Start removing if the word is a leaf, if it is a parent, just make
             [is_word] false, propagate up [true] if can continue removing. *)
          if StringMap.is_empty tree then
            (Node (false, priority, StringMap.empty), true)
          else (Node (false, priority, tree), false)
      | h :: t ->
          let prefix = prefix ^ String.make 1 h in
          let Node (is_word', _, new_tree), is_remove =
            remove_p t (StringMap.find prefix tree) prefix
          in
          let updated_tree =
            if is_remove && StringMap.is_empty new_tree then
              StringMap.remove prefix tree
            else StringMap.add prefix (Node (is_word', priority, new_tree)) tree
          in
          ( Node (is_word, priority, updated_tree),
            is_remove && StringMap.is_empty updated_tree )
    in
    fst (remove_p prefix_list (Node (is_word, priority, tree)) "")
  with Not_found -> trie

let all_words (Node (is_word, priority, tree)) =
  search (to_char_list "") (Node (is_word, priority, tree))

let rec traverse_all (Node (is_word, priority, tree)) depth =
  let pairs = StringMap.bindings tree in
  let rec search_pairs pairs =
    match pairs with
    | [] -> ""
    | (key, _) :: t ->
        let (Node (is_word, priority, _)) = StringMap.find key tree in
        let a = search_pairs t in
        if StringMap.cardinal (get_tree (StringMap.find key tree)) != 0 then
          if is_word then
            "\n" ^ string_of_int depth ^ " : (WORD: " ^ key ^ ") -> "
            ^ traverse_all (StringMap.find key tree) (depth + 1)
            ^ a
          else
            "\n" ^ string_of_int depth ^ " : (" ^ key ^ " -> "
            ^ traverse_all (StringMap.find key tree) (depth + 1)
            ^ a ^ ")"
        else "\n" ^ string_of_int depth ^ " : (WORD: " ^ key ^ ")" ^ a
  in
  search_pairs pairs

let rec to_string (Node (is_word, priority, tree)) =
  let pairs = StringMap.bindings tree in
  let rec traverse pairs depth =
    match pairs with
    | [] -> ""
    | (key, _) :: t ->
        let (Node (is_word, priority, _)) = StringMap.find key tree in
        let a = traverse t depth in
        string_of_int depth ^ " : (" ^ key ^ " -> "
        ^ traverse_all (StringMap.find key tree) (depth + 1)
        ^ ")\n" ^ a
  in
  traverse pairs 0
