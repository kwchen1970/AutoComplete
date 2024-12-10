type color =
  | Red
  | Black

type 'a t =
  | Leaf
  | Node of color * 'a list * 'a t * 'a t

let empty = Leaf
let is_empty tree = tree = Leaf

let balance = function
  | Black, z, Node (Red, y, Node (Red, x, a, b), c), d
  | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
  | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
  | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
      Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
  | c, v, l, r -> Node (c, v, l, r)

let rec insert_help x tree cmp =
  match tree with
  | Leaf -> Node (Red, [ x ], Leaf, Leaf)
  | Node (c, v, l, r) -> (
      let new_tree =
        if cmp x (List.hd v) < 0 then Node (c, v, insert_help x l cmp, r)
        else if cmp x (List.hd v) > 0 then Node (c, v, l, insert_help x r cmp)
        else if cmp x (List.hd v) = 0 then Node (c, x :: v, l, r)
        else tree
      in
      match new_tree with
      | Node (Black, v, l, r) -> balance (Black, v, l, r)
      | _ -> new_tree)

let insert x tree cmp =
  match insert_help x tree cmp with
  | Node (c, v, l, r) -> Node (Black, v, l, r)
  | _ -> tree

let rec remove x tree cmp =
  match tree with
  | Leaf -> tree
  | Node (c, v, l, r) -> (
      let new_tree =
        if cmp x (List.hd v) < 0 then Node (c, v, remove x l cmp, r)
        else if cmp x (List.hd v) > 0 then Node (c, v, l, remove x r cmp)
        else if cmp x (List.hd v) = 0 then
          let rec remove_list x v =
            match v with
            | [] -> []
            | h :: t -> if x = h then t else h :: remove_list x t
          in
          let new_list = remove_list x v in
          match new_list with
          | [] -> Leaf
          | _ -> Node (c, new_list, l, r)
        else tree
      in
      match new_tree with
      | Node (Black, v, l, r) -> balance (Black, v, l, r)
      | _ -> new_tree)

let rec mem x tree cmp =
  match tree with
  | Leaf -> false
  | Node (c, v, l, r) ->
      if cmp x (List.hd v) < 0 then mem x l cmp
      else if cmp x (List.hd v) > 0 then mem x r cmp
      else if cmp x (List.hd v) = 0 && List.mem x v then true
      else false

let string_of_color = function
  | Black -> "Black"
  | Red -> "Red"

let rec preorder_traversal tree =
  match tree with
  | Leaf -> []
  | Node (c, v, l, r) -> v @ preorder_traversal l @ preorder_traversal r

let rec inorder_traversal tree =
  match tree with
  | Leaf -> []
  | Node (c, v, l, r) -> preorder_traversal l @ v @ preorder_traversal r

let rec postorder_traversal tree =
  match tree with
  | Leaf -> []
  | Node (c, v, l, r) -> preorder_traversal l @ preorder_traversal r @ v

let rec to_string f tree depth =
  match tree with
  | Leaf -> "Leaf"
  | Node (c, v, l, r) ->
      let start = if depth > 0 then "\n" else "" in
      start ^ string_of_int depth ^ ": " ^ string_of_color c ^ " Node: (" ^ f v
      ^ ", "
      ^ to_string f l (depth + 1)
      ^ ", "
      ^ to_string f r (depth + 1)
      ^ ")"
