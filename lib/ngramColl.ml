module StringMap = Map.Make (String)

(* TODO: use a priority queue with priority = occ instead *)
type s_elt = {
  word : string;
  occ : int;
}

type elt = {
  occ : int;
  suggestion : s_elt list;
}

type t = elt StringMap.t

exception NotFound

let empty = StringMap.empty

let add_ngram coll str c =
  StringMap.add str { occ = 1; suggestion = [ { word = c; occ = 1 } ] } coll

let search_ngram coll str =
  match StringMap.find_opt str coll with
  | None -> raise NotFound
  | Some x -> x

(* TODO: use priority queue rather than list. *)
let update_suggestions c c_lst =
  let c_entry = List.find_opt (fun x -> x.word = c) c_lst in
  match c_entry with
  | Some { word; occ } ->
      List.map
        (fun x -> if x.word = c then { x with occ = occ + 1 } else x)
        c_lst
  | None -> { word = c; occ = 1 } :: c_lst

let update_ngram coll str c =
  StringMap.update str
    (function
      | None -> Some { occ = 1; suggestion = [ { word = c; occ = 1 } ] }
      | Some elt ->
          Some
            {
              occ = elt.occ + 1;
              suggestion = update_suggestions c elt.suggestion;
            })
    coll

let get_occ coll str =
  let ngram = StringMap.find_opt str coll in
  match ngram with
  | None -> raise NotFound
  | Some v -> v.occ

(* TODO: use priority queue to list. *)
let get_suggestion coll str =
  let ngram = StringMap.find_opt str coll in
  match ngram with
  | None -> raise NotFound
  | Some v -> List.map (fun x -> x.word) v.suggestion
