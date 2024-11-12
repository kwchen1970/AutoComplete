module StringTable = Hashtbl.Make (String)

type s_word = {
  word : string;
  mutable s_occ : int;
}

type ngram = {
  mutable occ : int;
  suggestion : s_word Pairing_heap.t;
  tokens : s_word Pairing_heap.Elt.t StringTable.t;
}

type t = ngram StringTable.t

exception NotFound of string

let empty () : t = StringTable.create 16

let new_heap () =
  let cmp_occ c1 c2 = compare c2.s_occ c1.s_occ in
  Pairing_heap.create ~cmp:cmp_occ ()

let new_ngram coll str c =
  let suggestion_heap = new_heap () in
  let token =
    Pairing_heap.add_removable suggestion_heap { word = c; s_occ = 1 }
  in
  let tokens = StringTable.create 16 in
  StringTable.add tokens c token;
  StringTable.add coll str { occ = 1; suggestion = suggestion_heap; tokens }

let search_ngram coll str =
  match StringTable.find_opt coll str with
  | None -> raise (NotFound str)
  | Some x -> x

let add_ngram coll str c =
  match StringTable.find_opt coll str with
  | None ->
      (* first time see str *)
      new_ngram coll str c
  | Some elt -> (
      elt.occ <- elt.occ + 1;
      let c_token = StringTable.find_opt elt.tokens c in
      match c_token with
      | Some token ->
          (* c has been in str's suggestions *)
          let c_entry = Pairing_heap.Elt.value_exn token in
          c_entry.s_occ <- c_entry.s_occ + 1;
          let new_token = Pairing_heap.update elt.suggestion token c_entry in
          StringTable.replace elt.tokens c new_token
      | None ->
          (* c not in str's suggestions *)
          let token =
            Pairing_heap.add_removable elt.suggestion { word = c; s_occ = 1 }
          in
          StringTable.add elt.tokens c token)

let get_occ coll str = (search_ngram coll str).occ

let get_s_occ coll str c =
  let ngram = search_ngram coll str in
  let c_token = StringTable.find_opt ngram.tokens c in
  match c_token with
  | Some token ->
      let c_entry = Pairing_heap.Elt.value_exn token in
      c_entry.s_occ
  | None -> raise (NotFound c)

(* Helper function: [to_list_ordered p_heap] returns s_word nodes in suggestion
   [p_heap] as a list ordered by descending s_occ (occurence of s_word). *)
let to_list_ordered p_heap =
  let rec make_list heap acc =
    let top = Pairing_heap.pop heap in
    match top with
    | None -> List.rev acc
    | Some x -> make_list heap (x :: acc)
  in
  make_list (Pairing_heap.copy p_heap) []

let get_suggestion coll str =
  let ngram = StringTable.find_opt coll str in
  match ngram with
  | None -> raise (NotFound str)
  | Some v -> List.map (fun x -> x.word) (to_list_ordered v.suggestion)

let get_top_suggestion coll str =
  let ngram = StringTable.find_opt coll str in
  match ngram with
  | None -> raise (NotFound str)
  | Some v -> (
      match Pairing_heap.top v.suggestion with
      | None -> raise (NotFound str)
      | Some x -> x.word)

let to_str_list coll = StringTable.fold (fun key _ acc -> key :: acc) coll []

let test_mutability =
  let suggestion_heap = new_heap () in
  let token =
    Pairing_heap.add_removable suggestion_heap { word = "testA"; s_occ = 1 }
  in
  let tokens = StringTable.create 16 in
  StringTable.add tokens "testA" token;
  let ngram = { occ = 1; suggestion = suggestion_heap; tokens } in
  ngram.occ <- ngram.occ + 1;
  let new_token =
    Pairing_heap.add_removable ngram.suggestion { word = "testB"; s_occ = 1 }
  in
  StringTable.add ngram.tokens "testB" new_token;
  ngram.occ <- ngram.occ + 1;
  let c_entry = Pairing_heap.Elt.value_exn token in
  c_entry.s_occ <- c_entry.s_occ + 1;
  let new_tokenA = Pairing_heap.update suggestion_heap token c_entry in
  StringTable.replace tokens "testA" new_tokenA;
  let suggestions =
    List.map (fun x -> x.word) (to_list_ordered ngram.suggestion)
  in
  (ngram.occ, suggestions)
(* c_entry.s_occ *)
