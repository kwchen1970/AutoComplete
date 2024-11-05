module StringMap = Map.Make (String)

type s_elt = {
  word : string;
  s_occ : int;
}

type elt = {
  occ : int;
  suggestion : s_elt Pairing_heap.t;
  tokens : s_elt Pairing_heap.Elt.t StringMap.t;
}

type t = elt StringMap.t

exception NotFound

let empty = StringMap.empty

let new_heap =
  let cmp_occ c1 c2 = compare c2.s_occ c1.s_occ in
  Pairing_heap.create ~cmp:cmp_occ ()

(* Helper function: [update_coll coll str elt] updates the value of [str] in
   [coll] with [elt].*)
let update_coll coll str elt = StringMap.add str elt coll

(* Helper function: [add_token elt c] adds word [c] to the suggestions and
   tokens of [elt]. *)
let add_token (elt : elt) c : elt =
  let token =
    Pairing_heap.add_removable elt.suggestion { word = c; s_occ = 1 }
  in
  { elt with occ = elt.occ + 1; tokens = StringMap.add c token elt.tokens }

(* Helper function: [update_token elt c token] updates the occurences and token
   for word [c] in [elt]'s suggestions and tokens. *)
let update_token (elt : elt) c token : elt =
  let c_entry = Pairing_heap.Elt.value_exn token in
  let new_c_entry = { c_entry with s_occ = c_entry.s_occ + 1 } in
  let new_token = Pairing_heap.update elt.suggestion token new_c_entry in
  { elt with occ = elt.occ + 1; tokens = StringMap.add c new_token elt.tokens }

let add_ngram coll str c =
  let suggestion_heap = new_heap in
  let token =
    Pairing_heap.add_removable suggestion_heap { word = c; s_occ = 1 }
  in
  StringMap.add str
    {
      occ = 1;
      suggestion = suggestion_heap;
      tokens = StringMap.add c token StringMap.empty;
    }
    coll

let search_ngram coll str =
  match StringMap.find_opt str coll with
  | None -> raise NotFound
  | Some x -> x

let update_ngram coll str c =
  match StringMap.find_opt str coll with
  | None ->
      (* first time see str *)
      add_ngram coll str c
  | Some elt -> (
      let org_token = StringMap.find_opt c elt.tokens in
      match org_token with
      | Some token ->
          (* c has been in str's suggestions *)
          let updated_elt = update_token elt c token in
          update_coll coll str updated_elt
      | None ->
          (* c not in str's suggestions *)
          let updated_elt = add_token elt c in
          update_coll coll str updated_elt)

let get_occ coll str =
  let ngram = StringMap.find_opt str coll in
  match ngram with
  | None -> raise NotFound
  | Some v -> v.occ

let get_suggestion coll str =
  let ngram = StringMap.find_opt str coll in
  match ngram with
  | None -> raise NotFound
  | Some v -> List.map (fun x -> x.word) (Pairing_heap.to_list v.suggestion)

let get_top_suggestion coll str =
  let ngram = StringMap.find_opt str coll in
  match ngram with
  | None -> raise NotFound
  | Some v -> (
      match Pairing_heap.top v.suggestion with
      | None -> raise NotFound
      | Some x -> x.word)

let to_str_list coll = List.map fst (StringMap.bindings coll)
