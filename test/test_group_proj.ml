open OUnit2
open Group_proj
include Tree.Trie
include Tree.Dict
module TestTrie = Trie

let fold_tree tree = List.fold_left (fun acc elem -> elem ^ " " ^ acc) "" tree
let tree = Trie.empty
let tree = Trie.insert (Trie.to_char_list "app") tree
let tree = Trie.insert (Trie.to_char_list "apple") tree
let tree = Trie.insert (Trie.to_char_list "applre") tree
let searched = Trie.search (Trie.to_char_list "a") tree

(* let _ = print_endline (fold_tree searched) *)
let last = snd !Trie.last_visited
let searched1 = Trie.search (Trie.to_char_list "pple") last
(* let _ = print_endline (fold_tree searched1) *)

let rec print_str_list lst =
  match lst with
  | [] -> ""
  | [ x ] -> x
  | h :: t -> h ^ "; " ^ print_str_list t

let list_equal lst1 lst2 = List.sort compare lst1 = List.sort compare lst2

module TrieTester (T : TRIE) = struct
  include T

  (* Thread each [insert] function in [insert_all]. *)
  (* Add tests for capitalization, punctuation, words that are substrings or
     other words, multiple element words (not allowed, used regex to stop). *)
  let rec insert_all word_list tree =
    match word_list with
    | [] -> tree
    | h :: t ->
        let new_tree = insert (to_char_list h) tree in
        insert_all t new_tree

  let fold_tree tree = List.fold_left (fun acc elem -> elem ^ " " ^ acc) "" tree

  let make_insert_test expected word_lst =
    "" >:: fun _ ->
    let rec cmp expected actual =
      match expected with
      | [] -> true
      | h :: t -> if List.mem h actual then cmp t actual else false
    in
    (* Check if all contents in expected are also in the tree containing words
       from [word_list]. *)
    assert_equal true
      (cmp expected (all_words (insert_all word_lst empty)))
      ~printer:string_of_bool;
    (* Check if the length of [expected] = number of word leaves in the tree
       containing words from [word_list]. *)
    assert_equal (List.length expected) (List.length word_lst)
      ~printer:string_of_int

  (* [make_search_test] *)
  let make_search_test expected prefix tree =
    "" >:: fun _ ->
    let rec cmp expected actual =
      match expected with
      | [] -> true
      | h :: t -> if List.mem h actual then cmp t actual else false
    in
    (* Check if all contents in expected are also in the tree containing words
       from [word_list]. *)
    let leaves = search (to_char_list prefix) tree in
    assert_equal true (cmp expected leaves) ~printer:string_of_bool;
    (* Check if the length of [expected] = number of word leaves in the tree
       containing words from [word_list]. *)
    assert_equal (List.length expected) (List.length leaves)
      ~printer:string_of_int

  let tree =
    insert_all
      [
        "apple";
        "aprle";
        "appol";
        "appla";
        "apole";
        "bapple";
        "barple";
        "triangle";
      ]
      empty

  (* Test suite that tests [insert]. *)
  let make_tree_tests =
    [
      make_insert_test
        [ "apple"; "appol"; "aprle" ]
        [ "apple"; "appol"; "aprle" ];
      (let word_lst =
         [
           "apple";
           "aprle";
           "appol";
           "appla";
           "apole";
           "bapple";
           "barple";
           "triangle";
         ]
       in
       let word_tree = insert_all word_lst empty in
       let _ = make_insert_test word_lst word_lst in
       let _ = make_search_test [ "apple"; "appol"; "appla" ] "app" word_tree in
       let _ =
         make_search_test (all_words (insert_all word_lst empty)) "" word_tree
       in
       let _ = make_search_test [ "bapple"; "barple" ] "b" word_tree in
       make_search_test [ "triangle" ] "triangl" word_tree);
      make_search_test [] "" empty;
      (let wood_dict = create_dict "../data/COMMON.TXT" [] in
       let wood_lst =
         [
           "wood alcohol";
           "wood anemone";
           "wood block";
           "wood coal";
           "wood duck";
           "wood engraving";
           "wood hyacinth";
           "wood ibis";
           "wood lot";
           "wood louse";
           "wood meadow grass";
           "wood mouse";
           "wood nymph";
           "wood pigeon";
           "wood pitch";
           "wood pulp";
           "wood rat";
           "wood sorrel";
           "wood spirit";
           "wood sugar";
           "wood vinegar";
           "wood warbler";
         ]
       in
       let word_tree = insert_all wood_dict empty in
       let _ = make_search_test wood_lst "wood " word_tree in
       make_search_test (List.rev wood_lst) "wood " word_tree);
    ]
end

module NGramTester (C : module type of NgramColl) = struct
  include C

  let add_all coll lst =
    List.iter (fun x -> C.add_ngram coll (fst x) (snd x)) lst;
    coll

  let make_add_test exp input =
    "" >:: fun _ ->
    let list_of_coll = C.to_str_list (add_all (C.empty ()) input) in
    assert_equal ~cmp:list_equal exp list_of_coll ~printer:print_str_list

  let make_top_suggestion_test exp coll str =
    "" >:: fun _ ->
    let top_suggestion = C.get_top_suggestion coll str in
    assert_equal exp top_suggestion ~printer:(fun x -> x)

  let make_suggestion_test exp coll str =
    "" >:: fun _ ->
    let suggestion = C.get_suggestion coll str in
    assert_equal exp suggestion ~printer:print_str_list

  let make_occ_test exp coll str =
    "" >:: fun _ -> assert_equal exp (C.get_occ coll str) ~printer:string_of_int

  let make_s_occ_test exp coll str c =
    "" >:: fun _ ->
    assert_equal exp (C.get_s_occ coll str c) ~printer:string_of_int

  let make_mutable_occ_test =
    "" >:: fun _ ->
    assert_equal 3 (fst C.test_mutability) ~printer:string_of_int

  let make_mutable_sug_test =
    "" >:: fun _ ->
    assert_equal ~cmp:list_equal [ "testA"; "testB" ] (snd C.test_mutability)
      ~printer:print_str_list

  (* let make_mutable_socc_test = "" >:: fun _ -> assert_equal 2
     C.test_mutability ~printer:string_of_int *)

  let coll_wood =
    add_all (C.empty ())
      [
        ("wood", "alcohol");
        ("wood", "anemone");
        ("wood", "block");
        ("wood", "coal");
        ("wood", "duck");
        ("wood", "engraving");
        ("wood", "engraving");
        ("wood", "engraving");
        ("wood", "meadow");
        ("wood", "mouse");
        ("wood", "nymph");
        ("wood", "pigeon");
        ("wood", "pigeon");
        ("wood", "pitch");
        ("wood", "pulp");
        ("wood", "sorrel");
        ("wood", "spirit");
        ("wood", "sugar");
        ("wood", "vinegar");
        ("wood", "warbler");
        ("nonwood", "yeah");
      ]

  let wood_suggestion =
    [
      "engraving";
      "pigeon";
      "warbler";
      "vinegar";
      "sugar";
      "spirit";
      "sorrel";
      "pulp";
      "pitch";
      "nymph";
      "mouse";
      "meadow";
      "duck";
      "coal";
      "block";
      "anemone";
      "alcohol";
    ]

  let make_ngram_test =
    [
      make_add_test
        [ "AA BB CCC"; "A B CC"; "AAA BB" ]
        [ ("A B CC", "c"); ("AA BB CCC", "aa"); ("AAA BB", "aaa") ];
      make_top_suggestion_test "engraving" coll_wood "wood";
      make_suggestion_test wood_suggestion coll_wood "wood";
      make_occ_test 20 coll_wood "wood";
      make_s_occ_test 3 coll_wood "wood" "engraving";
      make_s_occ_test 2 coll_wood "wood" "pigeon";
      make_mutable_occ_test;
      make_mutable_sug_test;
      (* make_mutable_socc_test; *)
    ]
end

module TrieTest = TrieTester (Trie)
module NGramTest = NGramTester (NgramColl)

let test_suite =
  "test suite"
  >::: List.flatten [ TrieTest.make_tree_tests; NGramTest.make_ngram_test ]

let _ = run_test_tt_main test_suite
