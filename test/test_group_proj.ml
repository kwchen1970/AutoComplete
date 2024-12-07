open OUnit2
open Group_proj
include Tree.Trie
include Tree.Dict

let fold_tree tree = List.fold_left (fun acc elem -> elem ^ " " ^ acc) "" tree

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
  let rec insert_all word_list tree : T.t =
    match word_list with
    | [] -> tree
    | h :: t ->
        let new_tree = insert (to_char_list h) tree in
        insert_all t new_tree

  let fold_tree tree = List.fold_left (fun acc elem -> elem ^ " " ^ acc) "" tree

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

  let make_insert_test expected word_lst =
    "" >:: fun _ ->
    let rec cmp expected actual =
      match expected with
      | [] -> true
      | h :: t -> if List.mem h actual then cmp t actual else false
    in
    (* Check if all contents in expected are also in the tree containing words
       from [word_list]. *)
    let all = all_words (insert_all word_lst empty) in
    assert_equal true (cmp expected all) ~printer:string_of_bool;
    (* Check if the length of [expected] >= number of word leaves in the tree
       containing words from [word_list]. *)
    assert_equal true
      (List.length expected >= List.length word_lst)
      ~printer:string_of_bool

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

    (* print_endline (List.fold_left (fun acc elem -> acc ^ " " ^ elem) ""
       leaves); *)
    assert_equal true (cmp leaves expected) ~printer:string_of_bool;

    (* Check if the length of [expected] = number of word leaves in the tree
       containing words from [word_list]. *)
    assert_equal true
      (if List.length expected > 5 then
         List.length expected >= List.length leaves
       else List.length expected = List.length leaves)
      ~printer:string_of_bool

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
       let word_tree =
         refresh_priorities;
         insert_all word_lst empty
       in
       let _ =
         refresh_priorities;
         make_insert_test word_lst word_lst
       in
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

       make_search_test wood_lst "wood " word_tree);
      (refresh_priorities;
       let wood_lst =
         [
           "wood alcohol";
           "wood anemone";
           "wood block";
           "wood ibis";
           "wood lot";
           "wood warbler";
           "wood rat";
           "wood sorrel";
           "wood anemone";
           "wood anemone";
           "wood anemone";
           "wood warbler";
         ]
       in
       let word_tree = insert_all wood_lst empty in
       let _ = make_search_test wood_lst "wood " word_tree in
       make_search_test (List.rev wood_lst) "wood " word_tree);
      (let tree = T.empty in
       let tree = T.insert (T.to_char_list "app") tree in
       let tree = T.insert (T.to_char_list "apple") tree in
       let tree = T.insert (T.to_char_list "applre") tree in
       let tree = T.remove "apple" tree in
       make_search_test [ "app"; "applre" ] "app" tree);
      (let wood_lst =
         [
           "wood alcohol";
           "wood anemone";
           "wood anemone";
           "wood anemone";
           "wood anemone";
           "wood warbler";
         ]
       in
       let word_tree = insert_all wood_lst empty in
       let _ = make_search_test wood_lst "wood " word_tree in
       let word_tree = T.remove "wood anemone" word_tree in
       let word_tree = T.remove "wood" word_tree in
       (* print_endline (List.fold_left (fun acc elem -> acc ^ " " ^ elem) ""
          (T.all_words word_tree)); *)
       make_search_test [ "wood alcohol"; "wood warbler" ] "wood " word_tree);
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

module RBTreeTester (RB : module type of Tree.Rbtree) = struct
  include RB

  let cmp (k1, p1) (k2, p2) = p1 - p2

  let rec function_all f list rbtree =
    match list with
    | [] -> rbtree
    | h :: t ->
        let new_rbtree = f h rbtree cmp in
        function_all f t new_rbtree

  let insert_all list rbtree = function_all RB.insert list rbtree
  let rec remove_all list rbtree = function_all RB.remove list rbtree

  let make_is_empty_test expected tree =
    "" >:: fun _ ->
    assert_equal expected (RB.is_empty tree) ~printer:string_of_bool

  let make_mem_test expected word tree =
    "" >:: fun _ ->
    assert_equal expected (RB.mem word tree cmp) ~printer:string_of_bool

  let rec make_insert_test words tree =
    match words with
    | [] -> "" >:: fun _ -> assert_equal true true
    | word :: t ->
        let new_tree = RB.insert word tree cmp in
        let _ =
          assert_equal true
            (List.length (RB.preorder_traversal tree)
            <= List.length (RB.preorder_traversal new_tree));
          assert_equal true
            (List.length (RB.inorder_traversal tree)
            <= List.length (RB.inorder_traversal new_tree));
          assert_equal true
            (List.length (RB.postorder_traversal tree)
            <= List.length (RB.postorder_traversal new_tree));
          assert_equal true (RB.mem word new_tree cmp) ~printer:string_of_bool
        in
        make_insert_test t new_tree

  let rec make_remove_test words tree =
    match words with
    | [] -> "" >:: fun _ -> assert_equal true true
    | word :: t ->
        let new_tree = RB.remove word tree cmp in
        let _ =
          assert_equal true
            (List.length (RB.preorder_traversal tree)
            >= List.length (RB.preorder_traversal new_tree));
          assert_equal true
            (List.length (RB.inorder_traversal tree)
            >= List.length (RB.inorder_traversal new_tree));
          assert_equal true
            (List.length (RB.postorder_traversal tree)
            >= List.length (RB.postorder_traversal new_tree));
          assert_equal false (RB.mem word new_tree cmp) ~printer:string_of_bool
        in
        make_remove_test t new_tree

  let arb_list =
    let possible_words = create_dict "../data/COMMON.TXT" [] in
    QCheck.(
      map
        (fun (k_index, p) -> [ (List.nth possible_words k_index, p) ])
        (pair (0 -- (List.length possible_words - 1)) (1 -- 100)))

  let rec make_random_insert_test words tree =
    match words with
    | [] -> true
    | word :: t ->
        (* Insert the word into the tree *)
        let new_tree = RB.insert word tree cmp in

        (* Check if the tree after insertion has greater or equal traversal
           lengths and contains the word *)
        let check_inserted =
          List.length (RB.preorder_traversal new_tree)
          >= List.length (RB.preorder_traversal tree)
          && List.length (RB.inorder_traversal new_tree)
             >= List.length (RB.inorder_traversal tree)
          && List.length (RB.postorder_traversal new_tree)
             >= List.length (RB.postorder_traversal tree)
          && RB.mem word new_tree cmp
        in

        (* If the condition holds true, recursively test the next words *)
        if check_inserted then make_random_insert_test t new_tree else false

  (* let test_random_rbtree_insert_test = QCheck.Test.make ~count:1000 arb_list
     make_random_insert_test *)

  let test_tree_A = RB.insert ("word", 1) RB.empty cmp

  let list_B =
    [
      ("wood alcohol", 1);
      ("wood anemone", 6);
      ("wood block", 3);
      ("wood ibis", 1);
      ("wood lot", 1);
      ("wood rat", 2);
      ("wood sorrel", 4);
      ("wood warbler", 2);
    ]

  let list_C =
    [
      ("wood meadow grass", 2);
      ("wood mouse", 13);
      ("wood nymph", 4);
      ("wood pigeon", 91);
    ]

  let test_tree_B = insert_all list_B RB.empty
  let test_tree_C = insert_all list_C test_tree_B

  let make_rbtree_mem_test =
    [
      make_mem_test false ("anything", 1) RB.empty;
      make_mem_test true ("word", 1) test_tree_A;
      make_mem_test false ("word", 2) test_tree_A;
      make_mem_test false ("ward", 1) test_tree_A;
      make_mem_test true ("wood anemone", 6) test_tree_B;
      make_mem_test true ("wood warbler", 2) test_tree_B;
      make_mem_test false ("wood warbler", 3) test_tree_B;
      make_mem_test false ("wood nynph", 4) test_tree_B;
      make_mem_test true ("wood lot", 1) test_tree_C;
      make_mem_test true ("wood pigeon", 91) test_tree_C;
      make_mem_test false ("wood warbler", 3) test_tree_C;
      make_mem_test false ("wood mouse", 3) test_tree_C;
    ]

  let make_rb_tree_insert_remove_test =
    [
      make_is_empty_test true RB.empty;
      make_is_empty_test false test_tree_A;
      make_insert_test [ ("word", 1) ] RB.empty;
      make_insert_test list_B RB.empty;
      make_insert_test list_B test_tree_A;
      make_remove_test [ ("word", 1) ] test_tree_A;
      (let empty_tree = RB.remove ("word", 1) test_tree_A cmp in
       make_is_empty_test true empty_tree);
      (let non_empty_tree = RB.remove ("word", 2) test_tree_A cmp in
       make_is_empty_test false non_empty_tree);
      make_remove_test [ ("word", 2) ] test_tree_A;
      make_remove_test [ ("word", 1) ] test_tree_B;
      make_remove_test
        [
          ("wood alcohol", 1);
          ("wood anemone", 6);
          ("wood block", 3);
          ("wood ibis", 1);
          ("wood lot", 1);
          ("wood rat", 2);
          ("wood sorrel", 4);
          ("wood warbler", 2);
        ]
        test_tree_B;
      (let empty_tree = remove_all list_B test_tree_B in
       make_is_empty_test true empty_tree);
      make_remove_test
        [
          ("wood lot", 1);
          ("wood pigeon", 91);
          ("wood warbler", 3);
          ("wood mouse", 3);
        ]
        test_tree_C;
      (* [test_tree_C] contains all words from both [list_B] and [list_C] *)
      (let empty_tree = remove_all list_C test_tree_C in
       make_is_empty_test false empty_tree);
      (let empty_tree = remove_all (list_B @ list_C) test_tree_C in
       make_is_empty_test true empty_tree);
    ]
end

module TrieTest = TrieTester (Trie)

(* module NGramTest = NGramTester (NgramColl) *)
module RBTreeTest = RBTreeTester (Tree.Rbtree)

let test_suite =
  "test suite"
  >::: List.flatten
         [
           (* TrieTest.make_tree_tests; *)
           (* NGramTest.make_ngram_test; *)
           RBTreeTest.make_rbtree_mem_test;
           RBTreeTest.make_rb_tree_insert_remove_test;
         ]

let _ = run_test_tt_main test_suite
