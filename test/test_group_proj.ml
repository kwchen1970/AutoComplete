open OUnit2
open Group_proj
include Tree.Trie
module TestTrie = Trie

let rec print_str_list lst =
  match lst with
  | [] -> ""
  | [ x ] -> x
  | h :: t -> h ^ "; " ^ print_str_list t

let list_equal lst1 lst2 = List.sort compare lst1 = List.sort compare lst2

module TrieTester (T : TRIE) = struct
  include T

  let rec insert_all word_list tree =
    match word_list with
    | [] -> tree
    | h :: t ->
        let new_tree = insert (to_char_list h) tree in
        insert_all t new_tree

  let fold_tree tree = List.fold_left (fun acc elem -> elem ^ " " ^ acc) "" tree

  (* [make_insert_test] *)
  let make_insert_test expected word_lst =
    "" >:: fun _ ->
    let rec cmp expected actual =
      match expected with
      | [] -> true
      | h :: t ->
          print_endline h;
          if List.mem h actual then cmp t actual else false
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
  let make_search_test expected prefix word_lst =
    "" >:: fun _ ->
    let rec cmp expected actual =
      match expected with
      | [] -> true
      | h :: t -> if List.mem h actual then cmp t actual else false
    in
    (* Check if all contents in expected are also in the tree containing words
       from [word_list]. *)
    let leaves = search (to_char_list prefix) (insert_all word_lst empty) in
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

  let _ = print_endline (fold_tree (search (to_char_list "triangle") tree))

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
       let _ = make_insert_test word_lst word_lst in
       let _ = make_search_test [ "apple"; "appol"; "appla" ] "app" word_lst in
       let _ =
         make_search_test (all_words (insert_all word_lst empty)) "" word_lst
       in
       let _ = make_search_test [ "bapple"; "barple" ] "b" word_lst in
       make_search_test [ "triangle" ] "triangl" word_lst);
    ]
end

module NGramTester (C : module type of NgramColl) = struct
  include C

  let add_all coll lst =
    List.fold_left (fun coll x -> C.add_ngram coll (fst x) (snd x)) coll lst

  let make_add_test exp input =
    "" >:: fun _ ->
    let list_of_coll = C.to_str_list (add_all C.empty input) in
    assert_equal ~cmp:list_equal exp list_of_coll ~printer:print_str_list

  let make_ngram_test =
    [
      make_add_test
        [ "AA BB CCC"; "A B CC"; "AAA BB" ]
        [ ("A B CC", "c"); ("AA BB CCC", "aa"); ("AAA BB", "aaa") ];
    ]
end

module TrieTest = TrieTester (Trie)
module NGramTest = NGramTester (NgramColl)

let test_suite =
  "test suite"
  >::: List.flatten [ TrieTest.make_tree_tests; NGramTest.make_ngram_test ]

let _ = run_test_tt_main test_suite
