open OUnit2
include Group_proj.Trie
module TestTrie = Trie

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
      | h :: t ->
          print_endline h;
          if List.mem h actual then cmp t actual else false
    in
    (* Check if all contents in expected are also in the tree containing words
       from [word_list]. *)
    let leaves = search (to_char_list prefix) (insert_all word_lst empty) in
    assert_equal true (cmp expected leaves) ~printer:string_of_bool;
    (* Check if the length of [expected] = number of word leaves in the tree
       containing words from [word_list]. *)
    assert_equal (List.length expected) (List.length leaves)
      ~printer:string_of_int

  (** Test suite that tests [insert]. *)
  let make_tree_tests =
    "Test Suite for [insert], [all_words] and [search]."
    >::: [
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
            let _ =
              make_search_test [ "apple"; "appol"; "appla" ] "app" word_lst
            in
            let _ =
              make_search_test
                (all_words (insert_all word_lst empty))
                "" word_lst
            in
            make_search_test [ "bapple"; "barple" ] "b" word_lst);
         ]

  let _ = run_test_tt_main make_tree_tests
end

module TrieTest = TrieTester (Trie)
