open OUnit2
open Main
open Board
open Trie
open Trie_func
open Words
open Score

(********************************************************************
   Here are some helper functions for your testing of set-like lists. 
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(********************************************************************
   Words Helper Functions.
 ********************************************************************)
let find_possible_words_test
    (name : string)
    (input_board : board)
    (ans : string list) =
  name >:: fun _ -> assert_equal (find_possible_words input_board) ans

let board_1 : board =
  {
    dim = 3;
    board_letters =
      [
        "C";
        "A";
        "B";
        "Z";
        "Z";
        "Z";
        "Z";
        "Z";
        "Z";
        "Z";
        "Z";
        "Z";
        "Z";
        "Z";
        "Z";
        "Z";
      ];
  }

let board_2 : board =
  {
    dim = 4;
    board_letters =
      [
        "A";
        "B";
        "C";
        "D";
        "E";
        "F";
        "G";
        "H";
        "I";
        "J";
        "K";
        "L";
        "M";
        "N";
        "O";
        "P";
      ];
  }

let board_3 : board =
  {
    dim = 4;
    board_letters =
      [
        "A";
        "O";
        "E";
        "I";
        "O";
        "U";
        "A";
        "E";
        "I";
        "O";
        "U";
        "A";
        "E";
        "I";
        "O";
        "U";
      ];
  }

let board_4 : board =
  {
    dim = 4;
    board_letters =
      [
        "B";
        "C";
        "D";
        "F";
        "G";
        "H";
        "J";
        "K";
        "L";
        "M";
        "N";
        "P";
        "Q";
        "R";
        "S";
        "T";
      ];
  }

(********************************************************************
  Trie Helper Functions
 ********************************************************************)
let trie_contains_test
    (name : string)
    (trie : t)
    (key : string list)
    (ans : bool) =
  name >:: fun _ -> assert_equal (trie_contains trie key) ans

let trie_contains_word_test
    (name : string)
    (trie : t)
    (key : string list)
    (ans : bool) =
  name >:: fun _ -> assert_equal (trie_contains_word trie key) ans

let trie_insert_test
    (name : string)
    (trie : t)
    (key : string list)
    (ans : t) =
  name >:: fun _ -> assert_equal (trie_insert trie key) ans

let trie_instantiate_test
    (name : string)
    (words : string list)
    (ans : t) =
  name >:: fun _ -> assert_equal (trie_instantiate words) ans

let string_list_1 = [ "RICHARD"; "EMILY" ]

let string_list_2 = [ "JACK" ]

let string_list_3 = [ "CHRIS" ]

(* Not proper implementation of TRIE yet*)
let empty_trie = empty ()

let trie_1 =
  let insert_1 = trie_insert empty_trie string_list_1 in
  let insert_2 = trie_insert insert_1 string_list_2 in
  trie_insert insert_2 string_list_3

let trie_2 = trie_insert empty_trie string_list_3

(********************************************************************
   Board Helper Functions
 ********************************************************************)

let adj_table_test (name : string) (x : int) (ans : int list array) =
  name >:: fun _ -> assert_equal (adj_table x) ans

let adj_table_test (name : string) (x : int) (ans : int list array) =
  name >:: fun _ -> assert_equal (adj_table x) ans

(********************************************************************
   Score Helper Functions 
 ********************************************************************)
let wordhunt_scoring_single_test
    (name : string)
    (words : string list)
    (ans : int) =
  name >:: fun _ -> assert_equal (wordhunt_scoring_single words) ans

let wordhunt_scoring_multi_test
    (name : string)
    (word_sets : string list list)
    (ans : int list) =
  name >:: fun _ -> assert_equal (wordhunt_scoring_multi word_sets) ans

let boggle_scoring_single_test
    (name : string)
    (words : string list)
    (ans : int) =
  name >:: fun _ -> assert_equal (boggle_scoring_single words) ans

let boggle_scoring_multi_test
    (name : string)
    (word_sets : string list list)
    (ans : int list) =
  name >:: fun _ -> assert_equal (boggle_scoring_multi word_sets) ans

let score_rarity_test = "Unimplemented, semioptional"

let word_single_list_empty = []

let word_single_list_1 = [ "APPLE"; "HEY"; "WOW"; "INTERESTING" ]

let word_single_list_2 = [ "AMAZING" ]

let word_multi_list_empty = [ []; []; [] ]

let word_multi_list_1 = [ word_single_list_1; word_single_list_2 ]

(********************************************************************
       End helper functions.
 ********************************************************************)

let words_tests =
  [
    find_possible_words_test
      "Test a simple board with only one possible word" board_1
      (find_possible_words board_1);
    find_possible_words_test "Test a random board in alphabetical order"
      board_2
      (find_possible_words board_2);
    find_possible_words_test "Test a board with only vowels" board_3
      (find_possible_words board_3);
    find_possible_words_test "Test a board with only consonants" board_4
      (find_possible_words board_4);
  ]

let board_tests = []

let trie_tests = []

let score_tests =
  [
    wordhunt_scoring_single_test
      "Test if the score of EMPTY single word list using wordhunt \
       method is correct"
      word_single_list_empty 0;
    wordhunt_scoring_single_test
      "Test if the score of a single word list using wordhunt method \
       is correct"
      word_single_list_1 4400;
    wordhunt_scoring_multi_test
      "Test if the score of an EMPTY multi word list using wordhunt \
       method is correct"
      word_multi_list_empty [ 0; 0; 0 ];
    wordhunt_scoring_multi_test
      "Test if the score of an multi word list using wordhunt method \
       is correct"
      word_multi_list_1 [ 4400; 1800 ];
    boggle_scoring_single_test
      "Test if the score an EMPTY single word list using boggle method \
       is correct "
      word_single_list_empty 0;
    boggle_scoring_single_test
      "Test if the score of a single word list using boggle method is \
       correct"
      word_single_list_1 15;
    boggle_scoring_multi_test
      "Test if the score of an EMPTY multi word list using boggle \
       method is correct"
      word_multi_list_empty [ 0; 0; 0 ];
    boggle_scoring_multi_test
      "Test if the score of an multi word list using boggle method is \
       correct"
      word_multi_list_1 [ 15; 5 ];
  ]

let suite =
  "search test suite"
  >::: List.flatten
         [ words_tests; board_tests; trie_tests; score_tests ]

let _ = run_test_tt_main suite
