open OUnit2
open Board
open Trie
open Words
open Score
open Ai_player

(* For our testing approach, we tried our best to implement Test Driven
   Development throughout our coding cycle. Compared to other labs, this
   final project of creating a well polished Boggle game was a much
   bigger challenge than anything else; it is a machine with much more
   moving pieces and “cogs” than what we’ve worked with previously. That
   is, because we are building our project from the ground up instead of
   simply working on a couple components of a large system in a lab, we
   cannot assume any module automatically works and has been thoroughly
   tested by staff. Thus, we took extra effort to anticipate edge cases
   for methods as we wrote them. It was important that we tested as we
   coded each method and module, lest there be cascading errors that
   carry through our system.

   In our testing suite, we tested all the modules that were able to be
   tested through our OUnit Suite. We used a glass box approach for
   every function that didn’t involve a randomly generated item and
   opted for black box testing for random functions like the generation
   of a random boggle board. Helper functions were not tested. User
   experience in terminal was tested manually, as were most of functions
   in files like main.ml.

   We combined our OUnit testing with user testing of the game itself as
   we added features with each sprint. For instance, we’d input an
   invalid number (negative seconds, for instance) to see if the game
   would respond accordingly by prompting for a new input. We also asked
   friends not in the class to play the game to ensure it was
   user-friendly for non-coders. With our comprehensive testing suite
   and a great deal of hands-on testing, we believe that our testing
   approach demonstrates the correctness of our system. *)

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
  name >:: fun _ ->
  assert_equal
    (cmp_set_like_lists (find_possible_words input_board) ans)
    true

let validate_words_test
    (name : string)
    (word_list : string list)
    (possible_words : string list)
    (ans : string list) =
  name >:: fun _ ->
  assert_equal (validate_words word_list possible_words) ans

let word_list_empty = []

let word_list_1 = [ "CHRIS"; "JACK"; "EMILY"; "RICHARD" ]

let word_list_2 = [ "chris"; "jack"; "emily"; "richard" ]

let word_list_3 = [ "Chris"; "Jack"; "Emily"; "Richard" ]

let word_list_4 = [ "RICHARD"; "JACK"; "EMILY" ]

let possible_words_list = [ "CHRIS"; "RICHARD" ]

let board_1 : board =
  {
    dim = 3;
    board_letters = [ "B"; "Z"; "Z"; "Z"; "Z"; "Z"; "Z"; "Z"; "Z" ];
  }

let ans_1 = [ "ZZZ" ]

let board_3 : board =
  {
    dim = 3;
    board_letters = [ "A"; "O"; "E"; "I"; "O"; "U"; "A"; "E"; "I" ];
  }

let ans_3 =
  [ "AE"; "AI"; "AIA"; "EA"; "EUOI"; "IO"; "OE"; "OI"; "OO"; "OU" ]

let board_4 : board =
  {
    dim = 3;
    board_letters = [ "B"; "C"; "D"; "F"; "G"; "Z"; "J"; "K"; "L" ];
  }

let board_5 : board =
  {
    dim = 3;
    board_letters = [ "A"; "+"; "D"; "A"; "+"; "D"; "A"; "+"; "D" ];
  }

let ans_5 = [ "AA" ]

let board_6 : board =
  {
    dim = 3;
    board_letters = [ "C"; "+"; "D"; "+"; "A"; "+"; "+"; "+"; "B" ];
  }

let ans_6 =
  [ "AB"; "AD"; "BA"; "BAC"; "BAD"; "CAB"; "CAD"; "DA"; "DAB" ]

let tony's_board =
  {
    dim = 4;
    board_letters =
      [
        "T";
        "O";
        "N";
        "Y";
        "T";
        "O";
        "N";
        "Y";
        "T";
        "O";
        "N";
        "Y";
        "T";
        "O";
        "N";
        "Y";
      ];
  }

let tony's_board_ans =
  [
    "NO";
    "NON";
    "NONNY";
    "NOO";
    "NOON";
    "NOT";
    "NOTT";
    "NY";
    "ON";
    "ONO";
    "ONY";
    "OO";
    "OON";
    "OOT";
    "OTTO";
    "TO";
    "TON";
    "TONY";
    "TOO";
    "TOON";
    "TOOT";
    "TOT";
  ]

(********************************************************************
  Trie Helper Functions
 ********************************************************************)
let extract_st_option_test
    (name : string)
    (o : (string * t) option)
    (ans : string * t) =
  name >:: fun _ -> assert_equal (extract_st_option o) ans

let extract_t_option_test (name : string) (o : t option) (ans : t) =
  name >:: fun _ -> assert_equal (extract_t_option o) ans

let word_to_list_test
    (name : string)
    (trie : string)
    (ans : string list) =
  name >:: fun _ -> assert_equal (word_to_list trie) ans

let return_child_opt_test
    (name : string)
    (children : (string * t) list)
    (stem : string)
    (ans : t option) =
  name >:: fun _ -> assert_equal (return_child_opt children stem) ans

let trie_insert_test
    (name : string)
    (trie : t)
    (key : string list)
    (ans : t) =
  name >:: fun _ -> assert_equal (trie_insert trie key) ans

let trie_contains_word_test
    (name : string)
    (trie : t)
    (key : string list)
    (ans : bool) =
  name >:: fun _ -> assert_equal (trie_contains_word trie key) ans

let trie_contains_test
    (name : string)
    (trie : t)
    (key : string list)
    (ans : bool) =
  name >:: fun _ -> assert_equal (trie_contains trie key) ans

let trie_instantiate_test
    (name : string)
    (words : string list)
    (ans : t) =
  name >:: fun _ -> assert_equal (trie_instantiate words) ans

let new_word = "NEW"

let string_list_1 = [ "RICHARD"; "EMILY"; "JACK" ]

let string_list_2 = [ "JACK"; "CHRIS" ]

let empty_trie = empty ()

let trie_1 = trie_instantiate string_list_1

let trie_2 = trie_instantiate string_list_2

let trie_expand =
  Node
    ( false,
      [
        ( "N",
          Node
            (false, [ ("E", Node (false, [ ("W", Node (true, [])) ])) ])
        );
      ] )

let trie_2_expand =
  Node
    ( false,
      [
        ( "J",
          Node
            ( false,
              [
                ( "A",
                  Node
                    ( false,
                      [
                        ("C", Node (false, [ ("K", Node (true, [])) ]));
                      ] ) );
              ] ) );
        ( "C",
          Node
            ( false,
              [
                ( "H",
                  Node
                    ( false,
                      [
                        ( "R",
                          Node
                            ( false,
                              [
                                ( "I",
                                  Node
                                    (false, [ ("S", Node (true, [])) ])
                                );
                              ] ) );
                      ] ) );
              ] ) );
      ] )

let child_2 =
  Some
    (Node
       ( false,
         [
           ( "H",
             Node
               ( false,
                 [
                   ( "R",
                     Node
                       ( false,
                         [
                           ( "I",
                             Node (false, [ ("S", Node (true, [])) ]) );
                         ] ) );
                 ] ) );
         ] ))

(********************************************************************
   Board Helper Functions
 ********************************************************************)

let form_list_test
    (name : string)
    (start : int)
    (tail : int)
    (ans : int list) =
  name >:: fun _ -> assert_equal (start +++ tail) ans

let adj_check_test
    (name : string)
    (ind : int)
    (comp_ind : int)
    (x : int)
    (direc : (int * int) list)
    (ans : bool) =
  name >:: fun _ -> assert_equal (adj_check ind comp_ind x direc) ans

let grab_board_length (b : board) = List.length b.board_letters

let init_classic_test
    (name : string)
    (dim : int)
    (corr_dim : int)
    (corr_letters : int) =
  name >:: fun _ ->
  assert_equal
    ((init_classic dim).dim == corr_dim
    && corr_letters == grab_board_length (init_classic dim))
    true

let rand_init_test
    (name : string)
    (size : int)
    (corr_dim : int)
    (corr_letters : int) =
  name >:: fun _ ->
  assert_equal
    ((rand_init size).dim == corr_dim
    && corr_letters == grab_board_length (rand_init size))
    true

let adj_table_test (name : string) (x : int) (ans : int list array) =
  name >:: fun _ -> assert_equal (adj_table x) ans

let dim2_adj_table = [| [ 1; 2; 3 ]; [ 3 ]; [ 0; 1; 3 ]; [ 0; 1 ] |]

let dim3_adj_table =
  [|
    [ 1; 3; 4 ];
    [ 0; 2; 3; 4; 5 ];
    [ 1; 4; 5 ];
    [ 0; 1; 4; 6; 7 ];
    [ 0; 1; 2; 3; 5; 6; 7; 8 ];
    [ 1; 2; 4; 7; 8 ];
    [ 3; 4; 7 ];
    [ 3; 4; 5; 6; 8 ];
    [ 4; 5; 7 ];
  |]

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

let word_single_list_3 = [ "APPLE"; "HEY" ]

let word_single_list_4 = [ "apple"; "hey"; "wow"; "interesting" ]

let word_multi_list_empty = [ []; []; [] ]

let word_multi_list_1 = [ word_single_list_1; word_single_list_2 ]

let word_multi_list_2 = [ word_single_list_1; word_single_list_3 ]

(********************************************************************
   AI-Players Helper Functions.
 ********************************************************************)
let length_filter_test
    (name : string)
    (lower_bound : int)
    (upper_bound : int)
    (lst : string list)
    (ans : string list) =
  name >:: fun _ ->
  assert_equal (length_filter lower_bound upper_bound lst) ans

let random_select_filter_test
    (name : string)
    (percent : float)
    (lst : string list)
    (ans : string list) =
  name >:: fun _ -> assert_equal (random_select_filter percent lst) ans

let diff_to_name_test (name : string) (diff : int) (ans : string) =
  name >:: fun _ -> assert_equal (diff_to_name diff) ans

let length_list_1 =
  [ "HELLOOO"; "EMILY"; "RICHARD"; "CHRISTOPH"; "JACK"; "HUR" ]

let length_list_2 = [ "H"; "H"; "H"; "H"; "H"; "H"; "H"; "H" ]

let length_filter_list_1 = [ "EMILY"; "JACK"; "HUR" ]

let length_filter_list_2 = [ "HUR" ]

let length_filter_list_3 = [ "H"; "H"; "H"; "H" ]

let length_filter_list_4 = [ "H"; "H" ]

(********************************************************************
       End helper functions.
 ********************************************************************)

let words_tests =
  [
    find_possible_words_test
      "Test a board with the same letters except one letter with only \
       one possible word"
      board_1 ans_1;
    find_possible_words_test "Test a board with only vowels" board_3
      ans_3;
    find_possible_words_test
      "Test a board with only consonants. Should return no possible \
       words."
      board_4 [];
    find_possible_words_test
      "Ensures adjacency is working and makes sure no words overflow. \
       Should return one possible word."
      board_5 ans_5;
    find_possible_words_test
      "Tests a board with only possible words along the diagonal"
      board_6 ans_6;
    find_possible_words_test "Testing a 4x4 TONY BOARD" tony's_board
      tony's_board_ans;
    validate_words_test
      "Tests an empty word list against the list of possible words.  \
       Should return no valid word list"
      word_list_empty possible_words_list [];
    validate_words_test
      "Tests an full uppercase word list against the list of possible \
       words. Should return the words in possible word list"
      word_list_1 possible_words_list [ "CHRIS"; "RICHARD" ];
    validate_words_test
      "Tests an full lowercase word list against the list of possible \
       words. Should return the words in possible word list"
      word_list_2 possible_words_list [ "chris"; "richard" ];
    validate_words_test
      "Tests an mix of lower and uppercase word list against the list \
       of possible words. Should return the words in possible word \
       list"
      word_list_3 possible_words_list [ "Chris"; "Richard" ];
    validate_words_test
      "Tests a word list that has some words contained in the possible \
       word list against the list of possible words. Should return the \
       words in possible word list"
      word_list_4 possible_words_list [ "RICHARD" ];
  ]

let board_tests =
  [
    form_list_test "Forms an int list starting from 0 to 10" 0 10
      [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ];
    form_list_test "Forms an empty list if tail is smaller than start" 5
      3 [];
    form_list_test
      "Forms an list with just start if tail is equal to start" 5 5
      [ 5 ];
    adj_table_test
      " Checks if the correct adjacency table of dimension 2 is \
       produced."
      2 dim2_adj_table;
    adj_table_test
      " Checks if the correct adjacency tbale of dimension 3 is \
       produced"
      3 dim3_adj_table;
    init_classic_test
      "Checks if an initialized dimension 10 board produced the \
       correct dimensions and number of board letters"
      5 5 25;
    init_classic_test
      "Checks if an initialized dimension 4 board produced the correct \
       dimensions and number of board letters"
      4 4 16;
    rand_init_test
      "Checks if an random initialized dimension 10 board produced the \
       correct dimensions and number of board letters"
      10 10 100;
    rand_init_test
      "Checks if an random initialized dimension 10 board produced the \
       correct dimensions and number of board letters"
      8 8 64;
  ]

let trie_tests =
  [
    extract_st_option_test
      "Test if it extracts the right data type [string * t] given an \
       Some option"
      (Some ("H", trie_2))
      ("H", trie_2);
    extract_st_option_test
      "Test if it extracts the right data type [string * t] given an \
       None option"
      None (" ", empty_trie);
    extract_t_option_test
      "Test if it extracts the right data type t given an Some option"
      (Some trie_1) trie_1;
    extract_t_option_test
      "Test if it extracts the right data type t given an Some option"
      None empty_trie;
    word_to_list_test
      "Test if the string \"HELLO\" breaks into the proper string list"
      "HELLO"
      [ "H"; "E"; "L"; "L"; "O" ];
    return_child_opt_test
      "Test if a child with a certain character stem exists in the \
       list of children of type (string,t). Should return None with no \
       children."
      (children trie_2) "H" None;
    return_child_opt_test
      "Test if a child with a certain character stem exists in the \
       list of children of type (string,t). Should return Some with \
       children children."
      (children trie_2) "C" child_2;
    trie_contains_word_test
      "Test if the input is an actual word inside of the emtpy trie. \
       Should return false"
      empty_trie (word_to_list "HI") false;
    trie_contains_word_test
      "Test if the input is an actual word inside of the provided \
       non-emptytrie. Should return true"
      trie_1
      (word_to_list "RICHARD")
      true;
    trie_contains_word_test
      "Test if the input is an actual word inside of the provided \
       non_empty trie. Should return false"
      trie_2
      (word_to_list "RICHARD")
      false;
    trie_contains_word_test
      "Test if the input is an actual word inside of the provided \
       non_empty trie. Node inside tree but is not a word. Should \
       return false"
      trie_2 (word_to_list "RIC") false;
    trie_contains_test
      "Test if the input is an actual node inside of the emtpy trie. \
       The key doesn't have to be a word. Should return false"
      empty_trie (word_to_list "HELLO") false;
    trie_contains_test
      "Test if the input is an actual node inside of the provided trie \
       non empty trie. The trie contains the key but it is not a word. \
       Should return true"
      trie_1 (word_to_list "RIC") true;
    trie_instantiate_test
      "Test if the instantiation of a word list is correct."
      string_list_2 trie_2_expand;
    trie_insert_test "Test if insert of a new word is correct."
      empty_trie (word_to_list new_word) trie_expand;
  ]

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
    boggle_scoring_single_test
      "Test if the score of a single word list written in lower case \
       using boggle method is correct"
      word_single_list_4 15;
    boggle_scoring_multi_test
      "Test if the score of an EMPTY multi word list using boggle \
       method is correct"
      word_multi_list_empty [ 0; 0; 0 ];
    boggle_scoring_multi_test
      "Test if the score of an multi word list using boggle method is \
       correct"
      word_multi_list_1 [ 15; 5 ];
    boggle_scoring_multi_test
      "Test if the score of an multi word list using boggle method is \
       correct when people have the same words. Niether player gets \
       points for same words"
      word_multi_list_2 [ 12; 0 ];
  ]

let ai_player_tests =
  [
    length_filter_test
      "Tests if bounding the string list to 3 to 5 characters returns \
       the correct output"
      3 5 length_list_1 length_filter_list_1;
    length_filter_test
      "Tests if bounding the string list to 0 to 10 characters returns \
       the correct output"
      0 10 length_list_1 length_list_1;
    length_filter_test
      "Test if boudning the string list to only 3 characters return \
       the correct output"
      3 3 length_list_1 length_filter_list_2;
    random_select_filter_test
      "Tests if filtering half of a list returns exactly half the list"
      0.5 length_list_2 length_filter_list_3;
    random_select_filter_test
      "Tests if filtering half of a list returns exactly half the list"
      0.25 length_list_2 length_filter_list_4;
    diff_to_name_test "Outputs correct difficulty level" 5
      "COMPUTER (LEVEL : IMPOSSIBLE)";
  ]

let suite =
  "search test suite"
  >::: List.flatten
         [
           words_tests;
           board_tests;
           trie_tests;
           score_tests;
           ai_player_tests;
         ]

let _ = run_test_tt_main suite
