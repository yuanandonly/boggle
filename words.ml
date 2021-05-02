open Board
open Trie
open Str
open Trie_func

let read_whole_file (filename : string) =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let string_space str =
  Str.global_replace (Str.regexp "[\r\n]") " " (read_whole_file str)

let listed_strings str = String.split_on_char ' ' (string_space str)

let filtered_list =
  List.filter
    (fun x -> String.length x > 0)
    (listed_strings "corpus.txt")

let word_trie = trie_instantiate filtered_list

let adjacent_tiles_4 =
  [|
    [ 1; 5; 4 ];
    [ 0; 2; 6; 5; 4 ];
    [ 1; 3; 5; 6; 7 ];
    [ 2; 6; 7 ];
    [ 0; 1; 5; 9; 8 ];
    [ 0; 1; 2; 4; 6; 8; 9; 10 ];
    [ 1; 2; 3; 5; 7; 9; 10; 11 ];
    [ 2; 3; 6; 10; 11 ];
    [ 4; 5; 9; 12; 13 ];
    [ 4; 5; 6; 8; 10; 12; 13; 14 ];
    [ 5; 6; 7; 9; 11; 13; 14; 15 ];
    [ 6; 7; 10; 14; 15 ];
    [ 8; 9; 13 ];
    [ 8; 9; 10; 12; 14 ];
    [ 9; 10; 11; 13; 15 ];
    [ 10; 11; 14 ];
  |]

let rec fold_custom
    (curr_word : string)
    (board_loc : int)
    (input_board : board)
    (visited : int list)
    (found : string list)
    (adj_arr : int list array)
    (adj_ind : int list) 
     : string list =
  match adj_ind with
  | [] -> found
  | h :: t ->
      if List.mem h visited then
        (* no new words, immediately go onto next adj ind*)
        fold_custom curr_word board_loc input_board visited found adj_arr t
      else
        (* check if new word is in trie and if word, add*)
        let tile = List.nth input_board.board_letters h in
        let new_word = curr_word ^ tile in
        if trie_contains word_trie (word_to_list new_word) then
          if trie_contains_word word_trie (word_to_list new_word) then
            find_helper new_word h input_board (new_word :: found)
              (h :: visited) adj_arr
            @ fold_custom curr_word board_loc input_board visited found
              adj_arr t
          else
            find_helper new_word h input_board found (h :: visited) adj_arr
            @ fold_custom curr_word board_loc input_board visited found
              adj_arr t
        else fold_custom curr_word board_loc input_board visited found adj_arr t

and find_helper
    (curr_word : string)
    (board_loc : int)
    (input_board : board)
    (found : string list)
    (visited : int list) 
    (adj_arr : int list array) : string list =
  let adjacent_indices = Array.get adj_arr board_loc in
  fold_custom curr_word board_loc input_board visited found
    adj_arr adjacent_indices 

let rec fold_left_ind
    (f : string list -> string -> int -> string list)
    (accu : string list)
    (l : string list)
    (ind : int) : string list =
  match l with
  | [] -> accu
  | a :: l -> fold_left_ind f (f accu a ind) l (ind + 1)

let find_possible_words (input_board : board) : string list =
  let adjacencies = adj_table input_board.dim in
  let with_duplicates =
    fold_left_ind
      (fun accu a ind ->
        accu @ find_helper a ind input_board [] [ ind ] adjacencies)
      [] input_board.board_letters 0
  in
  List.sort_uniq compare with_duplicates

(* TODO lookup table for letters <=> indices of board instead of passing *)

let ex_board : board =
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
