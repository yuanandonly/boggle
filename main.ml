(* open Str *)
(* ********************************************************************** *)
type board = {
  dim : int;
  board_letters : string list;
}

let init x = 
  let rand_chr () = 26 
  |> Random.int 
  |> ( + ) 65 
  |> Char.chr 
  |> Char.escaped in
    let rec loop i acc =
      if i = (x * x) then acc 
      else loop (i + 1) (rand_chr () :: acc)
    in
    {dim = x; board_letters = loop 0 []}

(** Requires: t.dim > 0 *)
  let print_board t = 
    (* frame_loop used to generate strings for the frame *)
    let rec frame_loop str i acc = 
      if i = t.dim then acc ^ "|"
      else frame_loop str (i + 1) (acc ^ str) in
    (* letter_loop used to generate strings for lines with letters*)
    let rec letter_loop start i acc =
      if i = t.dim then acc ^ "|"
      else letter_loop start (i + 1) (acc ^ "| " ^ 
          (List.nth t.board_letters (start + i)) ^ " ") in
    let top1 = " " ^ String.make (t.dim * 4 - 1) '_' in
    let top2 = frame_loop "|   " 0 "" in
    let between = frame_loop "|---" 0 "" in
    let bottom = frame_loop "|___" 0 "" in
    let rec print_loop i =
      if i = t.dim then print_endline bottom
      else if i = 0 then let () = print_endline top1 in
                         let () = print_endline top2 in
                         let () = print_endline (letter_loop (i * t.dim) 0 "") in 
                         print_loop (i + 1)
      else let () = print_endline between in
           let () = print_endline (letter_loop (i * t.dim) 0 "") in
            print_loop(i + 1) in
  print_loop 0
(* ********************************************************************** *)
module Trie_func  = struct
  type t = Node of bool * (string * t) list

  let empty () = Node(false, [])

  let extract_st_option (o : (string * t) option) = 
    match o with
    | Some i -> i
    | None -> (" ", empty ())

    let extract_t_option (o : t option) = 
      match o with
      | Some i -> i
      | None -> empty ()

  (* check if a child with a certain word stem exists in the list of children
    and returns an option of that trie [t]. Some [t] if it exists, None else. *)
  let return_child_opt (children : (string * t) list) (stem : string) : 
      t option = 
    let compare_char stem (pair : (string * t)) : bool = (fst pair = stem) in
      let result = List.find_opt (compare_char stem) children in
        if result = None then None else Some (snd (extract_st_option result))

  (* let rec find_node (trie : t) (key : string list) : t option =
    match key with
    | words_containing -> Some trie
    | [] -> None
    | _ -> let result = return_child_opt d key.[0] in
        if result = None then None else 
          find_node (extract_t_option result) 
            (String.sub key 1 (String.length key - 1)) *)

  (* splitting a given string [input] into a list of letters of that word. 
   No such String function does this, String.split_on_char needs a character
        with which to split*)          
  let word_to_list (input : string) : string list =
    let rec iterate ct acc = 
      if ct >= String.length input then acc 
          else iterate (ct + 1) (acc @ [Char.escaped input.[ct]])
    in iterate 0 []



  (* adds a new word to the tire. Takes in current trie and word. 
  returns a new trie with the new word added. If the word node exists in the
  trie but is not marked as a word, then returns a trie with that word marked as
  a word.*)
  let rec trie_insert (trie : t) (key : string list) : t = 
    let Node(is_word, children) = trie in
    match key with
    | [] -> Node(true, children)
    | stem::rest -> let matching_child = extract_t_option 
        (return_child_opt children stem) in
      if matching_child = empty () (* theoretically, every leaf node, i.e. node
       with no children in the trie should have is_word = true, so empty would 
       not match an actual node in the trie *)
      then (* not in children, need to create new child node *)
        let new_child = trie_insert (Node(false, [])) rest in
        Node(is_word, children @ [(stem, new_child)])
      else (* yes in children,return current Node with children edited so 
      that the child with matching stem is changed *)
        (* return list of current children where the stem does not match *)
        let Node(c_is_word, c_children) = matching_child in 
          let non_matching_children = List.filter 
          ((fun (a,b) -> a <> stem)) children in
          let new_child = trie_insert (Node(c_is_word, c_children)) rest in
            Node(is_word, non_matching_children @ [(stem, new_child)])
  
  let trie_instantiate (words : string list) : t = 
    List.fold_left (fun acc (word : string) 
        -> trie_insert acc (word_to_list word)) 
        (empty ()) words 
                

  (* checks if input [key] is an actual word inside of the provided trie. 
    returns boolean*)
  let rec trie_contains_word (trie : t) (key : string list) : bool = 
    let Node(is_word, children) = trie in
    match key with
    | [] -> is_word
    | stem::rest -> let matching_child = extract_t_option 
        (return_child_opt children stem) in
      if matching_child = empty () 
      then (* not child has stem, doesnt contain*)
        false
      else (*child does contain stem, rec call again*)
      trie_contains_word matching_child rest

  (* checks if input [key] is an actual node inside of the provided trie. [key]
    DOES NOT HAVE TO BE A WORD. returns boolean*)      
  let rec trie_contains (trie : t) (key : string list) : bool = 
    let Node(is_word, children) = trie in
    match key with
    | [] -> true
    | stem::rest -> let matching_child = extract_t_option 
        (return_child_opt children stem) in
      if matching_child = empty () 
      then (* not child has stem, doesnt contain*)
        false
      else (*child does contain stem, rec call again*)
        trie_contains matching_child rest

end

(* tests *)

(* module Test = Trie_func;; *)
(* let test_inserting let test = Test.empty ();; *)
(* ********************************************************************** *)


let corpus =  "corpus.txt"
(** Converts [corpus.txt] into a readable string *)
let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
(** Takes in the [corpus] as a string and replaces the \r\n with a space*)
let string_space = Str.global_replace (Str.regexp"[\r\n]") (" ") 
  (read_whole_file corpus)
(** Turns [string_space] into a list seperated by empty space *)
let listed_strings = String.split_on_char (' ') string_space 
(** Filters all empty space from the list *)
let filtered_list = List.filter (fun x -> String.length x > 0) listed_strings

module Trie_module = Trie_func
let word_trie = Trie_module.trie_instantiate filtered_list

let adjacent_tiles = 
  [|
    [1;5;4] ;
    [0;2;6;5;4] ;
    [1;3;5;6;7] ;
    [2;6;7] ;
    [0;1;5;9;8] ;
    [0;1;2;4;6;8;9;10] ;
    [1;2;3;5;7;9;10;11] ;
    [2;3;6;10;11] ;
    [4;5;9;12;13] ;
    [4;5;6;8;10;12;13;14] ;
    [5;6;7;9;11;13;14;15] ;
    [6;7;10;14;15] ;
    [8;9;13] ;
    [8;9;10;12;14] ;
    [9;10;11;13;15] ;
    [10;11;14]
  |]

let rec fold_custom (curr_word : string) (board_loc : int) (input_board : board) 
  (visited : int list) (found : string list) (adj_ind : int list): string list =
  match adj_ind with
  | [] -> found
  | h::t -> if List.mem h visited
    then (* immediately go onto next adj ind*)
      fold_custom curr_word board_loc input_board visited found t
    else (* check if new word is in trie and if word, add*)
      let tile = List.nth input_board.board_letters h in
        let new_word = curr_word ^ tile in
          if Trie_module.trie_contains word_trie 
              (Trie_module.word_to_list new_word)
            then 
              if Trie_module.trie_contains_word word_trie 
                (Trie_module.word_to_list new_word)
                then
                  find_helper new_word h input_board (new_word :: found)
                    (h :: visited)
                else
                  find_helper new_word h input_board found
                    (h :: visited)
            else
              fold_custom curr_word board_loc input_board visited found t


and find_helper (curr_word : string) (board_loc : int) (input_board : board) 
    (found : string list) (visited : int list) : string list = 
  let adjacent_indices = Array.get adjacent_tiles board_loc in
    fold_custom curr_word board_loc input_board visited found adjacent_indices
       
let rec fold_left_ind f (accu : string list) (l : string list) (ind : int) =
  match l with
    [] -> accu
  | a::l -> fold_left_ind f (f accu a ind) l (ind+1)

let find_possible_words (input_board : board) : string list = 
  fold_left_ind (fun accu a ind -> 
      accu @ (find_helper a ind input_board [] [ind])) [] 
      input_board.board_letters 0

let ex_board = {dim = 4; board_letters = 
    ["A";"B";"C";"D";"E";"F";"G";"H";"I";"J";"K";"L";"M";"N";"O";"P"]}