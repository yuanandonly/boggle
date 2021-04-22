
(* module type Trie_sig = sig
  type t
end

module Trie = struct
  type t = { 
    value: string; 
    is_word: bool;
    words_containing: int;
    leaves: (string * t) list
  }
end *)

module Trie_func = struct
  (* the type of trie node: a pair of a boolean representing whether the node
  in the trie represents a word, and a list of pairs. Each element is a list is
  a pair of a string of a character and the child node that represents the new
  word formed with that character. *)
  type t = Node of bool * (string * t) list

  (* empty trie *)
  let empty () = Node(false, [])

  (* extracts the data type [string * t] from the option, returns empty pair
    if the option is none *)
  let extract_st_option (o : (string * t) option) : (string * t) = 
    match o with
    | Some i -> i
    | None -> (" ", empty ())

  (* extracts the data type [t] from the option, returns empty trie
    if the option is none *) 
  let extract_t_option (o : t option) : t = 
    match o with
    | Some i -> i
    | None -> empty ()

  (* splitting a given string [input] into a list of letters of that word. 
   No such String function does this, String.split_on_char needs a character
        with which to split*)          
  let word_to_list (input : string) : string list =
    let rec iterate ct acc = 
      if ct >= String.length input then acc 
          else iterate (ct + 1) (acc @ [Char.escaped input.[ct]])
    in iterate 0 []
      
  (* check if a child with a certain character stem exists in the list of 
    children of type (string * t) list, and returns an option of that trie. *)
  let return_child_opt (children : (string * t) list) (stem : string) : 
      t option = 
    let compare_char stem (pair : (string * t)) : bool = (fst pair = stem) in
      let result = List.find_opt (compare_char stem) children in
        if result = None then None else Some (snd (extract_st_option result))

  (* adds a new word to the trie. Takes in current trie and word. 
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

  (* checks if input [key] is an actual word inside of the provided trie. 
    returns boolean *)
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
    does not have to be a word! returns boolean *)      
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
  
  (* creates a new trie and populates it with everything in the string list *)
  let trie_instantiate (words : string list) : t = 
    List.fold_left (fun acc (word : string) 
        -> trie_insert acc (word_to_list word)) 
        (empty ()) words       

end

(* tests *)

(* module Test = Trie_func;; *)
(* let test_inserting let test = Test.empty ();; *)


