type t = Node of bool * (string * t) list

let empty () = Node (false, [])

let extract_st_option (o : (string * t) option) : string * t =
  match o with Some i -> i | None -> (" ", empty ())

let extract_t_option (o : t option) : t =
  match o with Some i -> i | None -> empty ()

let word_to_list (input : string) : string list =
  let rec iterate ct acc =
    if ct >= String.length input then acc
    else iterate (ct + 1) (acc @ [ Char.escaped input.[ct] ])
  in
  iterate 0 []

let children (trie : t) : (string * t) list =
  match trie with Node (is_word, children) -> children

let return_child_opt (children : (string * t) list) (stem : string) :
    t option =
  let compare_char stem (pair : string * t) : bool = fst pair = stem in
  let result = List.find_opt (compare_char stem) children in
  if result = None then None else Some (snd (extract_st_option result))

let rec trie_insert (trie : t) (key : string list) : t =
  let (Node (is_word, children)) = trie in
  match key with
  | [] -> Node (true, children)
  | stem :: rest ->
      let matching_child =
        extract_t_option (return_child_opt children stem)
      in
      if matching_child = empty () then
        let new_child = trie_insert (Node (false, [])) rest in
        Node (is_word, children @ [ (stem, new_child) ])
      else
        let (Node (c_is_word, c_children)) = matching_child in
        let non_matching_children =
          List.filter (fun (a, b) -> a <> stem) children
        in
        let new_child =
          trie_insert (Node (c_is_word, c_children)) rest
        in
        Node (is_word, non_matching_children @ [ (stem, new_child) ])

let rec trie_contains_word (trie : t) (key : string list) : bool =
  let (Node (is_word, children)) = trie in
  match key with
  | [] -> is_word
  | stem :: rest ->
      let matching_child =
        extract_t_option (return_child_opt children stem)
      in
      if matching_child = empty () then false
      else trie_contains_word matching_child rest

let rec trie_contains (trie : t) (key : string list) : bool =
  let (Node (is_word, children)) = trie in
  match key with
  | [] -> true
  | stem :: rest ->
      let matching_child =
        extract_t_option (return_child_opt children stem)
      in
      if matching_child = empty () then false
      else trie_contains matching_child rest

let trie_instantiate (words : string list) : t =
  List.fold_left
    (fun acc (word : string) -> trie_insert acc (word_to_list word))
    (empty ()) words
