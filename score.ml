let wordhunt_scoring_helper (acc : int) (word : string) : int =
  acc
  +
  match String.length word with
  | 3 -> 100
  | 4 -> 400
  | 5 -> 800
  | x -> if x < 3 then 0 else (x * 400) - 1000

let wordhunt_scoring_single (words : string list) : int =
  List.fold_left wordhunt_scoring_helper 0 words

let wordhunt_scoring_multi (word_sets : string list list) : int list =
  List.map wordhunt_scoring_single word_sets

let boggle_scoring_helper (acc : int) (word : string) : int =
  acc
  +
  match String.length word with
  | 3 -> 1
  | 4 -> 1
  | 5 -> 2
  | 6 -> 3
  | 7 -> 5
  | x -> if x < 3 then 0 else 11

module StringMap = Map.Make (String)

let boggle_scoring_single (words : string list) : int =
  List.fold_left boggle_scoring_helper 0 words

let freqmap_word_helper (acc : int StringMap.t) (word : string) :
    int StringMap.t =
  match StringMap.find_opt word acc with
  | None -> StringMap.add word 1 acc
  | Some x -> StringMap.add word (1 + x) acc

let freqmap_list_helper
    (acc : int StringMap.t)
    (player_word_list : string list) : int StringMap.t =
  List.fold_left freqmap_word_helper acc player_word_list

let boggle_scoring_multi (word_sets : string list list) : int list =
  (* create frequency map*)
  let freqmap =
    List.fold_left freqmap_list_helper StringMap.empty word_sets
  in
  (* comparer_helper adds value of word into acc, after checking against
     freqmap *)
  let comparer_helper (acc : int) (word : string) =
    acc
    +
    match StringMap.find word freqmap with
    | 1 -> boggle_scoring_helper 0 word
    | _ -> 0
  in
  (* comparer uses comparer_helper on a list of words, returning the
     accumulated score *)
  let comparer (player_word_list : string list) : int =
    List.fold_left comparer_helper 0 player_word_list
  in
  (* finally, using comparer to map player word lists to integer scores*)
  List.map comparer word_sets

let score_rarity (words : string list) : int =
  failwith "Unimplemented, semioptional"
