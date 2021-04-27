(* SCORING: 1. single player word hunt testing 2. boggle testing
   https://www.fgbradleys.com/rules/Boggle.pdf 3. possible scoring on
   rarity? extra
   >>https://www3.nd.edu/~busiforc/handouts/cryptography/letterfrequencies.html
   look at this, and maybe come up with a system to score a word based
   on the rarities of the word? as in each letter is worth a certain
   value, with J and Z etc worth more than E. Maybe make the scoring
   somehow the related to inverse of the letter frequency*)

(* 1. single player word hunt testing, returns int score scoring scheme,
   based on word length: <3: 0 3: 100 4: 400 5: 800 6+: 400*length -
   1000 (i.e. 6 is 1400 and add 400 after for each letter)*)

let score_helper (acc : int) (word : string) : int =
  acc
  +
  match String.length word with
  | 3 -> 100
  | 4 -> 400
  | 5 -> 800
  | x -> if x < 3 then 0 else (x * 400) - 1000

let score_singleplayer (words : string list) : int =
  List.fold_left score_helper 0 words

(* 2. legal boggle game rules following boggle testing
   https://www.fgbradleys.com/rules/Boggle.pdf

   takes in a list of each player's word list, return a corresponding
   list of each player's scores (i.e. player whos list was first in list
   has score at the front of the int list)

   best way to do this is to go through every word of every player and
   make a frequency map of every input word. then for each player go
   thru their word list and if a word has frequency 1 (i.e only that
   player submitted it), then give them credit. *)

module StringMap = Map.Make (String)

(* freqmap_word_helper adds a word to the frequency map (acc) *)
let freqmap_word_helper (acc : int StringMap.t) (word : string) :
    int StringMap.t =
  match StringMap.find_opt word acc with
  | None -> StringMap.add word 1 acc
  | Some x -> StringMap.add word (1 + x) acc

(* freqmap_list_helper adds a list of words to the frequency map (acc) *)
let freqmap_list_helper
    (acc : int StringMap.t)
    (player_word_list : string list) : int StringMap.t =
  List.fold_left freqmap_word_helper acc player_word_list

let score_boggle (word_sets : string list list) : int list =
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
    | 1 -> score_helper 0 word
    | _ -> 0
  in
  (* comparer uses comparer_helper on a list of words, returning the
     accumulated score *)
  let comparer (player_word_list : string list) : int =
    List.fold_left comparer_helper 0 player_word_list
  in
  (* finally, using comparer to map player word lists to integer scores*)
  List.map comparer word_sets

(* 3. possible scoring on rarity? semioptional
   https://www3.nd.edu/~busiforc/handouts/cryptography/letterfrequencies.html
   look at this, and maybe come up with a system to score a word based
   on the rarities of the word? as in each letter is worth a certain
   value, with J and Z etc worth more than E. Maybe make the scoring
   somehow the related to inverse of the letter frequency *)
let score_rarity (words : string list) : int =
  failwith "Unimplemented, semioptional"
