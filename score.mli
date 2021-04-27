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

   val score_helper: int -> string -> int 

   val score_singleplayer: string list -> int 

(* 2. legal boggle game rules following boggle testing
   https://www.fgbradleys.com/rules/Boggle.pdf

   takes in a list of each player's word list, return a corresponding
   list of each player's scores (i.e. player whos list was first in list
   has score at the front of the int list)

   best way to do this is to go through every word of every player and
   make a frequency map of every input word. then for each player go
   thru their word list and if a word has frequency 1 (i.e only that
   player submitted it), then give them credit. *)

   (* freqmap_word_helper adds a word to the frequency map (acc) *)
   val freqmap_word_helper: int StringMap.t -> string
   
   (* freqmap_list_helper adds a list of words to the frequency map (acc) *)
   val freqmap_list_helper: int StringMap.t -> string list -> int StringMap.t 

   val score_boggle: string list list -> int list 

   (* 3. possible scoring on rarity? semioptional
   https://www3.nd.edu/~busiforc/handouts/cryptography/letterfrequencies.html
   look at this, and maybe come up with a system to score a word based
   on the rarities of the word? as in each letter is worth a certain
   value, with J and Z etc worth more than E. Maybe make the scoring
   somehow the related to inverse of the letter frequency *)
   val score_rarity: string list -> int 
   