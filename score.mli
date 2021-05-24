(** Scoring Boggle/Word Hunt games. *)

(* Word Hunt scoring based on length of word: <3: 0 3: 100 4: 400 5: 800
   6+: 400*length - 1000 (i.e. 6 is 1400 and add 400 after for each
   letter)

   Boggle scoring: https://www.fgbradleys.com/rules/Boggle.pdf*)
val wordhunt_scoring_helper : int -> string -> int

(** [wordhunt_scoring_single wlst] is the wordhunt score for a word-list
    [wlst]. *)
val wordhunt_scoring_single : string list -> int

(** [wordhunt_scoring_multi wllst] is a list of wordhunt scores
    corresponding to a list of a word-lists [wllst]. *)
val wordhunt_scoring_multi : string list list -> int list

val boggle_scoring_helper : int -> string -> int

(* initializes map with strings as keys *)
module StringMap : Map.S with type key = String.t

(** boggle_scoring_single lst is the boggle score for a list `lst` of
    player words playing on their own *)
val boggle_scoring_single : string list -> int

(* freqmap_word_helper adds a word to the frequency map (acc) *)
val freqmap_word_helper : int StringMap.t -> string -> int StringMap.t

(* freqmap_list_helper adds a list of words to the frequency map (acc) *)
val freqmap_list_helper :
  int StringMap.t -> string list -> int StringMap.t

(** boggle_scoring_multi word_lsts is a list of integers representing
    the boggle scores corresponding to each player's found words in
    string list list word_lsts *)
val boggle_scoring_multi : string list list -> int list
