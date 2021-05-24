(** Implements an AI player to compete against. *)

(* given chosen difficulty, the computer will randomly choose a subset
   of the possible letters on the board EASY (0) : 5% of words length
   3-4 NOVICE (1) : 10% of words length 3-5 MEDIUM (2) : 15% of words
   length 3-6 HARD (3) : 20% of all words INSANE (4) : 30% of all words
   IMPOSSIBLE (5) : 60% of all words) *)

(** given a string list, filters out all the strings that are not within
    the [lower_bound] and [upper_bound] *)
val length_filter : int -> int -> string list -> string list

(** helper function to recursively select [num] number of random
    elements from list [lst]. Uses [chosen] as a lookup table to make
    sure the same element isnt randomly selected twice *)
val random_select_helper :
  string list -> int -> bool array -> string list -> string list

(** takes in a float [percent] and randomly selects a portion of
    elements from the list [lst] to match that percentage. Does not
    regard string length *)
val random_select_filter : float -> string list -> string list

(** given all [possible_words] found from a board and a computer player
    [difficulty], returns a subselection of the possible words list
    based on the difficulty *)
val ai_found_words : string list -> int -> string list

(** returns a string name according to the computer [difficulty] level
    int *)
val diff_to_name : int -> string
