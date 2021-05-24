(* given chosen difficulty, the computer will randomly choose a subset of the
  possible letters on the board 
  EASY (0) : 5% of words length 3-4 
  NOVICE (1) : 10% of words length 3-5
  MEDIUM (2) : 15% of words length 3-6
  HARD (3) : 20% of all words
  INSANE (4) : 30% of all words
  IMPOSSIBLE (5) : 60% of all words)
  *)

val length_filter : int -> int -> string list -> string list

val random_select_helper : string list -> int -> bool array -> string list ->
  string list

val random_select_filter : float -> string list -> string list

val ai_found_words : string list -> int -> string list

val diff_to_name : int -> string