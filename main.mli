open Board
open Trie
open Trie_func

val printlist : string list -> unit

val printllist : string list list -> unit

val validate_words : t -> string list -> string list -> string list  

val repeat : string -> int -> string

val clear : string

val choose_game_mode : int option -> int

val terminal_player_name : string list

val scoring_single : int option -> string list -> int

val play_again : string list -> unit

val view_all_words : string list -> unit

(* prompts user for list of words, and then prints out words nicely and
   scores nicely, then says game over For Later: add play again feature *)
val game_end_single : board -> string list -> string -> unit

val player_word_input : string list -> string list list -> string list list

val multi_score_print : string list -> int list -> unit

val scoring_multi : int option -> string list list -> int list

val player_word_input : string list -> string list list -> string list list

val game_end_multi : board -> string list -> string list -> unit

val countdown : board -> int -> float -> string list -> unit

val choose_board_size : int option -> int

val choose_time : int option -> int

val main : unit -> unit