open Board
open Trie
open Trie_func

val validate_words : t -> string list -> string list -> string list  

(* prompts user for list of words, and then prints out words nicely and
   scores nicely, then says game over For Later: add play again feature *)
val game_end_single : board -> string list -> string -> unit

val player_word_input : string list -> string list list -> string list list

val multi_score_print : string list -> int list -> unit

val game_end_multi : board -> string list -> string list -> unit

val repeat : string -> int -> string

val clear : string

val countdown : board -> int -> float -> string list -> unit

val terminal_player_name : string list

val main : unit -> unit