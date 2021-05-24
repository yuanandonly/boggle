(** Runs the game. *)
open Board

open Trie

(** iterates through list [l] and prints each string *)
val printlist : string list -> unit

(** iterates through a list [l] of string lists and prints each string
    list *)
val printllist : string list list -> unit

(** given a [word_list] string list, filters through and removes all
    words that are not in [possible_words] and therefore cannot be found
    from given board *)
val validate_words : string list -> string list -> string list

(** repeats a string [s] [n] number of times *)
val repeat : string -> int -> string

(** string to clear up the terminal from user perspective *)
val clear : string

(** takes in [input] int option and ensures that the player enters an
    input of either 1, 2, 3 to match game mode *)
val choose_game_mode : int option -> int

(** takes in string [name] and ensures that the single player's name
    does not conflict with integer names that represent computer player *)
val name_process : string -> string

(** takes in string list [name_list] and ensures that all player's name
    does not conflict with integer names that represent computer player *)
val name_process_list : string list -> string list

(** takes in [input] int option and ensures that the player enters an
    input of either 1, 2, 3, 4, 5 to match computer player difficulty *)
val choose_difficulty : int option -> int

(** queries users to choose the game mode, and accordingly creates a
    string list that either contains the single player's name, multiple
    players' names, or a single player and a computer player's
    difficulty *)
val terminal_player_name : unit -> string list

(** takes in [input] int option and string [lst] of player-found words,
    and ensures the player provides valid input for how to score (either
    1 or 2) and then returns the score accordingly as an integer *)
val scoring_single : int option -> string list -> int * int

(** queries the player(s) if they wish to play again and accordingly
    ends the game or restarts from beginning *)
val play_again : string list -> unit

(** provides option for player(s) to view all the words that the board
    contained and displays their lengths and score values *)
val view_all_words : string list -> unit

(** prompts user for list of words, as well as scoring method. then,
    prints score and calls view_all_words and play again
    functionalities. takes in [input_board], [possible_words] for
    scoring, and the [player_name]*)
val game_end_single : board -> string list -> string -> int -> unit

(** for each player in the [player_name_list], queries for the words
    found of that player and accumulates this into a list of string
    lists*)
val player_word_input :
  string list -> string list list -> string list list

(** iterates simultaneously through liast of player names
    [player_name_list] and [score_list] and prints what each player
    scored *)
val multi_score_print : string list -> int list -> unit

(** takes in [input] int option and string list list [lst] of
    player-found words , and ensures the players provides valid input
    for how to score (either 1 or 2) and then returns the score
    accordingly as an int list to match the player word list *)
val scoring_multi : int option -> string list list -> int list * int

(** prompts players for their lists of words, as well as scoring method.
    then, prints score and calls view_all_words and play again
    functionalities. takes in [input_board], [possible_words] for
    scoring, and the [player_name_list] *)
val game_end_multi : board -> string list -> string list -> int -> unit

(** given a player and a computer player in [player_name_list] and their
    scores in [score_list], prints out their scores and responds if the
    player has been beaten by the AI or not. *)
val ai_score_print : string list -> int list -> unit

(** prompts user for list of words, as well as scoring method. then,
    prints score and calls view_all_words and play again
    functionalities. congratulates player if the AI is beaten, consoles
    otherwise. takes in [input_board], [possible_words] for scoring, and
    the [player_name]*)
val game_end_ai : board -> string list -> string list -> int -> unit

(** recursive function to countdown and show clock ticking animation and
    the [input_board] to the player. once the specified second limit is
    reached as calculatd by [time_length] from [time_start], calls the
    correct game_end function based on game mode as found from
    [player_name_list] *)
val countdown : board -> int -> float -> string list -> unit

(** queries player for board size and ensures valid input for the
    dimension, between 4 and 10 side length. takes in [input] int option
    and returns valid size *)
val choose_board_size : int option -> int

(** queries player for time length of game and ensures valid input for
    time length; makes sure length is less than 10000 seconds and is not
    neg. *)
val choose_time : int option -> int

(** queries player for to either play or view, ensuring valid
    input.contents [input] int option and returns valid size *)
val choose_play_view : int option -> int

(** queries player for viewing option and ensures valid input answer, in
    1, 2, 3, 4, 5, or 6. takes in [input] int option and returns valid
    size *)
val choose_view_option : int option -> int

(* matches player choice to view scores to appropriate actions *)
val view_scores : int -> unit

(* presents score viewing options to player *)
val ask_view : unit -> unit

(* queries player(s) for name / gamemode, board size, round length and
   initiates countdown *)
val play_game : unit -> unit

(** main function that welcomes player, asks player to either view
    scores or play game*)
val main : unit -> unit
