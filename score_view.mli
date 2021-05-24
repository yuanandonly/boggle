(** [print_highscores gm] prints the the top 10 highscores played on
    classic boards (4 x 4) for gamemode [gm] into the terminal.

    - Raises "Not a gamemode" if gm is neither "boggle" or "wordhunt" *)
val print_highscores : string -> unit

(** [print_player gm pl] prints all the scores for the singleplayer
    games with gamemode [gm] played by player [pl].

    - Raises "Not a gamemode" if gm is neither "boggle" or "wordhunt" *)
val print_player : string -> string -> unit
