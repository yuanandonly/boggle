(** [entry] is the type of an entry corresponding to a player's game,
    including player name, the score, the size of the board, and the
    length of time. *)
type entry = {
  name : string;
  score : int;
  boardsize : int;
  time : int;
}

(** [read gm] is a list of all the entries for gamemode [gm] in string
    form

    - Raises "Not a gamemode" if [gm] is neither "boggle" or "wordhunt" *)
val read : string -> string list

(** [convert str_entries] is a list of entries converted from string
    form in [str_entries].

    - Raises "Line in file does not correspond to entry" if an element
      in [str_entries] is not an entry*)
val convert : string list -> entry list

(** [get_entries gm] creates an list of entries of all singleplayer
    games played with gamemode [gm]

    - Raises "Not a gamemode" if [gm] is neither "boggle" or "wordhunt" *)
val get_entries : string -> entry list

(** [write gm n s bs t] writes an entry with name [n], score [s],
    boardsize [bs], and length of time [t] into a file corresponding to
    gamemode [gm]*)
val write : string -> string -> int -> int -> int -> unit
