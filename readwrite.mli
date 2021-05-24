(** [entry] is the type of an entry corresponding to a player's game *)
type entry

(** [read gm] is a list of all the entries for gamemode [gm] in string
    form

    - Raises "Not a gamemode" if [gm] is neither "boggle" or "wordhunt" *)
val read : string -> string list

(** [convert str_entries] is a list of entries converted from string
    form in [str_entries].

    - Raises "Line in file does not correspond to entry" if an element
      in [str_entries] is not an entry*)
val convert : string list -> entry list
