(** Representation of the Boggle game board.

    This module represents the static board that Boggle is played on. *)

(** [board] is a board representing the game state. *)
type board = {
  dim : int;
  board_letters : string list;
}

(** [die] is a boggle die *)
type die = { letters : string list }

(** [manual_init i str] is a manually generated Boggle game board with
    dimension [i] and letters [str]. Raises an exception if the
    dimension does not match the number of letters in [str]. *)
val manual_init : int -> string -> board

(** [rand_init i] is a randomized Boggle game board with width and
    length int [i]. Letters are derived by rolling a random die from the
    5x5 boggle board.*)
val rand_init : int -> board

(** [init_classic i] is a randomized classic boggle board of size 4 or 5
    represented by [i] and implemented with dice *)
val init_classic : int -> board

(** for a pair (a, b) in the assoc list, a tile "a" away from a given
    tile in the board should be b rows away from that tile *)
val direc_list : int -> (int * int) list

(** check if a given index [comp_ind], if it represents a valid
    neighboring tile to index [ind] in a square board with index [x] *)
val adj_check : int -> int -> int -> (int * int) list -> bool

(** [i +++ j] is an int list starting with i and ending on j *)
val ( +++ ) : int -> int -> int list

(** [adj_table i] is an array of integer lists where the index of the
    array corresponds to the index of the board tile and the int list at
    that index represents the board indices of the adjacent tiles *)
val adj_table : int -> int list array

(** [print_board t] produces a graphical representation of board [t]. *)
val print_board : board -> unit
