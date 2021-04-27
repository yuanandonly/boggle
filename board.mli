(** Representation of the Boggle game board.

    This module represents the static board that Boggle is played on. *)

(** The abstract type of values representing the game state. *)
type board = {
  dim : int;
  board_letters : string list;
}

(** [init i] is a randomized Boggle game board with width and length int
    [i]. *)
val init : int -> board

(* for a pair (a, b) in the assoc list, a tile "a" away from a given
   tile in the board should be b rows away from that tile *)
val direc_list : int -> (int * int) list

(* check if a given index [comp_ind], if it represents a valid
   neighboring tile to index [ind] in a square board with index [x] *)
val adj_check : int -> int -> int -> (int * int) list -> bool

(* return int list starting with i and ending on j *)
val ( +++ ) : int -> int -> int list

(* return an array of integer lists where the index of the array
   corresponds to the index of the board tile and the int list at that
   index represents the board indices of the adjacent tiles *)
val adj_table : int -> int list array

(** [print_board t] produces a graphical representation of board [t]. *)
val print_board : board -> unit
