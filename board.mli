(** Representation of the Boggle game board.

    This module represents the static board that Boggle is played on. *)

(** The abstract type of values representing the game state. *)
type t

(** [init i] is a randomized Boggle game board with width and length int [i]. *)
val init : int -> t

(** [print_board t] produces a graphical representation of board [t]. *)
val print_board : t -> unit