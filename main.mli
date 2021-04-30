open Board
open Trie
open Trie_func

val validate_words : t -> string list -> string list -> string list  

val game_end : board -> string list -> unit

val repeat : string -> int -> string

val clear : string

val countdown : board -> int -> float -> unit

val main : unit -> unit