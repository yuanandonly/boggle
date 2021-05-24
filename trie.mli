(** Enables word identification using a trie. *)

(** [t] is a Trie node that contains a boolean value and a list of
    tuples of children nodes and their corresponding character. *)
type t = Node of bool * (string * t) list

val empty : unit -> t

val extract_st_option : (string * t) option -> string * t

val extract_t_option : t option -> t

val word_to_list : string -> string list

val children : t -> (string * t) list

val return_child_opt : (string * t) list -> string -> t option

val trie_insert : t -> string list -> t

val trie_contains_word : t -> string list -> bool

val trie_contains : t -> string list -> bool

val trie_instantiate : string list -> t
