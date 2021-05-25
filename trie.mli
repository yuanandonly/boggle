(** Enables word identification using a trie. *)

(** [t] is a Trie node that contains a boolean value and a list of
    tuples of children nodes and their corresponding character. *)
type t = Node of bool * (string * t) list

(** Empty Trie *)
val empty : unit -> t

(** Extracts the data type (string, t) from the option. Returns empty
    pair if the option is none, otherwise return the (string, t) *)
val extract_st_option : (string * t) option -> string * t

(** Extracts the data type [t] from the option, returns an empty trie if
    the option if none, otherwise returns the trie. *)
val extract_t_option : t option -> t

(** Splits a given string into a list of letters of that word. No such
    String function does this, String.split_on_char needs a character
    with which to split*)
val word_to_list : string -> string list

(** Returns the children of a given Trie. *)
val children : t -> (string * t) list

(** Check if a child with a certain character stem exists in the list of
    children of type (string, t) list, and returns an option of that
    trie*)
val return_child_opt : (string * t) list -> string -> t option

(** Adds a new word to the trie. Takes in current trie and a word. If
    the word node exists in the trie but is not marked as a word, then
    returns a trie with that word marked as a word.*)
val trie_insert : t -> string list -> t

(** Checks if the input of a word is an actual word inside of the
    provided Trie. *)
val trie_contains_word : t -> string list -> bool

(** Checks if the input of a word is an actual Node inside of the
    provided Trie. The input doesn't have to be an actual word. *)
val trie_contains : t -> string list -> bool

(** Creates a new trie and populates it with everything in the string
    list*)
val trie_instantiate : string list -> t
