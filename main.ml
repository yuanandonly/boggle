open Str
let corpus =  "corpus.txt"
(** Converts [corpus.txt] into a readable string *)
let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
(** Takes in the [corpus] as a string and replaces the \r\n with a space*)
let string_space = Str.global_replace (Str.regexp"[\r\n]") (" ") (read_whole_file corpus)
(** Turns [string_space] into a list seperated by empty space *)
let listed_strings = String.split_on_char (' ') string_space 
(** Filters all empty space from the list *)
let filtered_list = List.filter (fun x -> String.length x > 0) listed_strings