open Trie
open Board
open Words
open ANSITerminal
open Art

(* prompts user for board *)
(* let game_end  *)

(* takes in time, keeps counting down every sec (or 5?) to terminal + erasing??
 and then printing times up, then calling game end*)
let rec countdown (input_board : board) (time_start : int) (time : int): unit = 
  failwith "unimplemented"


(* intializes game
>>greet user
>>ask for name
>>ask for size of board
>>generates board
*)
let main () = 
  ANSITerminal.(print_string [magenta; Bold] 
      welcome_ascii);
  ANSITerminal.(print_string [cyan; Bold] 
      boggle_ascii2);

  ANSITerminal.(print_string [green] 
  "How large would you like your board to be? Provide a side-length between 4 and 10.");
  ANSITerminal.(print_string [green; Blink] 
  "\n>> ");
  let board_size = 
    let input = read_line () |> int_of_string_opt in
    match input with
    | None -> (ANSITerminal.(print_string [red; Bold] 
    "Invalid input. Ending game...\n"); exit 0)
    | Some int -> if int >= 4 && int <= 10 
        then int 
        else (ANSITerminal.(print_string [red; Bold] 
        "Invalid input. Ending game...\n"); exit 0)
  in
  let get_board dim = 
    match dim with
    | 4 
    | 5 -> init_classic dim
    | _ -> rand_init dim in 
  let new_board = get_board board_size in
  begin
    ANSITerminal.(print_string [green] "Here is your board: \n");
    print_board new_board;
    (* ask for how much time, then call countdown*)
  end  

  (* ANSITerminal.(print_string [yellow] 
                  "Would you like to clear the existing scores first? (y/n) ");
  let clear = read_line () |> String.lowercase_ascii in 
  clear |> clear_score "scores.txt";

  print_endline "\n";

  let board = generate_board_init 16 in 
  let dictionary = word_list "dictionary.txt" in 
  let prefix_tree = dictionary |> create_trie in 
  let lst_of_all_words = all_words board prefix_tree |> intersect dictionary in 
  begin
    ANSITerminal.(print_string [yellow; Bold] "This is your board: \n\n");
    board |> to_board_str_list |> display;

    print_endline "\n";

    ANSITerminal.(print_string [yellow; Bold] 
                    "How many seconds would you like? ");

    let req_time = read_line () |> float_of_string_opt in 
    match req_time with 
    | None -> 
      ANSITerminal.(print_string [magenta; Bold] 
                      "\n\nGame ended due to invalid time.\n\n");
    | Some time -> 
      Unix.gettimeofday () |> play lst_of_all_words [] dictionary board time *)


(* launches game *)
let () = main ()