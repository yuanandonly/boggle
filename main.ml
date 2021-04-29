open Trie
open Board
open Words
open ANSITerminal
open Art

(* prompts user for list of words, and then prints out words nicely and
   scores nicely, then says game over For Later: add play again feature *)
let game_end (input_board : board) : unit = failwith "Unimplemented"

let rec repeat (s : string) (n : int) : string =
  if n = 0 then "" else s ^ repeat s (n - 1)

let clear = repeat "\n" 45

(* try let proc = Unix.open_process_in "clear" in try let chars =
   input_line proc in ignore (Unix.close_process_in proc); chars with e
   -> ignore (Unix.close_process_in proc); "" with _ -> "" *)

(* takes in time, keeps counting down every sec (or 5?) to terminal +
   erasing?? and then printing times up, then calling game end*)
let rec countdown
    (input_board : board)
    (time_length : int)
    (time_start : float) : unit =
  ANSITerminal.(resize 120 45);
  (* (print_board input_board); *)
  let time_passed = Unix.gettimeofday () -. time_start in
  (* let _ = ANSITerminal.(print_string [green; Bold] ("\n" ^
     (time_passed |> Float.to_int |> string_of_int) ^ "seconds passed
     out of " ^ (time_length |> string_of_int) ^ "seconds \n")) in *)
  let _ =
    let step = Float.to_int (2. *. time_passed) mod 8 in
    match step with
    | 0 ->
        ANSITerminal.(print_string [ cyan; Bold ] (clock_ascii0 ^ "\n"))
    | 1 ->
        ANSITerminal.(print_string [ cyan; Bold ] (clock_ascii1 ^ "\n"))
    | 2 ->
        ANSITerminal.(print_string [ cyan; Bold ] (clock_ascii2 ^ "\n"))
    | 3 ->
        ANSITerminal.(print_string [ cyan; Bold ] (clock_ascii3 ^ "\n"))
    | 4 ->
        ANSITerminal.(print_string [ cyan; Bold ] (clock_ascii4 ^ "\n"))
    | 5 ->
        ANSITerminal.(print_string [ cyan; Bold ] (clock_ascii5 ^ "\n"))
    | 6 ->
        ANSITerminal.(print_string [ cyan; Bold ] (clock_ascii6 ^ "\n"))
    | 7 ->
        ANSITerminal.(print_string [ cyan; Bold ] (clock_ascii7 ^ "\n"))
    | _ ->
        ANSITerminal.(print_string [ cyan; Bold ] (clock_ascii0 ^ "\n"))
  in
  if time_passed > Float.of_int time_length then begin
    ANSITerminal.(erase Screen);
    print_board input_board;
    ANSITerminal.(print_string [ magenta; Bold ] (time_up ^ "\n"));
    game_end input_board
  end
  else begin
    print_board input_board;
    (* print_endline ""; *)
    ANSITerminal.(
      print_string [ cyan; Bold ]
        ("\n"
        ^ (time_passed |> Float.to_int |> string_of_int)
        ^ " seconds passed out of "
        ^ (time_length |> string_of_int)
        ^ " seconds "));
    print_endline "";
    Unix.sleepf 0.1;
    ANSITerminal.(print_string [ white ] clear);
    (* ANSITerminal.(erase Above); *)
    countdown input_board time_length time_start
  end

(* intializes game >>greet user >>ask for name >>ask for size of board
   >>generates board *)
let main () =
  ANSITerminal.(print_string [ magenta; Bold ] welcome_ascii);
  ANSITerminal.(print_string [ cyan; Bold ] boggle_ascii2);

  ANSITerminal.(
    print_string [ green ]
      "How large would you like your board to be? Provide a \
       side-length between 4 and 10.");
  ANSITerminal.(print_string [ green; Blink ] "\n>> ");
  let board_size =
    let input = read_line () |> int_of_string_opt in
    match input with
    | None ->
        ANSITerminal.(
          print_string [ red; Bold ] "Invalid input. Ending game...\n");
        exit 0
    | Some int ->
        if int >= 4 && int <= 10 then int
        else (
          ANSITerminal.(
            print_string [ red; Bold ] "Invalid input. Ending game...\n");
          exit 0)
  in
  let get_board dim =
    match dim with 4 | 5 -> init_classic dim | _ -> rand_init dim
  in
  let new_board = get_board board_size in
  ANSITerminal.(print_string [ green ] "Here is your board: \n");
  print_board new_board;

  ANSITerminal.(
    print_string [ green ]
      "How long would you like for the round? Provide a number in \
       seconds. \n\
       (Normal boggle round is 3 minutes = 180 seconds)");
  ANSITerminal.(print_string [ green; Blink ] "\n>> ");
  let time_input =
    let input = read_line () |> int_of_string_opt in
    match input with
    | None ->
        ANSITerminal.(
          print_string [ red; Bold ] "Invalid input. Ending game...\n");
        exit 0
    | Some int ->
        if int >= 0 && int <= 10000 then int
        else (
          ANSITerminal.(
            print_string [ red; Bold ] "Invalid input. Ending game...\n");
          exit 0)
  in
  let curr_time = Unix.gettimeofday () in
  countdown new_board time_input curr_time

(* ANSITerminal.(print_string [yellow] "Would you like to clear the
   existing scores first? (y/n) "); let clear = read_line () |>
   String.lowercase_ascii in clear |> clear_score "scores.txt";

   print_endline "\n";

   let board = generate_board_init 16 in let dictionary = word_list
   "dictionary.txt" in let prefix_tree = dictionary |> create_trie in
   let lst_of_all_words = all_words board prefix_tree |> intersect
   dictionary in begin ANSITerminal.(print_string [yellow; Bold] "This
   is your board: \n\n"); board |> to_board_str_list |> display;

   print_endline "\n";

   ANSITerminal.(print_string [yellow; Bold] "How many seconds would you
   like? ");

   let req_time = read_line () |> float_of_string_opt in match req_time
   with | None -> ANSITerminal.(print_string [magenta; Bold] "\n\nGame
   ended due to invalid time.\n\n"); | Some time -> Unix.gettimeofday ()
   |> play lst_of_all_words [] dictionary board time *)

(* launches game *)
let () = main ()
