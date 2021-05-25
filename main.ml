open Trie
open Board
open Words
open ANSITerminal
open Art
open Score
open Ai_player
open Score_view
open Readwrite

let printlist l = List.iter (Printf.printf "%s ") l

let printllist l = List.iter (fun ll -> printlist ll) l

let rec repeat (s : string) (n : int) : string =
  if n = 0 then "" else s ^ repeat s (n - 1)

let clear = repeat "\n" 45

let rec choose_game_mode (input : int option) : int =
  let try_again = "Invalid Input. Try Again\n" in
  match input with
  | None ->
      ANSITerminal.(print_string [ red; Bold ] try_again);
      ANSITerminal.(print_string [ red; Blink ] ">> ");
      let new_input = read_line () |> int_of_string_opt in
      choose_game_mode new_input
  | Some int -> (
      match int with
      | 1 -> 1
      | 2 -> 2
      | 3 -> 3
      | _ ->
          ANSITerminal.(print_string [ red; Bold ] try_again);
          ANSITerminal.(print_string [ red; Blink ] ">> ");
          let new_input = read_line () |> int_of_string_opt in
          choose_game_mode new_input)

let rec name_process (name : string) : string =
  let try_again =
    "Please enter a different name. Integer names are not allowed.\n"
  in
  match name with
  | "0" | "1" | "2" | "3" | "4" | "5" ->
      ANSITerminal.(print_string [ red; Bold ] try_again);
      ANSITerminal.(print_string [ red; Blink ] ">> ");
      let new_input = read_line () in
      name_process new_input
  | _ -> name

let rec name_process_list (name_list : string list) : string list =
  let try_again =
    "Please enter a different list of names. Integer names are not \
     allowed. \n"
  in
  let check = List.nth name_list 1 in
  match check with
  | "0" | "1" | "2" | "3" | "4" | "5" ->
      ANSITerminal.(print_string [ red; Bold ] try_again);
      ANSITerminal.(print_string [ red; Blink ] ">> ");
      let new_input = read_line () |> String.split_on_char ' ' in
      name_process_list new_input
  | _ -> name_list

let rec choose_difficulty (input : int option) : int =
  let try_again = "Invalid Input. Try Again\n" in
  match input with
  | None ->
      ANSITerminal.(print_string [ red; Bold ] try_again);
      ANSITerminal.(print_string [ red; Blink ] ">> ");
      let new_input = read_line () |> int_of_string_opt in
      choose_game_mode new_input
  | Some int -> (
      match int with
      | 0 -> 0
      | 1 -> 1
      | 2 -> 2
      | 3 -> 3
      | 4 -> 4
      | 5 -> 5
      | _ ->
          ANSITerminal.(print_string [ red; Bold ] try_again);
          ANSITerminal.(print_string [ red; Blink ] ">> ");
          let new_input = read_line () |> int_of_string_opt in
          choose_game_mode new_input)

let rec scoring_single (input : int option) (lst : string list) :
    int * int =
  let try_again = "Invalid Input. Try Again\n" in
  match input with
  | None ->
      ANSITerminal.(print_string [ red; Bold ] try_again);
      ANSITerminal.(print_string [ red; Blink ] ">> ");
      let new_input = read_line () |> int_of_string_opt in
      scoring_single new_input lst
  | Some int -> (
      match int with
      | 1 -> (boggle_scoring_single lst, 1)
      | 2 -> (wordhunt_scoring_single lst, 2)
      | _ ->
          ANSITerminal.(print_string [ red; Bold ] try_again);
          ANSITerminal.(print_string [ red; Blink ] ">> ");
          let new_input = read_line () |> int_of_string_opt in
          scoring_single new_input lst)

let rec play_again (player_name_list : string list) : unit =
  ANSITerminal.(
    print_string [ green ] "Would you like to restart? Y/N\n");
  ANSITerminal.(print_string [ green; Blink ] ">> ");
  let input = read_line () in
  match input with
  | "Y" | "y" | "yes" | "Yes" | "YES" ->
      ANSITerminal.(erase Screen);
      main ()
  | _ ->
      ANSITerminal.(print_string [ cyan; Bold ] "GOODBYE! \n");
      ()

and view_all_words (possible_words : string list) : unit =
  ANSITerminal.(
    print_string [ green ]
      "Would you like to view all the possible words you could've \
       found in the board? Y/N\n");
  ANSITerminal.(print_string [ green; Blink ] ">> ");
  let input = read_line () in
  match input with
  | "Y" | "y" | "yes" | "Yes" | "YES" ->
      ANSITerminal.(
        print_string [ cyan; Bold ]
          "WORD  LENGTH  BOGGLE SCORE  WORDHUNT SCORE\n");
      List.iter
        (fun x ->
          ANSITerminal.(printf [ magenta; Bold ])
            "| %s %d %d %d\n" x (String.length x)
            (boggle_scoring_helper 0 x)
            (wordhunt_scoring_helper 0 x))
        possible_words;
      ()
  | _ -> ()

and game_end_single
    (input_board : board)
    (possible_words : string list)
    (player_name : string)
    (time_length : int) : unit =
  ANSITerminal.(
    print_string [ green ]
      "Please enter the list of words that you found in the board. \
       Separate each word with a space. ");
  ANSITerminal.(print_string [ green; Blink ] "\n>> ");
  let validated_word_list =
    let word_list =
      read_line () |> String.uppercase_ascii |> String.split_on_char ' '
    in
    validate_words word_list possible_words
  in
  ANSITerminal.(
    print_string [ green ]
      "How would you like your words to be scored? Type (1) for Boggle \
       scoring and (2) for WordHunt scoring. ");
  ANSITerminal.(print_string [ green; Blink ] "\n>> ");
  let input = read_line () |> int_of_string_opt in
  let score, gamemode = scoring_single input validated_word_list in
  ANSITerminal.(
    print_string [ magenta; Bold ]
      ("CONGRATULATIONS! Your score is: \n" ^ string_of_int score ^ "\n"));
  let game_name = if gamemode = 1 then "boggle" else "wordhunt" in
  write game_name player_name score input_board.dim time_length;
  view_all_words possible_words;
  play_again [ player_name ]

and player_word_input
    (player_name_list : string list)
    (acc : string list list) : string list list =
  match player_name_list with
  | [] -> acc
  | h :: t ->
      let input =
        ANSITerminal.(
          printf [ green ]
            "Please enter the list of words for: < %s >. Separate each \
             word with a space. "
            h);
        ANSITerminal.(print_string [ green; Blink ] "\n>> ");
        read_line () |> String.uppercase_ascii
        |> String.split_on_char ' '
      in
      player_word_input t (acc @ [ input ])

and multi_score_print
    (player_name_list : string list)
    (score_list : int list) : unit =
  ANSITerminal.(print_string [ magenta; Bold ] "SCORES\n");
  List.iter2
    (fun name score ->
      ANSITerminal.(printf [ green ] "%s => %d\n" name score);
      ())
    player_name_list score_list;
  ()

and scoring_multi (input : int option) (lst : string list list) :
    int list * int =
  let try_again = "Invalid Input. Try Again\n" in
  match input with
  | None ->
      ANSITerminal.(print_string [ red; Bold ] try_again);
      ANSITerminal.(print_string [ red; Blink ] ">> ");
      let new_input = read_line () |> int_of_string_opt in
      scoring_multi new_input lst
  | Some int -> (
      match int with
      | 1 -> (boggle_scoring_multi lst, 1)
      | 2 -> (wordhunt_scoring_multi lst, 2)
      | _ ->
          ANSITerminal.(print_string [ red; Bold ] try_again);
          ANSITerminal.(print_string [ red; Blink ] ">> ");
          let new_input = read_line () |> int_of_string_opt in
          scoring_multi new_input lst)

and game_end_multi
    (input_board : board)
    (possible_words : string list)
    (player_name_list : string list)
    (time_length : int) : unit =
  let player_word_list = player_word_input player_name_list [] in
  let validated_word_list =
    List.map (fun x -> validate_words x possible_words) player_word_list
  in
  ANSITerminal.(
    print_string [ green ]
      "How would you like your words to be scored? Type (1) for Boggle \
       scoring and (2) for WordHunt scoring. ");
  ANSITerminal.(print_string [ green; Blink ] "\n>> ");
  let input = read_line () |> int_of_string_opt in
  let score_list, gamemode = scoring_multi input validated_word_list in
  multi_score_print player_name_list score_list;
  let game_name = if gamemode = 1 then "boggle" else "wordhunt" in
  List.iter2
    (fun x y -> write game_name x y input_board.dim time_length)
    player_name_list score_list;
  view_all_words possible_words;
  play_again player_name_list

and ai_score_print
    (player_name_list : string list)
    (score_list : int list) : unit =
  let diff = int_of_string (List.nth player_name_list 1) in
  let new_name_list =
    [ List.nth player_name_list 0; diff_to_name diff ]
  in
  ANSITerminal.(print_string [ magenta; Bold ] "SCORES\n");
  List.iter2
    (fun name score ->
      ANSITerminal.(printf [ green ] "%s \t => %d\n" name score);
      ())
    new_name_list score_list;
  (if List.nth score_list 0 > List.nth score_list 1 then
   ANSITerminal.(
     print_string [ magenta; Bold ]
       "CONGRATULATIONS! You have beat the computer.\n")
  else
    ANSITerminal.(
      print_string [ magenta; Bold ] "Good luck next time :(\n"));
  ()

and game_end_ai
    (input_board : board)
    (possible_words : string list)
    (player_name_list : string list)
    (time_length : int) : unit =
  ANSITerminal.(
    print_string [ green ]
      "Please enter the list of words that you found in the board. \
       Separate each word with a space. ");
  ANSITerminal.(print_string [ green; Blink ] "\n>> ");
  let validated_word_list =
    let word_list =
      read_line () |> String.uppercase_ascii |> String.split_on_char ' '
    in
    validate_words word_list possible_words
  in
  let ai_words =
    ai_found_words possible_words
      (int_of_string (List.nth player_name_list 1))
  in
  let lst = [ validated_word_list; ai_words ] in
  ANSITerminal.(
    print_string [ green ]
      "How would you like your words to be scored? Type (1) for Boggle \
       scoring and (2) for WordHunt scoring. ");
  ANSITerminal.(print_string [ green; Blink ] "\n>> ");
  let input = read_line () |> int_of_string_opt in
  let score_list, gamemode = scoring_multi input lst in
  ai_score_print player_name_list score_list;
  let game_name = if gamemode = 1 then "boggle" else "wordhunt" in
  write game_name
    (List.nth player_name_list 0)
    (List.nth score_list 0) input_board.dim time_length;
  view_all_words possible_words;
  play_again player_name_list

and countdown
    (input_board : board)
    (time_length : int)
    (time_start : float)
    (player_name_list : string list) : unit =
  ANSITerminal.(resize 120 45);

  let time_passed = Unix.gettimeofday () -. time_start in

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
    ANSITerminal.(print_string [ green; Bold ] "Stop finding words!\n");
    if List.length player_name_list = 1 then
      game_end_single input_board
        (find_possible_words input_board)
        (List.hd player_name_list)
        time_length
    else if
      List.mem
        (List.nth player_name_list 1)
        [ "0"; "1"; "2"; "3"; "4"; "5" ]
    then
      game_end_ai input_board
        (find_possible_words input_board)
        player_name_list time_length
    else
      game_end_multi input_board
        (find_possible_words input_board)
        player_name_list time_length
  end
  else begin
    print_board input_board;

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

    countdown input_board time_length time_start player_name_list
  end

and choose_board_size (input : int option) : int =
  let try_again = "Invalid Input. Try Again\n" in
  match input with
  | None ->
      ANSITerminal.(print_string [ red; Bold ] try_again);
      ANSITerminal.(print_string [ red; Blink ] ">> ");
      let new_input = read_line () |> int_of_string_opt in
      choose_board_size new_input
  | Some int ->
      if int >= 4 && int <= 10 then int
      else (
        ANSITerminal.(print_string [ red; Bold ] try_again);
        ANSITerminal.(print_string [ red; Blink ] ">> ");
        let new_input = read_line () |> int_of_string_opt in
        choose_board_size new_input)

and choose_time (input : int option) : int =
  let try_again = "Invalid Input. Try Again\n" in
  match input with
  | None ->
      ANSITerminal.(print_string [ red; Bold ] try_again);
      ANSITerminal.(print_string [ red; Blink ] ">> ");
      let new_input = read_line () |> int_of_string_opt in
      choose_time new_input
  | Some int ->
      if int >= 0 && int <= 10000 then int
      else (
        ANSITerminal.(print_string [ red; Bold ] try_again);
        ANSITerminal.(print_string [ red; Blink ] ">> ");
        let new_input = read_line () |> int_of_string_opt in
        choose_time new_input)

and choose_play_view (input : int option) : int =
  let try_again = "Invalid Input. Try Again\n" in
  match input with
  | None ->
      ANSITerminal.(print_string [ red; Bold ] try_again);
      ANSITerminal.(print_string [ red; Blink ] ">> ");
      let new_input = read_line () |> int_of_string_opt in
      choose_play_view new_input
  | Some int -> (
      match int with
      | 1 -> 1
      | 2 -> 2
      | _ ->
          ANSITerminal.(print_string [ red; Bold ] try_again);
          ANSITerminal.(print_string [ red; Blink ] ">> ");
          let new_input = read_line () |> int_of_string_opt in
          choose_play_view new_input)

and choose_view_option (input : int option) : int =
  let try_again = "Invalid Input. Try Again\n" in
  match input with
  | None ->
      ANSITerminal.(print_string [ red; Bold ] try_again);
      ANSITerminal.(print_string [ red; Blink ] ">> ");
      let new_input = read_line () |> int_of_string_opt in
      choose_view_option new_input
  | Some int -> (
      match int with
      | 1 -> 1
      | 2 -> 2
      | 3 -> 3
      | 4 -> 4
      | 5 -> 5
      | 6 -> 6
      | _ ->
          ANSITerminal.(print_string [ red; Bold ] try_again);
          ANSITerminal.(print_string [ red; Blink ] ">> ");
          let new_input = read_line () |> int_of_string_opt in
          choose_view_option new_input)

and view_scores (view_mode : int) : unit =
  match view_mode with
  | 1 ->
      print_highscores "boggle";
      ask_view ()
  | 2 ->
      print_highscores "wordhunt";
      ask_view ()
  | 3 | 4 ->
      ANSITerminal.(
        print_string [ green ]
          "Type the name of whom you would like the see the scores. \n");
      ANSITerminal.(print_string [ green; Blink ] ">> ");
      let name = read_line () in
      if view_mode = 3 then (
        print_player "boggle" name;
        ask_view ())
      else (
        print_player "wordhunt" name;
        ask_view ())
  | 5 ->
      play_game ();
      ask_view ()
  | 6 | _ ->
      ANSITerminal.(print_string [ cyan; Bold ] "GOODBYE! \n");
      exit 0

and main () =
  ANSITerminal.(resize 120 45);
  ANSITerminal.(print_string [ magenta; Bold ] welcome_ascii);
  ANSITerminal.(print_string [ cyan; Bold ] boggle_ascii2);
  ANSITerminal.(
    print_string [ green ]
      "Would you like to (1) play the game or (2) view scores? Type a \
       number. \n");
  ANSITerminal.(print_string [ green; Blink ] ">> ");
  let input = read_line () |> int_of_string_opt in
  let play_view = choose_play_view input in
  if play_view = 1 then play_game ()
  else (
    ANSITerminal.(erase Screen);
    ask_view ())

and ask_view () =
  ANSITerminal.(
    print_string [ green ]
      "Enter (1) if you would like to view all time boggle classic \
       high scores (4 x 4). \n\
       Enter (2) if you would like to view all time word hunt high \
       scores (4 x 4). \n\
       Enter (3) if you would like to search for a player’s boggle \
       scores.\n\
       Enter (4) if you would like to search for a player’s word \
       hunt scores.\n\
       Enter (5) if you would like to play.\n\
       Enter (6) if you would like to leave.\n");
  ANSITerminal.(print_string [ green; Blink ] ">> ");
  let input = read_line () |> int_of_string_opt in
  let view_mode = choose_view_option input in
  view_scores view_mode

and play_game () =
  let player_name_list = terminal_player_name () in
  ANSITerminal.(
    print_string [ green ]
      "How large would you like your board to be? Provide a \
       side-length between 4 and 10.\n");
  ANSITerminal.(print_string [ green; Blink ] ">> ");
  let input = read_line () |> int_of_string_opt in
  let board_size = choose_board_size input in
  let get_board dim =
    match dim with 4 | 5 -> init_classic dim | _ -> rand_init dim
  in
  let new_board = get_board board_size in
  ANSITerminal.(
    print_string [ green ]
      "How long would you like for the round? Provide a number in \
       seconds. \n\
       (Normal boggle round is 3 minutes = 180 seconds)");
  ANSITerminal.(print_string [ green; Blink ] "\n>> ");
  let input = read_line () |> int_of_string_opt in
  let time_input = choose_time input in
  let curr_time = Unix.gettimeofday () in
  ANSITerminal.(
    print_string [ green ] (String.concat " " player_name_list));
  countdown new_board time_input curr_time player_name_list

and terminal_player_name () : string list =
  ANSITerminal.(
    print_string [ green ]
      "\n\
       How would you like to play? Type (1) for Singleplayer, (2) for \
       Multiplayer, or \n\
       (3) to play against the computer.\n");
  ANSITerminal.(print_string [ green; Blink ] ">> ");
  let input = read_line () |> int_of_string_opt in
  let game_mode = choose_game_mode input in
  match game_mode with
  | 1 ->
      ANSITerminal.(
        print_string [ green ]
          "What is your name? Use the same name if you've played \
           previously.\n");
      ANSITerminal.(print_string [ green; Blink ] ">> ");
      let input = read_line () |> name_process in
      [ input ]
  | 2 ->
      ANSITerminal.(
        print_string [ green ]
          "Please type all the names of the players into the same \
           line, separated by spaces. Use the same names if you've \
           played previously. \n");
      ANSITerminal.(print_string [ green; Blink ] ">> ");
      let input =
        read_line () |> String.split_on_char ' ' |> name_process_list
      in
      input
  | 3 | _ ->
      ANSITerminal.(
        print_string [ green ]
          "What is your name? Use the same name if you've played \
           previously.\n");
      ANSITerminal.(print_string [ green; Blink ] ">> ");
      let name_input = read_line () |> name_process in
      ANSITerminal.(
        print_string [ green ]
          "What difficulty would you like the computer to be? \n\
           (0) for EASY\n\
           (1) for NOVICE\n\
           (2) for MEDIUM\n\
           (3) for HARD\n\
           (4) for INSANE\n\
           (5) for IMPOSSIBLE\n");
      ANSITerminal.(print_string [ green; Blink ] ">> ");
      let input = read_line () |> int_of_string_opt in
      let diff_input = choose_difficulty input in
      [ name_input; Int.to_string diff_input ]

let () = main ()
