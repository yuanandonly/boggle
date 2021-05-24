type entry = {
  name : string;
  score : int;
  board : int;
  time : int;
}

let rec read_helper ic lst =
  match input_line ic with
  | entry -> read_helper ic (entry :: lst)
  | exception End_of_file ->
      close_in ic;
      lst

let read (gamemode : string) : string list =
  match gamemode with
  | "boggle" -> read_helper (open_in "highscores_boggle.txt") []
  | "wordhunt" -> read_helper (open_in "highscores_wordhunt.txt") []
  | _ -> failwith "Not a gamemode"

let convert_helper (s : string) : entry =
  match String.split_on_char ' ' s with
  | [ n; s; b; t ] ->
      {
        name = n;
        score = int_of_string s;
        board = int_of_string b;
        time = int_of_string t;
      }
  | _ -> failwith "Line in file does not correspond to entry"

let convert (string_entries : string list) : entry list =
  List.map convert_helper string_entries

let write gamemode name score board time : unit =
  let oc =
    match gamemode with
    | "boggle" ->
        open_out_gen [ Open_append ] 0o666 "highscores_boggle.txt"
    | "wordhunt" ->
        open_out_gen [ Open_append ] 0o666 "highscores_wordhunt.txt"
    | _ -> failwith "Not a gamemode"
  in
  Printf.fprintf oc "%s %d %d %d" name score board time;
  close_out oc

let print_to_file file score =
  let oc = open_out_gen [ Open_append; Open_creat ] 0o666 file in
  Printf.fprintf oc "%d\n" score;
  close_out oc

let clear_score file ans =
  if ans = "y" then
    let oc = open_out_gen [ Open_trunc ] 0o666 file in
    close_out oc
  else if ans = "n" then ()
  else
    let () =
      ANSITerminal.(
        print_string [ magenta; Bold ]
          "\n\nGame ended due to invalid response.\n\n")
    in
    exit 0
