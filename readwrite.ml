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
  | "boggle" -> read_helper (open_in "score_entries_boggle.txt") []
  | "wordhunt" -> read_helper (open_in "score_entries_wordhunt.txt") []
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

let get_entries (gamemode : string) : entry list =
  gamemode |> read |> convert

let write gamemode name score board time : unit =
  let oc =
    match gamemode with
    | "boggle" ->
        open_out_gen [ Open_append ] 0o666 "score_entries_boggle.txt"
    | "wordhunt" ->
        open_out_gen [ Open_append ] 0o666 "score_entries_wordhunt.txt"
    | _ -> failwith "Not a gamemode"
  in
  Printf.fprintf oc "%s %d %d %d" name score board time;
  close_out oc
