open Readwrite

let center (size : int) (s : string) =
  let length = String.length s in
  let s = if length > size then String.sub s 0 size else s in
  let extra = size - length in
  let lside, rside =
    match extra mod 2 with
    | 0 -> (extra / 2, extra / 2)
    | 1 -> ((extra / 2) + 1, extra / 2)
    | _ -> failwith "impossible"
  in
  String.make lside ' ' ^ s ^ String.make rside ' '

let highscore_helper i e_opt : string =
  let ten = if i = 10 then "" else " " in
  let num = ten ^ string_of_int i ^ ". " in
  match e_opt with
  | Some e ->
      let score = e.score |> string_of_int |> center 8 in
      let name = e.name |> center 8 in
      let name =
        if String.length e.name > 8 then name ^ ".." else name ^ "  "
      in
      let time = (e.time |> string_of_int) ^ " secs" |> center 9 in
      num ^ score ^ " " ^ name ^ time
  | None -> num ^ String.make 28 '-'

let get_entries_subset f gamemode =
  get_entries gamemode |> List.filter f
  |> List.sort (fun e1 e2 -> compare e1.score e2.score)
  |> List.rev

let print_highscores (gamemode : string) : unit =
  let entries =
    get_entries_subset
      (fun entry -> if entry.board = 4 then true else false)
      gamemode
  in
  Printf.printf "------------ TOP 10 ------------";
  Printf.printf "      SCORE    NAME       TIME  ";
  for i = 1 to 10 do
    let e_opt =
      try Some (List.nth entries (i - 1)) with exn -> None
    in
    Printf.printf "%s" (highscore_helper i e_opt)
  done

let player_helper i e : string =
  let score = e.score |> string_of_int |> center 7 in
  let board = e.board |> string_of_int |> center 3 in
  let time = (e.time |> string_of_int) ^ " secs" |> center 9 in
  " " ^ score ^ " " ^ board ^ " " ^ time

let print_player (gamemode : string) (player : string) =
  let player_entries =
    get_entries_subset
      (fun entry -> if entry.name = player then true else false)
      gamemode
  in
  Printf.printf "%s" (center 28 (player ^ "'s " ^ gamemode ^ " scores"));
  match List.length player_entries with
  | 0 ->
      Printf.printf "%s" (center 28 "No scores found...");
      Printf.printf "%s" (center 28 "Go play some " ^ gamemode ^ "!")
  | x ->
      for i = 1 to x do
        Printf.printf "%s"
          (player_helper i (List.nth player_entries (i - 1)))
      done
