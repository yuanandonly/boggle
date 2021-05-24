(* prints formatted top 10 scores. the input is a list of tuples where
   each tuple is contains the player name, boggle score, and wordhunt
   score.*)
(* FOR EMILY: just make there be like three columns with lines, format
   it how you think would look nice basically, can use Board.print_board
   as inspiration*)
let print_leaderboard (info : (string * int * int) list) : unit =
  failwith "unimplemented"

(* assumption, size >= length of string*)
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

let print_highscores (gamemode : string) : unit =
  let sorted =
    Readwrite.get_entries gamemode
    |> List.filter (fun entry ->
           if entry.board = 4 then true else false)
    |> List.sort (fun e1 e2 -> compare e1.score e2.score)
    |> List.rev
  in
  Printf.printf "------------ TOP 10 ------------";
  Printf.printf "      SCORE    NAME       TIME  ";
  let makestring i e : string =
    let ten = if i = 10 then "" else " " in
    let num = ten ^ string_of_int i ^ ". " in
    let score = e.score |> string_of_int |> center 8 in
    let name = e.name |> center 8 in
    let name =
      if String.length e.name > 8 then name ^ ".." else name ^ "  "
    in
    let time = (e.time |> string_of_int) ^ " secs" |> center 9 in
    num ^ score ^ " " ^ name ^ time
  in
  for i = 1 to 10 do
    Printf.printf (makestring i (List.nth sorted (i - 1)))
  done
