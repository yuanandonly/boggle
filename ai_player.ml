
let length_filter (lower_bound : int) (upper_bound : int) (lst : string list)
  : string list =
  List.filter (fun x -> String.length x >= lower_bound
   && String.length x <= upper_bound) lst

let rec random_select_helper (lst : string list) (num : int) 
  (chosen : bool array) (acc : string list) : string list = 
  match num with
  | 0 -> acc
  | _ -> 
    let ran = Random.int (List.length lst) in
      if chosen.(ran) 
        then random_select_helper lst num chosen acc
        else 
          begin
            chosen.(ran) <- true;
            random_select_helper lst (num - 1) chosen 
              ((List.nth lst ran) :: acc)
          end

let random_select_filter (percent : float) (lst : string list) : string list =
  let number = Float.to_int(percent *. Float.of_int (List.length lst)) in
  let chosen = Array.make (List.length lst) false in
  random_select_helper lst number chosen []

let ai_found_words (possible_words : string list) (difficulty : int) 
  : string list = 
  match difficulty with
  | 0 -> possible_words 
        |> length_filter 3 4 
        |> random_select_filter 0.05
  | 1 -> possible_words 
        |> length_filter 3 5 
        |> random_select_filter 0.1
  | 2 -> possible_words 
        |> length_filter 3 6 
        |> random_select_filter 0.15
  | 3 -> possible_words 
        |> random_select_filter 0.20
  | 4 -> possible_words 
        |> random_select_filter 0.30
  | 5 -> possible_words 
        |> random_select_filter 0.60
  | _ -> possible_words

let diff_to_name (diff : int) : string = 
  let difficulty = 
    match diff with 
    | 0 -> "EASY"
    | 1 -> "NOVICE"
    | 2 -> "MEDIUM"
    | 3 -> "HARD"
    | 4 -> "INSANE"
    | 5 -> "IMPOSSIBLE"
    | _ -> ""
  in
  "COMPUTER (LEVEL : " ^ difficulty ^ ")"