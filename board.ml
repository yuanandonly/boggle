type board = {
  dim : int;
  board_letters : string list;
}

type die = { letters : string list }

let _ = Random.self_init ()

let roll (d : die) : string =
  let face = Random.int 6 in
  List.nth d.letters face

let die_letters4 =
  "AACIOTAHMORSEGKLUYABILITYACDEMPEGINTVGILRUWELPSTUDENOSWACELRSABJMOQEEFHIYEHINPSDKNOTUADENVZBIFORZ"

let die_letters5 =
  "QBZJXKTOUOTOOVWRGRAAAFSRAUMEEGHHLRDONHDTHOLHNRODAFAISRYIFASRTELPCISSNSEURIYPRHDORDLNCCWNSTTTOTEMSCTIEPEANDNNMNNEAGUOTOWNAEAEEEYIFPSREEEEMAITITIEETILIC"

let q_convert (letter : string) : string =
  if letter = "Q" then "Qu" else letter

let die_init (die_num : int) (dim : int) : die =
  let die_letters = if dim = 4 then die_letters4 else die_letters5 in
  let rec die_init_helper (acc : string list) (count : int) :
      string list =
    match count with
    | -1 -> acc
    | x ->
        die_init_helper
          (Char.escaped die_letters.[count + (6 * die_num)] :: acc)
          (count - 1)
  in
  { letters = die_init_helper [] 5 }

let rec dice_helper (acc : die list) (die_num : int) (dim : int) :
    die list =
  if die_num < 0 then acc
  else dice_helper (die_init die_num dim :: acc) (die_num - 1) dim

let dice4 = dice_helper [] 15 4

let dice5 = dice_helper [] 24 5

let rec dice_randomizer (acc : int list) (count : int) (dim : int) =
  if count < 0 then acc
  else
    let rec find_num x =
      if not (List.mem x acc) then x
      else find_num ((x + 1) mod (dim * dim))
    in
    dice_randomizer
      ((dim * dim |> Random.int |> find_num) :: acc)
      (count - 1) dim

let init_classic (dim : int) =
  let dice_loc = dice_randomizer [] ((dim * dim) - 1) dim in
  let rec init_helper (acc : string list) (count : int) : string list =
    match count with
    | -1 -> acc
    | x ->
        let letter =
          count |> List.nth dice_loc
          |> List.nth (if dim = 4 then dice4 else dice5)
          |> roll
        in
        init_helper (letter :: acc) (count - 1)
  in
  { dim; board_letters = init_helper [] ((dim * dim) - 1) }

let manual_init (size : int) (letters : string) : board =
  if String.length letters - 1 <> size * size then
    failwith "invalid board"
  else
    let rec split (s : string) (acc : string list) =
      match String.length s with
      | 1 -> List.rev_append acc [ s ]
      | _ -> Char.escaped s.[0] :: acc
    in
    { dim = size; board_letters = split letters [] }

let rand_init (size : int) =
  let rand_letter () = 25 |> Random.int |> List.nth dice5 |> roll in
  let rec loop i acc =
    if i = size * size then acc else loop (i + 1) (rand_letter () :: acc)
  in
  { dim = size; board_letters = loop 0 [] }

let direc_list (x : int) : (int * int) list =
  [
    (-x - 1, 1);
    (-x, 1);
    (-x + 1, 1);
    (-1, 0);
    (1, 0);
    (x - 1, 1);
    (x, 1);
    (x + 1, 1);
  ]

let adj_check
    (ind : int)
    (comp_ind : int)
    (x : int)
    (direc : (int * int) list) : bool =
  let inc = comp_ind - ind in
  try
    Int.abs ((ind / x) - ((ind + inc) / x)) = List.assoc inc direc
    && ind + inc >= 0
    && ind + inc < x * x
  with Not_found -> false

let ( +++ ) (i : int) (j : int) : int list =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux j []

let adj_table (x : int) : int list array =
  let direc = direc_list x in
  let arr = Array.init (x * x) (fun x -> []) in
  let indices = 0 +++ ((x * x) - 1) in
  for index = 0 to (x * x) - 1 do
    let adj_list =
      List.filter (fun y -> adj_check index y x direc) indices
    in
    arr.(index) <- adj_list
  done;
  arr

let print_board t =
  let rec frame_loop str i acc =
    if i = t.dim then acc ^ "|" else frame_loop str (i + 1) (acc ^ str)
  in
  let rec letter_loop start i acc =
    if i = t.dim then acc ^ "|"
    else
      let letter = List.nth t.board_letters (start + i) in
      letter_loop start (i + 1)
        (acc ^ "| " ^ letter ^ if letter = "Qu" then "" else " ")
  in
  let top1 = " " ^ String.make ((t.dim * 4) - 1) '_' in
  let top2 = frame_loop "|   " 0 "" in
  let between = frame_loop "|---" 0 "" in
  let bottom = frame_loop "|___" 0 "" in
  let rec print_loop i =
    if i = t.dim then print_endline bottom
    else if i = 0 then
      let () = print_endline top1 in
      let () = print_endline top2 in
      let () = print_endline (letter_loop (i * t.dim) 0 "") in
      print_loop (i + 1)
    else
      let () = print_endline between in
      let () = print_endline (letter_loop (i * t.dim) 0 "") in
      print_loop (i + 1)
  in
  print_loop 0
