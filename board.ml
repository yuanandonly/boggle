type board = {
  dim : int;
  board_letters : string list;
}

type die = { letters : string list }

let roll (d : die) : string = List.nth d.letters (Random.int 6)

let die_letters4 =
  "AACIOTAHMORSEGKLUYABILITYACDEMPEGINTVGILRUWELPSTUDENOSWACELRSABJMOQEEFHIYEHINPSDKNOTUADENVZBIFORZ"

let die_init int =
  {
    letters =
      [
        Char.escaped die_letters4.[0 + (4 * int)];
        Char.escaped die_letters4.[1 + (4 * int)];
        Char.escaped die_letters4.[2 + (4 * int)];
        Char.escaped die_letters4.[3 + (4 * int)];
      ];
  }

let rec dice4_helper (acc : die list) (die_num : int) : die list =
  if die_num < 0 then acc
  else dice4_helper (die_init die_num :: acc) (die_num - 1)

let dice4 = dice4_helper [] 15

let rec rand_die_helper (acc : int list) (count : int) =
  if count < 0 then acc
  else
    let rec find_num x =
      if List.mem x acc then x else find_num ((x + 1) mod 16)
    in
    rand_die_helper ((16 |> Random.int |> find_num) :: acc) (count - 1)

let init4 =
  let rand4 = rand_die_helper [] 15 in
  let rec init4_helper (acc : string list) (count : int) : string list =
    match count with
    | -1 -> acc
    | x ->
        init4_helper
          ((count |> List.nth rand4 |> List.nth dice4 |> roll) :: acc)
          (count - 1)
  in
  { dim = 4; board_letters = init4_helper [] 15 }

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
  let rand_chr () =
    26 |> Random.int |> ( + ) 65 |> Char.chr |> Char.escaped
  in
  let rec loop i acc =
    if i = size * size then acc else loop (i + 1) (rand_chr () :: acc)
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

(** Requires: t.dim > 0 *)
let print_board t =
  (* frame_loop used to generate strings for the frame *)
  let rec frame_loop str i acc =
    if i = t.dim then acc ^ "|" else frame_loop str (i + 1) (acc ^ str)
  in
  (* letter_loop used to generate strings for lines with letters*)
  let rec letter_loop start i acc =
    if i = t.dim then acc ^ "|"
    else
      letter_loop start (i + 1)
        (acc ^ "| " ^ List.nth t.board_letters (start + i) ^ " ")
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
