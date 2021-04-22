type board = {
  dim : int;
  board_letters : string list;
}

let init x = 
  let rand_chr () = 26 
  |> Random.int 
  |> ( + ) 65 
  |> Char.chr 
  |> Char.escaped in
    let rec loop i acc =
      if i = (x * x) then acc 
      else loop (i + 1) (rand_chr () :: acc)
    in
    {dim = x; board_letters = loop 0 []}

let direc_list (x : int) : ((int * int) list) =
  [ (-x - 1, 1);
    (-x, 1); 
    (-x + 1, 1); 
    (-1, 0); 
    (1, 0);
    (x - 1, 1);
    (x, 1);
    (x + 1, 1)]  

let adj_check (ind : int) (comp_ind : int) (x : int) 
(direc : ((int * int) list)) : bool = 
  let inc = comp_ind - ind in
  try 
    (Int.abs(ind / x - (ind + inc)/x) = List.assoc inc direc 
      && ind + inc >= 0 
      && ind + inc < x * x) 
  with Not_found -> false

let (+++) (i : int) (j : int) : int list = 
  let rec aux n acc =
    if n < i then acc else aux (n - 1) (n :: acc)
in aux j [] ;;

let adj_table (x : int) : int list array =
  let direc = direc_list x in
  let arr = Array.init (x * x) (fun x-> []) in
  let indices = 0 +++ (x*x - 1) in
  for index = 0 to x * x - 1 do
    let adj_list = List.filter
      (fun y -> adj_check index y x direc) indices in
    Array.set arr index adj_list
  done;
  arr

(** Requires: t.dim > 0 *)
let print_board t = 
  (* frame_loop used to generate strings for the frame *)
  let rec frame_loop str i acc = 
    if i = t.dim then acc ^ "|"
    else frame_loop str (i + 1) (acc ^ str) in
  (* letter_loop used to generate strings for lines with letters*)
  let rec letter_loop start i acc =
    if i = t.dim then acc ^ "|"
    else letter_loop start (i + 1) (acc ^ "| " ^ (List.nth t.board_letters (start + i)) ^ " ") in
  let top1 = " " ^ String.make (t.dim * 4 - 1) '_' in
  let top2 = frame_loop "|   " 0 "" in
  let between = frame_loop "|---" 0 "" in
  let bottom = frame_loop "|___" 0 "" in
  let rec print_loop i =
    if i = t.dim then print_endline bottom
    else if i = 0 then let () = print_endline top1 in
                        let () = print_endline top2 in
                        let () = print_endline (letter_loop (i * t.dim) 0 "") in 
                        print_loop (i + 1)
    else let () = print_endline between in
          let () = print_endline (letter_loop (i * t.dim) 0 "") in
          print_loop(i + 1) in
print_loop 0