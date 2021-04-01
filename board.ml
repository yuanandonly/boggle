type board = {
  dim : int;
  board_letters : string list;
}

let init x = 
  let rand_chr () = 26 |> Random.int |> ( + ) 65 |> Char.chr |> Char.escaped in
  let rec loop i acc =
    if i = (x * x) then acc 
    else loop (i + 1) (rand_chr () :: acc)
  in
  {dim = x; board_letters = loop 0 []}

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