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

let print_board t = 
  failwith "Unimplemented"