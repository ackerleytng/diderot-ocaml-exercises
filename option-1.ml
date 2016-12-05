exception NotFound

let rec loop p f x =
  if p x then x
  else loop p f (f x)
  
let exists p l = 
  List.exists p l

let find p l =
  try
    List.find p l
  with
    Not_found -> raise NotFound

(* Part A: A Generic Problem Solver *)
               
type 'e rel = 'e -> 'e list

let near (x : int) : int list = 
  Array.init 5 (fun i -> x - 2 + i) |> Array.to_list

let rec flat_map (f : 'e rel) : 'e list -> 'e list = function
    [] -> []
  | x :: xs -> (f x) @ flat_map f xs
  
let rec iter_rel (rel : 'e rel) (times : int) : 'e rel =
  if times <= 1 then rel
  else fun x -> flat_map rel (iter_rel rel (times - 1) x)

type 'e prop = 'e -> bool
             
let solve (r : 'a rel) (p : 'a prop) (x : 'a) =
  let rec aux list =
    try
      find p list
    with
      NotFound -> aux (flat_map r list)
  in
  aux [x]

let solve_path (r : 'a rel) (p : 'a prop) (x : 'a) =
  let solution = solve r p x in
  let rec aux level = 
    solve near (fun k -> exists p (iter_rel r level k)) x
  in
  aux 0
