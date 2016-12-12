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
  if times < 1 then (fun x -> [x])
  else fun x -> flat_map rel (iter_rel rel (times - 1) x)

type 'e prop = 'e -> bool
             
(* 
 * solve takes a generator function, a checker function, and an initial state
 *
 * In each step, the checker function is applied on the possible states.
 * If the solution is among the possible states, the problem is solved.
 * Otherwise, the generator is applied and the possible states are checked again.
 *)
let solve (r : 'a rel) (p : 'a prop) (x : 'a) =
  let rec aux list =
    try
      find p list
    with
      NotFound -> aux (flat_map r list)
  in
  aux [x]

(*
 * solve_path takes a generator function, a checker function, and an initial state
 *
 * In each step, we try to solve a question.
 * In the initial step, the question is:
 *   What is the answer if you iteratively apply the generator 0 times (identity function)
 *   This will get us the final solution
 * In the next step, the question is:
 *   What is the answer if you apply the generator once?
 * Then, 
 *   _____________________________________________ twice?
 * And so on.
 * In this way, the path is built up.
 *)
let solve_path (r : 'a rel) (p : 'a prop) (x : 'a) =
  let rec aux accum level =
    let solution = solve r (fun k -> exists p (iter_rel r level k)) x in
    if solution = x then solution :: accum
    else aux (solution :: accum) (level + 1)
  in
  aux [] 0

type ('a, 'set) set_operations =
  { empty : 'set ;
    mem : 'a -> 'set -> bool ;
    add : 'a -> 'set -> 'set }

let archive_map (opset : ('a, 'set) set_operations) (r : 'a rel) (s, l) =
  let results = flat_map r l in
  let l' = List.filter (fun e -> not (opset.mem e s)) results in
  let s' = List.fold_left (fun s e -> opset.add e s) s l' in
  (s', l')

(* 
 * solve' takes a set of functions for manipulating a set of elements, 
 *   a generator function, a checker function, and an initial state
 *
 * In each step, the checker function is applied on the 
 *   POSSIBLE new states that have NEVER BEEN CHECKED.
 * If the solution is among those states, the problem is solved.
 * Otherwise, the generator is applied and checking is done again.
 *)
let solve' (opset : ('a, 'set) set_operations) (r : 'a rel) p x =
  let rec aux (s, l) =
    try
      find p l
    with
      NotFound -> aux (archive_map opset r (s, l))
  in
  aux (opset.empty, [x])

(* 
 * solve_path' takes a set of functions for manipulating a list of elements,
 *   a generator function, a checker function, and an initial state
 *
 * I suspect the element list in the set are paths that have been checked before,
 *   so the set is actually used to constrain the paths chosen
 *
 * In each step, the checker function is applied on the 
 *   POSSIBLE new states that have NEVER BEEN CHECKED.
 * If the solution is among those states, the problem is solved.
 * Otherwise, the generator is applied and checking is done again.
 *)
let solve_path' (opset : ('a list, 'set) set_operations) (r : 'a rel) p x =
  let rec aux accum level =
    let solution = solve' opset r (fun k -> exists p (iter_rel r level k)) x in
    if solution = x then solution :: accum
    else aux (solution :: accum) (level + 1)
  in
  aux [] 0
