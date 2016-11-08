type queue = int list * int list

let is_empty = function
  | [], [] -> true
  | _ -> false

let enqueue x (front, back) = (front, x :: back)

(*

Write a function split : int list -> int list * int list

such that split l = (foo, bar) where l = bar @ List.rev foo 

and the length of bar and foo is List.length l / 2 or List.length l / 2 + 1

 *)
      
let split l = 
  let valid (a, b) = 
    let a_len = List.length a and
        b_len = List.length b in
    a_len = b_len || abs (a_len - b_len) = 1
  in
  let rec aux s =
    if valid s then
      let (foo, bar) = s in
      (List.rev bar, List.rev foo)
    else
      let (foo, bar) = s in
      match bar with
      | [] -> (foo, bar)
      | x :: xs -> aux (x :: foo, xs)
  in
  aux ([] , l)

let dequeue = function
  | ([], []) -> (0, ([], []))
  | ([], back) -> (let queue = List.rev back in
                   match queue with
                   | [] -> (0, ([], []))
                   | x :: xs -> (x, ([], List.rev xs)))
  | (x :: xs, back) -> (x, (xs, back))
