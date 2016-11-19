let rec print_int_list = function
    [] -> ()
  | i :: is -> print_int i ; print_newline () ; print_int_list is
              
let rec print_every_other k l =
  let rec drop k l = if k = 0 then l
                   else if List.length l < k then [] 
                   else drop (k - 1) (List.tl l) 
  in
  match l with
    [] -> ()
  | i :: is -> print_int i ; print_newline () ; print_every_other k (drop k l)

let rec print_list print = function
    [] -> ()
  | x :: xs -> print x ; print_newline () ; print_list print xs
