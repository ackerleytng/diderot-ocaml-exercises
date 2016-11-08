type operation =
    Op of string * operation * operation
  | Value of int

type env = (string * (int -> int -> int)) list
                                          
let rec lookup_function n = function
  | [] -> invalid_arg "lookup_function"
  | (x, f) :: xs when x = n -> f
  | x :: xs -> lookup_function n xs

let add_function name op env = 
  (name, op) :: env

let initial_env = []
let my_env = ("min", (fun x y -> if x < y then x else y)) :: initial_env

let rec compute env = function
  | Value x -> x
  | Op (string, op1, op2) -> (lookup_function string env) (compute env op1) (compute env op2)
  
