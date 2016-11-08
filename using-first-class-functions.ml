type int_ff = int -> int

let rec compose = function
  | [] -> (function x -> x)
  | f :: fs -> (function x -> f ((compose fs) x))

let rec fixedpoint f start delta =
  let y = f start in
  if (abs_float ((f y) -. y)) < delta
  then y
  else fixedpoint f y delta
