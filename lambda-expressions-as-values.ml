let rec last_element = function
  | [] -> invalid_arg "last_element"
  | [x] -> x
  | x :: xs -> last_element xs
                            
let rec is_sorted = function
  | ([] | [_]) -> true
  | x :: (y :: ys as rest) -> x < y && is_sorted rest
