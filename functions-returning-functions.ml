let rec equal_on_common = function
  | [] -> fun _ -> true
  | x :: xs -> function
             | [] -> true
             | y :: ys -> x = y && equal_on_common xs ys
        
