type index = Index of int

let read a index = match index with
  | Index k -> a.(k);;

let inside a index = match index with
  | Index k -> k < Array.length a && k >= 0

let next index = match index with
  | Index k -> Index (k + 1)

let min_index a =
  let rec aux i min min_index =
    if not (inside a i) then min_index
    else if read a i < min then
      aux (next i) (read a i) i
    else aux (next i) min min_index
  in
  aux (Index 0) (read a (Index 0)) (Index 0)
    
