type 'a bt =
  | Empty
  | Node of 'a bt * 'a * 'a bt
                            
let rec height = function
  | Empty -> 0
  | Node (l, _, r) ->
     let l_height = height l and
         r_height = height r in
     1 + (max l_height r_height)

let rec balanced = function
  | Empty -> true
  | Node (l, _, r) ->
     let l_height = height l and
         r_height = height r in
     l_height = r_height && balanced l && balanced r
