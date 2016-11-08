let filter p l = 
  List.fold_left (fun accumulator element -> if p element then element :: accumulator else accumulator) [] l

let partition p l =
  List.fold_right 
    (fun element accumulator -> 
      let (lpos, lneg) = accumulator in
      if p element then (element :: lpos, lneg) else (lpos, element :: lneg)) 
    l 
    ([], [])

let rec sort = function
  | [] -> []
  | h :: r ->
     let smaller, larger = partition (fun v -> v <= h) r in
     (sort smaller) @ h :: (sort larger)
