let find a w =
  let rec aux i =
    if i >= Array.length a then None
    else if a.(i) = w then Some i
    else aux (i + 1)
  in
  aux 0

let default_int o = function
    Some x -> x
  | None -> 0

let merge x y = match x, y with
    None, None -> None
  | Some a, None -> Some a
  | None, Some b -> Some b
  | Some a, Some b -> Some (a + b)
