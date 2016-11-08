let is_sorted a = 
  let rec aux i =
    if (Array.length a) - i < 2 then true
    else if a.(i) < a.(i + 1) then aux (i + 1)
    else false in
  aux 0

let find dict word =
  let rec aux l r = 
    if l > r then -1
    else 
      let m = (l + r)/2 in
        let value = dict.(m) in
          if value = word then m
          else if word < value then aux l (m - 1)
          else aux (m + 1) r in
  aux 0 ((Array.length dict) - 1)
