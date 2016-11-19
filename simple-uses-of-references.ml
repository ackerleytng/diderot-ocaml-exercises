exception Empty

let swap ra rb =
  let tmp = !ra in
  ra := !rb ; rb := tmp
  
let update r f =
  let tmp = !r in
  r := f tmp ; tmp

let move l1 l2 =
  let first_list = !l1 in
  if List.length first_list = 0 then raise Empty
  else
    let head = List.hd first_list in
    l1 := List.tl first_list ; l2 := head :: !l2 

let reverse l =
  let dst = ref [] and
      src = ref l in  
  try
    while true do
      move src dst
    done ; []
  with
    Empty -> !dst
