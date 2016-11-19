let rotate a =
  let n = Array.length a in
  if Array.length a = 0 then ()
  else 
    let v = a.(0) in
    (for i = 0 to n-2 do
      a.(i) <- a.(i+1)
    done ;
    a.(n-1) <- v)

let rotate_by a times =
  let t = if times < 0 then Array.length a + times else times in
  for i = 0 to t - 1 do
    rotate a
  done
