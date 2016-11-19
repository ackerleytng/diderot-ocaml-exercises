let is_multiple i x = i mod x = 0

let output_multiples x n m =
  for i = n to m do
    if is_multiple i x then
      (print_int i ; print_string ",")
  done

exception Is_zero
  
let display_sign_until_zero f m =
  try 
    for i = 0 to m do
      if (f i) < 0 then (print_string "negative" ; print_newline ())
      else if (f i) > 0 then (print_string "positive" ; print_newline ())
      else raise Is_zero
    done
  with
    Is_zero -> (print_string "zero" ; print_newline ())
