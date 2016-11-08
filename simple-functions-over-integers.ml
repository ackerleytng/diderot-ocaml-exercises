let multiple_of n d = 
  let divided_float_of_int = float_of_int (n / d)
  and divided_float = (float_of_int n) /. (float_of_int d) in
  if d = 0 then false else divided_float_of_int = divided_float;;

let integer_square_root n = int_of_float (sqrt (float_of_int n));;
