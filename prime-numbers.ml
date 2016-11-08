let rec gcd n m = if m = 0 then n else gcd m (n mod m);;

let rec multiple_upto n r =
  let remainder_zero a b = a mod b = 0 in
  if r = 1 then false else (if remainder_zero n r then true else multiple_upto n (r - 1));;

let is_prime n =
  if n <= 1 then
    false
  else if n <= 3 then
    true
  else let rec check x = if x >= n then true
                     else if n mod x = 0 then false
                     else check (x + 1) in
       check 2;;
      
