let exchange k = k / 10 + k mod 10 * 10 ;;

let is_valid_answer (grand_father_age, grand_son_age) =
  (grand_son_age * 4  = grand_father_age) &&
  ((exchange grand_father_age) * 3 = (exchange grand_son_age));;

let find answer =
  let (max_grand_father_age, min_grand_son_age) = answer in
  let rec test_grand_son_age f s = if (is_valid_answer (f, s)) then (f, s)
                               else if s = min_grand_son_age then (-1, -1)
                               else test_grand_son_age f (s - 1) in
  let rec test_grand_father_age f s = 
    let result = test_grand_son_age f s in
    if result = (-1, -1) then
      test_grand_father_age (f + 1) s
    else if f > max_grand_father_age then
      (-1, -1)
    else result in
  test_grand_father_age min_grand_son_age max_grand_father_age;;
    
