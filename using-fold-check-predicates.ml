let for_all p l =
  List.fold_left (fun acc v -> p v && acc) true l

let exists p l =
  List.fold_left (fun acc v -> p v || acc) false l

let sorted cmp = function
  | [] | [_] -> true
  | x :: y :: xs ->
     match (List.fold_left 
              (fun acc v -> match acc with
                            | None -> None
                            | Some (prev, prev_comparison_result) -> 
                               let comparison_result = cmp prev v in
                               if comparison_result >= 0 && prev_comparison_result >= 0 then Some (v, comparison_result) 
                               else if comparison_result <= 0 && prev_comparison_result <= 0 then Some (v, comparison_result) 
                               else if comparison_result = 0 && prev_comparison_result = 0 then Some (v, comparison_result) 
                               else None)
              (Some (y, (cmp x y)))
              xs) with
     | None -> false
     | Some _ -> true
