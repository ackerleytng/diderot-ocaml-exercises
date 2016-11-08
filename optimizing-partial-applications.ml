(*
 (cos a) (cos b) (cos c) = 0.5 (cos (a + b) + cos (a - b)) (cos c)
                         = 0.5 (cos (a + b) (cos c) + cos (a - b) (cos c)) 
                         = 0.25 (cos (a + b + c) + cos (a + b - c) + cos (a - b + c) + cos (a - b - c))

 But this seems to need one more application of cos, so...
 *)

let ccr = fun a -> 
  let step_0_res = 8. *. cos (a /. 2.) in
  fun b -> 
  let step_1_res = step_0_res *. cos (b /. 2.) in
  fun c -> 
  let step_2_res = step_1_res *. cos (c /. 2.) in
  fun s -> 
  s /. step_2_res

