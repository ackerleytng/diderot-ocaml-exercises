let x = Random.int 9 + 1 (* not 0 *)

let x_power_8 =
  let x_power_4 =
    let x_power_2 = x * x in
    x_power_2 * x_power_2 in
  x_power_4 * x_power_4
