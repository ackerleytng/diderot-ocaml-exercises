type point  = { x : float; y : float; z : float }
type dpoint = { dx : float; dy : float; dz : float }
type physical_object = { position : point; velocity : dpoint }
                         
let move p dp = { 
    x = p.x +. dp.dx;
    y = p.y +. dp.dy;
    z = p.z +. dp.dz;
  }

let next obj = {
    position = move obj.position obj.velocity;
    velocity = obj.velocity;
  }
                 
let will_collide_soon p1 p2 =
  let { position = { x = n_p1_x; y = n_p1_y; z = n_p1_z }; _ } = next p1 and
      { position = { x = n_p2_x; y = n_p2_y; z = n_p2_z }; _ } = next p2 in
  sqrt ((n_p2_x -. n_p1_x) ** 2. +. (n_p2_y -. n_p1_y) ** 2. +. (n_p2_z -. n_p1_z) ** 2.) < 2.
