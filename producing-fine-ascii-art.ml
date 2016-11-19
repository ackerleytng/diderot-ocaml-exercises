type image = int -> int -> bool

let all_white = fun x y -> false

let all_black = fun x y -> true

let checkers = fun x y -> y/2 mod 2 = x/2 mod 2

let square cx cy s = fun x y ->
  let minx = cx - s / 2 in
  let maxx = cx + s / 2 in
  let miny = cy - s / 2 in
  let maxy = cy + s / 2 in
  x >= minx && x <= maxx && y >= miny && y <= maxy

let disk cx cy r = fun x y ->
  let x' = x - cx in
  let y' = y - cy in
  (x' * x' + y' * y') <= r * r

type blend =
  | Image of image
  | And of blend * blend
  | Or of blend * blend
  | Rem of blend * blend
         
let display_image width height f_image =
  for y = 0 to height do
    (for x = 0 to width do
      print_string (if f_image x y then "#" else " ")
    done ; print_newline ())
  done
    
  
let rec render blend x y = match blend with 
    Image f -> f x y
  | And (ba, bb) -> (render ba x y) && (render bb x y)
  | Or (ba, bb) -> (render ba x y) || (render bb x y)
  | Rem (ba, bb) -> (render ba x y) && not (render bb x y)

let display_blend width height blend =
  display_image width height (fun x -> fun y -> render blend x y)

;;
  
display_blend 10 10 (Rem (Image all_black, Image (disk 5 5 5)))
