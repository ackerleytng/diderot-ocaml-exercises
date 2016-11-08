type 'a clist =
  | CSingle of 'a
  | CApp of 'a clist * 'a clist
  | CEmpty

let example =
  CApp (CApp (CSingle 1,
              CSingle 2),
        CApp (CSingle 3,
              CApp (CSingle 4, CEmpty)))
       
let rec to_list = function
  | CSingle a -> [a]
  | CEmpty -> []
  | CApp (a, b) -> to_list a @ to_list b     
         
let rec of_list = function
  | [] -> CEmpty
  | r :: rs -> CApp (CSingle r, of_list rs)

let append a b = match a, b with
  | CEmpty, y -> y
  | y, CEmpty -> y
  | x, y -> CApp (x, y)

let rec hd = function
  | CEmpty -> None
  | CSingle a -> Some a
  | CApp (a, b) -> let a_hd = hd a in
                   match a_hd with
                   | None -> hd b
                   | Some x -> Some x

let rec tl = function
  | CEmpty -> None
  | CSingle a -> Some (CEmpty)
  | CApp (a, b) -> let a_tl = tl a in
                   match a_tl with
                   | None -> tl b
                   | Some CEmpty -> Some (CApp (CEmpty, b))
                   | Some h -> Some (CApp (h, b))
