type 'a xlist =
  { mutable pointer : 'a cell }
and 'a cell =
  | Nil
  | List of 'a * 'a xlist

let nil () =
  { pointer = Nil }

let cons elt rest =
  { pointer = List (elt, rest) }

exception Empty_xlist
        
let head l =
  let { pointer = cell } = l in
  match cell with 
    Nil -> raise Empty_xlist
  | List (elt, _) -> elt
  
let tail l =
  let { pointer = cell } = l in
  match cell with 
    Nil -> raise Empty_xlist
  | List (_, xl) -> xl

let add a l =
  l.pointer <- List (a, { pointer = l.pointer })
  
let chop l = 
  let { pointer = cell } = l in
  match cell with 
    Nil -> raise Empty_xlist
  | List (_, { pointer = next }) -> l.pointer <- next

let rec append l l' =
  let { pointer = cell } = l in
  match cell with 
    Nil -> l.pointer <- l'.pointer
  | List (_, xl) -> append xl l'
                                  
let rec filter p l = match l with
    { pointer = Nil } -> ()
  | { pointer = List (a, { pointer = ptr }) } when not (p a) -> l.pointer <- ptr ; filter p l
  | { pointer = List (a, xl) } -> filter p xl
                  
