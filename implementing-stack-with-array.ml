type stack = int array
exception Full
exception Empty
        
let create size =
  Array.make (size + 1) 0
  
let push buf elt =
  let stack_size = buf.(0) in
  if stack_size >= (Array.length buf) - 1 then
    raise Full
  else
    let new_stack_size = stack_size + 1 in
    buf.(new_stack_size) <- elt ; buf.(0) <- new_stack_size

let append buf arr =
  for i = (Array.length arr) - 1 downto 0 do
    push buf arr.(i)
  done

let pop buf =
  let stack_size = buf.(0) in
  if stack_size <= 0 then
    raise Empty
  else
    buf.(0) <- stack_size - 1 ; buf.(stack_size)
