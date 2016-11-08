type trie = Trie of int option * char_to_children
 and char_to_children = (char * trie) list

let empty = Trie (None, [])

let example =
  Trie (None,
        [('i', Trie (Some 11,
                     [('n', Trie (Some 5, [('n', Trie (Some 9, []))]))]));
         ('t',
          Trie (None,
                [('e',
                  Trie (None,
                        [('n', Trie (Some 12, [])); ('d', Trie (Some 4, []));
                         ('a', Trie (Some 3, []))]));
                 ('o', Trie (Some 7, []))]));
         ('A', Trie (Some 15, []))])
       
let rec children_from_char m c = match m with
  | [] -> None
  | (char, t) :: cs -> if char = c then Some t else children_from_char cs c
                                                                       
let rec update_children m c t = match m with
  | [] -> [(c, t)]
  | (c_orig, t_orig) :: cs -> if c_orig = c then ((c_orig, t) :: cs) else (c_orig, t_orig) :: (update_children cs c t)

let lookup trie w =
  let rec aux i t =
    if i >= String.length w then
      let Trie (value, _) = t in value
    else
      let char = String.get w i 
      and Trie (_, m) = t in
      match children_from_char m char with
      | None -> None
      | Some t' -> aux (i + 1) t'
  in
  aux 0 trie
      
let insert trie w v : trie =
  let rec aux t i : trie =
    if i >= String.length w then
      let Trie (_, m) = t in
      Trie (Some v, m)
    else
      let char = String.get w i
      and Trie (value, m) = t in
      Trie (value, update_children m char (aux (match children_from_char m char with
                                                | None -> empty
                                                | Some t -> t) (i + 1)))
  in
  aux trie 0

