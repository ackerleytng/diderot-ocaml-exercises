let wrap l =
  List.map (fun x -> [x]) l
           
type 'a tree = 
  | Node of 'a tree * 'a * 'a tree
  | Leaf of 'a;;

let rec tree_map f = function
  | Leaf x -> Leaf (f x)
  | Node (l, x, r) -> Node ((tree_map f l), (f x), (tree_map f r))
