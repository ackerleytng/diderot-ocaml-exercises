module type DictSig = sig
  type ('key, 'value) t
  val empty : ('key, 'value) t
  val add : ('key, 'value) t -> 'key -> 'value -> ('key, 'value) t
  exception NotFound
  val lookup : ('key, 'value) t -> 'key -> 'value
  val remove : ('key, 'value) t -> 'key -> ('key, 'value) t
end ;;
  
module Dict : DictSig = struct
  type ('key, 'value) t =
    | Empty
    | Node of ('key, 'value) t * 'key * 'value * ('key, 'value) t
            
  let empty = Empty
            
  let rec add d k v =
    match d with
    | Empty -> Node (Empty, k, v, Empty)
    | Node (l, k', v', r) ->
       if k = k' then Node (l, k, v, r)
       else if k < k' then Node (add l k v, k', v', r)
       else Node (l, k', v', add r k v)
      
  exception NotFound
          
  let rec lookup d k = 
    match d with
    | Empty ->
       raise NotFound
    | Node (l, k', v', r) ->
       if k = k' then v'
       else if k < k' then lookup l k
       else lookup r k
      
  let rec minimum d k =
    match d with 
    | Node (Empty, k, v, _) -> (k, v)
    | Node (l, _, _, _) -> minimum l k
    | Empty -> raise NotFound
      
  let rec remove d k =
    match d with
    | Node (Empty, k', v', Empty) -> if k = k' then Empty else d
    | Node (Empty, k', v', r) -> if k = k' then r else Node (Empty, k', v', remove r k)
    | Node (l, k', v', Empty) -> if k = k' then l else Node (remove l k, k', v', Empty)
    | Node (l, k', v', r) ->
       if k = k' then 
         let (min_k, min_v) = minimum r k in
         Node (l, min_k, min_v, remove r min_k)
       else if k < k' then Node (remove l k, k', v', r)
       else Node (l, k', v', remove r k)
    | Empty -> d
end
