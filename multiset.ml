module type MultiSet_S = sig

  (* A multi-set of type ['a t] is a collection of values of
     type ['a] that may occur several times. *)
  type 'a t

  (* [occurrences s x] return the number of time [x] occurs
     in [s]. *)
  val occurrences : 'a t -> 'a -> int

  (* The empty set has no element. There is only one unique
     representation of the empty set. *)
  val empty : 'a t

  (* [insert s x] returns a new multi-set that contains all
     elements of [s] and a new occurrence of [x]. Typically,
     [occurrences s x = occurrences (insert s x) x + 1]. *)
  val insert : 'a t -> 'a -> 'a t

  (* [remove s x] returns a new multi-set that contains all elements
     of [s] minus an occurrence of [x] (if [x] actually occurs in
     [s]). Typically, [occurrences s x = occurrences (remove s x) x -
     1] if [occurrences s x > 0]. *)
  val remove : 'a t -> 'a -> 'a t

end

module MultiSet : MultiSet_S = struct
  type 'a t = ('a * int) list
            
  let occurrences set a = try 
      List.assoc a set
    with
      Not_found -> 0
                        
  let empty = []
            
  let insert set a =
    let times = occurrences set a in
    (a, times + 1) :: set
    
  let remove set a =
    List.remove_assoc a set
end

let letters word =
  let length = String.length word in
  let rec aux set i =
    if i = length then set
    else aux (MultiSet.insert set (String.get word i)) (i + 1)
  in
  aux MultiSet.empty 0

(* 
 * I would have added some sort of comparison function to the module but since that wouldn't 
 *   be accepted by the web ocaml environment, this should do
 *)
let anagram word1 word2 = 
  let word1_ms = letters word1 in
  let remove_all word ms =
    let length = String.length word in
    let rec aux set i =
      if i = length then set
      else aux (MultiSet.remove set (String.get word i)) (i + 1)
    in
    aux ms 0
  in
  let removed = remove_all word2 word1_ms in
  removed = MultiSet.empty
  
