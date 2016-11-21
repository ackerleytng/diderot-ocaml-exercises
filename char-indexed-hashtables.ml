module type GenericTrie = sig
  type 'a char_table
  type 'a trie = Trie of 'a option * 'a trie char_table
  val empty : unit -> 'a trie
  val insert : 'a trie -> string -> 'a -> 'a trie
  (*  val lookup : 'a trie -> string -> 'a option *)
end
                        
module CharHashedType =
  struct
    type t = char
    let equal i j = i = j
    let hash i = Char.code i
  end

module CharHashtbl = Hashtbl.Make(CharHashedType)

module Trie : GenericTrie
  with type 'a char_table = 'a CharHashtbl.t = 
struct
  type 'a char_table = 'a CharHashtbl.t
  type 'a trie = Trie of 'a option * 'a trie char_table
                       
  let empty () = Trie (None, CharHashtbl.create 5)
               
  let add_value trie v = 
    let _, table = trie in
    Trie (v, table)
               
  let rec insert trie w v =
    let len = String.length w in
    if len <= 0 then trie
    else if len = 1 then 
    else 
      let c = String.get w 0 and
          _, hash_table = trie in
      try
        let old_trie = CharHashtbl.find hash_table c in
      with
        Not_found -> CharHashtbl.add 
                       (CharHashtbl.copy hash_table) 
                       c 
                       (insert (empty ()) (String.sub 1 (len -1)) v)
          
end
                                     
