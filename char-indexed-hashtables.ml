module type GenericTrie = sig
  type 'a char_table
  type 'a trie = Trie of 'a option * 'a trie char_table
  val empty : unit -> 'a trie
  val insert : 'a trie -> string -> 'a -> 'a trie
  val lookup : 'a trie -> string -> 'a option
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
    let Trie (_, table) = trie in
    Trie (Some v, table)
               
  let rec insert (trie : 'a trie) w v =
    let len = String.length w in
    if len <= 0 then add_value trie v
    else 
      let c = String.get w 0 and
          Trie (this_v, hash_table) = trie in
      try
        let next_trie = CharHashtbl.find hash_table c and
            new_hash_table = CharHashtbl.copy hash_table in
        CharHashtbl.replace new_hash_table c (insert next_trie (String.sub w 1 (len -1)) v);
        Trie (this_v, new_hash_table)
      with
        Not_found -> let new_hash_table = CharHashtbl.copy hash_table in
                     CharHashtbl.add new_hash_table c (insert (empty ()) (String.sub w 1 (len -1)) v);
                     Trie (this_v, new_hash_table)
                          
  let lookup trie w =
    let len = String.length w and
        Trie (this_v, this_table) = trie in
    if len <= 0 then this_v
    else 
      let c = String.get w 0 in
      try
        let next_trie = CharHashtbl.find hash_table c and
            new_hash_table = CharHashtbl.copy hash_table in
        CharHashtbl.replace new_hash_table c (insert next_trie (String.sub w 1 (len -1)) v);
        Trie (this_v, new_hash_table)
      with
        Not_found -> let new_hash_table = CharHashtbl.copy hash_table in
                     CharHashtbl.add new_hash_table c (insert (empty ()) (String.sub w 1 (len -1)) v);
                     Trie (this_v, new_hash_table)
end
                                     
