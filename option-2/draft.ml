type ltable = (string * string list) list

let is_alphabet char = 
  let code = (Char.code (Char.uppercase char)) in
  (code >= 0x41 && code <= 0x5a)

let keep_if predicate string =
  let buffer = Buffer.create (String.length string) in
  let operate c = if predicate c then Buffer.add_char buffer c
                  else ()
  in
  String.iter operate string;
  Buffer.contents buffer

let keep_alphabets_and_spaces string =
  keep_if (fun c -> is_alphabet c || Char.code c = 0x20) string

(* Given a string, builds the first word and returns (word, rest of string)
 *   Assumes that string has only spaces and alphabets, and is trimmed
 *)
let split_at char string =
  let len = String.length string in
  if len = 0 then ("", "")
  else
    try
      let space_index = String.index string char in
      (String.sub string 0 space_index, String.sub string (space_index + 1) (len - space_index - 1))
    with
      Not_found -> (string, "")
      
let words string =
  let rec aux accum string =
    match split_at ' ' string with
      ("", "") -> List.rev accum
    | ("", rest) -> aux accum rest
    | (w, rest) -> aux (w :: accum) rest
  in
  string |> String.trim |> keep_alphabets_and_spaces |> aux []

let build_ltable string_list =
  let operate ((accum, prev) : ltable * string) (s : string) =
    try
      let old_list = List.assoc prev accum in
      let removed = List.remove_assoc prev accum in
      (((prev, s :: old_list) :: removed), s)
    with
      Not_found -> (((prev, [s]) :: accum), s) 
  in
  let aux sl =
    List.fold_left operate ([], (List.hd sl)) (List.tl sl)
  in
  let result, _ = aux (("START" :: string_list) @ ["STOP"]) in
  result
  
let rec print_string_list = function
    [] -> ()
  | x :: xs -> print_string x; print_string "; "; print_string_list xs
  
let next_in_ltable lt str = 
  let results = List.assoc str lt in
  let len = List.length results in
  List.nth results (Random.int len)

let walk_ltable lt =
  let rec aux accum str = 
    let next = next_in_ltable lt str in
    if next = "STOP" then List.rev accum
    else aux (next :: accum) next
  in
  aux [] "START"

(* ------------------------------------------------------------------------------------------ *)
 
type distribution =
  { total : int ;
    amounts : (string * int) list }
type htable = (string, distribution) Hashtbl.t
            
let count_first = function
    [] -> ("", 0, [])
  | x :: xs -> let rest = List.filter (fun e -> e <> x) xs in
               (x, ((List.length xs) + 1 - (List.length rest)), rest)

let compute_distribution list : distribution =
  let rec aux accum list =
    if List.length list = 0 then accum
    else let e, count, rest = count_first list in
         aux ((e, count) :: accum) rest
  in
  { total = List.length list ; amounts = (aux [] (List.sort compare list)) }
   
type sl_htable = (string, string list) Hashtbl.t
  
let build_htable words : htable =
  let operate ((accum, prev) : sl_htable * string) (s : string) =
    try
      let old_list = Hashtbl.find accum prev in
      Hashtbl.replace accum prev (s :: old_list);
      (accum, s)
    with
      Not_found -> Hashtbl.add accum prev [s];
                   (accum, s) 
  in
  let aux sl =
    List.fold_left operate (Hashtbl.create 10, (List.hd sl)) (List.tl sl)
  in
  let result, _ = aux (("START" :: words) @ ["STOP"]) in
  let htable = Hashtbl.create 10 in
  Hashtbl.iter (fun k v -> Hashtbl.add htable k (compute_distribution v)) result;
  htable

let rec print_probabilities probs =
  List.iter (fun (k, v) -> print_string k ; print_string ":"; print_float v; print_newline ()) probs

(* https://oroboro.com/non-uniform-random-numbers/ *) 
let compute_probabilities { total = total; amounts = amounts } =
  let len = List.length amounts in
  List.map 
    (fun (word, count) -> (word, ((float_of_int count) /. (float_of_int total) *. (float_of_int (len - 1)))))
    amounts
   
let next_in_htable htable word : string =
  let distribution = Hashtbl.find htable word in
  match compute_probabilities distribution with
    [] -> ""
  | (x_str, x_prob) :: xs ->
     let len = List.length xs in
     if len = 0 then x_str
     else
       let (str, prob) = List.nth xs (Random.int len) in
       if Random.float 1.0 <= prob then str
       else x_str

let rec print_assoc_list = function 
    [] -> ()
  | (k, v) :: l -> print_string k ; print_string ":" ; print_int v ; print_string " " ; print_assoc_list l

let print_distribution { total = total ; amounts = amounts } = print_assoc_list amounts 

let print_htable (ht : htable) =
  Hashtbl.iter (fun k l -> print_string k ; print_string "-->" ; print_distribution l ; print_newline ()) ht

let walk_htable ht =
  let rec aux accum str = 
    let next = next_in_htable ht str in
    if next = "STOP" then List.rev accum
    else aux (next :: accum) next
  in
  aux [] "START"

(* ------------------------------------------------------------------------------------------ *)
  
type ptable =
  { prefix_length : int ;
    table : (string list, distribution) Hashtbl.t }

let is_sentence_terminator = function
    '?' | '!' | '.' -> true
    | _ -> false
  
let is_number char = 
  let code = (Char.code (Char.uppercase char)) in
  (code >= 48 && code <= 57)
           
let is_non_ascii_character char = 
  let code = (Char.code (Char.uppercase char)) in
  (code >= 128 && code <= 255)
           
let is_single_punctuation_word = function
    ';' | ',' | ':' | '-' | '"' | '\'' -> true
    | x -> is_sentence_terminator x

let split_at_predicate predicate string join_splitting_character =
  let accum = ref [] and 
      last_index = ref 0 in
  let add_last_string result : string list =
    let len = String.length string in
    if !last_index = len - 1 then result
    else (String.sub string !last_index (len - !last_index)) :: result
  in
  String.iteri (fun i x -> if predicate x 
                           then 
                             (if join_splitting_character then
                                accum := (String.sub string !last_index (i - !last_index + 1)) :: !accum
                              else 
                                (let split_character = (String.sub string i 1) in
                                 accum := split_character 
                                          :: (String.sub string !last_index (i - !last_index)) 
                                          :: !accum)
                             ; last_index := i + 1)
                           else ()) string ;
  !accum |> add_last_string |> List.rev

let should_collect c = 
  is_alphabet c || is_number c || is_non_ascii_character c
             
let get_word string =
  let len = String.length string in
  let buffer = Buffer.create len in
  let rec aux i =
    if i >= len then i
    else 
      let c = String.get string i in
      if should_collect c then (Buffer.add_char buffer c; aux (i + 1))
      else i
  in
  let index = aux 0 in
  if len = 0 then ("", "")
  else 
    let first_char = String.get string 0 in
    if is_single_punctuation_word first_char then ((String.sub string 0 1), 
                                                                 (String.sub string 1 (len - 1)))
    else if not (should_collect first_char) then ("", String.sub string 1 (len - 1))
    else (Buffer.contents buffer, 
          if index = len then ""
          else String.sub string index (len - index))

let get_all_words str =
  let rec aux accum = function
      "", "" -> List.rev accum
    | "", rest -> aux accum (get_word rest)
    | x, rest -> aux (x :: accum) (get_word rest)
  in
  aux [] ("", str)
                   
let sentences string =
  let unsplit_sentences = split_at_predicate is_sentence_terminator string true and
      filter_empty sl = List.filter (fun s -> s <> "" && s <> " " && s <> "\n") sl 
  in
  List.map
    (fun s -> get_all_words s |> filter_empty)
    (filter_empty unsplit_sentences)
  
let rec start i =
  if i = 0 then []
  else "START" :: start (i - 1)

let shift list str = match list with
    [] -> [str]
  | x :: xs -> xs @ [str]
  
let split string_list len =
  let rec split accum remainder =
    if List.length accum = len then accum, remainder
    else split (accum @ [List.hd remainder]) (List.tl remainder)
  in
  split [] string_list
  
type sl_sl_htable = (string list, string list) Hashtbl.t
             
let build_ptable string_list prefix_length : ptable =
  let operate ((accum, prev_list, prev) : sl_sl_htable * string list * string) (s : string) =
    let key = shift prev_list prev in
    try
      let old_list = Hashtbl.find accum key in
      Hashtbl.replace accum key (s :: old_list);
      (accum, key, s)
    with
      Not_found -> Hashtbl.add accum key [s];
                   (accum, key, s) 
  in
  let result, _, _ = List.fold_left operate 
                                    (Hashtbl.create 10, (start prefix_length), "START") 
                                    (string_list @ ["STOP"])
  in
  let table = Hashtbl.create 10 in
  Hashtbl.iter (fun k v -> Hashtbl.add table k (compute_distribution v)) result;
  { prefix_length = prefix_length ; table = table }
  
let print_ptable ({ prefix_length = _ ; table = table } : ptable) =
  Hashtbl.iter (fun k l -> print_string_list k ; print_string "-->" ; print_distribution l ; print_newline ()) table

let walk_ptable { prefix_length = prefix_length ; table = table } =
  let rec aux accum sl = 
    let next = next_in_htable table sl in
    if next = "STOP" then 
      if Random.bool () then List.rev accum
      else aux accum (start prefix_length)
    else aux (next :: accum) (shift sl next)
  in
  aux [] (start prefix_length)

let add_amount list (str, amt) =
  try
    let orig_amt = List.assoc str list in
    (str, orig_amt + amt) :: (List.remove_assoc str list)
  with
    Not_found -> (str, amt) :: list

let add_amounts list_1 list_2 =
  List.fold_left add_amount list_1 list_2
  
let merge_distributions { total = total_1 ; amounts = amounts_1 }
                        { total = total_2 ; amounts = amounts_2 } =
  { total = total_1 + total_2 ; amounts = add_amounts amounts_1 amounts_2 }
  
let add_table_entry table sl distribution =
  try
    let orig_distribution = Hashtbl.find table sl in
    Hashtbl.replace table sl (merge_distributions orig_distribution distribution)
  with
    Not_found -> Hashtbl.add table sl distribution

let merge_tables table_1 table_2 =
  let new_table = Hashtbl.copy table_1 in
  Hashtbl.iter (add_table_entry new_table) table_2 ;
  new_table

exception Incompatible_ptables
  
let merge_ptable { prefix_length = prefix_length_1 ; table = table_1 }
                 { prefix_length = prefix_length_2 ; table = table_2 } =
  if prefix_length_1 = prefix_length_2 then
    { prefix_length = prefix_length_1 ; table = merge_tables table_1 table_2 }
  else
    raise Incompatible_ptables
  
exception No_ptables_to_merge
  
let merge_ptables = function
    [] -> raise No_ptables_to_merge
  | [p] -> p
  | p :: pl -> List.fold_left merge_ptable p pl
