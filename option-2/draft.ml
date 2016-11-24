type ltable = (string * string list) list

let is_alphabet char = 
  let code = (Char.code (Char.uppercase char)) in
  (code >= 0x41 && code <= 0x5a)
  
let keep_alphabets_and_spaces string =
  let buffer = Buffer.create (String.length string) in
  let operate c = if is_alphabet c || Char.code c = 0x20 then Buffer.add_char buffer c
                  else ()
  in
  String.iter operate string;
  Buffer.contents buffer
  
(* Given a string, builds the first word and returns (word, rest of string)
 *   Assumes that string has only spaces and alphabets, and is trimmed
 *)
let word string =
  let len = String.length string in
  if len = 0 then ("", "")
  else
    try
      let space_index = String.index string ' ' in
      (String.sub string 0 space_index, String.sub string (space_index + 1) (len - space_index - 1))
    with
      Not_found -> (string, "")
      
let words string =
  let rec aux accum string =
    match word string with
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
