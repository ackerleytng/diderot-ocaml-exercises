type report = message list
and message = string * status
and status = Successful | Failed

type 'a result = Ok of 'a | Error of exn
  
let exn_to_string e = "placeholder function"
                                   
let exec f x =
  try 
    Ok (f x)
  with
    e -> Error e
       
let compare user reference to_string =
  let successful = if user = reference then Successful else Failed in
  let expected = match successful with Successful -> "correct " | _ -> "unexpected " in
  match user with
    Ok x -> ("got " ^ expected ^ "value " ^ (to_string x), successful)
  | Error e -> ("got " ^ expected ^ "exception " ^ (exn_to_string e), successful)
  
let test user reference sample to_string =
  let rec aux accum =
    if List.length accum = 10 then List.rev accum
    else
      let s = sample () in
      let user_result = exec user s and 
          reference_result = exec reference s in
      let r = compare user_result reference_result to_string in
      aux (r :: accum)
  in
  aux []
    
