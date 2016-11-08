type phone_number = int * int * int * int

type contact = {
  name : string;
  phone_number : phone_number
}

let nobody = { name = ""; phone_number = (0, 0, 0, 0) }

type database = {
  number_of_contacts : int;
  contacts : contact array;
}

let make max_number_of_contacts = {
  number_of_contacts = 0;
  contacts = Array.make max_number_of_contacts nobody
}

type query = {
  code : int;
  contact : contact;
}

let search db contact =
  let rec aux idx =
  if idx >= db.number_of_contacts then
    (false, db, nobody)
  else if db.contacts.(idx).name = contact.name then
    (true, db, db.contacts.(idx))
  else
    aux (idx + 1)
  in
  aux 0

let insert db contact =
  if db.number_of_contacts >= Array.length db.contacts then
    (false, db, nobody)
  else
    let (status, db, _) = search db contact in
    if status then (false, db, contact) else
      let cells i =
        if i = db.number_of_contacts then contact else db.contacts.(i)
      in
      let db' = {
        number_of_contacts = db.number_of_contacts + 1;
        contacts = Array.init (Array.length db.contacts) cells
      }
      in
      (true, db', contact)

let delete db contact =
  let (status, db, contact) = search db contact in
  if not status then (false, db, contact)
  else
    let cells i =
      if db.contacts.(i).name = contact.name then nobody
      else db.contacts.(i) in
    let db' = {
      number_of_contacts = db.number_of_contacts - 1;
      contacts = Array.init (Array.length db.contacts) cells
    }
  in
  (true, db', contact)

let engine db { code ; contact } =
  if code = 0 then insert db contact
  else if code = 1 then delete db contact
  else if code = 2 then search db contact
  else (false, db, nobody)

(* ====================================================================== *)
         
let proof_of_bug =
  [| 
    { code = 0 ; contact = { name = "user-0"; phone_number = (1, 2, 3, 4) } };
    { code = 0 ; contact = { name = "user-1"; phone_number = (5, 6, 7, 8) } };
    { code = 1 ; contact = { name = "user-0"; phone_number = (1, 2, 3, 4) } };
    { code = 2 ; contact = { name = "user-1"; phone_number = (5, 6, 7, 8) } };
  |]

let delete db contact =
  let (status, db, contact) = search db contact in
  if not status then (false, db, contact)
  else
    let move = 
      let rec iterate i list removed =
        if i >= Array.length db.contacts then Array.of_list (List.rev list)
        else if db.contacts.(i).name = contact.name || removed then
          iterate (i + 1) (true :: list) true
        else iterate (i + 1) (false :: list) false
      in
      iterate 0 [] false
    in
    let cells i = 
      if i + 1 >= Array.length db.contacts then nobody
      else if move.(i) then
        db.contacts.(i + 1)
      else db.contacts.(i)
    in
    let db' = {
      number_of_contacts = db.number_of_contacts - 1;
      contacts = Array.init (Array.length db.contacts) cells
    }
  in
  (true, db', contact)

let update db contact = 
  let (deleted, db', _) = delete db contact in
  insert db' contact

let engine db { code ; contact } =
  if code = 0 then insert db contact
  else if code = 1 then delete db contact
  else if code = 2 then search db contact
  else if code = 3 then update db contact
  else (false, db, nobody);;
