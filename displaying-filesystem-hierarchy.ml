type filesystem =
  (string * node) list
and node =
  | File
  | Dir of filesystem
  | Symlink of string list
             
let rec print_path = function
    [] -> ()
  | [p] -> print_string p
  | p :: ps -> print_string p ; print_string "/" ; print_path ps
              
let rec print_file lvl name =
  if lvl <= 0 then (print_string name)
  else (print_string "| " ; print_file (lvl - 1) name)
  
let rec print_symlink lvl name path =
  if lvl <= 0 then (print_string name ; print_string " -> " ; print_path path)
  else (print_string "| " ; print_symlink (lvl - 1) name path)

let rec print_dir lvl name =
  if lvl <= 0 then (print_string "/" ; print_string name)
  else (print_string "| " ; print_dir (lvl - 1) name)
  
let print_filesystem root =
  let rec print_filesystem lvl items =
    let print_node lvl = function
        name, File -> print_file lvl name
      | name, Dir fs -> print_dir lvl name ; print_newline () ; print_filesystem (lvl + 1) fs
      | name, Symlink path -> print_symlink lvl name path
    in
    match items with 
      [] -> ()
    | [i] -> print_node lvl i
    | i :: is -> print_node lvl i ; print_newline () ; print_filesystem lvl is
  in
  print_filesystem 0 root

let resolve sym path =
  let rec resolve acc = function
      ".." :: ps -> if List.length acc = 0 
                    then resolve acc ps (* This part is kind of wrong, but for this exercise... *)
                    else resolve (List.tl acc) ps
    | [] -> List.rev acc
    | ps -> resolve (List.hd ps :: acc) (List.tl ps)
  in
  resolve (List.tl (List.rev sym)) path  

let rec file_exists (root: filesystem) path = match root, path with
    _, [] -> true
  | (identifier, (Dir fs)) :: rest, p :: ps when identifier = p -> file_exists fs ps
  | (identifier, File) :: rest, [p] when identifier = p -> true
  | (identifier, _) :: rest, path -> file_exists rest path
  | _, _ -> false

let print_filesystem root =
  let rec print_filesystem cwd items =
    let print_node cwd = function
        name, File -> print_file (List.length cwd) name
      | name, Dir fs -> print_dir (List.length cwd) name ; print_newline () ; print_filesystem (cwd @ [name]) fs
      | name, Symlink sym -> 
         let symlink_path = cwd @ [name] in
         let resolved_path = resolve symlink_path sym in
         print_symlink (List.length cwd) name 
                       (if file_exists root resolved_path 
                        then sym
                        else ["INVALID"])
    in
    match items with 
      [] -> ()
    | [i] -> print_node cwd i
    | i :: is -> print_node cwd i ; print_newline () ; print_filesystem cwd is
  in
  print_filesystem [] root
