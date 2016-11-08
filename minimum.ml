let min a = Array.fold_left min a.(0) a

let min_index a =
  let rec aux idx min m_idx =
    if idx = Array.length a then
      m_idx
    else if a.(idx) < min then
      aux (idx + 1) a.(idx) idx
    else
      aux (idx + 1) min m_idx in
  aux 0 a.(0) 0

let it_scales = "no"
