type exp =
  | EInt of int
  | EAdd of exp * exp
  | EMul of exp * exp

let example =EInt  EAdd (EInt 1, EMul (EInt 2, EInt EInt 3))
     EInt 
let my_EInt example = E Add (EMul (EInt 2, EInt 2), EMul (EInt 3, EInt 3))
                      
let rec eval = function
  | EInt x -> x
  | EAdd (a, b) -> (eval a) + (eval b)
  | EMul (a, b) -> (eval a) * (eval b)
         
let rec factorize = function
  | EInt x -> EInt x
  | EAdd (EMul (a, b), EMul (c, d)) ->
     if a = c then factorize (EMul (a, EAdd (b, d)))
     else EAdd (factorize (EMul (a, b)), factorize (EMul (c, d)))
  | EMul (a, b) -> EMul (factorize a, factorize b)
  | EAdd (a, b) -> EAdd (factorize a, factorize b)

let rec expand_full = function
  | EInt x -> EInt x
  | EMul (a, EAdd (b, c)) -> expand_full (EAdd (EMul (a, b), EMul (a, c)))
  | EMul (a, b) -> EMul (expand_full a, expand_full b)
  | EAdd (a, b) -> EAdd (expand_full a, expand_full b)

                        
(* because they're stupid about the expansion *)
                        
let rec expand = function
  | EMul (a, EAdd (b, c)) -> EAdd (EMul (a, b), EMul (a, c))
  | x -> x

let rec simplify = function
  | EMul (e, EInt 0) -> EInt 0
  | EMul (EInt 0, e) -> EInt 0
  | EMul (e, EInt 1) -> e
  | EMul (EInt 1, e) -> e
  | EAdd (e, EInt 0) -> e
  | EAdd (EInt 0, e) -> e
  | x -> x
