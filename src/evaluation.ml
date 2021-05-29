open Syntax

exception Error of string

let err s = raise (Error s)

(* [x → t]exp *)
let rec substitute x t exp = match exp with
    Var id -> if x = id then t else exp
  | Abstraction (id, ex) -> 
    Abstraction (id, substitute x t ex)
  | Application (ex1, ex2) ->
    Application (substitute x t ex1, substitute x t ex2)

(* call-by-value beta reduction
  only outermost redexes are reduced and where a redex is reduced only when its right-hand side has already been reduced to a value
  (λx. t1) t2 -> [x → t2]t1
*)
let rec beta = function
    Application (Abstraction (x, t1), exp2) ->
      let t2 = beta exp2 in
      substitute x t2 t1
  | Application (exp1, exp2) ->
      let t = beta exp1 in
      Application (t, exp2)
  | e -> e

(* 
  TODO:
    - consider better way to detect stop condition
    - detect infinite reduction
    - set timeout
*)
let rec eval t =
  let b = beta t in
  if (string_of_exp b) = (string_of_exp t)
  then [t] else t :: (eval b)
