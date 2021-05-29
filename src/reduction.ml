open Syntax

exception Error of string

let err s = raise (Error s)

let rec string_of_exp = function
    Var id -> id
  | Abstraction (id, exp) -> "(" ^ "λ" ^ id ^ ". " ^ (string_of_exp exp) ^ ")"
  | Application (exp1, exp2) -> 
    "(" ^ (string_of_exp exp1) ^ " " ^ (string_of_exp exp2) ^ ")"

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
  | e -> e
