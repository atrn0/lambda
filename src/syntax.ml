type id = string

type exp =
    Var of id
  | Abstraction of id * exp
  | Application of exp * exp

let rec string_of_exp = function
    Var id -> id
  | Abstraction (id, exp) -> "(" ^ "λ" ^ id ^ ". " ^ (string_of_exp exp) ^ ")"
  | Application (exp1, exp2) -> 
    "(" ^ (string_of_exp exp1) ^ " " ^ (string_of_exp exp2) ^ ")"
