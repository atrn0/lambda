type id = string

type exp =
    Var of id
  | Abstraction of id * exp
  | Application of exp * exp
