open Syntax

exception Error of string

let err s = raise (Error s)

type strategy =
    Full
  | CallByValue

type option = {verbose: bool; strategy: strategy}

(* alpha reduction
  substitute id with id ^ "'"
*)
let rec alpha id exp = 
  let new_id = id ^ "'" in
  match exp with
    Var i -> if i = id then Var new_id else exp
  | Abstraction (i, e) -> if i = id then Abstraction (new_id, alpha id e) else Abstraction (i, alpha id e)
  | Application (e1, e2) -> Application (alpha id e1, alpha id e2)

(* whether the id is free var or not in t *)
let is_fv id t =
  let rec is_fv2 id t binded = match t with
      Var i -> i = id && not (List.exists ((=) id) binded)
    | Abstraction (i, t2) -> is_fv2 id t2 (i :: binded)
    | Application (e1, e2) -> is_fv2 id e1 binded && is_fv2 id e2 binded
  in is_fv2 id t []

(* [x → t]exp *)
let rec substitute x t exp = match exp with
    Var id -> if x = id then t else exp
  | Abstraction (id, ex) ->
    if id <> x && not (is_fv id t) then
    (* when id ≠ x and id ∉ FV(t) *)
    Abstraction (id, substitute x t ex) else
    (* when id = x or id ∈ FV(t) *)
    let new_abst = alpha id exp in
    substitute x t new_abst
  | Application (ex1, ex2) ->
    Application (substitute x t ex1, substitute x t ex2)

(* call-by-value beta reduction
  only outermost redexes are reduced and where a redex is reduced only when its right-hand side has already been reduced to a value
  - E-App1 (t1 → t1': t1 t2 → t1' t2)
  - E-App2 (t2 → t2': v1 t2 → v1 t2')
  - E-AppAbs ((λx.t12) v2 → [x → v2]t12)
*)
let rec call_by_value = function
    (* E-AppAbs *)
    Application (Abstraction (x1, t1), Abstraction (x2, t2)) -> 
      substitute x1 (Abstraction (x2, t2)) t1
    (* E-App2 *)
  | Application (Abstraction(x, t1), t2) -> Application (Abstraction(x, t1), call_by_value t2)
    (* E-App1 *)
  | Application (t1, t2) -> Application (call_by_value t1, t2)
  | e -> e

let full t =
  let rec full2 = function
      Application (Abstraction (x, t1), t2) -> substitute x t2 t1
    | Application (Var x, t) -> Application (Var x, full2 t)
    | Application (t1, t2) -> Application (full2 t1, t2)
    | Abstraction (x, t) -> Abstraction (x, full2 t)
    | t -> t
  in
  let b = call_by_value t in
  if (string_of_exp b) = (string_of_exp t) then full2 b else b

(*
  TODO:
    - consider better way to detect stop condition
    - detect infinite reduction
    - set timeout
*)
let eval env t option =
  let t1 = Environment.fold_right (fun (x, t) exp -> substitute x t exp) env t in
  let reduce = (match option.strategy with Full -> full | CallByValue -> call_by_value) in
  let rec eval1 t2 =
    let b = reduce t2 in
    if (string_of_exp b) = (string_of_exp t2) then t2 else
      (if option.verbose then print_endline ("β " ^ (string_of_exp t2));
      eval1 b)
  in eval1 t1
