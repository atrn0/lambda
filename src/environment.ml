type 'a t = (Syntax.id * 'a) list

exception Not_bound
exception Bound_Several_Times of string

let empty = []
let extend x v env = (x,v)::env

let lookup x env =
  try List.assoc x env with Not_found -> raise Not_bound

let rec map f = function
    [] -> []
  | (id, v)::rest -> (id, f v) :: map f rest

let rec fold_right f env a =
  match env with
    [] -> a
  | hd::rest -> f hd (fold_right f rest a)

let rec from_list = function
  [] -> empty
| (id, v) :: tl -> 
  if List.mem_assoc id tl then
    raise (Bound_Several_Times
    ("Variable " ^ id ^ " is bound several times"))
  else
    extend id v (from_list tl)
