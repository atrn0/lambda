
type 'a t

exception Not_bound
exception Bound_Several_Times of string

val empty : 'a t
val extend : Syntax.id -> 'a -> 'a t -> 'a t
val lookup : Syntax.id -> 'a t -> 'a
val map : ('a -> 'b) -> 'a t -> 'b t
val fold_right : ((Syntax.id * 'a) -> 'b -> 'b) -> 'a t -> 'b -> 'b
val from_list : (Syntax.id * 'a) list -> 'a t
