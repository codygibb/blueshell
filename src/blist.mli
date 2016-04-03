exception Index_out_of_bounds of int

type 'a t

val create : ?min_cap:int -> 'a list -> 'a t

val get : 'a t -> int -> 'a

val set : 'a t -> int -> 'a -> unit

val push : 'a t -> 'a -> unit

val pop : 'a t -> 'a option

val len : 'a t -> int

val capacity : 'a t -> int

val to_str : 'a t -> v_to_str:('a -> string) -> string

val iter : 'a t -> f:('a -> unit) -> unit
