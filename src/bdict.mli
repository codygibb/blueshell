exception Key_not_found of string

type 'a t

val create : (string * 'a) list -> 'a t

val get : 'a t -> string -> 'a

val set : 'a t -> string -> 'a -> unit

val del : 'a t -> string -> unit

val len : 'a t -> int

val to_str : 'a t -> v_to_str:('a -> string) -> string
