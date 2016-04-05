exception Index_out_of_bounds of int

exception Invalid_slice of int * int

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

val norm_slice_exn : int -> int -> int -> int
(* Awkward helper method which is used for both list and string slicing.
 * `norm_slice_exn start stop len` normalizes the stop index and checks
 * the bounds of the slice, raising Index_out_of_bounds or Invalid_slice
 * if necessary. *)

val slice : 'a t -> int -> int -> 'a t

val to_list : 'a t -> 'a list
