exception Var_already_defined of Ast.id

exception Var_not_found of Ast.id

type 'a t

val create : unit -> 'a t

val extend : 'a t -> 'a t

val lookup : 'a t -> Ast.id -> 'a

val bind : 'a t -> Ast.id -> 'a -> unit
(* Raises [Var_already_defined] exception if id already defined *)

val update : 'a t -> Ast.id -> 'a -> unit
(* Raises [Var_not_found] exception if id not defined *)
