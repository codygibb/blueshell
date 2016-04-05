exception Tracked_exec_error of int * Err.t

val get_lexbuf : string -> Lexing.lexbuf

val run : Lexing.lexbuf -> string list -> unit
