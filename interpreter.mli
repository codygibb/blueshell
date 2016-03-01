type exec_error

exception Tracked_exec_error of int * exec_error

val get_err_msg : exec_error -> string

val get_lexbuf : string -> Lexing.lexbuf

val run : Lexing.lexbuf -> unit
