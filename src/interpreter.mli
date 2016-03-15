type err

exception Tracked_exec_error of int * err

val err_to_str : err -> string

val get_err_msg : err -> string

val get_lexbuf : string -> Lexing.lexbuf

val run : Lexing.lexbuf -> string list -> unit
