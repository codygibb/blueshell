open Core.Std
open Printf

let usage () =
  printf "Usage: %s [filename] [args]\n" Sys.argv.(0);
  exit 1

let () =
  let file = if Array.length Sys.argv < 2 then usage () else Sys.argv.(1) in
  let lexbuf = Interpreter.get_lexbuf file in
  try
    (* Ignore the first element in argv, i.e. the interpreter binary. *)
    let argv = match Array.to_list Sys.argv with 
    | _ :: argv -> argv 
    | [] -> failwith "empty Sys.argv"
    in
    Interpreter.run lexbuf argv
  with
  | Lexer.Error | Parser.Error ->
      Logger.log_syntax_err lexbuf;
      exit 1
  | Interpreter.Tracked_exec_error (lnum, err) ->
      Logger.log_exec_err err file lnum;
      exit 1

