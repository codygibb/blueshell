open Core.Std
open Printf

(* TODO: move in_channels to Core.Std *)

let usage () =
  printf "Usage: %s [filename]\n" Sys.argv.(0);
  exit 1

let get_str lexbuf =
  let n = (Lexing.lexeme_end lexbuf) - (Lexing.lexeme_start lexbuf) in
  String.init n (Lexing.lexeme_char lexbuf)

let tab_aligned_spacing s =
  let len = String.length s in
  let new_s = String.make len ' ' in
  List.iteri (String.to_list s) ~f:(fun i c -> if c = '\t' then String.set new_s i '\t');
  new_s

let get_line file offset =
  let ic = open_in file in
  seek_in ic offset;
  let line = input_line ic in
  close_in ic;
  line

let nth_line file n =
  let ic = open_in file in
  let rec aux i =
    let line = input_line ic in
    if i = n then line else aux (i + 1)
  in
  let line = aux 1 in
  close_in ic;
  line

let syntax_err lexbuf =
  let open Lexing in
  let p = lexbuf.lex_start_p in
  let line = get_line p.pos_fname p.pos_bol in
  let col = p.pos_cnum - p.pos_bol in
  eprintf "File \"%s\", line %d, column %d:\n\n" p.pos_fname p.pos_lnum col;
  eprintf "  %s\n" line;
  eprintf "  %s^\n" (tab_aligned_spacing (String.slice line 0 col));
  eprintf "Syntax error\n";
  exit 1

let exec_err err file lnum =
  let line = nth_line file lnum in
  eprintf "File \"%s\", line %d:\n\n" file lnum;
  eprintf "  %s\n" line;
  eprintf "\nError: %s\n" (Interpreter.get_err_msg err);
  exit 1

let () =
  let file = if Array.length Sys.argv != 2 then usage () else Sys.argv.(1) in
  let lexbuf = Interpreter.get_lexbuf file in
  try
    Interpreter.run lexbuf
  with
  | Lexer.Error | Parser.Error -> syntax_err lexbuf
  | Interpreter.Tracked_exec_error (lnum, err) -> exec_err err file lnum
