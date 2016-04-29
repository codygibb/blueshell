open Core.Std
open Printf

let get_str lexbuf =
  let n = (Lexing.lexeme_end lexbuf) - (Lexing.lexeme_start lexbuf) in
  String.init n (Lexing.lexeme_char lexbuf)

let tab_aligned_spacing s =
  let len = String.length s in
  let new_s = String.make len ' ' in
  List.iteri (String.to_list s) ~f:(fun i c -> if c = '\t' then String.set new_s i '\t');
  new_s

let get_line file offset =
  let ic = In_channel.create file in
  In_channel.seek ic offset;
  let line = match In_channel.input_line ic with
  | Some l -> l
  | None -> failwith "no line at offset"
  in
  In_channel.close ic;
  line

let nth_line file n =
  let ic = In_channel.create file in
  let rec aux i =
    let line = match In_channel.input_line ic with
    | Some l -> l
    | None -> failwith "n > num lines in file"
    in
    if i = n then line else aux (i + 1)
  in
  let line = aux 1 in
  In_channel.close ic;
  line

let log_syntax_err lexbuf =
  let open Lexing in
  let p = lexbuf.lex_start_p in
  let line = get_line p.pos_fname (Int_conversions.int_to_int64 p.pos_bol) in
  let col = p.pos_cnum - p.pos_bol in
  eprintf "File \"%s\", line %d, column %d:\n\n" p.pos_fname p.pos_lnum col;
  eprintf "  %s\n" line;
  eprintf "  %s^\n" (tab_aligned_spacing (String.slice line 0 col));
  eprintf "Syntax error\n";
  ()

let log_exec_err err file lnum =
  let line = nth_line file lnum in
  eprintf "File \"%s\", line %d:\n\n" file lnum;
  eprintf "  %s\n" line;
  eprintf "\nError: %s\n" (Err.user_msg err);
  ()
