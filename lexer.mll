{
  open Lexing
  open Parser

  exception Unexpected_char of int

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with
        pos_bol = lexbuf.lex_curr_pos;
        pos_lnum = pos.pos_lnum + 1
      }
}

let whitespace = [' ' '\t']+
let digit = ['0'-'9']
let integer = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = (letter | '_') (letter | digit | '_')*
let str = '"' [^'"']* '"'
let bool_ = "true" | "false"

rule read = parse
  | '\n' { next_line lexbuf; NEWLINE (lexbuf.lex_start_p.pos_lnum) }
  | whitespace { read lexbuf }
  | "lambda" { LAMBDA }
  | "func" { FUNC }
  | "print" { PRINT }
  | "return" { RETURN }
  | "if" { IF }
  | "else" { ELSE }
  | bool_ { BOOL ((lexeme lexbuf) = "true") }
  | integer { INT (int_of_string (lexeme lexbuf)) }
  | id { ID (lexeme lexbuf) }
  | str { STR (lexeme lexbuf) }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | "==" { EQ }
  | "!=" { NE }
  | "<" { LT }
  | ">" { GT }
  | "<=" { LTE }
  | ">=" { GTE }
  | "!" { NOT }
  | "&&" { AND }
  | "||" { OR }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | ":=" { DEF }
  | "=" { ASGN }
  | "," { COMMA }
  | eof { EOF }
  | _ { raise (Unexpected_char (lexeme_start lexbuf)) }
