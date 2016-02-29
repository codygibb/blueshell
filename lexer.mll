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
let str = ('"' [^'"']* '"') | ('\'' [^'\'']* '\'')
let bool_ = "true" | "false"
let comment = "#" [^'\n']*

rule read = parse
  | comment { read lexbuf }
  | whitespace { read lexbuf }
  | '\n' { next_line lexbuf; NEWLINE (lexbuf.lex_start_p.pos_lnum) }
  | "func" { FUNC }
  | "print" { PRINT }
  | "return" { RETURN }
  | "if" { IF }
  | "else" { ELSE }
  | bool_ { BOOL ((lexeme lexbuf) = "true") }
  | integer { INT (int_of_string (lexeme lexbuf)) }
  | id { ID (lexeme lexbuf) }
  | str (* Strip quote characters at end of string. *)
    { let s = lexeme lexbuf in
      STR (String.sub s 1 ((String.length s) - 2)) }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | "%" { MOD }
  | "==" { EQ }
  | "!=" { NE }
  | "<" { LT }
  | ">" { GT }
  | "<=" { LTE }
  | ">=" { GTE }
  | "!" { NOT }
  | "&&" { AND }
  | "||" { OR }
  | "&" { BITAND }
  | "|" { BITOR }
  | "^" { BITXOR }
  | "<<" { LEFTSHIFT }
  | ">>" { RIGHTSHIFT }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | ":=" { DEF }
  | "=" { ASGN }
  | "+=" { ADDASGN }
  | "-=" { SUBASGN }
  | "*=" { MULTASGN }
  | "/=" { DIVASGN }
  | "%=" { MODASGN }
  | "&&=" { BOOLANDASGN }
  | "||=" { BOOLORASGN }
  | "&=" { BITANDASGN }
  | "|=" { BITORASGN }
  | "^=" { BITXORASGN }
  | "<<=" { LEFTSHIFTASGN }
  | ">>=" { RIGHTSHIFTASGN }
  | "," { COMMA }
  | eof { EOF }
  | _ { raise (Unexpected_char (lexeme_start lexbuf)) }
