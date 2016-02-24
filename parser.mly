%{
  open Ast 
%}

%token <int> INT
%token <bool> BOOL
%token <string> STR
%token <string> ID 
%token PLUS MINUS TIMES DIV
%token EQ NE LT GT LTE GTE
%token NOT AND OR
(* TODO: short circuit evaluation *)
(* %token AND OR NOT *)
%token LPAREN RPAREN
%token LBRACE RBRACE
%token DEF
%token ASGN
%token LAMBDA
%token FUNC
%token PRINT
%token RETURN
%token IF ELSE
%token COMMA
(*%token SEMI*)
%token <int> NEWLINE
%token EOF

%nonassoc LT GT LTE GTE

%left AND OR
%left PLUS MINUS
%left DIV TIMES

%start <Ast.prog> prog

%%

prog:
  | s=stmt; lnum=NEWLINE; p=prog { P_stmt (lnum, s, p) }
  | NEWLINE; p=prog { p }
  | EOF { P_exit }

stmt:
  | x=ID; DEF; e=expr { Def (x, e) }
  | x=ID; ASGN; e=expr { Asgn (x, e) }
  | FUNC; f=ID; LPAREN; a=arg_names; RPAREN; LBRACE; b=block; RBRACE
      { Def (f, Lambda (a, b)) }
  | PRINT; e=expr { Print e }
  | RETURN; e=expr { Return e }
  | IF; e=expr; LBRACE; tb=block; RBRACE; ELSE; LBRACE; fb=block; RBRACE
      { If_then_else (e, tb, fb) }
  | IF; e=expr; LBRACE; b=block; RBRACE
      { If_then_else (e, b, B_end) }

block:
  | s=stmt; lnum=NEWLINE; b=block { B_stmt (Some (lnum), s, b) }
  | lnum=NEWLINE; s=stmt { B_stmt ((Some (lnum + 1)), s, B_end) }
  | NEWLINE; b=block { b }
  | s=stmt { B_stmt (None, s, B_end) }
  | { B_end }

expr:
  | i=INT { Int i }
  | b=BOOL { Bool b }
  | s=STR { Str (String.sub s 1 ((String.length s) - 2)) } (* Strip "" characters *)
  | x=ID { Id x }
  | e1=expr; PLUS; e2=expr { Bin_op (Add, e1, e2) }
  | e1=expr; MINUS; e2=expr { Bin_op (Sub, e1, e2) }
  | e1=expr; TIMES; e2=expr { Bin_op (Mult, e1, e2) }
  | e1=expr; DIV; e2=expr { Bin_op (Div, e1, e2) }
  | e1=expr; EQ; e2=expr { Bin_op (Eq, e1, e2) }
  | e1=expr; NE; e2=expr { Bin_op (Ne, e1, e2) }
  | e1=expr; LT; e2=expr { Bin_op (Lt, e1, e2) }
  | e1=expr; GT; e2=expr { Bin_op (Gt, e1, e2) }
  | e1=expr; LTE; e2=expr { Bin_op (Lte, e1, e2) }
  | e1=expr; GTE; e2=expr { Bin_op (Gte, e1, e2) }
  | NOT; e=expr { Not e }
  | e1=expr; AND; e2=expr { And (e1, e2) }
  | e1=expr; OR; e2=expr { Or (e1, e2) }
  | LAMBDA; LPAREN; a=arg_names; RPAREN; LBRACE; b=block; RBRACE { Lambda (a, b) }
  | e=expr; LPAREN; a=arg_vals; RPAREN { Call (e, a) }
  | LPAREN; e=expr; RPAREN { e }

arg_names:
  | x=ID { [x] }
  | x=ID; COMMA; a=arg_names { x :: a }

arg_vals:
  | e=expr { [e] }
  | e=expr; COMMA; a=arg_vals { e :: a }
