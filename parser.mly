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
%token LPAREN RPAREN
%token LBRACE RBRACE
%token DEF
%token ASGN
%token FUNC
%token PRINT
%token RETURN
%token IF ELSE
%token COMMA
%token <int> NEWLINE
%token EOF

%nonassoc EQ NE

%nonassoc LT GT LTE GTE

%left OR
%left AND
%right NOT

%left PLUS MINUS
%left DIV TIMES

%start <Ast.stmt_list> prog

%%

prog:
  | sl=stmt_list; EOF { sl }

stmt_list:
  | s=stmt; lnum=NEWLINE; sl=stmt_list { (Some lnum, s) :: sl }
  | NEWLINE; sl=stmt_list { sl }
  | { [] }

stmt:
  | e=expr { Expr e }
  | x=ID; DEF; e=expr { Def (x, e) }
  | x=ID; ASGN; e=expr { Asgn (x, e) }
  | FUNC; f=ID; LPAREN; l=id_list; RPAREN; LBRACE; b=block; RBRACE
      { Def (f, Lambda (l, b)) }
  | PRINT; e=expr { Print e }
  | RETURN; e=expr { Return e }
  | IF; e=expr; LBRACE; tb=block; RBRACE; ELSE; LBRACE; fb=block; RBRACE
      { If_then_else (e, tb, fb) }
  | IF; e=expr; LBRACE; b=block; RBRACE { If_then_else (e, b, []) }

block:
  | s=stmt { [(None, s)] }
  | lnum=NEWLINE; s=stmt { [(Some (lnum + 1), s)] }
  | sl=stmt_list { sl }

expr:
  | i=INT { Int i }
  | b=BOOL { Bool b }
    (* Strip quote characters *)
  | s=STR { Str (String.sub s 1 ((String.length s) - 2)) }
  | x=ID { Id x }
  | LPAREN; e=expr; RPAREN { e }
  | FUNC; LPAREN; l=id_list; RPAREN; LBRACE; b=block; RBRACE { Lambda (l, b) }
  | e=expr; LPAREN; l=expr_list; RPAREN { Call (e, l) }
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

id_list:
  | l=nonempty_id_list { l }
  | { [] }

nonempty_id_list:
  | x=ID { [x] }
  | x=ID; COMMA; l=id_list { x :: l }

expr_list:
  | l=nonempty_expr_list { l }
  | { [] }

nonempty_expr_list:
  | e=expr { [e] }
  | e=expr; COMMA; l=expr_list { e :: l }
