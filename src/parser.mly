%{
  open Ast 
  open Lexing
%}

%token <int> INT
%token <bool> BOOL
%token <float> FLOAT
%token <string> STR
%token <string> ID 
%token <string> SHELLCALL
%token BOOLCAST INTCAST FLOATCAST STRCAST
%token TYPEOF
%token ADDASGN SUBASGN MULTASGN DIVASGN MODASGN BOOLANDASGN BOOLORASGN BITANDASGN BITORASGN BITXORASGN LEFTSHIFTASGN RIGHTSHIFTASGN
%token PLUS MINUS TIMES DIV MOD
%token EQ NE LT GT LTE GTE
%token BITAND BITOR BITXOR LEFTSHIFT RIGHTSHIFT
%token NOT AND OR
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET
%token DEF
%token ASGN
%token FUNC
%token PRINT
%token RETURN
%token IF ELSE
%token CD
%token WHILE
%token FOR
%token IN
%token COMMA
%token COLON
%token SEMICOLON
%token QUESTIONMARK
%token DOT
%token TRY
%token NEWLINE
%token EOF

%right QUESTIONMARK COLON

%left OR
%left AND

%left BITOR
%left BITXOR
%left BITAND

%nonassoc EQ NE
%nonassoc LT GT LTE GTE

%left LEFTSHIFT RIGHTSHIFT
%left PLUS MINUS
%left DIV TIMES

%right TYPEOF

%right NOT

%start <Ast.stmt_list> prog
%start <Ast.expr> inline_expr

%%

prog:
  | sl=stmt_list; EOF { sl }

inline_expr:
  | e=expr; SEMICOLON { e }

stmt_list:
  | s=stmt; NEWLINE; sl=stmt_list { ($startpos.pos_lnum, s) :: sl }
  | NEWLINE; sl=stmt_list { sl }
  | { [] }

stmt:
  | e=expr { Expr e }
  | x=ID; DEF; e=expr { Def (x, e) }
  | x=ID; ASGN; e=expr { Asgn (x, e) }
  | FUNC; f=ID; LPAREN; l=id_list; RPAREN; LBRACE; b=block; RBRACE
      { Def (f, Func (l, b)) }
  | PRINT; e=expr { Print e }
  | RETURN; e=expr { Return e }
  | IF; e=expr; LBRACE; tb=block; RBRACE; eif=eif_list
      { If_then_else (e, tb, eif) }
  | c=expr; LBRACKET; k=expr; RBRACKET; ASGN; v=expr
      { Set (c, k, v) }
  | x=ID; o=bin_op_asgn; e=expr { Asgn (x, Bin_op(o, Id x, e)) }
  | x=ID; BOOLANDASGN; e=expr { Asgn (x, And (Id x, e)) }
  | x=ID; BOOLORASGN; e=expr { Asgn (x, Or (Id x, e)) }
  | CD; e=expr; LBRACE; b=block; RBRACE { Cd (e, b) }
  | WHILE; e=expr; LBRACE; b=block; RBRACE { While (e, b) }
  | FOR; x=ID; IN; e=expr; LBRACE; b=block; RBRACE { For (x, e, b) }
  | x=ID; DEF; sc=SHELLCALL { Def (x, Captured_shellcall sc) }
  | x=ID; ASGN; sc=SHELLCALL { Asgn (x, Captured_shellcall sc) }
  | sc=SHELLCALL { Shellcall sc }
  | out=ID; COMMA; err=ID; DEF; TRY; sc=SHELLCALL
      { Multi_def ([out; err], Try_shellcall sc) }
  | out=ID; COMMA; err=ID; ASGN; TRY; sc=SHELLCALL
      { Multi_asgn ([out; err], Try_shellcall sc) }

eif_list:
  | ELSE; IF; e=expr; LBRACE; b=block; RBRACE; eif=eif_list
      { [($startpos.pos_lnum, If_then_else (e, b, eif))] }
  | ELSE; LBRACE; b=block; RBRACE { b }
  | { [] }

(* Blocks are distinct from stmt_lists because they can be inlined. *)
block:
  | s=stmt { [($startpos.pos_lnum, s)] }
  | NEWLINE; s=stmt { [($endpos.pos_lnum, s)] }
  | sl=stmt_list { sl }

expr:
  | i=INT { Int i }
  | b=BOOL { Bool b }
  | f=FLOAT { Float f }
  | s=STR { Str s }
  | x=ID { Id x }
  | t=typecast; LPAREN; e=expr; RPAREN { Cast(t, e) }
  | TYPEOF; e=expr { Typeof(e) }
  | LPAREN; e=expr; RPAREN { e }
  | FUNC; LPAREN; l=id_list; RPAREN; LBRACE; b=block; RBRACE { Func (l, b) }
  | e=expr; LPAREN; l=expr_list; RPAREN { Call (e, l) }
  | e=expr; DOT; x=ID { Field_lookup (e, x) }
  | LBRACKET; l=expr_list; RBRACKET { List l }
  | LBRACE; l=kv_list; RBRACE { Dict l }
  | c=expr; LBRACKET; k=expr; RBRACKET { Get (c, k) }
  | c=expr; LBRACKET; start=expr; COLON; stop=expr; RBRACKET
     { Slice (c, Some start, Some stop) }
  | c=expr; LBRACKET; start=expr; COLON; RBRACKET
     { Slice (c, Some start, None) }
  | c=expr; LBRACKET; COLON; stop=expr; RBRACKET
     { Slice (c, None, Some stop) }
  | LPAREN; l=expr_list; RPAREN { Tuple l }
  | e1=expr; PLUS; e2=expr { Bin_op (Add, e1, e2) }
  | e1=expr; MINUS; e2=expr { Bin_op (Sub, e1, e2) }
  | e1=expr; TIMES; e2=expr { Bin_op (Mult, e1, e2) }
  | e1=expr; DIV; e2=expr { Bin_op (Div, e1, e2) }
  | e1=expr; MOD; e2=expr { Bin_op (Mod, e1, e2) }
  | e1=expr; EQ; e2=expr { Bin_op (Eq, e1, e2) }
  | e1=expr; NE; e2=expr { Bin_op (Ne, e1, e2) }
  | e1=expr; LT; e2=expr { Bin_op (Lt, e1, e2) }
  | e1=expr; GT; e2=expr { Bin_op (Gt, e1, e2) }
  | e1=expr; LTE; e2=expr { Bin_op (Lte, e1, e2) }
  | e1=expr; GTE; e2=expr { Bin_op (Gte, e1, e2) }
  | e1=expr; BITAND; e2=expr { Bin_op (Bit_and, e1, e2) }
  | e1=expr; BITOR; e2=expr { Bin_op (Bit_or, e1, e2) }
  | e1=expr; BITXOR; e2=expr { Bin_op (Bit_xor, e1, e2) }
  | e1=expr; LEFTSHIFT; e2=expr { Bin_op (Left_shift, e1, e2) }
  | e1=expr; RIGHTSHIFT; e2=expr { Bin_op (Right_shift, e1, e2) }
  | NOT; e=expr { Not e }
  | e1=expr; AND; e2=expr { And (e1, e2) }
  | e1=expr; OR; e2=expr { Or (e1, e2) }
  | e1=expr; QUESTIONMARK; e2=expr; COLON e3=expr { Ternary (e1, e2, e3) }


bin_op_asgn:
  | ADDASGN { Add }
  | SUBASGN { Sub }
  | MULTASGN { Mult }
  | DIVASGN { Div }
  | MODASGN { Mod }
  | BITANDASGN { Bit_and }
  | BITORASGN { Bit_or }
  | BITXORASGN { Bit_xor }
  | LEFTSHIFTASGN { Left_shift }
  | RIGHTSHIFTASGN { Right_shift }

typecast:
  | BOOLCAST { Bool_cast }
  | INTCAST { Int_cast }
  | FLOATCAST { Float_cast }
  | STRCAST { Str_cast }

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

kv_list:
  | l=nonempty_kv_list { l }
  | { [] }

nonempty_kv_list:
  | kv=key_val { [kv] }
  | kv=key_val; COMMA; l=kv_list { kv :: l }

key_val:
  | k=STR; COLON; v=expr { (k, v) }
