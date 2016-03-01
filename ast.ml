type id = string

type stmt_list = (int option * stmt) list

and stmt =
  | Expr of expr
  | Def of id * expr
  | Asgn of id * expr
  | Print of expr
  | Return of expr
  | If_then_else of expr * stmt_list * stmt_list

and expr =
  | Int of int
  | Bool of bool
  | Float of float
  | Str of string
  | Cast of cast_types * expr
  | Id of id
  | Bin_op of binop * expr * expr
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Ternary of expr * expr * expr
  | Func of id list * stmt_list
  | Call of expr * expr list

and binop =
  | Add | Sub | Mult | Div | Mod
  | Eq | Ne | Lt | Gt | Lte | Gte
  | Bit_and | Bit_or | Bit_xor | Left_shift | Right_shift

and cast_types =
  | Int_cast
  | Float_cast
  | Str_cast
  | Bool_cast

let to_str = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Ne -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Lte -> "<="
  | Gte -> ">="
  | Bit_and -> "&"
  | Bit_or -> "|"
  | Bit_xor -> "^"
  | Left_shift -> "<<"
  | Right_shift -> ">>"

let cast_type_to_str = function
  | Int_cast -> "int"
  | Bool_cast -> "bool"
  | Float_cast -> "float"
  | Str_cast -> "str"
