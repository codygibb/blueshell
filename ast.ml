type id = string

type stmt_list = (int option * stmt) list

and stmt =
  | Expr of expr
  | Def of id * expr
  | Asgn of id * expr
  | Print of expr
  | Return of expr
  | If_then_else of expr * stmt_list * stmt_list
  | Set of expr * expr * expr
  | Within of expr * stmt_list
  | While of expr * stmt_list
  | For of id * expr * stmt_list

and expr =
  | Int of int
  | Bool of bool
  | Float of float
  | Str of string
  | Cast of type_cast * expr
  | Typeof of expr
  | Id of id
  | Bin_op of binop * expr * expr
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Ternary of expr * expr * expr
  | Func of id list * stmt_list
  | Call of expr * expr list
  | List of expr list
  | Dict of (string * expr) list
  | Get of expr * expr

and binop =
  | Add | Sub | Mult | Div | Mod
  | Eq | Ne | Lt | Gt | Lte | Gte
  | Bit_and | Bit_or | Bit_xor | Left_shift | Right_shift

and type_cast =
  | Int_cast
  | Float_cast
  | Str_cast
  | Bool_cast

let binop_to_str = function
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

let type_cast_to_str = function
  | Int_cast -> "int"
  | Bool_cast -> "bool"
  | Float_cast -> "float"
  | Str_cast -> "str"
