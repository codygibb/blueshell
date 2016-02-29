type id = string

type stmt_list = (int option * stmt) list

and stmt =
  | Expr of expr
  | Def of id * expr
  | Asgn of id * expr
  | Print of expr
  | Return of expr
  | If_then_else of expr * stmt_list * stmt_list
  | Op_Asgn of id * opasgn * expr

and expr =
  | Int of int
  | Bool of bool
  | Str of string
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
  | BitAnd | BitOr | BitXor | LeftShift | RightShift

and opasgn =
  | AddAsgn | SubAsgn | MultAsgn | DivAsgn | ModAsgn
  | BoolAndAsgn | BoolOrAsgn
  | BitAndAsgn | BitOrAsgn | BitXorAsgn | LeftShiftAsgn | RightShiftAsgn

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
  | BitAnd -> "&"
  | BitOr -> "|"
  | BitXor -> "^"
  | LeftShift -> "<<"
  | RightShift -> ">>"
