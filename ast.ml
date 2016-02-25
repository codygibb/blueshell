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
  | Str of string
  | Id of id
  | Bin_op of binop * expr * expr
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Lambda of id list * stmt_list
  | Call of expr * expr list

and binop =
  | Add | Sub | Mult | Div
  | Eq | Ne | Lt | Gt | Lte | Gte

let to_str = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Eq -> "=="
  | Ne -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Lte -> "<="
  | Gte -> ">="
