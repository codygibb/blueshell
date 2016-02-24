type id = string

and prog =
  | P_stmt of int * stmt * prog
  | P_exit

and block =
  | B_stmt of int option * stmt * block
  | B_end

and stmt =
  | Def of id * expr
  | Asgn of id * expr
  | Print of expr
  | Return of expr
  | If_then_else of expr * block * block

and expr =
  | Int of int
  | Bool of bool
  | Str of string
  | Id of id
  | Bin_op of binop * expr * expr
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Lambda of id list * block
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
