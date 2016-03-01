type t = 
  | Unit
  | Int of int
  | Bool of bool
  | Float of float
  | Str of string
  | Closure of (t Env.t * Ast.id list * Ast.stmt_list)

let to_str = function
  | Unit -> "unit"
  | Int _ -> "int"
  | Bool _ -> "bool"
  | Float _ -> "float"
  | Str _ -> "str"
  | Closure _ -> "func"
