type t = 
  | Unit
  | Int of int
  | Bool of bool
  | Float of float
  | Str of string
  | Closure of (t Env.t * Ast.id list * Ast.stmt_list)
  | List of t Blu_list.t
  | Dict of t Blu_dict.t

let rec to_str = function
  | Unit -> "()"
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Float f -> Printf.sprintf "%f" f
  | Str s -> s
  | Closure _ -> "<func>"
  | List l ->
      Blu_list.to_str l ~v_to_str:(fun p ->
        match p with
        | Str s -> "\"" ^ s ^ "\""
        | p -> to_str p)
  | Dict d ->
      Blu_dict.to_str d ~v_to_str:(fun p ->
        match p with
        | Str s -> "\"" ^ s ^ "\""
        | p -> to_str p)

let type_str = function
  | Unit -> "unit"
  | Int _ -> "int"
  | Bool _ -> "bool"
  | Float _ -> "float"
  | Str _ -> "str"
  | Closure _ -> "func"
  | List _ -> "list"
  | Dict _ -> "dict"
