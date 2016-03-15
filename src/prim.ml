open Core.Std
open Printf

type t = 
  | Unit
  | Int of int
  | Bool of bool
  | Float of float
  | Str of string
  | Closure of (t Env.t * Ast.id list * Ast.stmt_list)
  | Builtin_method of t * Ast.id
  | List of t Blu_list.t
  | Dict of t Blu_dict.t
  | Tuple of t list
  | Object of Blu_object.t

let tuple_to_str t v_to_str =
  let len = List.length t in
  sprintf "(%s)" (List.foldi t ~init:"" ~f:(fun i acc p ->
    let s = v_to_str p in
    if i < len - 1 then acc ^ s ^ ", "
    else acc ^ s
  ))

let rec to_str = function
  | Unit -> "()"
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Float f -> Printf.sprintf "%f" f
  | Str s -> s
  | Closure _ -> "<func>"
  | Builtin_method _ -> "<func>"
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
  | Tuple t -> tuple_to_str t to_str
  | Object o -> "<object>"

let rec type_str = function
  | Unit -> "unit"
  | Int _ -> "int"
  | Bool _ -> "bool"
  | Float _ -> "float"
  | Str _ -> "str"
  | Closure _ -> "func"
  | Builtin_method _ -> "func"
  | List _ -> "list"
  | Dict _ -> "dict"
  | Tuple t -> tuple_to_str t type_str

let list_builtins = String.Set.of_list ["push"; "pop"; "len"]
let str_builtins = String.Set.of_list ["len"; "substr"; "split"]
