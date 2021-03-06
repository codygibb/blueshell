open Core.Std
open Printf

type t = 
  | Nil
  | Int of int
  | Bool of bool
  | Float of float
  | Str of string
  | Closure of (t Env.t * Ast.id list * Ast.stmt_list)
    (* Object primitive and method name. *)
  | Builtin_method of t * Ast.id
    (* Takes an argument list and returns a primitive. *)
  | Builtin_func of (t list -> t)
  | List of t Blist.t
  | Dict of t Bdict.t
  | Tuple of t list

let tuple_to_str t v_to_str =
  let len = List.length t in
  sprintf "(%s)" (List.foldi t ~init:"" ~f:(fun i acc p ->
    let s = v_to_str p in
    if i < len - 1 then acc ^ s ^ ", "
    else acc ^ s
  ))

let rec to_str = function
  | Nil -> "nil"
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Float f -> Printf.sprintf "%f" f
  | Str s -> Util.unescape s
  | Closure _ -> "<func>"
  | Builtin_method _ -> "<func>"
  | Builtin_func _ -> "<func>"
  | List l ->
      Blist.to_str l ~v_to_str:(fun p ->
        match p with
        | Str s -> "\"" ^ s ^ "\""
        | p -> to_str p)
  | Dict d ->
      Bdict.to_str d ~v_to_str:(fun p ->
        match p with
        | Str s -> "\"" ^ s ^ "\""
        | p -> to_str p)
  | Tuple t -> tuple_to_str t to_str

let rec type_str = function
  | Nil -> "nil"
  | Int _ -> "int"
  | Bool _ -> "bool"
  | Float _ -> "float"
  | Str _ -> "str"
  | Closure _ -> "func"
  | Builtin_method _ -> "func"
  | Builtin_func _ -> "func"
  | List _ -> "list"
  | Dict _ -> "dict"
  | Tuple t -> tuple_to_str t type_str

let list_builtins = String.Set.of_list ["push"; "pop"; "len"]
let str_builtins = String.Set.of_list ["len"; "substr"; "split"]
