type id = string

(* TODO: Each statement and expression operator will have a
 * line number attached by the parser, such that any exec
 * errors that occur on said operator will know the exact
 * line number. This will be done by wrapping stmt and expr
 * with tagged_stmt and tagged_expr, which factor out line
 * number tracking out of the business logic of the already
 * existing stmt/expr evaluation.
 * *)

(* Each statement is tagged with a line number. *)
type stmt_list = (int * stmt) list

and stmt =
  | Expr of expr
  | Def of id * expr
  | Asgn of id * expr
  | Multi_def of id list * expr
  | Multi_asgn of id list * expr
  | Print of expr
  | Return of expr
  | If_then_else of expr * stmt_list * stmt_list
  | Set of expr * expr * expr
  | Cd of expr * stmt_list
  | While of expr * stmt_list
  | For of id * expr * stmt_list
  | Shellcall of string

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
  | Field_lookup of expr * id
  | List of expr list
  | Dict of (string * expr) list
  | Get of expr * expr
  | Slice of expr * expr option * expr option
  | Tuple of expr list
  | Captured_shellcall of string
  | Try_shellcall of string

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
