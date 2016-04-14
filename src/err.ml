open Core.Std
open Printf

type t =
  | Var_already_defined of Ast.id
  | Var_not_found of Ast.id
  | Invalid_cast of Ast.type_cast * Prim.t
  | Divide_by_zero
  | Mismatched_binop_types of Ast.binop * Prim.t * Prim.t
  | Unsupported_binop of Ast.binop * string
    (* (operator, arg given, type expected)  *)
  | Incorrect_type of string * Prim.t * string
    (* (operator, (1st arg given, type expected), (2nd arg given, type expected) *)
  | Incorrect_two_type of string * (Prim.t * string) * (Prim.t * string)
    (* (num given, num expected) *)
  | Incorrect_arg_num of int * int
  | Return_from_main
  | Key_not_found of string
    (* (receiver type, method name) *)
  | Undefined_method of string * string
  | Index_out_of_bounds of int
    (* (start index, stop index) *)
  | Invalid_slice of int * int
  | Shellcall_failed of string
  | Captured_shellcall_failed of string * Shell.err
  | Dir_not_found of string
  | Illegal_state of string
  | Illegal_argument of string

exception Exec_error of t

(* This method is used for testing purposes only. *)
let test_str = function
  | Var_already_defined id -> sprintf "Var_already_defined %s" id
  | Var_not_found id -> sprintf "Var_not_found %s" id
  | Invalid_cast (t, p) ->
      sprintf "Invalid_cast (%s, %s)" (Ast.type_cast_to_str t) (Prim.type_str p)
  | Divide_by_zero -> "Divide_by_zero"
  | Mismatched_binop_types (binop, left, right) ->
      sprintf "Mismatched_binop_types (%s, %s, %s)"
        (Ast.binop_to_str binop) (Prim.type_str left) (Prim.type_str right)
  | Unsupported_binop (binop, t) ->
      sprintf "Unsupported_binop (%s, %s)" (Ast.binop_to_str binop) t
  | Incorrect_type (op, p, exp) ->
      sprintf "Incorrect_type (%s, %s, %s)" op (Prim.type_str p) exp
  | Incorrect_two_type (op, (p1, exp1), (p2, exp2)) ->
      sprintf "Incorrect_multi_type (%s, (%s, %s), (%s, %s))"
        op (Prim.type_str p1) exp1 (Prim.type_str p2) exp2
  | Incorrect_arg_num (given, exp) ->
      sprintf "Incorrect_arg_num (%d, %d)" given exp
  | Return_from_main -> "Return_from_main"
  | Key_not_found k -> sprintf "Key_not_found %s" k
  | Undefined_method (t, m) -> sprintf "Undefined_method (%s, %s)" t m
  | Index_out_of_bounds i -> sprintf "Index_out_of_bounds %d" i
  | Invalid_slice (start, stop) -> sprintf "Invalid_slice (%d, %d)" start stop
  | Shellcall_failed cmd -> sprintf "Shellcall_failed %s" cmd
  | Captured_shellcall_failed (cmd, _) ->
      sprintf "Captured_shellcall_failed %s" cmd
  | Dir_not_found dir -> sprintf "Dir_not_found %s" dir
  | Illegal_state _ -> "Illegal_state"
  | Illegal_argument _ -> "Illegal_argument"

let user_msg = function
  | Var_already_defined id -> sprintf "variable already defined: '%s'" id
  | Var_not_found id -> sprintf "variable not defined: '%s'" id
  | Invalid_cast (t, p) ->
      sprintf "cannot cast type '%s' to type '%s'"
        (Ast.type_cast_to_str t) (Prim.type_str p)
  | Divide_by_zero -> "cannot divide by zero"
  | Mismatched_binop_types (binop, left, right) ->
      sprintf "mismatched operand types for '%s': left: '%s', right: '%s'"
        (Ast.binop_to_str binop) (Prim.type_str left) (Prim.type_str right)
  | Unsupported_binop (binop, t) ->
      sprintf "'%s' is not supported for '%s' types" (Ast.binop_to_str binop) t
  | Incorrect_type (op, p, exp) ->
      sprintf "'%s' cannot be applied to type '%s', expected '%s'"
        op (Prim.type_str p) exp
  | Incorrect_two_type (op, (p1, exp1), (p2, exp2)) ->
      sprintf "'%s' cannot be applied to types '%s' and '%s', expected '%s' and '%s'"
        op (Prim.type_str p1) (Prim.type_str p2) exp1 exp2
  | Incorrect_arg_num (given, exp) ->
      sprintf "incorrect number of arguments: given %d, expected %d" given exp
  | Return_from_main -> "cannot return from main"
  | Key_not_found k -> sprintf "key not found: '%s'" k
  | Undefined_method (t, m) -> sprintf "method '%s' not defined for type '%s'" m t
  | Index_out_of_bounds i -> sprintf "index out of bounds: %d" i
  | Invalid_slice (start, stop) -> sprintf "invalid slice: %d to %d" start stop
  | Shellcall_failed cmd -> sprintf "Command failed: %s" cmd
  | Captured_shellcall_failed (cmd, err) ->
      begin match err with
      | Shell.Exit (stdout, stderr, code) ->
          (sprintf "%s\n" cmd) ^
          (sprintf "exited non-zero: %d\n" code) ^
          (sprintf "---- stdout ----\n") ^
          (sprintf "%s" stdout) ^
          (sprintf "---- stderr ----\n") ^
          (sprintf "%s" stderr)
      | Shell.Signal (stdout, stderr, signal) ->
          (sprintf "$> %s\n" cmd) ^
          (sprintf "interrupted by signal: %s\n" signal) ^
          (sprintf "---- stdout ----\n") ^
          (sprintf "%s" stdout) ^
          (sprintf "---- stderr ----\n") ^
          (sprintf "%s" stderr)
      end
  | Dir_not_found dir -> sprintf "directory '%s' not found" dir
  | Illegal_state msg -> sprintf "illegal state: %s" msg
  | Illegal_argument msg -> sprintf "illegal argument: %s" msg

