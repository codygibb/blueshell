open Printf

exception Violated_invariant of string

type err =
  | Var_already_defined of Ast.id
  | Var_not_found of Ast.id
  | Invalid_cast of Ast.type_cast * Prim.t
  | Divide_by_zero
  | Mismatched_binop_types of Ast.binop * Prim.t * Prim.t
  | Unsupported_binop of Ast.binop * string
  | Incorrect_type of string * Prim.t * string
  | Incorrect_arg_num of int * int
  | Return_from_main

(* This method is used for testing purposes only, see get_err_msg for
 * user-readable error messages. *)
let err_to_str = function
  | Var_already_defined id -> sprintf "Var_already_defined %s" id
  | Var_not_found id -> sprintf "Var_not_found %s" id
  | Invalid_cast (t, p) ->
      sprintf "Invalid_cast (%s, %s)" (Ast.type_cast_to_str t) (Prim.to_str p)
  | Divide_by_zero -> "Divide_by_zero"
  | Mismatched_binop_types (binop, left, right) ->
      sprintf "Mismatched_binop_types (%s, %s, %s)"
        (Ast.binop_to_str binop) (Prim.to_str left) (Prim.to_str right)
  | Unsupported_binop (binop, t) ->
      sprintf "Unsupported_binop (%s, %s)" (Ast.binop_to_str binop) t
  | Incorrect_type (op, p, expected) ->
      sprintf "Incorrect_type (%s, %s, %s)" op (Prim.to_str p) expected
  | Incorrect_arg_num (expected, given) ->
      sprintf "Incorrect_arg_num (%d, %d)" expected given
  | Return_from_main -> "Return_from_main"

(* Generates a user-readable error message. *)
let get_err_msg = function
  | Var_already_defined id -> sprintf "variable already defined: '%s'" id
  | Var_not_found id -> sprintf "variable not defined: '%s'" id
  | Invalid_cast (t, p) ->
      sprintf "cannot cast type '%s' to type '%s'"
        (Ast.type_cast_to_str t) (Prim.to_str p)
  | Divide_by_zero -> "cannot divide by zero"
  | Mismatched_binop_types (binop, left, right) ->
      sprintf "mismatched operand types for '%s': left: '%s', right: '%s'"
        (Ast.binop_to_str binop) (Prim.to_str left) (Prim.to_str right)
  | Unsupported_binop (binop, t) ->
      sprintf "'%s' is not supported for '%s' types" (Ast.binop_to_str binop) t
  | Incorrect_type (op, p, expected) ->
      sprintf "'%s' cannot be applied to type '%s', expected '%s'" op (Prim.to_str p) expected
  | Incorrect_arg_num (expected, given) ->
      sprintf "incorrect number of arguments: expected %d, given %d" expected given
  | Return_from_main -> "cannot return from main"

exception Exec_error of err

exception Tracked_exec_error of int * err

module Step = struct
  type t =
    | Next
    | Return of Prim.t
end

let track_exn lnum f =
  try f ()
  with
  | Exec_error e -> raise (Tracked_exec_error (lnum, e))
  | Env.Var_already_defined id ->
      raise (Tracked_exec_error (lnum, (Var_already_defined id)))
  | Env.Var_not_found id ->
      raise (Tracked_exec_error (lnum, (Var_not_found id)))

let rec eval_expr env = function
  | Ast.Int i -> Prim.Int i
  | Ast.Bool b -> Prim.Bool b
  | Ast.Float f -> Prim.Float f
  | Ast.Str s -> Prim.Str s
  | Ast.Id id -> Env.lookup env id
  | Ast.Cast (t, e) ->
      begin match t, eval_expr env e with
      | Ast.Int_cast, Prim.Int i -> Prim.Int i
      | Ast.Int_cast, Prim.Float f -> Prim.Int (int_of_float f)
      | Ast.Int_cast, Prim.Str s -> Prim.Int (int_of_string s)
      | Ast.Float_cast, Prim.Int i -> Prim.Float (float_of_int i)
      | Ast.Float_cast, Prim.Float f -> Prim.Float f
      | Ast.Float_cast, Prim.Str s -> Prim.Float (float_of_string s)
      | Ast.Str_cast, Prim.Int i -> Prim.Str (string_of_int i)
      | Ast.Str_cast, Prim.Float f -> Prim.Str(string_of_float f)
      | Ast.Str_cast, Prim.Str s -> Prim.Str s
      | Ast.Str_cast, Prim.Bool b -> Prim.Str (string_of_bool b)
      | Ast.Bool_cast, Prim.Str s -> Prim.Bool (bool_of_string s)
      | t, p -> raise (Exec_error (Invalid_cast (t, p)))
      end
  | Ast.Bin_op (binop, e1, e2) ->
      begin match (eval_expr env e1), (eval_expr env e2) with
      | Prim.Int i1, Prim.Int i2 ->
          begin match binop with
          | Ast.Add -> Prim.Int (i1 + i2)
          | Ast.Sub -> Prim.Int (i1 - i2)
          | Ast.Mult -> Prim.Int (i1 * i2)
          | Ast.Div ->
              if i2 = 0 then raise (Exec_error Divide_by_zero)
              else Prim.Int (i1 / i2)
          | Ast.Mod -> Prim.Int (i1 mod i2)
          | Ast.Eq -> Prim.Bool (i1 = i2)
          | Ast.Ne -> Prim.Bool (i1 != i2)
          | Ast.Lt -> Prim.Bool (i1 < i2)
          | Ast.Gt -> Prim.Bool (i1 > i2)
          | Ast.Lte -> Prim.Bool (i1 <= i2)
          | Ast.Gte -> Prim.Bool (i1 >= i2)
          | Ast.Bit_and -> Prim.Int (i1 land i2)
          | Ast.Bit_or -> Prim.Int (i1 lor i2)
          | Ast.Bit_xor -> Prim.Int (i1 lxor i2)
          | Ast.Left_shift -> Prim.Int (i1 lsl i2)
          | Ast.Right_shift -> Prim.Int (i1 lsr i2)
          end
      | Prim.Float f1, Prim.Float f2 ->
          begin match binop with
          | Ast.Add -> Prim.Float (f1 +. f2)
          | Ast.Sub -> Prim.Float (f1 -. f2)
          | Ast.Mult -> Prim.Float (f1 *. f2)
          | Ast.Div ->
              if f2 = 0.0 then raise (Exec_error Divide_by_zero)
              else Prim.Float (f1 /. f2)
          | Ast.Eq -> Prim.Bool (f1 = f2)
          | Ast.Ne -> Prim.Bool (f1 != f2)
          | Ast.Lt -> Prim.Bool (f1 < f2)
          | Ast.Gt -> Prim.Bool (f1 > f2)
          | Ast.Lte -> Prim.Bool (f1 <= f2)
          | Ast.Gte -> Prim.Bool (f1 >= f2)
          | _ -> raise (Exec_error (Unsupported_binop (binop, "float")))
          end
      | Prim.Bool b1, Prim.Bool b2 -> 
          begin match binop with
          | Ast.Eq -> Prim.Bool (b1 = b2)
          | Ast.Ne -> Prim.Bool (b1 != b2)
          | _ -> raise (Exec_error (Unsupported_binop (binop, "bool")))
          end
      | Prim.Str s1, Prim.Str s2 ->
          begin match binop with
          | Ast.Add -> Prim.Str (s1 ^ s2)
          | Ast.Eq -> Prim.Bool (s1 = s2)
          | Ast.Ne -> Prim.Bool (s1 != s2)
          | _ -> raise (Exec_error (Unsupported_binop (binop, "str")))
          end
      | p1, p2 -> raise (Exec_error (Mismatched_binop_types (binop, p1, p2)))
      end
  | Ast.Not e ->
      begin match eval_expr env e with
      | Prim.Bool b -> Prim.Bool (not b)
      | p -> raise (Exec_error (Incorrect_type ("!", p, "bool")))
      end
  | Ast.And (e1, e2) ->
      begin match eval_expr env e1 with
      | Prim.Bool b1 ->
          if b1
          then
            begin match eval_expr env e2 with
            | Prim.Bool b2 -> Prim.Bool b2
            | p2 -> raise (Exec_error (Incorrect_type ("&&", p2, "bool")))
            end
          else Prim.Bool false (* Short circuit (don't eval e2) *)
      | p1 -> raise (Exec_error (Incorrect_type ("&&", p1, "bool")))
      end
  | Ast.Or (e1, e2) ->
      begin match eval_expr env e1 with
      | Prim.Bool b1 ->
          if b1
          then Prim.Bool true (* Short circuit (don't eval e2) *)
          else
            begin match eval_expr env e2 with
            | Prim.Bool b2 -> Prim.Bool b2
            | p2 -> raise (Exec_error (Incorrect_type ("||", p2, "bool")))
            end
      | p1 -> raise (Exec_error (Incorrect_type ("||", p1, "bool")))
      end
  | Ast.Ternary (e1, e2, e3) ->
      begin match eval_expr env e1 with
      | Prim.Bool b1 ->
          if b1 then eval_expr env e2
          else eval_expr env e3
      | p -> raise (Exec_error (Incorrect_type ("?", p, "bool")))
      end
  | Ast.Func (arg_ids, block) ->
      Prim.Closure ((Env.extend env), arg_ids, block)
  | Ast.Call (e, arg_exprs) ->
      let (c_env, arg_ids, body) = match eval_expr env e with
      | Prim.Closure c -> c
      | p -> raise (Exec_error (Incorrect_type ("func call", p, "func")))
      in
      let rec bind_args env = function
        | [], [] -> ()
        | (_ :: _, []) | ([], _ :: _) ->
            raise (Violated_invariant "bind_args lists should be equal length")
        | id :: ids, v :: vals ->
            Env.bind env id v;
            bind_args env (ids, vals)
      in
      let arg_vals = List.map (eval_expr env) arg_exprs in
      let args_expected = List.length arg_ids in
      let args_given = List.length arg_vals in
      (if args_expected <> args_given then
        raise (Exec_error (Incorrect_arg_num (args_expected, args_given))));
      let c_env' = Env.extend c_env in
      bind_args c_env' (arg_ids, arg_vals);
      begin match exec_block c_env' body with
      | Step.Return v -> v
      | Step.Next -> Prim.Unit
      end

and exec_block env sl =
  let env' = Env.extend env in
  let rec step sl =
    let aux s sl' =
      match exec_stmt env' s with
      | Step.Next -> step sl'
      | Step.Return v -> Step.Return v
    in
    match sl with
    | [] -> Step.Next
      (* No line number info (block inlined), cannot track exception *)
    | (None, s) :: sl' -> aux s sl'
    | (Some lnum, s) :: sl' -> track_exn lnum (fun () -> aux s sl')
  in
  step sl

and exec_stmt env = function
  | Ast.Expr e -> let _ = eval_expr env e in Step.Next
  | Ast.Def (id, e) -> Env.bind env id (eval_expr env e); Step.Next
  | Ast.Asgn (id, e) -> Env.update env id (eval_expr env e); Step.Next
  | Ast.Print e ->
      begin match eval_expr env e with
      | Prim.Int i -> printf "%d\n" i; Step.Next
      | Prim.Str s -> printf "%s\n" s; Step.Next
      | Prim.Bool b -> printf "%B\n" b; Step.Next
      | Prim.Float f -> printf "%f\n" f; Step.Next
      | p -> raise (Exec_error (Incorrect_type ("print", p, "primitive")))
      end
  | Ast.Return e -> Step.Return (eval_expr env e)
  | Ast.If_then_else (cond_e, true_b, false_b) ->
      begin match eval_expr env cond_e with
      | Prim.Bool b -> exec_block env (if b then true_b else false_b)
      | p -> raise (Exec_error (Incorrect_type ("if", p, "bool")))
      end

and exec_prog sl =
  let env = Env.create () in
  let rec step = function
    | [] -> ()
    | (None, _) :: _ ->
        raise (Violated_invariant "main program statement has no line number")
    | (Some lnum, s) :: sl' ->
        track_exn lnum (fun () ->
          match exec_stmt env s with
          | Step.Next -> step sl'
          | Step.Return _ -> raise (Exec_error Return_from_main))
  in
  step sl

let get_lexbuf file =
  let lexbuf = file |> open_in |> Lexing.from_channel in
  let open Lexing in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = file };
  lexbuf

let run lexbuf =
  exec_prog (Parser.prog Lexer.read lexbuf)
