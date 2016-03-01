open Printf

exception Violated_invariant of string

exception Exec_error of string

exception Tracked_exec_error of int * string

module Prim = struct
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
    | Str _ -> "string"
    | Closure _ -> "function"
end

module Step = struct
  type t =
    | Next
    | Return of Prim.t
end

let track_exn lnum f =
  try
    f ()
  with
  | Exec_error msg -> raise (Tracked_exec_error (lnum, msg))
  | Env.Var_already_defined id ->
      raise (Tracked_exec_error
        (lnum, sprintf "variable already defined: \"%s\"" id))
  | Env.Var_not_found id ->
      raise (Tracked_exec_error
        (lnum, sprintf "variable not defined: \"%s\"" id))

let rec eval_expr env = function
  | Ast.Int i -> Prim.Int i
  | Ast.Bool b -> Prim.Bool b
  | Ast.Float f -> Prim.Float f
  | Ast.Str s -> Prim.Str s
  | Ast.Id id -> Env.lookup env id
  | Ast.Cast (cast_type, e) ->
      begin match (cast_type), (eval_expr env e) with
      | Ast.IntCast, Prim.Int e2 -> Prim.Int e2
      | Ast.IntCast, Prim.Float e2 -> Prim.Int (int_of_float e2)
      | Ast.IntCast, Prim.Str e2 -> Prim.Int (int_of_string e2)
      | Ast.FloatCast, Prim.Int e2 -> Prim.Float (float_of_int e2)
      | Ast.FloatCast, Prim.Float e2 -> Prim.Float e2
      | Ast.FloatCast, Prim.Str e2 -> Prim.Float (float_of_string e2)
      | Ast.StrCast, Prim.Int e2 -> Prim.Str (string_of_int e2)
      | Ast.StrCast, Prim.Float e2 -> Prim.Str(string_of_float e2)
      | Ast.StrCast, Prim.Str e2 -> Prim.Str e2
      | Ast.StrCast, Prim.Bool e2 -> Prim.Str (string_of_bool e2)
      | Ast.BoolCast, Prim.Str e2 -> Prim.Bool (bool_of_string e2)
      | e1, e2 -> raise (Exec_error (sprintf "cannot cast %s to type %s" (Prim.to_str e2) (Ast.cast_type_to_str e1)))
      end
  | Ast.Bin_op (binop, e1, e2) ->
      begin match (eval_expr env e1), (eval_expr env e2) with
      | Prim.Int i1, Prim.Int i2 ->
          begin match binop with
          | Ast.Add -> Prim.Int (i1 + i2)
          | Ast.Sub -> Prim.Int (i1 - i2)
          | Ast.Mult -> Prim.Int (i1 * i2)
          | Ast.Div ->
              if i2 = 0 then raise (Exec_error "cannot divide by zero")
              else Prim.Int (i1 / i2)
          | Ast.Mod -> Prim.Int (i1 mod i2)
          | Ast.Eq -> Prim.Bool (i1 = i2)
          | Ast.Ne -> Prim.Bool (i1 != i2)
          | Ast.Lt -> Prim.Bool (i1 < i2)
          | Ast.Gt -> Prim.Bool (i1 > i2)
          | Ast.Lte -> Prim.Bool (i1 <= i2)
          | Ast.Gte -> Prim.Bool (i1 >= i2)
          | Ast.BitAnd -> Prim.Int (i1 land i2)
          | Ast.BitOr -> Prim.Int (i1 lor i2)
          | Ast.BitXor -> Prim.Int (i1 lxor i2)
          | Ast.LeftShift -> Prim.Int (i1 lsl i2)
          | Ast.RightShift -> Prim.Int (i1 lsr i2)
          | _ -> raise (Exec_error (sprintf "cannot apply %s to ints"
                                                   (Ast.to_str binop)))
          end
      | Prim.Float f1, Prim.Float f2 ->
          begin match binop with
          | Ast.Add -> Prim.Float (f1 +. f2)
          | Ast.Sub -> Prim.Float (f1 -. f2)
          | Ast.Mult -> Prim.Float (f1 *. f2)
          | Ast.Div ->
              if f2 = 0.0 then raise (Exec_error "cannot divide by zero")
              else Prim.Float (f1 /. f2)
          | Ast.Eq -> Prim.Bool (f1 = f2)
          | Ast.Ne -> Prim.Bool (f1 != f2)
          | Ast.Lt -> Prim.Bool (f1 < f2)
          | Ast.Gt -> Prim.Bool (f1 > f2)
          | Ast.Lte -> Prim.Bool (f1 <= f2)
          | Ast.Gte -> Prim.Bool (f1 >= f2)
          | _ -> raise (Exec_error (sprintf "cannot apply %s to floats"
                                                   (Ast.to_str binop)))
          end
      | Prim.Bool b1, Prim.Bool b2 -> 
          begin match binop with
          | Ast.Eq -> Prim.Bool (b1 = b2)
          | Ast.Ne -> Prim.Bool (b1 != b2)
          | _ -> raise (Exec_error (sprintf "cannot apply %s to bools"
                                            (Ast.to_str binop)))
          end
      | Prim.Str s1, Prim.Str s2 ->
          begin match binop with
          | Ast.Add -> Prim.Str (s1 ^ s2)
          | Ast.Eq -> Prim.Bool (s1 = s2)
          | Ast.Ne -> Prim.Bool (s1 != s2)
          | _ -> raise (Exec_error (sprintf "cannot apply %s to strings"
                                                   (Ast.to_str binop)))
          end
      | p1, p2 ->
          raise (Exec_error
            (sprintf "mismatched operand types for \"%s\": left: %s, right: %s"
                     (Ast.to_str binop) (Prim.to_str p1) (Prim.to_str p2)))
      end
  | Ast.Not e ->
      begin match eval_expr env e with
      | Prim.Bool b -> Prim.Bool (not b)
      | _ -> raise (Exec_error "cannot apply ! operator to non-bool")
      end
  | Ast.And (e1, e2) ->
      let err = Exec_error "cannot apply && operator to non-bool" in
      begin match eval_expr env e1 with
      | Prim.Bool b1 ->
          if b1
          then
            begin match eval_expr env e2 with
            | Prim.Bool b2 -> Prim.Bool b2
            | _ -> raise err
            end
          else Prim.Bool false (* Short circuit (don't eval e2) *)
      | _ -> raise err
      end
  | Ast.Or (e1, e2) ->
      let err = Exec_error "cannot apply || operator to non-bool" in
      begin match eval_expr env e1 with
      | Prim.Bool b1 ->
          if b1
          then Prim.Bool true (* Short circuit (don't eval e2) *)
          else
            begin match eval_expr env e2 with
            | Prim.Bool b2 -> Prim.Bool b2
            | _ -> raise err
            end
      | _ -> raise err
      end
  | Ast.Ternary (e1, e2, e3) ->
      let err = Exec_error "cannot apply ternary to non-bool" in
      begin match eval_expr env e1 with
      | Prim.Bool b1 ->
          if b1 then eval_expr env e2
          else eval_expr env e3
      | _ -> raise err
      end
  | Ast.Func (arg_ids, block) ->
      Prim.Closure ((Env.extend env), arg_ids, block)
  | Ast.Call (e, arg_exprs) ->
      let (c_env, arg_ids, body) = match eval_expr env e with
      | Prim.Closure c -> c
      | _ -> raise (Exec_error "cannot call non-function")
      in
      let rec bind_args env = function
        | [], [] -> ()
        | (_ :: _, []) | ([], _ :: _) ->
            raise (Exec_error "incorrect number of arguments")
        | id :: ids, v :: vals ->
            Env.bind env id v;
            bind_args env (ids, vals)
      in
      let arg_vals = List.map (eval_expr env) arg_exprs in
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
      | _ -> raise (Exec_error "cannot print type")
      end
  | Ast.Return e -> Step.Return (eval_expr env e)
  | Ast.If_then_else (cond_e, true_b, false_b) ->
      begin match eval_expr env cond_e with
      | Prim.Bool b -> exec_block env (if b then true_b else false_b)
      | _ -> raise (Exec_error "condition must be boolean")
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
          | Step.Return _ -> raise (Exec_error "cannot return from main"))
  in
  step sl

let get_lexbuf file =
  let lexbuf = file |> open_in |> Lexing.from_channel in
  let open Lexing in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = file };
  lexbuf

let run lexbuf =
  exec_prog (Parser.prog Lexer.read lexbuf)
