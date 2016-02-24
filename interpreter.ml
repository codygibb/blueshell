open Printf

exception Exec_error of string

exception Tracked_exec_error of int * string

module Prim = struct
  type t = 
    | Unit
    | Int of int
    | Bool of bool
    | Str of string
    | Closure of (t Env.t * Ast.id list * Ast.block)

  let to_str = function
    | Unit -> "unit"
    | Int _ -> "int"
    | Bool _ -> "bool"
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
  | Ast.Str s -> Prim.Str s
  | Ast.Id id -> Env.lookup env id
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
          | Ast.Eq -> Prim.Bool (i1 = i2)
          | Ast.Ne -> Prim.Bool (i1 != i2)
          | Ast.Lt -> Prim.Bool (i1 < i2)
          | Ast.Gt -> Prim.Bool (i1 > i2)
          | Ast.Lte -> Prim.Bool (i1 <= i2)
          | Ast.Gte -> Prim.Bool (i1 >= i2)
          (*| _ -> raise (Exec_error (sprintf "cannot apply %s to ints"*)
                                                   (*(Ast.to_str binop)))*)
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
  | Ast.Lambda (arg_names, block) ->
      Prim.Closure ((Env.extend env), arg_names, block)
  | Ast.Call (e, arg_exprs) ->
      let (c_env, arg_names, body) = match eval_expr env e with
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
      bind_args c_env' (arg_names, arg_vals);
      begin match eval_block c_env' body with
      | Step.Return v -> v
      | Step.Next -> Prim.Unit
      end

and eval_block env b =
  let env' = Env.extend env in
  let rec step = function
    | Ast.B_stmt (lnum_opt, s, b') ->
        let aux () =
          begin match exec_stmt env' s with
          | Step.Next -> step b'
          | Step.Return v -> Step.Return v
          end
        in
        begin match lnum_opt with
        | Some lnum -> track_exn lnum aux
        (* No line-number information because block statement is
         * in-lined. Do not protect call to aux from exceptions,
         * so the exception propogates up to the original program
         * statement. *)
        | None -> aux ()
        end
    | Ast.B_end -> Step.Next
  in
  step b

and exec_stmt env = function
  | Ast.Def (id, e) -> Env.bind env id (eval_expr env e); Step.Next
  | Ast.Asgn (id, e) -> Env.update env id (eval_expr env e); Step.Next
  | Ast.Print e ->
      begin match eval_expr env e with
      | Prim.Int i -> printf "%d\n" i; Step.Next
      | Prim.Str s -> printf "%s\n" s; Step.Next
      | _ -> raise (Exec_error "cannot print type")
      end
  | Ast.Return e -> Step.Return (eval_expr env e)
  | Ast.If_then_else (cond_e, true_b, false_b) ->
      begin match eval_expr env cond_e with
      | Prim.Bool b -> eval_block env (if b then true_b else false_b)
      | _ -> raise (Exec_error "condition must be boolean")
      end

and eval_prog p =
  let env = Env.create () in
  let rec step = function
    | Ast.P_stmt (lnum, s, p') ->
        track_exn lnum (fun () -> 
          match exec_stmt env s with
          | Step.Next -> step p'
          | Step.Return _ -> raise (Exec_error "cannot return from main"))
    | Ast.P_exit -> ()
  in
  step p

