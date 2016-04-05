open Core.Std
open Printf
module Re2 = Re2.Std.Re2

type err = [%import: Err.t]

exception Exec_error = Err.Exec_error

exception Violated_invariant of string

exception Not_implemented of string

exception Tracked_exec_error of int * Err.t

(* Converts internal module exceptions to external facing exceptions, and
 * tags each with the given line number. *)
let track_exn lnum f =
  try f ()
  with
  | Exec_error e -> raise (Tracked_exec_error (lnum, e))
  | Env.Var_already_defined id ->
      raise (Tracked_exec_error (lnum, (Var_already_defined id)))
  | Env.Var_not_found id ->
      raise (Tracked_exec_error (lnum, (Var_not_found id)))
  | Bdict.Key_not_found k ->
      raise (Tracked_exec_error (lnum, (Key_not_found k)))
  | Blist.Index_out_of_bounds i ->
      raise (Tracked_exec_error (lnum, (Index_out_of_bounds i)))
  | Blist.Invalid_slice (start, stop) ->
      raise (Tracked_exec_error (lnum, (Invalid_slice (start, stop))))
  | Shell.Call_failed cmd ->
      raise (Tracked_exec_error (lnum, (Shellcall_failed cmd)))
  | Sys_error msg ->
      raise (Tracked_exec_error (lnum, (Illegal_state msg)))

module Step = struct
  type t =
    | Next
    | Return of Prim.t
end

let rec interpolate_shellcall env s =
  let re = Re2.create_exn "\${([^}]*)}" in
  Re2.replace_exn re s ~f:(fun m ->
    let expr_str = Re2.Match.get_exn ~sub:(`Index 1) m in
    let lexbuf = (Lexing.from_string (expr_str ^ ";")) in
    Prim.to_str (eval_expr env (Parser.inline_expr Lexer.read lexbuf))
  )

and eval_expr env = function
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
  | Ast.Typeof (e) ->
      Prim.Str (Prim.type_str (eval_expr env e))
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
          | Ast.Ne -> Prim.Bool (i1 <> i2)
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
          | Ast.Ne -> Prim.Bool (f1 <> f2)
          | Ast.Lt -> Prim.Bool (f1 < f2)
          | Ast.Gt -> Prim.Bool (f1 > f2)
          | Ast.Lte -> Prim.Bool (f1 <= f2)
          | Ast.Gte -> Prim.Bool (f1 >= f2)
          | _ -> raise (Exec_error (Unsupported_binop (binop, "float")))
          end
      | Prim.Bool b1, Prim.Bool b2 -> 
          begin match binop with
          | Ast.Eq -> Prim.Bool (b1 = b2)
          | Ast.Ne -> Prim.Bool (b1 <> b2)
          | _ -> raise (Exec_error (Unsupported_binop (binop, "bool")))
          end
      | Prim.Str s1, Prim.Str s2 ->
          begin match binop with
          | Ast.Add -> Prim.Str (s1 ^ s2)
          | Ast.Eq -> Prim.Bool (s1 = s2)
          | Ast.Ne -> Prim.Bool (s1 <> s2)
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
      begin match eval_expr env e with
      | Prim.Closure (c_env, arg_ids, body) ->
          let c_env' = Env.extend c_env in
          let pairs = match List.zip arg_ids arg_exprs with
          | Some l -> l
          | None ->
              raise (Exec_error (Incorrect_arg_num
                (List.length arg_ids, List.length arg_exprs)));
          in
          (List.iter pairs ~f:(fun (id, e) -> Env.bind c_env' id (eval_expr env e)));
          begin match exec_block c_env' body with
          | Step.Return v -> v
          | Step.Next -> Prim.Unit
          end
      | Prim.Builtin_method (p, mid) ->
          let args = List.map arg_exprs ~f:(eval_expr env) in
          begin match p with
          | Prim.List l -> (Map.find_exn Builtin.list_methods mid) l args
          | Prim.Dict d -> (Map.find_exn Builtin.dict_methods mid) d args
          | Prim.Str s -> (Map.find_exn Builtin.str_methods mid) s args
          | _ -> raise (Violated_invariant "should not be calling builtin method on non-builtin")
          end
      | p -> raise (Exec_error (Incorrect_type ("func-call", p, "func")))
      end
  | Ast.Field_lookup (e, id) ->
      let p = eval_expr env e in
      let get_method method_map =
        if Map.mem method_map id then Prim.Builtin_method (p, id)
        else raise (Exec_error (Undefined_method (Prim.type_str p, id)))
      in
      begin match p with
      | Prim.List _ -> get_method Builtin.list_methods
      | Prim.Dict _ -> get_method Builtin.dict_methods
      | Prim.Str _ -> get_method Builtin.str_methods
      | _ -> raise (Exec_error (Incorrect_type ("field-lookup", p, "object|list|dict")))
      end
  | Ast.List expr_list -> Prim.List (Blist.create (List.map expr_list ~f:(eval_expr env)))
  | Ast.Dict kv_list ->
      Prim.Dict (Bdict.create (List.map kv_list ~f:(fun (k, v) -> (k, eval_expr env v))))
  | Ast.Get (e1, e2) ->
      begin match eval_expr env e1, eval_expr env e2 with
      | Prim.List l, Prim.Int i -> Blist.get l i
      | Prim.Dict d, Prim.Str s -> Bdict.get d s
      | Prim.Str s, Prim.Int i ->
          if i < 0 || i >= String.length s then raise (Exec_error (Index_out_of_bounds i))
          else Prim.Str (Char.to_string (String.get s i))
      | p1, p2 ->
          raise (Exec_error (Incorrect_two_type
            ("get", (p1, "list|dict|str"), (p2, "int|str"))))
      end
  | Ast.Slice (e, start_e_opt, stop_e_opt) ->
      let eval_i e_opt =
        match e_opt with
        | Some e ->
            begin match eval_expr env e with
            | Prim.Int i -> Some i
            | p -> raise (Exec_error (Incorrect_type ("slice-index", p, "int")))
            end
        | None -> None
      in
      let start_opt = eval_i start_e_opt in
      let stop_opt = eval_i stop_e_opt in
      begin match eval_expr env e with
      | Prim.List l ->
          let start = Option.value start_opt ~default:0 in
          let stop = Option.value stop_opt ~default:(Blist.len l) in
          Prim.List (Blist.slice l start stop)
      | Prim.Str s ->
          let len = String.length s in
          let start = Option.value start_opt ~default:0 in
          let stop = Option.value stop_opt ~default:len in
          let norm_stop = Blist.norm_slice_exn start stop len in
          if norm_stop = 0 then
            (* String.slice will normalize 0 to String.length, which we don't
             * want, so we handle that case manually. Note that if norm_stop
             * is 0, start must be 0. *)
            Prim.Str ""
          else
            Prim.Str (String.slice s start norm_stop)
      end
  | Ast.Tuple expr_list -> Prim.Tuple (List.map expr_list ~f:(eval_expr env))
  | Ast.Captured_shellcall s ->
      let s = interpolate_shellcall env s in
      begin match Shell.capture_call s with
      | Result.Ok out -> Prim.Str out
      | Result.Error err -> raise (Exec_error (Captured_shellcall_failed (s, err)))
      end
  | Ast.Try_shellcall s ->
      let s = interpolate_shellcall env s in
      begin match Shell.capture_call s with
      | Result.Ok out -> Prim.Tuple [Prim.Str out; Prim.Int 0]
      | Result.Error (Shell.Exit (stdout, _, code)) ->
          Prim.Tuple [Prim.Str stdout; Prim.Int code]
      | Result.Error (Shell.Signal (stdout, _, _)) ->
          Prim.Tuple [Prim.Str stdout; Prim.Int 2] (* TODO: signals *)
      end

and unpack id_list tuple f =
  match (List.zip id_list tuple) with
  | Some l -> List.iter l ~f:f
  | None ->
      raise (Exec_error (Incorrect_arg_num
        (List.length tuple, List.length id_list)))

and exec_stmt env = function
  | Ast.Expr e -> let _ = eval_expr env e in Step.Next
  | Ast.Def (id, e) ->
      let e' = eval_expr env e in
      (match e' with
      | Prim.Closure (c_env, _, _) as c -> Env.bind c_env id c
      | _ -> ());
      begin match id with
      | "_" -> Step.Next
      | _ -> Env.bind env id e'; Step.Next
      end
  | Ast.Asgn (id, e) ->
      let e' = eval_expr env e in
      begin match id with
      | "_" -> Step.Next
      | _ -> Env.update env id e'; Step.Next
      end
  | Ast.Multi_def (id_list, e) ->
      begin match eval_expr env e with
      | Prim.Tuple t ->
          unpack id_list t (fun (id, p) ->
            begin match id with
            | "_" -> ()
            | _ ->
                try Env.bind env id p
                with Env.Var_already_defined _ -> Env.update env id p
            end
          );
          Step.Next
      | p -> raise (Exec_error (Incorrect_type ("unpack-def", p, "tuple")))
      end
  | Ast.Multi_asgn (id_list, e) ->
      begin match eval_expr env e with
      | Prim.Tuple t ->
          unpack id_list t (fun (id, p) ->
            begin match id with
            | "_" -> ()
            | _ -> Env.update env id p
            end
          );
          Step.Next
      | p -> raise (Exec_error (Incorrect_type ("unpack-asgn", p, "tuple")))
      end
  | Ast.Print e -> (eval_expr env e) |> Prim.to_str |> print_endline; Step.Next
  | Ast.Return e -> Step.Return (eval_expr env e)
  | Ast.If_then_else (cond_e, true_b, false_b) ->
      begin match eval_expr env cond_e with
      | Prim.Bool b -> exec_block (Env.extend env) (if b then true_b else false_b)
      | p -> raise (Exec_error (Incorrect_type ("if", p, "bool")))
      end
  | Ast.Set (e1, e2, e3) ->
      (match eval_expr env e1, eval_expr env e2, eval_expr env e3 with
      | Prim.List l, Prim.Int i, p -> Blist.set l i p
      | Prim.Dict d, Prim.Str s, p -> Bdict.set d s p
      | p1, p2, _ ->
          raise (Exec_error (Incorrect_two_type
            ("set", (p1, "list|dict"), (p2, "int|str")))));
      Step.Next
  | Ast.Cd (dir_e, block) ->
      begin match eval_expr env dir_e with
      | Prim.Str s ->
          let old_dir = Sys.getcwd () in
          (try Unix.chdir (Shell.expand_path s)
           with Unix.Unix_error _ -> raise (Exec_error (Dir_not_found s)));
          (try
            exec_block (Env.extend env) block;
            (try Unix.chdir old_dir
             with Unix.Unix_error _ -> raise (Exec_error (Dir_not_found old_dir)))
           with e ->
            (try Unix.chdir old_dir
             with Unix.Unix_error _ -> raise (Exec_error (Dir_not_found old_dir)));
            raise e);
          Step.Next
      | p -> raise (Exec_error (Incorrect_type ("cd", p, "str")))
      end
  | Ast.While (cond_e, stmt_list) ->
      let rec aux () =
        match eval_expr env cond_e with
        | Prim.Bool b ->
            if b then
              let _ = exec_block (Env.extend env) stmt_list in
              aux ()
            else Step.Next
        | p -> raise (Exec_error (Incorrect_type ("while", p, "bool")))
      in
      aux ()
  | Ast.For (id, e, stmt_list) ->
      begin match eval_expr env e with
      | Prim.Closure (c_env, arg_ids, body) ->
          (* Returned closure should not accept any arguments, so pass []. *)
          let call = Ast.Call (Ast.Func (arg_ids, body), []) in
          let rec aux () =
            let env' = Env.extend env in
            match eval_expr c_env call with
            | Prim.Tuple (p :: (Prim.Bool b) :: []) ->
                Env.bind env' id p;
                if b then
                  let _ = exec_block env' stmt_list in
                  aux()
                else Step.Next
            | p -> raise (Exec_error (Incorrect_type ("iter", p, "(prim, bool)")))
          in
          aux ()
      | Prim.List l ->
          Blist.iter l ~f:(fun p ->
            let env' = Env.extend env in
            Env.bind env' id p;
            let _ = exec_block env' stmt_list in
            ()
          );
          Step.Next
      | p ->
          raise (Exec_error (Incorrect_type
            ("for", p, "list | func -> (prim, bool)")))
      end
  | Ast.Shellcall s ->
      Shell.call (interpolate_shellcall env s);
      Step.Next

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
    | (lnum, s) :: sl' -> track_exn lnum (fun () -> aux s sl')
  in
  step sl

and exec_prog sl argv =
  let env = Env.create () in
  Env.bind env "argv"
    (Prim.List (Blist.create (List.map argv ~f:(fun s -> Prim.Str s))));
  let rec step = function
    | [] -> ()
    | (lnum, s) :: sl' ->
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

let run lexbuf argv =
  exec_prog (Parser.prog Lexer.read lexbuf) argv
