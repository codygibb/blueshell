open Core.Std
open Printf

type err = [%import: Err.t]

exception Exec_error = Err.Exec_error

let raise_arg_err args num_exp =
  raise (Exec_error (Incorrect_arg_num
    (List.length args, num_exp)))

let list_methods = String.Map.of_alist_exn [
  ("push", (fun blist args ->
    match args with
    | [p] ->
        Blist.push blist p;
        Prim.Unit
    | _ -> raise_arg_err args 1
  ));
  ("pop", (fun blist args ->
    match args with
    | [] ->
        begin match Blist.pop blist with
        | Some p -> p
        | None ->
            raise (Exec_error (Illegal_state "cannot pop from empty list"))
        end
    | _ -> raise_arg_err args 0
  ));
  ("len", (fun blist args ->
    match args with
    | [] -> Prim.Int (Blist.len blist)
    | _ -> raise_arg_err args 0
  ));
]

let dict_methods = String.Map.of_alist_exn [
  ("len", (fun bdict args ->
    match args with
    | [] -> Prim.Int (Bdict.len bdict)
    | _ -> raise_arg_err args 0
  ));
  ("del", (fun bdict args ->
    match args with
    | [Prim.Str s] ->
        Bdict.del bdict s;
        Prim.Unit
    | [p] ->
        raise (Exec_error (Incorrect_type ("dict-del", p, "str")))
    | _ -> raise_arg_err args 1
  ));
]

let str_methods = String.Map.of_alist_exn [
  ("len", (fun s args ->
    match args with
    | [] -> Prim.Int (String.length s)
    | _ -> raise_arg_err args 0
  ));
  ("split", (fun s args ->
    match args with
    | [Prim.Str c] ->
        let illegal_arg =
          Exec_error (Illegal_argument "can only split on single character")
        in
        let split on =
          Prim.List (Blist.create
            (List.map (String.split s ~on) ~f:(fun s -> Prim.Str s)))
        in
        begin match String.length c with
        | 1 -> split (String.get c 0)
        | 2 ->
            begin match c with
            | "\\n" -> split '\n'
            | "\\t" -> split '\t'
            | _ -> raise illegal_arg
            end
        | _ -> raise illegal_arg
        end
    | _ -> raise_arg_err args 1
  ));
]
