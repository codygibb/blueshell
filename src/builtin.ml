open Core.Std
open Printf
module Re2 = Re2.Std.Re2

type err = [%import: Err.t]

exception Exec_error = Err.Exec_error

let raise_arg_err args num_exp =
  raise (Exec_error (Incorrect_arg_num
    (num_exp, List.length args)))

(* By using Maps, we can (1) check that a built-in method exists,
 * and (2) dispatch the associated code. *)

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
        raise (Exec_error (Incorrect_type ("dict.del", p, "str")))
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
    | [Prim.Str pattern] ->
        let s = Util.unescape s in
        let regex = match Re2.create pattern with
        | Ok r -> r
          (* TODO: More informative error message. *)
        | Error _ -> raise (Exec_error (Illegal_argument "invalid regex"))
        in
        Prim.List (Blist.create (List.map ~f:(fun s -> Prim.Str s) (
          Re2.split regex s
        )))
    | [p] ->
        raise (Exec_error (Incorrect_type ("str.split", p, "str")))
    | _ -> raise_arg_err args 1
  ));
  ("fmt", (fun s args ->
    (* "fmt" intentionally uses variable number of arguments. *)
    let buf = Bigbuffer.create(32) in
    let parts = Re2.split ~max:(List.length args) (Re2.create_exn "\{\}") s in
    let parts_len = (List.length parts) in
    let arg_stream = Stream.of_list args in
    List.iteri parts ~f:(fun i s ->
      Bigbuffer.add_string buf s;
      if i < parts_len - 1 then
        Bigbuffer.add_string buf (Prim.to_str (Stream.next arg_stream));
    );
    Prim.Str (Bigbuffer.contents buf)
  ));
  ("replace", (fun s args ->
    match args with
    | [Prim.Str pattern; Prim.Str s2] ->
        let s = Util.unescape s in
        let regex = match Re2.create pattern with
        | Ok r -> r
          (* TODO: More informative error message. *)
        | Error _ -> raise (Exec_error (Illegal_argument "invalid regex"))
        in
        Prim.Str (Re2.replace_exn regex s ~f:(fun m -> s2))
    | [Prim.Str _; p] ->
        raise (Exec_error (Incorrect_type ("str.replace", p, "str")))
    | [p; _] ->
        raise (Exec_error (Incorrect_type ("str.replace", p, "str")))
    | _ -> raise_arg_err args 2
  ));
]
