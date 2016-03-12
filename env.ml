open Core.Std

exception Var_already_defined of Ast.id

exception Var_not_found of Ast.id

type 'a t =
  { parent : 'a t option;
    tbl : (Ast.id, 'a ref) Hashtbl.t;
  }

let create () =
  { parent = None;
    tbl = String.Table.create ();
  }

(* Note: [freeze e] does not make the values in [e.tbl] static,
 * rather, it makes a shallow copy of [e.tbl] such no new bindings
 * may be added, but existing references may still be updated. *)
let freeze e =
  { parent = e.parent;
    tbl = Hashtbl.copy e.tbl;
  }

let extend e =
  { parent = Some (freeze e);
    tbl = String.Table.create ();
  }

let bind e id v =
  match Hashtbl.add_or_error e.tbl id (ref v) with
  | Ok _ -> ()
  | Error _ -> raise (Var_already_defined id)

let rec get_ref e id =
  match Hashtbl.find e.tbl id with
  | Some r -> r
  | None ->
      begin match e.parent with
      | Some e' -> get_ref e' id
      | None -> raise (Var_not_found id)
      end

let lookup e id =
  !(get_ref e id)

let rec update e id v =
  (get_ref e id) := v
