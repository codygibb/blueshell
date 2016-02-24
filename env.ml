exception Var_already_defined of Ast.id

exception Var_not_found of Ast.id

let init_size = 32

type 'a t =
  { parent : 'a t option;
    tbl : (Ast.id, 'a ref) Hashtbl.t;
  }

let create () =
  { parent = None;
    tbl = Hashtbl.create init_size;
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
    tbl = Hashtbl.create init_size;
  }

let bind e id v =
  if Hashtbl.mem e.tbl id then
    raise (Var_already_defined id)
  else
    Hashtbl.add e.tbl id (ref v)

let rec get_ref e id =
  try Hashtbl.find e.tbl id
  with Not_found ->
    match e.parent with
    | Some e' -> get_ref e' id
    | None -> raise (Var_not_found id)

let lookup e id =
  !(get_ref e id)

let rec update e id v =
  (get_ref e id) := v
