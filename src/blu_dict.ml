open Core.Std

exception Key_not_found of string

type 'a t = (string, 'a) Hashtbl.t

let create kv_list =
  let dict = String.Table.create () ~size:(List.length kv_list) in
  List.iter kv_list ~f:(fun (k, v) -> Hashtbl.set dict ~key:k ~data:v);
  dict

let get dict k =
  match Hashtbl.find dict k with
  | Some v -> v
  | None -> raise (Key_not_found k)

let set = Hashtbl.set

let del = Hashtbl.remove

let to_str d ~v_to_str =
  let buf = Bigbuffer.create 32 in
  let n = Hashtbl.length d in
  Bigbuffer.add_string buf "{";
  let add_kv k v =
      Bigbuffer.add_string buf ("\"" ^ k ^ "\"");
      Bigbuffer.add_string buf ": ";
      Bigbuffer.add_string buf (v_to_str v)
  in
  let i = ref 0 in
  Hashtbl.iteri d ~f:(fun ~key:k ~data:v ->
    if !i = n - 1 then add_kv k v
    else (
      add_kv k v;
      Bigbuffer.add_string buf ", ");
    i := !i + 1
  );
  Bigbuffer.add_string buf "}";
  Bigbuffer.contents buf

