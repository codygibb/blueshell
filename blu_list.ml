open Core.Std

exception Index_out_of_bounds of int

exception Invalid_arg_num

type 'a t = 'a Array.t ref

let create = ref Array.of_list 

let get l i =
  try (!l).(i)
  with Invalid_argument _ -> raise (Index_out_of_bounds i)

let set l i v =
  try (!l).(i) <- v
  with Invalid_argument _ -> raise (Index_out_of_bounds i)

let push l v =
  (* TODO: double-size of new array if full *)
  l := Array.append (!l) (Array.of_list [v])

let pop l =
  if Array.is_empty (!l) then None
  else
    let len = Array.length (!l) in
    let v = get l (len - 1) in
    l := Array.slice (!l) 0 (len - 1);
    Some v

let len l =
  Array.length (!l)

let to_str l ~v_to_str =
  let buf = Bigbuffer.create 32 in
  let n = Array.length (!l) in

  Bigbuffer.add_string buf "[";

  Array.iteri (!l) ~f:(fun i v ->
    if i = n - 1 then Bigbuffer.add_string buf (v_to_str v)
    else (
      Bigbuffer.add_string buf (v_to_str v);
      Bigbuffer.add_string buf ", "));

  Bigbuffer.add_string buf "]";

  Bigbuffer.contents buf

let builtins = String.Set.of_list ["push"; "pop"; "len"]
