open Core.Std

exception Index_out_of_bounds of int

type 'a t = 'a Array.t

let create = Array.of_list 

let get l i =
  try l.(i)
  with Invalid_argument _ -> raise (Index_out_of_bounds i)

let set l i v =
  try l.(i) <- v
  with Invalid_argument _ -> raise (Index_out_of_bounds i)

let to_str l ~v_to_str =
  let buf = Bigbuffer.create 32 in
  let n = Array.length l in

  Bigbuffer.add_string buf "[";

  Array.iteri l ~f:(fun i v ->
    if i = n - 1 then Bigbuffer.add_string buf (v_to_str v)
    else (
      Bigbuffer.add_string buf (v_to_str v);
      Bigbuffer.add_string buf ", "));

  Bigbuffer.add_string buf "]";

  Bigbuffer.contents buf
