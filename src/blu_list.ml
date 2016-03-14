open Core.Std
open Printf

exception Index_out_of_bounds of int

exception Violated_invariant

type 'a t = 
  { mutable arr : 'a option Array.t;
    mutable len : int;
  }

let create ?(min_cap=16) l = 
  let len = List.length l in
  let arr = Array.create ~len:(max min_cap len) None in
  List.iteri l ~f:(fun i v -> Array.set arr i (Some v));
  { arr = arr;
    len = len;
  }

let valid_index l i =
  i >= 0 && i < l.len

let resize l n =
  let new_arr = Array.create ~len:n None in
  Util.range l.len (fun i -> Array.set new_arr i (Array.get l.arr i));
  l.arr <- new_arr

let maybe_resize l =
  if l.len = Array.length l.arr then
    resize l (max (l.len * 2) 1)
  else if l.len > 1 && l.len < (Array.length l.arr) / 2 then
    resize l ((Array.length l.arr) / 2)

let get l i =
  if valid_index l i then
    match Array.get l.arr i with
    | Some v -> v
    | None -> raise Violated_invariant
  else
    raise (Index_out_of_bounds i)

let set l i v =
  if valid_index l i then
    Array.set l.arr i (Some v)
  else
    raise (Index_out_of_bounds i)

let push l v =
  maybe_resize l;
  Array.set l.arr l.len (Some v);
  l.len <- l.len + 1

let pop l =
  maybe_resize l;
  if l.len = 0 then None
  else begin
    let last_i = l.len - 1 in
    let v = get l last_i in
    Array.set l.arr last_i None;
    l.len <- l.len - 1;
    Some v
  end

let len l =
  l.len

let capacity l =
  Array.length l.arr

let to_str l ~v_to_str =
  let buf = Bigbuffer.create 32 in
  Bigbuffer.add_string buf "[";
  Util.range l.len (fun i ->
    let v = get l i in
    if i = l.len - 1 then
      Bigbuffer.add_string buf (v_to_str v)
    else begin
      Bigbuffer.add_string buf (v_to_str v);
      Bigbuffer.add_string buf ", "
    end
  );
  Bigbuffer.add_string buf "]";
  Bigbuffer.contents buf

