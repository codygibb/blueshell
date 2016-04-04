open Core.Std
open Printf

exception Index_out_of_bounds of int

exception Invalid_slice of int * int

exception Violated_invariant

let default_cap = 16

type 'a t = 
  { mutable arr : 'a option Array.t;
    mutable len : int;
  }

let create ?(min_cap=default_cap) l = 
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

let iter l ~f =
  Util.range l.len (fun i -> f (get l i))

let slice l start stop =
  (if stop > l.len then raise (Index_out_of_bounds stop));
  (if start < 0 then raise (Index_out_of_bounds start));
  let norm_stop =
    if stop < 0 then
      let norm_stop = l.len + stop in
      (if norm_stop < 0 then raise (Index_out_of_bounds stop));
      norm_stop
    else stop
  in
  (if norm_stop < start then raise (Invalid_slice (start, stop)));
  if norm_stop = 0 then
    (* Array.slice will normalize 0 to Array.length, which we don't want, so
     * we handle that case manually. Note that if norm_stop is 0, start must
     * be 0. *)
    { arr = Array.create ~len:default_cap None;
      len = 0 }
  else
    let new_arr = Array.slice l.arr start norm_stop in
    { arr = new_arr;
      len = Array.length new_arr
    }

let to_list l =
  (*Array.to_list (Array.slice l.arr 0 l.len)*)
  if l.len = 0 then []
  else
    List.map (Array.to_list (Array.slice l.arr 0 l.len)) ~f:(
      fun o -> Option.value_exn o
    )

