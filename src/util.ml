let range ?(start=0) stop f  =
  let rec aux i =
    if i < stop then begin
      f i;
      aux (i + 1)
    end
  in
  aux start 

let unescape s =
  Scanf.sscanf ("\"" ^ s ^ "\"") "%S" (fun u -> u)
