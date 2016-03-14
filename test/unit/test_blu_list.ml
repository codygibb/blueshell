open Core.Std
open OUnit2
module L = Blu_list

let suite =
  "Blu_list">:::
  [
    "len_get_set">::(fun ctx ->
      let l = L.create [4; 7; 5] in
      assert_equal 3 (L.len l);
      assert_equal 7 (L.get l 1);
      L.set l 2 9;
      assert_equal 9 (L.get l 2);
      ()
    );
    "out_of_bounds">::(fun ctx ->
      let l = L.create [8; 9] in
      assert_raises (L.Index_out_of_bounds 2) (fun () -> (L.get l 2));
      assert_raises (L.Index_out_of_bounds (-1)) (fun () -> (L.set l (-1) 7));
      ()
    );
    "push_pop">::(fun ctx ->
      let l = L.create [5; 6; 7] in
      L.push l 8;
      assert_equal 8 (L.get l 3);
      assert_equal 4 (L.len l);
      let v = L.pop l in
      assert_equal (Some 8) v;
      assert_equal 3 (L.len l);
      ()
    );
    "pop_empty">::(fun ctx ->
      let l = L.create [] in
      assert_equal 0 (L.len l);
      assert_equal None (L.pop l);
      assert_equal 0 (L.len l);
      ()
    );
    "resize">::(fun ctx ->
      let l = L.create [1; 2] ~min_cap:2 in
      assert_equal 2 (L.capacity l);
      L.push l 3;
      assert_equal 4 (L.capacity l);
      L.push l 4;
      assert_equal 4 (L.capacity l);
      L.push l 5;
      assert_equal 8 (L.capacity l);
      let _ = L.pop l in (* len = 4 *)
      let _ = L.pop l in (* len = 3 *)
      assert_equal 8 (L.capacity l);
      let _ = L.pop l in (* len = 2, resize *)
      assert_equal 4 (L.capacity l);
      let _ = L.pop l in (* len = 1 *)
      let _ = L.pop l in (* len = 0 *)
      let _ = L.pop l in (* None *)
      let _ = L.pop l in (* None *)
      (* Popping past the number of elements should never bring
       * the capacity down to 0. *)
      assert_equal ~printer:string_of_int 4 (L.capacity l);
      ()
    )
  ]
