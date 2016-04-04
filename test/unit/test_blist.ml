open Core.Std
open OUnit2
open Blist

let int_list_to_str l = String.concat ~sep:" " (List.map l ~f:string_of_int)

let suite =
  "Blist">:::
  [
    "len_get_set">::(fun ctx ->
      let l = create [4; 7; 5] in
      assert_equal 3 (len l);
      assert_equal 7 (get l 1);
      set l 2 9;
      assert_equal 9 (get l 2);
      ()
    );
    "out_of_bounds">::(fun ctx ->
      let l = create [8; 9] in
      assert_raises (Index_out_of_bounds 2) (fun () -> (get l 2));
      assert_raises (Index_out_of_bounds (-1)) (fun () -> (set l (-1) 7));
      ()
    );
    "push_pop">::(fun ctx ->
      let l = create [5; 6; 7] in
      push l 8;
      assert_equal 8 (get l 3);
      assert_equal 4 (len l);
      let v = pop l in
      assert_equal (Some 8) v;
      assert_equal 3 (len l);
      ()
    );
    "pop_empty">::(fun ctx ->
      let l = create [] in
      assert_equal 0 (len l);
      assert_equal None (pop l);
      assert_equal 0 (len l);
      ()
    );
    "resize">::(fun ctx ->
      let l = create [1; 2] ~min_cap:2 in
      assert_equal 2 (capacity l);
      push l 3;
      assert_equal 4 (capacity l);
      push l 4;
      assert_equal 4 (capacity l);
      push l 5;
      assert_equal 8 (capacity l);
      let _ = pop l in (* len = 4 *)
      let _ = pop l in (* len = 3 *)
      assert_equal 8 (capacity l);
      let _ = pop l in (* len = 2, resize *)
      assert_equal 4 (capacity l);
      let _ = pop l in (* len = 1 *)
      let _ = pop l in (* len = 0 *)
      let _ = pop l in (* None *)
      let _ = pop l in (* None *)
      (* Popping past the number of elements should never bring
       * the capacity down to 0. *)
      assert_equal ~printer:string_of_int 4 (capacity l);
      ()
    );
    "slice">::(fun ctx ->
      let l = create [4; 5; 6; 7] ~min_cap:4 in
      assert_equal [5; 6] (to_list (slice l 1 3));
      assert_equal [4; 5; 6] (to_list (slice l 0 (-1)));
      assert_equal ~printer:int_list_to_str [] (to_list (slice l 0 (-4)));
      assert_raises (Index_out_of_bounds (-1)) (fun () -> slice l (-1) 3);
      assert_raises (Index_out_of_bounds 5) (fun () -> slice l 0 5);
      assert_raises (Invalid_slice (1, -4)) (fun () -> slice l 1 (-4));
      (* Force a resize and make sure everything still works. *)
      push l 8;
      assert_equal [4; 5; 6; 7] (to_list (slice l 0 (-1)));
      assert_raises (Index_out_of_bounds 6) (fun () -> (slice l 0 6));
      ()
    );
  ]
