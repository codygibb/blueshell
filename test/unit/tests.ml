open OUnit2

let () =
  run_test_tt_main (test_list [
    Test_blu_list.suite
  ])
