open OUnit2

let () =
  run_test_tt_main (test_list [
    Test_blist.suite;
    Test_shell.suite;
  ])
