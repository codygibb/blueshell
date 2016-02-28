open Printf

let rec walk dir f =
  Array.iter
    (fun file ->
      let path = Filename.concat dir file in
      if Sys.is_directory path then walk path f else f path)
    (Sys.readdir dir)

let read f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let usage () =
  printf "Usage: %s [dir]\n" Sys.argv.(0);
  exit 1

let _  =
  let test_progs = if Array.length Sys.argv != 2 then usage () else Sys.argv.(1) in
  walk test_progs (fun infile ->
    if Filename.check_suffix infile ".blu" then begin
      printf "%s ... " infile;
      flush stdout;

      let outfile = infile ^ ".out" in
      let actual_outfile = Filename.temp_file "." ".actual" in
      let actual_oc = open_out actual_outfile in
      let oldstdout = Unix.dup Unix.stdout in

      (* Re-direct stdout to actual_outfile. *)
      Unix.dup2 (Unix.descr_of_out_channel actual_oc) Unix.stdout;

      Interpreter.run (Interpreter.get_lexbuf infile);
      flush stdout;
      close_out actual_oc;

      (* Restore stdout. *)
      Unix.dup2 oldstdout Unix.stdout;

      let expected = read outfile in
      let actual = read actual_outfile in 
      if expected = actual then printf "Passed!\n"
      else begin
        printf "Failed\n";
        printf "Expected output:\n%s\n" expected;
        printf "--------\n";
        printf "Actual output:\n%s\n" actual;
        printf "========\n";
      end
    end
  )
