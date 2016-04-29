open Core.Std

open Printf

let rec walk dir f =
  Array.iter (Sys.readdir dir) ~f:(fun file ->
    let path = Filename.concat dir file in
    match Sys.is_directory path with
    | `Yes -> walk path f
    | `No -> f path
    | `Unknown -> ()
  )

let read f =
  let ic = In_channel.create f in
  let s = In_channel.input_all ic in
  In_channel.close ic;
  s

let rm_tmp () =
  if Filename.basename (Sys.getcwd ()) <> "blueshell" then
    failwith ("not in blueshell directory, probably an error " ^
              "to remove .tmp directory");
  match Unix.system "rm -rf .tmp" with
  | Ok _ -> ()
  | Error _ -> failwith "could not remove .tmp directory"

let usage () =
  printf "Usage: %s [dir]\n" Sys.argv.(0);
  exit 1

let _  =
  rm_tmp ();
  let test_progs = if Array.length Sys.argv <> 2 then usage () else Sys.argv.(1) in
  let passed = ref 0 in
  let failed = ref 0 in
  let skipped = ref 0 in
  walk test_progs (fun infile ->
    if Filename.check_suffix infile ".blu" then begin
      eprintf "%s ... " infile;
      if String.is_prefix (read infile) ~prefix:"#skip\n" then begin
        eprintf "Skipped\n";
        skipped := !skipped + 1;
      end else begin
        flush stdout;

        let outfile = infile ^ ".out" in
        let actual_outfile = Filename.temp_file "." ".actual" in
        let actual_oc = Out_channel.create  actual_outfile in
        let oldstdout = Unix.dup Unix.stdout in

        (* Re-direct stdout to actual_outfile. *)
        Unix.dup2 (Unix.descr_of_out_channel actual_oc) Unix.stdout;

        let syntax_err = ref false in
        let lexbuf = Interpreter.get_lexbuf infile in
        Unix.mkdir ".tmp";
        begin
          try Interpreter.run lexbuf [infile]
          with
          | Interpreter.Tracked_exec_error (lnum, err) ->
              printf "error@%d: %s\n" lnum (Err.test_str err)
          | Lexer.Error | Parser.Error ->
              syntax_err := true;
        end;
        rm_tmp ();

        flush stdout;
        Out_channel.close actual_oc;

        (* Restore stdout. *)
        Unix.dup2 oldstdout Unix.stdout;

        let expected = read outfile in
        let actual = read actual_outfile in 
        if expected = actual then begin
          eprintf "Passed!\n";
          passed := !passed + 1;
        end else begin
          eprintf "Failed\n";
          if !syntax_err then begin
            Logger.log_syntax_err lexbuf;
          end else begin
            eprintf "Expected output:\n%s\n" expected;
            eprintf "--------\n";
            eprintf "Actual output:\n%s\n" actual;
            eprintf "========\n";
          end;
          failed := !failed + 1;
        end
      end
    end;
    flush stderr
  );
  printf "Passed: %d, Failed: %d, Skipped %d\n" !passed !failed !skipped
