open Printf
open Core.Std

let print_fd fd = fd |> Unix.in_channel_of_descr |> In_channel.input_all |> print_string

let () =
  let dir = Filename.concat (Unix.getcwd ()) (Filename.concat "test" "progs") in
  let cmd = "ls -A1 -R | grep '^.*blu$'" in
  Unix.chdir dir;
  let pinfo = Unix.create_process ~prog:"/bin/sh" ~args:["-c"; cmd] in
  let open Unix.Process_info in
  (match Unix.waitpid pinfo.pid with
  | Ok () ->  print_fd pinfo.stdout 
  | Error (`Exit_non_zero i) ->
      printf "exit non-zero: %d\n" i;
      print_fd pinfo.stderr
  | Error (`Signal signal) ->
      printf "exit signal: %s\n" (Signal.to_string signal);
      print_fd pinfo.stderr);
