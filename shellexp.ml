open Printf
open Core.Std

let () =
  let cmd = "ls | grep '^.*ml$'" in
  let pinfo = Unix.create_process ~prog:"/bin/sh" ~args:["-c"; sprintf "cd %s && %s" (Unix.getcwd ()) cmd] in
  let open Unix.Process_info in
  let ic = Unix.in_channel_of_descr pinfo.stdout in
  let output = In_channel.input_all ic in
  (match Unix.waitpid pinfo.pid with
  | Ok () -> ()
  | Error (`Exit_non_zero i) -> printf "exit non-zero: %d\n" i
  | Error (`Signal signal) -> printf "exit signal: %s\n" (Signal.to_string signal));
  print_endline output
