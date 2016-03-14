open Printf
open Core.Std

type err =
  | Exit of string * string * int 
  | Signal of string * string * string

let fd_to_str fd = fd |> Unix.in_channel_of_descr |> In_channel.input_all

let call cmd =
  let pinfo = Unix.create_process ~prog:"/bin/sh" ~args:["-c"; cmd] in
  let open Unix.Process_info in
  match Unix.waitpid pinfo.pid with
  | Ok () ->  Result.Ok (fd_to_str pinfo.stdout)
  | Error (`Exit_non_zero i) ->
      Result.Error (Exit (fd_to_str pinfo.stdout, fd_to_str pinfo.stderr, i))
  | Error (`Signal signal) ->
      Result.Error (Signal (fd_to_str pinfo.stdout, fd_to_str pinfo.stderr, Signal.to_string signal))
