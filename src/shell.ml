open Printf
open Core.Std
module Re2 = Re2.Std.Re2

exception Call_failed of string

type err =
  | Exit of string * string * int 
  | Signal of string * string * string

let fd_to_str fd = fd |> Unix.in_channel_of_descr |> In_channel.input_all

let capture_call cmd =
  let pinfo = Unix.create_process ~prog:"/bin/sh" ~args:["-c"; cmd] in
  let open Unix.Process_info in
  match Unix.waitpid pinfo.pid with
  | Ok () ->  Result.Ok (fd_to_str pinfo.stdout)
  | Error (`Exit_non_zero i) ->
      Result.Error (Exit
        (fd_to_str pinfo.stdout, fd_to_str pinfo.stderr, i))
  | Error (`Signal signal) ->
      Result.Error (Signal
        (fd_to_str pinfo.stdout, fd_to_str pinfo.stderr, Signal.to_string signal))

let call cmd =
  match Unix.system cmd with
  | Ok _  -> ()
  | Error _ -> raise (Call_failed cmd)

let expand_path ?(getenv=Sys.getenv) s =
  let rec aux s =
    let re = Re2.create_exn "\$([_a-zA-Z0-9]+|\*|@|#|\?|\-|\$|!)" in
    let s' = Re2.replace_exn re s ~f:(fun m ->
      let var = Re2.Match.get_exn ~sub:(`Index 1) m in
      Option.value (getenv var) ~default:""
    )
    in
    if s = s' then s' else aux s'
  in
  let home = Option.value (getenv "HOME") ~default:"" in
  let open String.Search_pattern in
  let s = replace_all (create "~") ~in_:s ~with_:home in
  aux s
