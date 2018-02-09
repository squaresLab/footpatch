type t = Indenter of string

let exec_cmd ?(fail_if_fail = true) cmd =
  match Sys.command cmd with
  | 0 -> ()
  | _ when fail_if_fail -> failwith "Error indenting"
  | _ -> ()

let copy f1 f2 = Utils.copy_file f1 f2 |> ignore

let create ~extern_cmd = Indenter extern_cmd

let apply ~filename (Indenter i) =
  let cmd = Format.sprintf "%s %s" i filename in
  exec_cmd cmd
