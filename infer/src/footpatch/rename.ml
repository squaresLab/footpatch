module F = Format

let fs = Format.sprintf

let rename fragment fix_var_name bug_pvar =
  fs "$INFER/patching/rename/rename %S %S %S"
    fragment fix_var_name bug_pvar

let exec_cmd ?(fail_if_fail = true) cmd =
  let fpath = Filename.temp_file ~temp_dir:"/tmp" "patch" ".footpatch" in
  let cmd = F.sprintf "%s > %s" cmd fpath in
  Footpatch_log.log (F.sprintf "\t[&]FFF Running cmd for rename: %s" cmd);
  match Sys.command cmd with
  | 0 ->
      (match Utils.read_file fpath with
       | Some lines ->
           (* Java parser renames 'object' to 'this', clearly not what
              we want. Rename any 'this' back to 'object' *)
           let lines =
             List.map (fun line ->
                 let result  = Str.replace_first (Str.regexp "this") "object" line in
                 result
               ) lines
           in
           Some (String.concat "\n" lines)
       | None ->
           Footpatch_log.log "\t[&] Nothing to read from tmp file when renaming";
           None)
  | _ when fail_if_fail ->
      Footpatch_log.log "\t[&] Error renaming";
      None
  | _ ->
      Footpatch_log.log  "\t[&] Rename command failed!";
      None
