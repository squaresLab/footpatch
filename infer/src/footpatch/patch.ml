module L = Logging

type t =
  (* change is a range of lines to delete, or a starting line and string list *)
  { change : Change.t
  (* diff is only the textual difference, with indentation *)
  ; diff: string
  (* the original absolute filename *)
  ; filename: string
  (* the command used to indent files *)
  ; indent: Indent.t option
  }

let exec_cmd ?(fail_if_fail = true) cmd =
  match Sys.command cmd with
  | 0 -> ()
  | _ when fail_if_fail ->
      let err = Format.sprintf "Error executing %s" cmd in
      failwith err
  | _ -> L.out "Error executing %s" cmd

let copy f1 f2 = Utils.copy_file f1 f2 |> ignore

let unified_patch_diff f1 f2 =
  try
    (* e.g., /tmp/diff123.diff *)
    let dst_file = Filename.temp_file ~temp_dir:"/tmp" "udiff_" ".footpatch" in
    let cmd_str = Format.sprintf "diff -u %s %s > %s" in
    let cmd = cmd_str f1 f2 dst_file in
    exec_cmd ~fail_if_fail:false cmd;
    (* Fix up patch format wrt source and destination files *)
    Footpatch_log.log @@ Format.sprintf "\t\t[p] Patch command 1: %S " cmd;
    let cmd = Format.sprintf "sed -i '1p;2d' %s" dst_file in
    Footpatch_log.log @@ Format.sprintf "\t\t[p] Patch command 2: %S " cmd;
    exec_cmd ~fail_if_fail:true cmd;
    exec_cmd ~fail_if_fail:true @@ Format.sprintf "echo >> %s" dst_file;
    match Utils.read_file dst_file with
    | Some res -> Some (String.concat "\n" res)
    | None -> failwith @@ Format.sprintf "No file %s" dst_file
  with
  | Failure _ -> None

let do_indent ?indent filename =
  match indent with
  | Some i -> Indent.apply ~filename i
  | None -> ()

(**
replace is for when we want to replace/write over the
    line where we're inserting. E.g., when

    if (foo) dothing();

    becomes

    if (foo) {
      fix();
      dothing();
    }

   XXX This can be replaced in future.
*)
let insert ?(replace=false) ~(this:string list) ~at ~content =
  List.mapi
    (fun i line ->
       if i = at then
         if replace then
           this
         else
           this @ [line]
       else
         [line])
    content
  |> List.flatten
  |> String.concat "\n"

let write_to_file file content =
  try
    let oc = open_out file in
    Printf.fprintf oc "%s\n" content;
    close_out oc;
    Footpatch_log.log @@ Format.sprintf "Wrote to file %s" file;
    Some ()
  with
  | _ ->
      Footpatch_log.log @@ Format.sprintf "Could not write to file %s" file;
      None

let insert_in_file ?replace pos lines file =
  try
    Footpatch_log.log @@ Format.sprintf "Reading from file %s@." file;
    match Utils.read_file file with
    | Some file_content ->
        let result = insert ?replace ~this:lines ~at:pos ~content:file_content in
        write_to_file file result
    | None ->
        Footpatch_log.log
        @@ Format.sprintf
          "Could not read from file %s when trying to insert content@."
          file;
        None
  with | _ ->
    Footpatch_log.log @@
    Format.sprintf "Could not insert in file.";
    None

let remove_in_file _ _ _ = failwith "Removal not implemented"

let apply_change ?replace change file =
  let open Change in
  match change with
  | Insert (pos, lines) -> insert_in_file ?replace pos lines file
  | Remove { start_line; end_line } -> remove_in_file start_line end_line file

let indent_maybe ?indent change file =
  let open Change in
  match change with
  | Insert _ -> do_indent ?indent file
  | Remove _ -> ()

let unified_diff_of_change ?replace filename change =
  let dst_filename = "/tmp/"^(Filename.basename filename) in
  let tmp_patched_diff = dst_filename^"_tmp_patched_diff" in
  copy filename tmp_patched_diff;
  match apply_change ?replace change tmp_patched_diff with
  | Some _ -> unified_patch_diff filename tmp_patched_diff
  | None -> None

let create ?indent ?replace ~filename change =
  (* temporary copy of the original file which we can apply the patch to,
     for when we want to dump tmp files in the directory of the file being patched *)
  let dst_fname = ((Filename.basename filename)^"_") in
  let tmp_original =
    Filename.temp_file ~temp_dir:"/tmp"
      (dst_fname^"_tmp_original_") ".footpatch"
  in
  let tmp_patched =
    Filename.temp_file ~temp_dir:"/tmp"
      (dst_fname^"tmp_patched_") ".footpatch"
  in
  copy filename tmp_original;
  copy filename tmp_patched;
  indent_maybe change tmp_patched; (* TODO: should use ?indent *)
  indent_maybe change tmp_original;
  begin
    match unified_diff_of_change ?replace filename change with
   | Some diff -> Some {change; diff; filename; indent}
   | None -> None
  end

let dump ~dir ~id patch =
  let filename = Format.sprintf "%s/%s.patch" dir id in
  let oc = open_out filename in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf  fmt "%s" patch.diff;
  close_out oc

let to_string (patch : t) = Format.sprintf "%s" patch.diff

let pp ppf patch = Format.fprintf ppf "%s" (to_string patch)

let pps () patch = to_string patch
