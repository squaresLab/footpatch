module L = Logging

let log line =
  let (//) = Filename.concat in
  let dir = Config.results_dir // "footpatch" in
  DB.create_dir dir;
  let logfile = (dir // "log.txt") in
  let oc =
    open_out_gen
      [Open_wronly; Open_append; Open_creat; Open_text]
      0o666 logfile
  in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%s\n%!" line;
  close_out oc;
  L.out "%s@." line (* Log for multicore *)

let create_new_dir ~dir =
  let (//) = Filename.concat in
  let dir' = Config.results_dir // "footpatch" in
  DB.create_dir dir';
  DB.create_dir (dir' // dir);
  dir' // dir

let log_subdir ~dir ~filename s =
  let (//) = Filename.concat in
  let dir = create_new_dir ~dir in
  let logfile = dir // filename in
  let oc =
    open_out_gen
      [Open_wronly; Open_append; Open_creat; Open_text]
      0o666 logfile in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%s%!" s;
  close_out oc
