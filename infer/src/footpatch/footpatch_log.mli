(** Log a string to the topdir "footpatch/log.txt" *)
val log : string -> unit

(** Log a string to "footpatch/dir/filename." *)
val log_subdir : dir:string -> filename:string -> string -> unit
