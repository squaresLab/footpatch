type t

(** Provide the absolute path to an external indent utility*)
val create: extern_cmd:string -> t

(** Indets a [filename] with external command [t] *)
val apply: filename:string -> t -> unit
