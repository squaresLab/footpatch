type t

(** A patch is a change bound to a file. Possible indentation is applied to the
    change. *)
val create:
  ?indent:Indent.t ->
  ?replace:bool ->
  filename:string ->
  Change.t ->
  t option

(** Dump the patch diff in [dir] with the given prefix id. Output will be id.patch *)
val dump : dir:string -> id:string -> t -> unit

val to_string : t -> string

val pp : Format.formatter -> t -> unit

val pps : unit -> t -> string
