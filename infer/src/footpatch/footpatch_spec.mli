type t

val create:
  ?pre_pat:Pat.t ->
  ?post_pat:Pat.t ->
  ?f:(unit -> string option) ->
  name:string ->
  bucket:int ->
  t

val name : t -> string

val bucket : t -> int

val matching_pred : t -> string option

val candidates_of_spec: t -> Candidate.Top_down.t list

(** Specs *)

val null_exn_deref_spec_void_type : t

val null_exn_deref_spec_with_type : string -> t

val close_file_spec_with_type : string -> t

val close_file_spec_void_type : t

val memory_leak_spec_free : t

val memory_leak_spec_free_with_type : string -> t
