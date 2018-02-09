(** Extract types of variables *)

val typ_of_pvar_java : Cfg.node -> string -> string list

val lookup_typ_from_call_params_c : Sil.instr -> string list
