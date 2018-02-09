(** Create a new subdirectory "footpatch/dir" *)
val create_new_dir : dir:string -> string

(** Extract the object name that caused the violation *)
val pvar_of_null_deref_err_desc : string -> string

(** Return insns containing a pvar *)
val insns_with_pvar : Sil.instr list -> string -> Sil.instr list

val normalize_java_type_name : string -> string

val normalize_c_type_name : string -> string

val dedup : 'a list -> 'a list

val lookup_hpred_typ_from_new_mem_pvar :
  Pvar.t -> Sil.hpred list -> Typename.t option

val lookup_hpred_typ_from_new_mem_pvar_as_string :
  string -> Sil.hpred list -> Typename.t option

val lookup_typ_of_irvar_or_pvar : Exp.t -> Sil.hpred list -> Typename.t option

(** Returns the pvar for new mem if found *)
val new_mem_predsymb : Utils.colormap -> Sil.atom -> Pvar.t option

val pvar_to_string : Pvar.t -> string

val exn_is_changed : Utils.colormap -> Sil.hpred -> bool

val normalize_pvar_name : string -> string

(** Give before and after prop for an assert instruction, and
    get back Some pvar/type if it changed it to null. *)
val assert_pvar_null :
  Prop.normal Prop.t -> Prop.normal Prop.t -> Pvar.t option * string option

(** Given a start and end node, return the source file lines
    spanning these *)
val get_line_span : Cfg.Node.t -> Cfg.Node.t -> string -> (int * int) option

val lines_from_file : int * int -> DB.source_file -> string -> string option

(** bug_pvar fix_name_var source_fragment *)
val rename_if_needed : string -> string -> string -> string option

val get_primary_patch_location_of_bug : Cfg.Node.t -> int

val get_optional_patch_locations_of_bug : Cfg.Node.t -> int list

val dump_patch : int -> string -> Location.t -> string -> int -> string -> unit

val lookup_hpred_var_of_irvar : Exp.t -> Sil.hpred list -> string option

val lookup_hpred_typ_of_irvar_as_string : Exp.t -> Sil.hpred list -> string option

val get_pvar_and_typ_from_mem_alloc_err : Cfg.Node.t -> string ->
  (string * string) option

val get_pvar_and_typ_from_resource_err : Cfg.Node.t -> string ->
  (string * string) option
