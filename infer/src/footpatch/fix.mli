type prop = Prop.normal Prop.t

type candidate =
  { procname : Procname.t
  ; pre_props : prop
  ; post_props : prop list
  }

type pat =
  | Exists_pi of (Sil.atom -> bool)
  | Not_exists_pi of (Sil.atom -> bool)
  | Exists_sigma of (Sil.hpred -> bool)
  | Not_exists_sigma of (Sil.hpred -> bool)

(** Whether this formula contains the desired fixing atom *)
val is_candidate_prop : pat -> prop -> bool

val contains_candidate_prop : pat -> prop list -> bool

(** candidates containing the fixing atom *)
val candidates :
  ?pre_pat : pat ->
  ?post_pat : pat ->
  Procname.t list ->
  candidate list
