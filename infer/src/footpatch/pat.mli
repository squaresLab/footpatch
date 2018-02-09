type prop = Prop.normal Prop.t

type t =
  | Exists_pi of (prop -> Sil.atom -> bool)
  | Not_exists_pi of (prop -> Sil.atom -> bool)
  | Exists_sigma of (prop -> Sil.hpred -> bool)
  | Not_exists_sigma of (prop -> Sil.hpred -> bool)

val matches_prop: ?pat:t -> prop list -> bool
