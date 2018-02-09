type prop = Prop.normal Prop.t

type t =
  | Exists_pi of (prop -> Sil.atom -> bool)
  | Not_exists_pi of (prop -> Sil.atom -> bool)
  | Exists_sigma of (prop -> Sil.hpred -> bool)
  | Not_exists_sigma of (prop -> Sil.hpred -> bool)

let is_candidate_prop (pat : t) prop =
  let p = prop.Prop.pi in
  let s = prop.Prop.sigma in
  match pat with
  | Exists_pi pat -> IList.exists (pat prop) p
  | Exists_sigma pat -> IList.exists (pat prop) s
  | Not_exists_pi pat -> not @@ IList.exists (pat prop) p
  | Not_exists_sigma pat -> not @@ IList.exists (pat prop) s

let contains_candidate_prop (pat : t) (props : prop list) =
  IList.exists (is_candidate_prop pat) props

let matches_prop ?pat props =
  match pat with
  | Some pat -> contains_candidate_prop pat props
  | None -> true
