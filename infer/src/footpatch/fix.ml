module L = Logging
module F = Format

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

let debug = true

let inc = L.d_increase_indent
let dec = L.d_decrease_indent

let is_candidate_prop_debug (pat : pat) prop =
  let p = prop.Prop.pi in
  let s = prop.Prop.sigma in
  inc 1;
  L.d_strln_color Red "Checking prop:";
  inc 1;
  Prop.d_prop_with_typ prop;
  dec 1;
  dec 1;
  L.d_ln ();
  let debug_pi atom matched =
    inc 1;
    L.d_strln_color Red "Checking atom:";
    inc 1;
    Sil.d_atom atom;
    dec 1;
    dec 1;
    L.d_ln ();
    if matched then L.d_strln_color Red "MATCH!"
  in
  let debug_sigma hpred matched =
    L.d_strln_color Red "Checking hpred:";
    inc 1;
    Sil.d_hpred hpred;
    dec 1;
    L.d_ln ();
    if matched then L.d_strln_color Red "MATCH!"
  in
  match pat with
  | Exists_pi pat -> IList.exists (fun atom ->
      let res = pat atom in
      debug_pi atom res; res) p
  | Exists_sigma pat -> IList.exists (fun hpred ->
      let res = pat hpred in
      debug_sigma hpred res; res) s
  | Not_exists_pi pat -> not @@ IList.exists (fun atom ->
      let res = pat atom in
      debug_pi atom res; res) p
  | Not_exists_sigma pat -> not @@ IList.exists (fun hpred ->
      let res = pat hpred in
      debug_sigma hpred res; res) s

let is_candidate_prop (pat : pat) prop =
  if debug then is_candidate_prop_debug pat prop
  else
    let p = prop.Prop.pi in
    let s = prop.Prop.sigma in
    match pat with
    | Exists_pi pat -> IList.exists pat p
    | Exists_sigma pat -> IList.exists pat s
    | Not_exists_pi pat -> not @@ IList.exists pat p
    | Not_exists_sigma pat -> not @@ IList.exists pat s

let contains_candidate_prop (pat : pat) (props : prop list) =
  IList.exists (is_candidate_prop pat) props

let match_in_post ?pat { post_props; _ } =
  if debug then L.d_strln_color Red "Matching in POST";
  let res =
    match pat with
    | Some pat -> contains_candidate_prop pat post_props
    | None -> true in
  res

let match_in_pre ?pat { pre_props; _ } =
  if debug then L.d_strln_color Red "Matching in PRE";
  let res =
    match pat with
    | Some pat -> contains_candidate_prop pat [pre_props]
    | None -> true in
  res

let candidates ?pre_pat ?post_pat procs =
  IList.fold_left (fun acc procname ->
      let specs = Specs.get_specs procname in
      IList.fold_left (fun acc spec ->
          let pre_props = spec.Specs.pre |> Specs.Jprop.to_prop in
          let post_props = spec.Specs.posts |> IList.map fst in
          let candidate =
            { procname
            ; pre_props
            ; post_props
            }
          in
          match
            (match_in_pre ?pat:pre_pat candidate)
          , (match_in_post ?pat:post_pat candidate)
          with
          | true, true -> candidate::acc
          | _ -> acc
        ) acc specs
    ) [] procs
