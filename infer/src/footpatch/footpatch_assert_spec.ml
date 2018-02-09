module FL = Footpatch_log
module F = Format
module FU = Footpatch_utils

let fs = F.sprintf

type fix =
  | Null_exn
  | Null_instantiate

type t =
  { typ : fix
   ; fixes_var_type : string option (* normalized typname *)
   ; node: Cfg.node
   ; pvar : Pvar.t option
   ; insn : Sil.instr (* The insn for location *)
  }

let store : (Sil.instr, t) Hashtbl.t ref = ref (Hashtbl.create 10)

let save c =
  if not (Hashtbl.mem !store c.insn) then Hashtbl.replace !store c.insn c

type prune_info =
  { pvar : Pvar.t option
  ; typ : string option
  ; insn : Sil.instr
  }

let prune_store : (Sil.instr, prune_info) Hashtbl.t ref =
  ref (Hashtbl.create 10)

let save_prune p =
  if not (Hashtbl.mem !prune_store p.insn) then Hashtbl.replace !prune_store p.insn p

let lookup_prune_parent node =
  let insns = Cfg.Node.get_instrs node in
  let prune_infos = List.fold_left (fun acc insn ->
      match insn with
      | Sil.Prune _ ->
          let prune_info =
            try Some (Hashtbl.find !prune_store insn)
            with Not_found -> None in
          begin
            match prune_info with
            | Some prune_info -> prune_info::acc
            | None -> acc
          end
      | _ -> acc) [] insns
  in
  match prune_infos with
  | hd :: _ -> Some hd
  | _ -> None

(** Same as footpatch, except that we supply a starting parent node *)
let patch_for_single_candidate bug_pvar bug_node name candidate parent i =
  let (!) = Footpatch_utils.pvar_to_string in

  let repair_fragment =
    (* 1. Check fixable pvar *)
    match lookup_prune_parent parent with
    | Some { pvar = Some fix_var_name ; _ } ->
        (* 2. Get fragment and rename *)
        begin
          match FU.get_line_span parent candidate.node name with
          | Some (start_line,end_line) ->
              let open Location in
              FL.log
                (fs "Linespan is %d - %d (ZERO INDEXED. ADD 1 IF LOOKING \
                     AT SOURCE)" start_line end_line);
              let file = Cfg.Node.get_loc parent |> fun loc -> loc.file in
              begin
                match
                  FU.lines_from_file (start_line,end_line) file "<not implemented>"
                with
                | Some fragment ->
                    FU.rename_if_needed bug_pvar !fix_var_name fragment
                | None -> None
              end
          | None -> None
        end
    | _ -> None in

  match repair_fragment with
  | Some fragment ->
      (* 3. Get patch location *)
      let patch_location = FU.get_primary_patch_location_of_bug bug_node in

      (* 4. Create directories to leave patchs in *)
      let dir = name in
      let _ = FU.create_new_dir ~dir in
      let dir = dir^"/patches" in
      let dir = FU.create_new_dir ~dir in

      let function_name =
        Cfg.Node.get_proc_desc bug_node
        |> Cfg.Procdesc.get_proc_name
        |> Procname.to_string in

      (* 5. Get loc so we know where to dump patches *)
      let loc = Cfg.Node.get_loc bug_node in
      FL.log (fs "\t[+] Writing patches to %s." dir);

      FU.dump_patch patch_location dir loc fragment i function_name
  | None -> FL.log "\t[-] No candidates found."

let fix_null_exn bug_node bug_pvar _bug_typ =
  let candidates = Hashtbl.fold (fun _ (c : t) acc ->
      match c.typ with
      | Null_exn -> c::acc
      | _ -> acc) !store [] in

  FL.log
  @@ fs "[+] Filtered %d null_exn candidates"
  @@ IList.length candidates;

  (* Null exn version does not provide fixes_var_type or pvar. we must find it
     in assert store. Span two parent nodes for exn *)
  let patch_routine c =
    try
      let preds = Cfg.Node.get_preds c.node in
      let parent = preds |> IList.hd |> Cfg.Node.get_preds |> IList.hd in
      patch_for_single_candidate bug_pvar bug_node "null_exn_assert" c parent 0
    with Failure _ -> FL.log "\t[-] No candidates found."
  in
  List.iter patch_routine candidates

(** Needs testing. Currently disabled *)
let fix_null_instantiate bug_node bug_pvar _bug_typ =
  let candidates =
    Hashtbl.fold
      (fun _ (c : t) acc ->
         match c.typ with
         | Null_instantiate -> c::acc
         | _ -> acc)
      !store
      []
  in

  FL.log
  @@ fs "[+] Filtered %d null_instantiate candidates"
  @@ IList.length candidates;

  (* Single parent node *)
  let patch_routine c =
    try
      let preds = Cfg.Node.get_preds c.node in
      let parent = preds |> IList.hd in
      patch_for_single_candidate
        bug_pvar
        bug_node
        "null_instantiate_assert"
        c
        parent
        0
    with Failure _ -> FL.log "\t[-] No candidates found (no parent)."
  in
  List.iter patch_routine candidates
