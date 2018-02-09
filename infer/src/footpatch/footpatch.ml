module L = Logging
module F = Format
module FL = Footpatch_log
module FU = Footpatch_utils

let fs = F.sprintf

let log_patch_locations (primary_patch_location,patch_locations) =
  FL.log (fs "\t[+] Filtered patch %d locations (optional): %s"
            (IList.length patch_locations) @@
          IList.to_string (fun i -> fs "%d" i) patch_locations);
  FL.log (fs "\t[+] Patching at primary location: %d" primary_patch_location)

let log_candidates spec_filter candidates =
  IList.iter
    (Candidate.Top_down.log
       ~dir:(Footpatch_spec.name spec_filter))
    candidates

(** Return true if success, false if fail *)
let patch_for_single_candidate bug_pvar bug_node spec_filter i candidate : bool =
  let open Candidate.Top_down in
  FL.log "--------------Patch routine for 1 candidate------------------";

  let repair_fragment =
    (* 1. Check fixable pvar *)
    match candidate.f with
    | Some fix_var_name ->
        (* 2. Get fragment and rename *)
        let start_line = candidate.loc.line - 1 in
        let src_file = candidate.loc.file in
        let function_name = candidate.pname in
        (* function name containing repair frag *)
        (match
           FU.lines_from_file
             (start_line,start_line+1) src_file
             function_name
         with
         | Some fragment ->
             FU.rename_if_needed bug_pvar fix_var_name fragment;
         | None ->
             FL.log "No fragment in lines";
             None)
    | None ->
        FL.log "Candidate does not have a renamable fixing var. \
                Abandoning candidate";
        None
  in

  match repair_fragment with
  | Some fragment ->
      (* 3. Get patch location *)
      let opt_patch_locations = FU.get_optional_patch_locations_of_bug bug_node in
      let primary_patch_location = FU.get_primary_patch_location_of_bug bug_node in
      log_patch_locations (primary_patch_location, opt_patch_locations);
      let patch_location = primary_patch_location in

      (* 4. Create directories to leave patches in *)
      (* Use the spec name as a default name for dir *)
      let dir = (Footpatch_spec.name spec_filter)^"/patches" in
      let dir = FU.create_new_dir ~dir in

      (* 5. Get loc contains filename, so we know where to dump patches *)
      let loc = Cfg.Node.get_loc bug_node in
      (* function name containing bug *)
      let function_name =
        Cfg.Node.get_proc_desc bug_node
        |> Cfg.Procdesc.get_proc_name
        |> Procname.to_string in

      FL.log (fs "\t[+] Writing patches to %s." dir);
      (try
         FU.dump_patch patch_location dir loc fragment i function_name;
       with | _ -> FL.log "Uncaught crash when dumping patch");

      FL.log (fs "\t[+] Patching complete for spec with bucket %d"
              @@ Footpatch_spec.bucket spec_filter);
      FL.log "-------------------------FIN-----------------------------------";
      true
  | None ->
      FL.log "\t[-] No candidates found.";
      FL.log "-------------------------FIN-----------------------------------";
      false

(** Deduplicates candidates based on the starting line *)
let filter_candidates_by_line candidates =
  let open Candidate.Top_down in
  IList.fold_left (fun ((lines, cs) as acc) c ->
      let line = c.loc.line in
      if IList.exists ((=) line) lines then acc
      else (line::lines, c::cs)) ([],[]) candidates
  |> snd

let print_candidates candidates =
  let open Candidate.Top_down in
  let print_env = Utils.pe_text in
  List.iteri (fun i candidate ->
      FL.log @@ fs "\t[+] Candidate %d: %s" i @@
      Utils.pp_to_string (Sil.pp_instr print_env) candidate.insn)
    candidates

let filter_candidates_by_type typ candidates =
  let open Candidate.Top_down in
  match typ with
  | Some typ ->
      FL.log @@ fs "[+] Prioritizing compatibility for type: %s" @@ typ;
      IList.find_map_opt (fun c ->
          let typs' = Compatibility.lookup_typ_from_call_params_c (c.insn) in
          if IList.exists ((=) typ) typs' then Some c
          else None) candidates
      |> begin function
        | Some res ->
            FL.log @@ fs "[+] Found candidate containing type %s" @@ typ;
            [res]
        | None ->
            FL.log @@ fs
              "[+] No specific candidate for type %s. Returning %d \
               additional candidates. (multiple candidates will issue \
               patch_1, patch_2...)"
              typ @@ IList.length candidates;
            candidates
      end
  | None -> candidates

let do_patching spec_filter curr_node pvar (typ : string option) =
  let candidates =
    Footpatch_spec.candidates_of_spec spec_filter |>
    filter_candidates_by_line
  in
  log_candidates spec_filter candidates;

  FL.log @@ fs "[+] Filtered candidates: %d" @@ IList.length candidates;

  print_candidates candidates;

  let candidates = filter_candidates_by_type typ candidates in

  FL.log @@ fs "[+] Filtered candidates by type: %d" @@ IList.length candidates;

  let patch_routine = patch_for_single_candidate pvar curr_node spec_filter in

  IList.fold_lefti
    (fun acc i cand ->
       match acc with
       | true -> acc
       | false -> patch_routine i cand)
    false
    candidates


let run curr_node bug_type pvar typ =
  FL.log (fs "[+] Patch generation routine started for bug %S." bug_type);

  let spec_filters =
    match bug_type with

    | "NULL_DEREF" ->
        let typs = Compatibility.typ_of_pvar_java curr_node pvar in
        begin
          match typs with
          | typ::_ ->
              FL.log @@ fs "[+] pvar NAME is null: %S" pvar;
              FL.log @@ fs "[+] pvar TYPE is null: %S" typ;

              let null_deref_filters =
                [Footpatch_spec.null_exn_deref_spec_void_type;
                 Footpatch_spec.null_exn_deref_spec_with_type typ]
              in

              (* Null deref fix with assert spec *)
              FL.log "[+] Performing assert spec patch routine";
              Footpatch_assert_spec.fix_null_exn curr_node pvar typ;
              (* Disable instantiate *)
              (* Footpatch_assert_spec.fix_null_instantiate curr_node pvar typ; *)

              Some null_deref_filters
          | _ -> FL.log @@ fs "[-] No type for pvar found"; None
        end

    | "RESOURCE_LEAK" ->
        FL.log @@ fs "[+] pvar NAME is leaked: %S" pvar;
        begin
          match typ with
          | Some typ ->
              FL.log @@ fs "[+] adding spec for type:  %S" typ;
              Some [Footpatch_spec.close_file_spec_with_type typ]
          | None -> FL.log @@ fs "[+] No type, using only general spec";
              Some [Footpatch_spec.close_file_spec_void_type]
        end

    | "MEMORY_LEAK" ->
        FL.log @@ fs "[+] pvar NAME not freed: %S" pvar;
        begin
          match typ with
          | Some typ ->
              FL.log @@ fs "[+] pvar TYPE not freed: %S" typ;
              Some [Footpatch_spec.memory_leak_spec_free_with_type typ]
          | None ->
              FL.log @@ fs "[-] No type for pvar found";
              None
        end
    | _ -> None
  in

  match spec_filters with
  | Some spec_filters ->
      List.iter (fun spec_filter ->
          (FL.log @@
           fs "\t[=] Attempting patching with spec %S"
           @@ Footpatch_spec.name spec_filter);
          do_patching spec_filter curr_node pvar typ |> ignore)
        spec_filters
  | None -> ()
