type t =
  { name : string
  ; pre_pat : Pat.t option
  ; post_pat : Pat.t option
  ; f : unit -> string option (* f result partial *)
  ; bucket : int
  }

module F = Format
module FU = Footpatch_utils

let fs = Format.sprintf

let create ?pre_pat ?post_pat ?(f = fun _ -> None) ~name ~bucket =
  { name; pre_pat; post_pat; f; bucket }

let name { name; _ } = name

let bucket { bucket; _ } = bucket

let matching_pred { f; _ } = f ()

let candidates_of_spec spec =
  let store = Candidate.Top_down.get_store () in
  Footpatch_log.log
  @@ F.sprintf "\t[+] Footpatch_spec : Store size %d"
  @@ Hashtbl.length store;
  Hashtbl.fold (fun _ value (acc : Candidate.Top_down.t list) ->
      let partial_result =
        IList.fold_left (fun acc v ->
            match
              (Pat.matches_prop ?pat:spec.pre_pat [v.Candidate.Top_down.pre])
            , (Pat.matches_prop ?pat:spec.post_pat v.Candidate.Top_down.post)
            with
            | true, true ->
                begin match matching_pred spec with
                  | Some res -> { v with Candidate.Top_down.f = Some res }::acc
                  | None -> v::acc
                end
            | true, false -> acc
            | false, true -> acc
            | false, false -> acc
          ) [] value
      in
      acc @ partial_result
    ) store []
  |> Footpatch_utils.dedup

let reject_if_irvar = function
  | "$irvar0" -> false
  | _ -> true

(** SPECS *)

(** For null_exn specs: we have a less accurate version that matches when the
    variable is VOID. We have a more accurate version that matches when the
    variable has some type. *)

let null_exn_deref_spec_void_type =
  let name = "null_exn_deref_bottom_type" in
  let tmp_res = ref "" in
  let n_is_null (_ : Pat.prop) : Sil.hpred -> bool =
    begin
      function
      | Sil.Hpointsto
          (
            Lvar f_lvar,
            Eexp (Exp.Const (Cint intlit),_),
            Sizeof (Tvoid,_,_)
          )
        when IntLit.isnull intlit ->
          Footpatch_log.log
          @@ F.sprintf
            "\t\t\t\t\t[%%] Candidate pre operates \
             on var called %s"
          @@ Pvar.to_string f_lvar;

          (* Set up callback with result *)
          tmp_res := Pvar.to_string f_lvar;
          reject_if_irvar !tmp_res;
      | _ -> false
    end
  in
  let exn_occurs (_ : Pat.prop) : Sil.hpred -> bool =
    begin
      function
      | Sil.Hpointsto (Lvar f', Eexp (Exp.Exn _, _), _) ->
          Footpatch_log.log
          @@ F.sprintf
            "\t\t\t\t\t[%%] Candidate post operates \
             on var called %s"
          @@ Pvar.to_string f';
          true
      | _ -> false
    end
  in
  let pre_pat : Pat.t = Exists_sigma n_is_null in
  let post_pat : Pat.t = Exists_sigma exn_occurs in
  let f () = Some !tmp_res in (* sigh *)
  create ~name ~pre_pat ~post_pat ~f ~bucket:0

(** Narrow down specs with a type *)
let null_exn_deref_spec_with_type typ =
  let name = "null_exn_deref_with_type" in
  let tmp_res = ref "" in
  let n_is_null (_ : Pat.prop) : Sil.hpred -> bool =
    begin
      function
      | Sil.Hpointsto
          (
            Lvar f_lvar,
            Eexp (Exp.Const (Cint intlit), _),
            Sizeof (Tptr (Tstruct typname, _), _, _)
          )
        when IntLit.isnull intlit &&
             Footpatch_utils.normalize_java_type_name
             @@ Typename.to_string typname = typ ->
          Footpatch_log.log
          @@ F.sprintf
            "\t\t\t\t\t[%%] Discovered candidate \
             with type %s"
          @@ Footpatch_utils.normalize_java_type_name
          @@ Typename.to_string typname;

          Footpatch_log.log
          @@ F.sprintf
            "\t\t\t\t\t[%%] Candidate pre operates \
             on var called %s"
          @@ Pvar.to_string f_lvar;

          tmp_res := Pvar.to_string f_lvar;
          reject_if_irvar !tmp_res;
      | _ -> false
    end
  in

  let exn_occurs (_ : Pat.prop) : Sil.hpred -> bool =
    begin function
      | Sil.Hpointsto (Lvar f', Eexp (Exp.Exn _, _), _) ->
          Footpatch_log.log
          @@ F.sprintf
            "\t\t\t\t\t[%%] Candidate post operates \
             on var called %s"
          @@ Pvar.to_string f';
          true
      | _ -> false
    end
  in
  let pre_pat : Pat.t = Exists_sigma n_is_null in
  let post_pat : Pat.t = Exists_sigma exn_occurs in
  let f () = Some !tmp_res in
  create ~name ~pre_pat ~post_pat ~f ~bucket:1

let close_file_spec_with_type t =
  let name = "close_file_resource_with_type" in
  let log = Footpatch_log.log in
  let pe = Utils.pe_text in
  let (!!) = Utils.pp_to_string in
  let norm = Footpatch_utils.normalize_java_type_name in
  let tmp_res = ref "" in
  log @@ fs "[+] Spec asked to look for type %S" @@ norm t;
  let callee_param_is_file_resource (_ : Pat.prop) : Sil.hpred -> bool =
    function | _ -> true in
  (*
     (function
     | Sil.Hpointsto
        (_,
         Eexp (_,Iformal _), (* need formal?*)
         Sizeof (Tptr (Tstruct _,_),_,_)
        ) ->
        when (norm @@ Typename.to_string typname) = norm t ->
        true
     | _ ->
        false)
  *)
  let file_resource_is_closed (post : Pat.prop) : Sil.atom -> bool =
    function
    | Apred ((Aresource { ra_res = Rfile; ra_kind = Rrelease }), l) ->
        begin
          match l with
          | hd::_ ->
              begin
                match FU.lookup_hpred_var_of_irvar hd post.sigma with
                | Some pvar ->
                    log @@ fs "[+] irvar %s lookup gives pvar %s."
                      (!!(Sil.pp_exp pe) hd)
                      pvar;
                    tmp_res := pvar;
                    begin
                      match FU.lookup_typ_of_irvar_or_pvar hd post.sigma with
                      | Some typ ->
                          log @@ fs "[?] Comparing IRVAR TYPE %s and PVAR type %s."
                            (norm @@ Typename.to_string typ) (norm t);
                          if norm @@ Typename.to_string typ = (norm t) then begin
                            tmp_res := pvar;
                            true
                          end
                          else false
                      | None ->
                          begin
                            match
                              FU.lookup_hpred_typ_of_irvar_as_string
                                hd post.sigma
                            with
                            | Some typ ->
                                log
                                @@ fs "[?] Comparing HPRED TYPE of IRVAR %s \
                                       and PVAR type %s."
                                  (norm typ) (norm t);
                                if norm typ = (norm t) then begin
                                  tmp_res := pvar;
                                  true
                                end
                                else
                                  false
                            | None -> false
                          end
                    end
                | None -> false
              end
          | _ -> false
        end
    | _ -> false (* where is my option monad :'( *)
  in
  let pre_pat : Pat.t = Exists_sigma callee_param_is_file_resource in
  let post_pat : Pat.t = Exists_pi file_resource_is_closed in
  let f () = Some !tmp_res in
  create ~name ~pre_pat ~post_pat ~f ~bucket:0

let close_file_spec_void_type =
  let name = "close_file_resource_unused" in
  let tmp_res = ref "" in
  let callee_param_is_file_resource (_ : Pat.prop) : Sil.hpred -> bool =
    begin function
      | Sil.Hpointsto
          (
            _,
            Eexp (_, _),
            Sizeof (Tptr (Tstruct _, _), _, _)
          ) -> true
      | _ -> true
    end
  in
  let file_resource_is_closed (post: Pat.prop) : Sil.atom -> bool =
    let open Sil in
    function
    | Apred ((Aresource { ra_res = Rfile; ra_kind = Rrelease; _}), l) ->
        begin match l with
          | hd::_ ->
              begin
                match FU.lookup_hpred_var_of_irvar hd post.sigma with
                | Some pvar -> tmp_res := pvar
                | None -> ()
              end
          | _ -> ()
        end;
        true
    | _ -> false in
  let pre_pat : Pat.t = Exists_sigma callee_param_is_file_resource in
  let post_pat : Pat.t = Exists_pi file_resource_is_closed in
  let f () = Some !tmp_res in
  create ~name ~pre_pat ~post_pat ~f ~bucket:0

let memory_leak_spec_free =
  let name = "free_memory_leak_bottom_type" in
  let log = Footpatch_log.log in
  let tmp_res = ref "" in
  let not_free (_ : Pat.prop) : Sil.hpred -> bool =
    function _ -> true in
  (*
     (function
     | Sil.Hpointsto
        (_,
         Eexp (_,Iformal _), (* need formal?*)
         Sizeof (Tptr (Tstruct _,_),_,_)
        ) ->
        when (norm @@ Typename.to_string typname) = norm t ->
        true
     | _ ->
        false)
  *)
  let is_freed (post: Pat.prop) : Sil.atom -> bool =
    let open Sil in
    function
    | Apred ((Aresource { ra_res = Rmemory Mmalloc; ra_kind = Rrelease; _}), l) ->
        log @@ Format.sprintf "[+] Memory released found in POST@.";
        begin match l with
          | hd::_ ->
              begin match FU.lookup_hpred_var_of_irvar hd post.sigma with
                | Some pvar ->
                    begin
                      match
                        FU.lookup_hpred_typ_from_new_mem_pvar_as_string
                          pvar post.sigma
                      with
                      | Some typ ->
                          log
                          @@ fs "[+] type fixing pvar is %s"
                          @@ Typename.to_string typ
                      | None -> ()
                    end;
                    tmp_res := pvar
                | None -> ()
              end
          | _ -> ()
        end;
        true
    | _ -> false in
  let pre_pat : Pat.t = Exists_sigma not_free in
  let post_pat : Pat.t = Exists_pi is_freed in
  let f () = Some !tmp_res in
  create ~name ~pre_pat ~post_pat ~f ~bucket:0

let memory_leak_spec_free_with_type t =
  let name = "memory_leak_with_type" in
  let log = Footpatch_log.log in
  let pe = Utils.pe_text in
  let (!!) = Utils.pp_to_string in
  let norm = Footpatch_utils.normalize_c_type_name in
  log @@ fs "\t[=] This spec matches against Type %S. Compare to pvar TYPE" t;
  let tmp_res = ref "" in
  let not_free (_ : Pat.prop) : Sil.hpred -> bool =
    function _ -> true in
  (*
     (function
     | Sil.Hpointsto
        (_,
         Eexp (_,Iformal _), (* need formal?*)
         Sizeof (Tptr (Tstruct _,_),_,_)
        ) ->
        when (norm @@ Typename.to_string typname) = norm t ->
        true
     | _ ->
        false)
  *)
  let is_freed (post: Pat.prop) : Sil.atom -> bool =
    let open Sil in
    function
    | Apred ((Aresource { ra_res = Rmemory Mmalloc; ra_kind = Rrelease; _ }), l) ->
        log @@ Format.sprintf "[+] Malloced memory FREED found in POST@.";
        let res =
          (match l with
           | hd::_ ->
               (match FU.lookup_hpred_var_of_irvar hd post.sigma with
                | Some pvar ->
                    log @@ fs "[+] irvar %s to pvar %s."
                      (!!(Sil.pp_exp pe) hd)
                      pvar;
                    (match
                       FU.lookup_hpred_typ_from_new_mem_pvar_as_string
                         pvar post.sigma
                     with
                     | Some typ' ->
                         if (norm (Typename.to_string typ')) = (norm t) then begin
                           log @@ fs "[+] typ matches needed typ of spec %s" t;
                           tmp_res := pvar;
                           true
                         end
                         else begin
                           log @@ fs "[+] type %S does not match needed typ of spec %S"
                             (norm @@ Typename.to_string typ') (norm t);
                           false
                         end
                     | _ ->
                         (match FU.lookup_typ_of_irvar_or_pvar hd post.sigma with
                          | Some typ ->
                              log @@ fs "[?] Comparing IRVAR TYPE %s and PVAR type %s."
                                (norm @@ Typename.to_string typ) (norm t);
                              if norm @@ Typename.to_string typ = (norm t) then begin
                                tmp_res := pvar;
                                true
                              end
                              else false
                          | None -> begin
                              match
                                FU.lookup_hpred_typ_of_irvar_as_string
                                  hd post.sigma
                              with
                              | Some typ ->
                                  log
                                  @@ fs "[?] Comparing HPRED TYPE of \
                                         IRVAR %s and PVAR type %s."
                                    (norm typ) (norm t);

                                  if norm typ = (norm t) then begin
                                    tmp_res := pvar;
                                    true
                                  end
                                  else false
                              | None -> false
                            end
                         )
                    )
                | None -> false);
           | _ -> false) in
        res
    | _ -> false in
  let pre_pat : Pat.t = Exists_sigma not_free in
  let post_pat : Pat.t = Exists_pi is_freed in
  let f () = Some !tmp_res in
  create ~name ~pre_pat ~post_pat ~f ~bucket:0
