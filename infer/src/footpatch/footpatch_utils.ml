module F = Format
module L = Logging
module FL = Footpatch_log

module Monad = struct
  type 'a t = 'a option

  let return a =
    Some a

  let bind f = function
    | None -> None
    | Some a -> f a

  let (>>=) x f = bind f x
end

open Monad

let fs = Format.sprintf

let create_new_dir ~dir =
  let (//) = Filename.concat in
  let dir' = Config.results_dir // "footpatch" in
  FL.log (fs "\t[+] Create dir %s" dir');
  DB.create_dir dir';
  FL.log (fs "\t[+] Create dir %s" dir' // dir);
  DB.create_dir (dir' // dir);
  FL.log "\t[+] Success creating dir!";
  dir' // dir

let pvar_of_null_deref_err_desc s =
  let words = Str.split (Str.regexp "[ \t]+") s in
  try
    let pvar = IList.nth words 2 in
    pvar
  with
  | _ -> ""

(** Strip & from pvar *)
let normalize_pvar_name = Str.replace_first (Str.regexp "&") ""

let pvar_to_string (var : Pvar.t) =
  normalize_pvar_name @@ Pvar.to_string var

let irvar_to_string (var : Ident.t) : string =
  let (!!) = Ident.to_string in
  Format.sprintf "%s" !!var

(**
   Extract type from error message, e.g.,
   [rd_stats.c]:[1763] :[ resource of type _IO_FILE acquired
   by call to fopen() at line 1759, column 12 is not released after line
   1763, column 3] = _IO_FILE
*)
let type_of_file_resource_err_desc s =
  let words = Str.split (Str.regexp "[ \t]+") s in
  try IList.nth words 3 with | _ -> ""

(** Find the insn containing a pvar. It is an Lvar expression *)
let insns_with_pvar insns pvar = IList.filter (function
    | Sil.Load (_, Lvar pvar', _, _) when pvar = (Pvar.to_string pvar') -> true
    | _ -> false) insns

(** Internal types by infer are usually called something like "class <typ> *".
    Normalize the name by stripping out class, and * at end *)
let normalize_java_type_name s =
  s
  |> Str.replace_first (Str.regexp "class ") ""
  |> Str.replace_first (Str.regexp " *") ""
  |> Str.replace_first (Str.regexp " ") ""
  |> Str.replace_first (Str.regexp "*") ""

let normalize_c_type_name s =
  s
  |> Str.replace_first (Str.regexp "class ") ""
  |> Str.replace_first (Str.regexp " *") ""
  |> Str.replace_first (Str.regexp " ") ""
  |> Str.replace_first (Str.regexp "*") ""
  |> Str.replace_first (Str.regexp " ") ""

let dedup l = IList.remove_duplicates (fun x y -> if x = y then 0 else -1) l

(** Utility functions for pulling out things we care about*)

(** Resolve program variables from logical variables *)
let lookup_hpred_var_of_irvar (i : Exp.t) (hpreds : Sil.hpred list) =
  IList.find_map_opt (function
      | Sil.Hpointsto (Lvar pvar,Eexp (Var _ as i2, _), _) when i = i2 ->
          Some (pvar_to_string pvar)
      | _ -> None
    ) hpreds

let lookup_hpred_typ_of_irvar_as_string (i : Exp.t) (hpreds : Sil.hpred list) =
  IList.find_map_opt (function
      | Sil.Hpointsto
          (
            Lvar _,Eexp (Var _ as i2,_),
            Sizeof (t,_,_)
          ) when i = i2 ->
          Some (Typ.to_string t)
      | _ -> None
    ) hpreds

let lookup_typ_of_irvar_or_pvar (i : Exp.t ) (hpreds: Sil.hpred list) =
  match i with
  | Lvar pvar ->
      IList.find_map_opt (function
          | Sil.Hpointsto (Lvar pvar', _, Sizeof (Tptr (Tstruct typname, _), _, _)) ->
              if pvar = pvar' then Some typname
              else None
          | _ -> None)
        hpreds

  | Var irvar ->
      IList.find_map_opt (function
          | Sil.Hpointsto (Var irvar', _, Sizeof (Tstruct typname, _, _)) ->
              FL.log
                (fs "Comparing [tstruct] irvar %S and irvar' %S"
                   (irvar_to_string irvar)
                   (irvar_to_string irvar'));
              if irvar = irvar' then Some typname
              else None
          | Sil.Hpointsto (Var irvar', _, Sizeof (Tptr (Tstruct typname, _), _, _)) ->
              FL.log
                (fs "Comparing [tptr] irvar %S and irvar' %S"
                   (irvar_to_string irvar)
                   (irvar_to_string irvar'));
              if irvar = irvar' then Some typname
              else None
          | _ -> None) hpreds
  | _ -> None

let lookup_hpred_program_var_at_alloc = function
  | Exp.Lvar pvar::_ -> Some pvar
  | _ -> None

(** Whyyyy use Obj *)
let changed cmap v =
  match cmap (Obj.repr v) with
  | Utils.Red -> true
  | _ -> false

let lookup_hpred_typ_from_new_mem_pvar pvar hl : Typename.t option =
  IList.find_map_opt (function
      | Sil.Hpointsto
          (
            Lvar pvar',
            _,
            Sizeof (Tptr (Tstruct typname, _), _,_)
          )
        when Pvar.equal pvar pvar' ->
          Some typname
      | _ -> None)
    hl

let lookup_hpred_typ_from_new_mem_pvar_as_string pvar hl : Typename.t option =
  let (!) = pvar_to_string in
  IList.find_map_opt (function
      | Sil.Hpointsto
          (Lvar pvar',
           _,
           Sizeof (Tptr (Tstruct typname, _), _,_)
          )
        when
          pvar = !pvar' ->
          Some typname
      | _ -> None)
    hl

(** Pvar extraction for allocations *)
let new_mem_predsymb cmap p =
  match p with
  | Sil.Apred (Aresource { ra_res = Rmemory Mnew; ra_vpath = Some (Dpvar v); _ }, _)
    when changed cmap p -> Some v
  | _ -> None

let exn_is_changed cmap h =
  match h with
  | Sil.Hpointsto (_, (Eexp (Exp.Exn _, _) as e), _)
    when changed cmap e -> true
  | _ -> false

let pre_contains_null_hpred_for_pvar pre pvar =
  let does_not_contain =
    IList.for_all (function
        | Sil.Hpointsto
            (Lvar pvar',
             Eexp (Exp.Const (Cint intlit), _),
             _) when pvar = pvar' && IntLit.isnull intlit ->
            false
        | _ -> true
      ) pre
  in
  not does_not_contain

let assert_pvar_null pre post =
  let post_sigmas = post.Prop.sigma @ post.Prop.sigma_fp in
  let pre_sigmas = pre.Prop.sigma @ pre.Prop.sigma_fp in
  IList.find_map_opt (function
      | Sil.Hpointsto
          (Lvar pvar,
           Eexp (Exp.Const (Cint intlit), _),
           Sizeof (Tptr (Tstruct typname, _), _, _))
        when IntLit.isnull intlit ->
          begin
            match pre_contains_null_hpred_for_pvar pre_sigmas pvar with
            | false ->
                Some (
                  Some pvar,
                  Some (normalize_java_type_name
                        @@ Typename.to_string typname)
                )
            | true -> None
          end
      | Sil.Hpointsto (* Same as above, but void type *)
          (
            Lvar pvar,
            Eexp (Exp.Const (Cint intlit),_),
            Sizeof (Tvoid,_,_)
          )
        when IntLit.isnull intlit ->
          begin
            match pre_contains_null_hpred_for_pvar pre_sigmas pvar with
            | false -> Some (Some pvar, None)
            | true -> None
          end
      | _ -> None)
    post_sigmas
  |> function
  | Some x -> x
  | None -> None, None

(** Climb nodes to determine source linespan from IR.*)
let get_other_parent_succ node =
  match Cfg.Node.get_preds node with
  (* Only one pred *)
  | [pred] ->
      begin
        match Cfg.Node.get_succs pred with
        (* Two successors *)
        | c1::c2::[] ->
            if Cfg.Node.get_id c1 = Cfg.Node.get_id node then Some c2
            else if  Cfg.Node.get_id c2 = Cfg.Node.get_id node then Some c1
            else None
        | _ -> None
      end
  | _ -> None

let get_line_span first_node last_node =
  function
  | "null_exn_assert" ->
      begin try
          let line x = Cfg.Node.get_loc x |> fun loc -> loc.Location.line in
          let line_start = line first_node - 1 in (* 1 indexed! *)
          (* Need to get the other node at the start point *)
          match get_other_parent_succ first_node with
          | Some other_node ->
              begin
                match Cfg.Node.get_succs other_node with
                | end_node::_ -> Some (line_start, line end_node -1 )
                | _ -> None
              end
          | _ -> None
        with
        | Failure _ -> None
      end
  | "null_instantiate_assert" ->
      begin try
          let line x = Cfg.Node.get_loc x |> fun loc -> loc.Location.line in
          let line_start = line first_node - 1 in (* 1 indexed! *)
          let child_of_last_node = Cfg.Node.get_succs last_node in
          match child_of_last_node with
          | child::_ -> Some (line_start, line child -1)
          | _ -> None
        with
        | Failure _ -> None
      end
  | _ -> None


let lines_from_file (line_start,line_end) (file : DB.source_file) fn_name =
  let src_fname = DB.source_file_to_abs_path file in
  let lines = Utils.read_file src_fname in
  FL.log (fs "\t\t[*] Origin filename: %s" src_fname);
  FL.log (fs "\t\t\t[*] Origin line num range: %d - %d" line_start line_end);
  FL.log (fs "\t\t\t[*] Origin function: %s" fn_name);
  lines >>= fun l ->
  IList.fold_lefti
    (fun acc i line ->
       if line_start <= i && i < line_end then
         begin
           FL.log (fs "\t\t\t[*] extracted: %s" line);
           acc @ [line]
         end
       else acc)
    []
    l
  |> function
  | [] -> None
  | result -> Some (String.concat "\n" result)

let rename_if_needed bug_pvar fix_var_name source_fragment =
  if fix_var_name = bug_pvar then
    begin
      FL.log "\t[&] No need to rename fragment fixing var";
      Some source_fragment
    end
  else
    begin
      FL.log
      @@ fs "\t[&] Renaming fragment fixing var to pvar: %s -> %s"
        fix_var_name bug_pvar;
      Rename.exec_cmd (Rename.rename source_fragment fix_var_name bug_pvar)
    end

let get_primary_patch_location_of_bug bug_node : int =
  let node_loc = Cfg.Node.get_loc bug_node in
  node_loc.Location.line

let get_optional_patch_locations_of_bug bug_node : int list =
  let patch_locations = ref [] in
  let pdesc = Cfg.Node.get_proc_desc bug_node in
  let pname = Cfg.Procdesc.get_proc_name pdesc in
  match Specs.get_summary pname with
  | Some summary ->
      let err_log = summary.Specs.attributes.ProcAttributes.err_log in
      let f _ (_ : Location.t) _ _ _ _ _ _ (trace : Errlog.loc_trace) _ _ =
        IList.iter (fun e ->
            let line = e.Errlog.lt_loc.Location.line in
            if not (IList.mem (fun x y -> x = y) line !patch_locations) then
              patch_locations := line::!patch_locations) trace
      in
      Errlog.iter f err_log;
      !patch_locations
  | None -> []

(** Some syntax got in the way, too bad, safely reject *)
let filter_garbage patch_line regex_string =
  try
    Str.search_forward (Str.regexp regex_string) patch_line 0 |> ignore;
    Str.matched_string patch_line |> ignore;
    None
  with | _ -> Some patch_line

(** Rewrite if statements with brackets *)
let rewrite_if_statement' next_line original_line fragment =
  try
    Str.search_forward (Str.regexp "{") original_line 0 |> ignore;
    Str.matched_string original_line |> ignore;
    FL.log "\t\t\t[+] Line does not contain {";
    None
  with | _ ->
  try
    (* if it contains if do bracketing. space is significant *)
    Str.search_forward (Str.regexp ".*if .*") original_line 0 |> ignore;
    let result =
      original_line ^
      " {\n" ^ fragment ^ "\n" ^ next_line ^ "\n" ^ "}" ^ "\n"
    in
    Some result
  with Not_found -> None

(** Rewrite if statements with brackets *)
let rewrite_if_statement original_line fragment =
  try
    Str.search_forward (Str.regexp "{") original_line 0 |> ignore;
    Str.matched_string original_line |> ignore;
    FL.log "\t\t\t[+] Line does not contain {";
    None
  with | _ ->
    begin
      FL.log "\t\t\t [+] No match for line containing {";
      try
        Str.search_forward (Str.regexp "\(.*)\)[ \n]\(.*\)") original_line 0
        |> ignore;
        let conditional_part = Str.matched_group 1 original_line in
        let line_part = Str.matched_group 2 original_line in
        let result =
          conditional_part ^
          " {\n" ^ fragment ^ "\n" ^ line_part ^ "\n" ^ "}" ^ "\n"
        in
        Some result
      with
      | _ ->
          FL.log @@ fs "\t\t[-]not modifying fragment";
          None
    end

let extract_line file_name line_num =
  try
    match Utils.read_file file_name with
    | Some res -> Some (IList.nth res line_num)
    | None -> None
  with
  | _ -> None

let is_return line =
  try
    Str.search_forward (Str.regexp ".*return.*")
      line 0 |> ignore;
    Str.matched_string line |> ignore;
    true
  with | _ -> false

let check_for_if fragment line_at_patch_location abs_filename patch_location =
  begin
    match rewrite_if_statement line_at_patch_location fragment with
    (* if it's an if statement with a single line, and no braces, add
       those *)
    | Some fragment ->
        FL.log @@ fs "\t\t[+] Fragment was if-bracketed:@.%s" fragment;
        return (fragment,true)
    | None ->
        (* check if its a return. if it is, then do bracketize_if_statement
           on line_at_patch-1 *)
        begin match is_return line_at_patch_location with
          | true ->
              FL.log "\t\t[+] Fragment is return. Doing if bracket at\
                      location-1 if needed";
              extract_line abs_filename (patch_location-1-1)
              >>= fun new_line_at_patch_location ->
              rewrite_if_statement'
                line_at_patch_location
                new_line_at_patch_location
                fragment
              >>= fun fragment ->
              FL.log
              @@ fs "\t\t[+] NEW Fragment was if-bracketed:%s@." fragment;
              return (fragment, true)
          | false ->
              FL.log "\t\t[+] Fragment was NOT if-bracketed";
              return (fragment, false)
        end
  end
  |> function
  | Some x -> x
  | None -> (fragment, false)

let do_patch bracketed patch_location fragment abs_filename line_at_patch_location =
  match bracketed with
  | true ->
      let change = Change.insert ~start_line:(patch_location-1) [fragment] in
      let indent = Indent.create ~extern_cmd:"indent" in
      Patch.create ~replace:true ~indent ~filename:abs_filename change
  | false ->
      (* If not bracketed, means there was a { at error loc. so the
         position 'after' is not patch_location-1, but just
         patch_location, ahead of the {. *)
      let is_return =
        try
          Str.search_forward (Str.regexp ".*return.*")
            line_at_patch_location 0 |> ignore;
          Str.matched_string line_at_patch_location |> ignore;
          true
        with | _ -> false
      in
      let patch_location =
        if is_return then patch_location-1
        else patch_location
      in
      let change = Change.insert ~start_line:patch_location [fragment] in
      let indent = Indent.create ~extern_cmd:"indent" in
      Patch.create ~indent ~filename:abs_filename change

let invalid_patch_location patch_location loc =
  FL.log "\t\t[-] patch location turns out to be invalid!";
  FL.log @@ fs "\t\t\t[*] patch location: %d" patch_location;
  FL.log @@ fs "\t\t\t[*] bug node loc location: %d" loc.Location.nLOC;
  FL.log "\t[-] Aborting patch";
  None

let valid_patch_message id abs_filename fn_name patch_location =
  FL.log @@ fs "\t\t[*] Destination filename: %s" abs_filename;
  FL.log @@ fs "\t\t\t[*] Destination function: %s" fn_name;
  FL.log
  @@ fs "\t\t\t[*] Destination line insert: %d" patch_location;
  FL.log @@ fs "\t[+] Wrote patch %s.patch" id

let dump_patch patch_location dir (loc : Location.t) fragment i fn_name =
  let result =
    filter_garbage fragment ".*if.*" >>= fun fragment ->
    (* filter garbage succeeds *)
    FL.log @@ fs "\t\t[+] Have fragment %S%!" fragment;
    let abs_filename = DB.source_file_to_abs_path loc.file in
    FL.log @@ fs "\t\t[+] Attempting to extract line %d from file %s%!"
      patch_location abs_filename;
    (* patch location is zero indexed wrt error message, so subtract
       one from the 1 - indexed message. UGH: bracket logic *)
    extract_line abs_filename (patch_location-1) >>= fun line_at_patch_location ->
    (* Bracketize succeeds *)
    FL.log @@ fs "\t\t[+] Succesfully extracted %S" line_at_patch_location;

    check_for_if fragment
      line_at_patch_location
      abs_filename
      patch_location
    |> fun (fragment, bracketed) ->

    let abs_filename = loc.file |> DB.source_file_to_abs_path in
    let rel_filename = Filename.basename abs_filename |> Filename.chop_extension in

    do_patch
      bracketed
      patch_location
      fragment
      abs_filename
      line_at_patch_location
    (* may fail because of, e.g., sed *)
    >>= fun patch ->

    let valid_patch_location =
      ((not (patch_location > loc.nLOC || patch_location < 0))
       || (patch_location > loc.nLOC && patch_location > 0))
    in
    (* second case: the bug node is a weird location, like 0, but
       patch location is still OK so let's try *)

    if valid_patch_location then
      let id = Format.sprintf "%s_%d_%d" rel_filename patch_location i in
      valid_patch_message id abs_filename fn_name patch_location;
      return (Patch.dump ~dir ~id patch)
    else
      invalid_patch_location patch_location loc
  in
  match result with
  | Some _ -> ()
  | None -> ()



(** e.g.,. n$43.name -> col->name *)
(**
   n$43=*&col:struct swTableColumn * [line 86];
   n$44=*&name:char * [line 86];
   n$45=*&len:int  [line 86];
   n$46=_fun_swString_dup(n$44:char *,n$45:int ) [line 86];
 *n$43.name:struct _swString *=n$46 [line 86];
*)
let resolve_logical_to_pvar_ptr logical_var_name pdesc =
  try
    let pe = Utils.pe_text in
    let (!) = Utils.pp_to_string in
    (* e.g., n$43.name -> n$43 *)
    let stripped_var_name =
      Str.search_forward
        (Str.regexp "\(n\\$[0-9][0-9]\).*") logical_var_name 0 |> ignore;
      Str.matched_group 1 logical_var_name
    in
    let desired_pvar =
      Cfg.Procdesc.fold_instrs
        (fun acc _ insn ->
           match insn with
           | Sil.Load (ident_exp,rvalue,_,_)
             when Ident.to_string ident_exp = stripped_var_name ->
               let rexp = (!(Sil.pp_exp pe) rvalue) in
               rexp
           | _ -> acc)
        ""
        pdesc
    in
    Str.replace_first
      (Str.regexp "n\\$[0-9][0-9].")
      (desired_pvar^"->")
      logical_var_name
    |> Str.replace_first (Str.regexp "&") ""
  with
  | Not_found -> logical_var_name

(** Try get the pvar/type from error description if other attempts failed
   error string and parse sil instruction *)
let get_pvar_and_typ_using_err_desc node err =
  let pe = Utils.pe_text in
  let (!) = Utils.pp_to_string in
  FL.log @@ fs "[+] Complete err string: %S" err;
  let index_to_find_line_num = 2 in

  (* Override the typ if we see it in the err message *)
  let typ =
    (try
       Str.search_forward (Str.regexp ".*_IO_FILE.*") err 0 |> ignore;
       Str.matched_string err |> ignore;
       Some "struct _IO_FILE *"
     with | _ -> None) in

  try
    begin
      let alloc_line =
        Str.search_forward (Str.regexp "at line.*") err 0 |> ignore;
        Str.matched_string err
        |> Str.split (Str.regexp "[ \t,]+")
      in

      let alloc_line =
        IList.nth alloc_line index_to_find_line_num
        |> int_of_string in
      FL.log @@ fs "[+] ERROR LINE IS: %d" alloc_line;
      let pdesc = Cfg.Node.get_proc_desc node in
      (* filter all IR insns for the source line *)
      let instrs_for_line = Cfg.Procdesc.fold_instrs (fun acc _ insn ->
          match insn with
          | Sil.Store (ident_exp, typ', _, loc)
            when loc.Location.line = alloc_line ->
              let exp = (!(Sil.pp_exp pe) ident_exp) in
              let typ =
                match typ with
                | Some typ -> typ (* set to typ above if we already determined it *)
                | None -> Typ.to_string typ'
              in
              if String.contains exp '$' then
                let exp = resolve_logical_to_pvar_ptr exp pdesc in
                (exp,typ)::acc
              else
                (exp,typ)::acc
          | _ -> acc) [] pdesc in
      match instrs_for_line with
      | (pvar, typ)::_ ->
          let pvar = normalize_pvar_name pvar in
          Some (pvar, typ)
      | _ -> None
    end
  with _ -> None

(** for mem alloc, we need to take the word at offset 8 in err desc:
   memory dynamically allocated by call to malloc() at line 101 |*)
let get_pvar_and_typ_from_mem_alloc_err node err =
  get_pvar_and_typ_using_err_desc node err

let get_pvar_from_err_assignment node err =
  let abs_filename =
    let loc = Cfg.Node.get_loc node in
    DB.source_file_to_abs_path loc.file
  in

  (* get error line from node/err *)
  let source_line_of_error =
    (try
       let index_to_find_line_num = 2 in
       let source_line_of_error =
         Str.search_forward (Str.regexp "at line.*") err 0 |> ignore;
         Str.matched_string err |>
         Str.split (Str.regexp "[ \t,]+")
       in

       let source_line_of_error =
         IList.nth source_line_of_error index_to_find_line_num
         |> int_of_string in
       (* SUBTRACT ONE 0 INDEX LINE *)
       let source_line_of_error = source_line_of_error - 1 in
       FL.log @@ fs "[+] ERROR LINE IS: %d" source_line_of_error;
       begin match extract_line abs_filename source_line_of_error with
         | Some line ->
             FL.log @@ fs "[+] Extracted line %s" line;
             Some line
         | None -> None
       end
     with | _ ->
       FL.log @@ fs "\t\t\t[-] Could not get source_line_of_error";
       None)
  in
  try
    let pvar_from_assignment_in_err_line =
      match source_line_of_error with
      | Some source_line_of_error ->
          FL.log @@ fs "\t\t\t[+] Source line of error is %s" source_line_of_error;
          Str.search_forward (Str.regexp ".*(\([a-z]*\) = fopen([a-z]*")
            source_line_of_error 0
          |> ignore;
          FL.log "\t\t\t[.] Trying to get match group...";
          let match_ = Str.matched_group 1 source_line_of_error in
          FL.log @@ fs "\t\t\t[+] Regex match: %S" match_;
          Some (match_)
      | None ->
          FL.log "\t\t\t[-] Could not get source_line_of_Error";
          None
    in
    begin
      match pvar_from_assignment_in_err_line with
      | Some x ->
          FL.log @@ fs "\t\t\t[+] success, pvar from assignment is %s" x;
          x
      | None ->
          FL.log "\t\t\t[-] fail, no pvar from assignment";
          "return" (* return garbage *)
    end
  with | _ ->
    FL.log "\t\t\t[-] Some match group failed";
    "return" (* return garbage *)

(** Try resolve type/name of resource error from error message. For
   file resource, we need to take the word at offset 10 in err desc:
   resource acquired to return by call to open() at line 555 resource
   of type _IO_FILE acquired by call to fopen() at line 1759 |*)
let get_pvar_and_typ_from_resource_err node err =
  match get_pvar_and_typ_using_err_desc node err with
  | Some (pvar,typ) ->
      let pvar = normalize_pvar_name pvar in
      let pvar_by_heuristic =
        try
          Str.search_forward (Str.regexp "acquired to.*") err 0 |> ignore;
          Str.matched_string err |>
          Str.split (Str.regexp "[ \t,]+")
          |> fun string_list ->
          (* 2 is index to find pvar *)
          IList.nth string_list 2
        with | _ -> "return" (* return junk. *)
      in

      FL.log
      @@ fs
        "[+] NOTE: pvar: %s (heuristic from error message) \
         and pvar %s (extracted from error message \
         line number)."
        pvar_by_heuristic
        pvar;

      begin
        match pvar_by_heuristic, pvar with
        | "return","return" ->
            FL.log
            @@ fs ("[+] Both heurstic and pvar is return. Final check \
                    for error description pvar match%!");
            let pvar = get_pvar_from_err_assignment node err in
            begin match pvar with
              | "return" ->
                  FL.log @@ fs "[-] get_pvar_from_err_assignment is return";
                  None
              | pvar -> Some (pvar,typ)
            end
        | "return",pvar ->
            FL.log @@ fs "[+] heurstic is return, but pvar is %s. Returning %s"
              pvar pvar;
            Some (pvar, typ)
        | pvar,"return" ->
            FL.log @@ fs "[+] pvar is return, but heuristic is %s. Returning %s"
              pvar pvar;
            Some (pvar, typ)
        | pvar, x ->
            FL.log @@ fs "[+] pvar is %s and heurstic is %s. Using pvar %s" pvar x
              pvar;
            Some (pvar, typ)
      end
  | None -> None
