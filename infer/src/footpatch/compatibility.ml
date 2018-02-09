module F = Format
module FL = Footpatch_log
module FU = Footpatch_utils

let some a = Some a

let args_contain_ident args ident =
  IList.exists (fun (exp, _) ->
      match exp with
      | Exp.Var ident' when ident = ident' -> true
      | _ -> false)
    args

(** Supply an identifier and a list of Sil instructions. Function returns
    Some call if there is a call containing the identifier in its args *)
let get_call_containing_ident insns ident =
  try
    IList.find (function
        | Sil.Call (_, _, args_ts, _, _)
          when args_contain_ident args_ts ident
          -> true
        | _ -> false) insns
    |> some
  with
  | Not_found -> None

(** Get the index of a matching ident in a list of args *)
let idx_of_ident_in_args args ident =
  IList.fold_lefti (fun acc i (exp, _) ->
      match exp with
      | Exp.Var ident' when ident = ident' -> i
      | _ -> acc) 0 args

(** Supply an ident and instructions. This will extract a type from
   call params if the ident can be found in a calls arguments *)
let lookup_typ_from_call_params_java insns ident =
  let call = get_call_containing_ident insns ident in
  match call with
  | Some (Sil.Call (_, Const (Cfun (Java proc)), args_ts, _, _)) ->
      (* since ident exists in arg we'll get a correct idx back*)
      let idx = idx_of_ident_in_args args_ts ident in
      (* Now search in e_fun in its formals at the index. e_fun is not a
         closure, but const *)
      begin
        try
          Some (List.nth (Procname.java_get_parameters_as_strings proc) idx)
        with | Failure _ -> None
      end
  | _ -> None

(** Use local proc to get typ of pvar *)
let typ_of_pvar_java curr_node pvar =
  let pdesc = Cfg.Node.get_proc_desc curr_node in
  let pname = Cfg.Procdesc.get_proc_name pdesc in
  Footpatch_log.log
  @@ F.sprintf "[+] Looking for pvar %s in pname %s" pvar
  @@ Procname.to_string pname;

  let insns = Cfg.Node.get_instrs curr_node in
  let insns_with_pvar = Footpatch_utils.insns_with_pvar insns pvar in

  (* For all instructions containing a pvar, collect the type. *)
  let hints = IList.fold_left (fun acc insn ->
      match insn with
      | Sil.Load (ident, _,typ, _) -> (ident, typ)::acc
      | _ -> acc) [] insns_with_pvar in

  let typs =
    IList.fold_left (fun acc (ident,typ) ->
        if Typ.to_string typ = "class java.lang.Object *" then
          (* type is top, see if we can do better *)
          match lookup_typ_from_call_params_java insns ident with
          | Some typ ->
              (* add the better type to the hd, and keep top *)
              typ::"class java.lang.Object *"::acc
          | None -> (Typ.to_string typ)::acc
        else (Typ.to_string typ)::acc) [] hints
  in
  Footpatch_log.log (F.sprintf "[=] I found these typs for pvar %s" pvar);
  IList.iter (fun typ -> Footpatch_log.log (F.sprintf "\t[=] %s" typ)) typs;
  typs

let lookup_typ_from_call_params_c = function
  | Sil.Call (_, Const (Cfun (C _)),args_ts, _, _) ->
      IList.map (fun x -> snd x |> Typ.to_string) args_ts
  | _ -> []
