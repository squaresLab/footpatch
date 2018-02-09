module L = Logging

type prop = Prop.normal Prop.t

let in_reexec : bool ref = ref false

let set_reexec v = in_reexec := v

module Top_down = struct
  type t =
    { insn : Sil.instr
    ; pre : prop
    ; post : prop list
    ; f : string option
    ; loc : Location.t
    (* No obvious way to extract procname from insn so save it here *)
    ; pname : string;
    }

  let to_string c =
    let print_env = Utils.pe_text in
    let pis_and_sigmas prop =
      let open Prop in
      let pi =  Utils.pp_to_string (Prop.pp_pi print_env) prop.pi in
      let sigma = Utils.pp_to_string (Prop.pp_sigma print_env) prop.sigma in
      let pi_fp = Utils.pp_to_string (Prop.pp_pi print_env) prop.pi_fp in
      let sigma_fp = Utils.pp_to_string (Prop.pp_sigma print_env) prop.sigma_fp in
      Format.sprintf
        "\tPI: %s@.\tSIGMA: %s@.\tFOOTPRINT PI: %s@.\tFOOTPRINT SIGMA: %s@."
        pi sigma pi_fp sigma_fp in
    let pre = pis_and_sigmas c.pre in
    let post = String.concat "@." @@ IList.map pis_and_sigmas c.post in
    let insn = Utils.pp_to_string (Sil.pp_instr print_env) c.insn in
    let with_src_insn = false in
    if with_src_insn then
      let src_line_num = c.loc.line - 1 in
      let src_file = c.loc.file |> DB.source_file_to_abs_path in
      let src_lines = Utils.read_file src_file in
      match src_lines with
      | Some src_lines ->
          let src_insn = IList.nth src_lines src_line_num in
          Format.sprintf
            "===================================================================================@.\
             PRE:@.%s@.\
             POST:@.%s@.\
             SIL INSN:@.%s@.\
             SOURCE INSN:@.%s@." pre post insn src_insn
      | None -> ""
    else
      Format.sprintf
        "===================================================================================@.\
         PRE:@.%s@.\
         POST:@.%s@.\
         SIL INSN:%s@." pre post insn

  let log ?(dir="all_candidates") ?(filename="top_down_candidates.txt") c =
    Footpatch_log.log_subdir ~dir ~filename (to_string c)

  let store : (Sil.instr, t list) Hashtbl.t ref = ref (Hashtbl.create 10)

  let get_store () = !store

  let save c =
    let existing =
      if Hashtbl.mem !store c.insn then Hashtbl.find !store c.insn
      else []
    in
    Hashtbl.replace !store c.insn (c::existing)

  let candidates_serializer () : t list Serialization.serializer =
    Serialization.create_serializer Serialization.td_patch_candidates_key

  (** Dump all candidates to a DB, using the filename of logs and special
      dirname. Triggered through environment variable 'DUMP_CANDS' *)
  let dump_candidates_to_db () =
    try
      Sys.getenv "DUMP_CANDS" |> ignore;
      (* we have to manually delete candidates in parallel mode... *)
      let td_candidates_dir = "/tmp/td_candidates" in
      DB.create_dir td_candidates_dir;
      let filepath =
        Filename.temp_file
          ~temp_dir:td_candidates_dir
          "td_candidates_" ".db"
      in
      L.out "Dumping ALL candidates to DB %s@.%!" filepath;
      Format.printf "Dumping ALL candidates to DB %s@.%!" filepath;
      let store = get_store () in
      let filepath = DB.filename_from_string filepath in
      let to_serialize = Hashtbl.fold (fun _ v acc -> v @ acc) store [] in
      Serialization.to_file (candidates_serializer ()) filepath to_serialize
    with
    | Not_found -> L.out "Dumping candidates disabled@."

  (** merge store' into !store *)
  let merge_candidate_dbs store' =  IList.iter save store'

  let load_global_store () =
    Hashtbl.clear !store;
    let td_candidates_dir = "/tmp/td_candidates" in
    let db_files = DB.paths_matching td_candidates_dir (fun _ -> true) in
    IList.iter (fun db_file ->
        let db_file = DB.filename_from_string db_file in
        match Serialization.from_file (candidates_serializer ()) db_file with
        | Some store -> merge_candidate_dbs store
        | None -> L.out "Could not deserialize store@.")
      db_files
end

module Bottom_up = struct
  type t =
    { procname : Procname.t
    ; pre : prop
    ; post : prop list
    }

  let to_string c =
    let print_env = Utils.pe_text in
    let pis_and_sigmas prop =
      let open Prop in
      let pi =  Utils.pp_to_string (Prop.pp_pi print_env) prop.pi in
      let sigma = Utils.pp_to_string (Prop.pp_sigma print_env) prop.sigma in
      let pi_fp = Utils.pp_to_string (Prop.pp_pi print_env) prop.pi_fp in
      let sigma_fp =
        Utils.pp_to_string (Prop.pp_sigma print_env) prop.sigma_fp in
      Format.sprintf
        "\tPI: %s@.\tSIGMA: %s@.\tFOOTPRINT PI: %s@.\tFOOTPRINT SIGMA: %s@."
        pi sigma pi_fp sigma_fp in
    let pre = pis_and_sigmas c.pre in
    let post = String.concat "@." @@ IList.map pis_and_sigmas c.post in
    let procname = Utils.pp_to_string Procname.pp c.procname in
    Format.sprintf
      "===================================================================================@.\
       PRE:@.%s@.\
       POST:@.%s@.\
       PROC:@.%s@." pre post procname

  let log ?(dir="all_candidates") ?(filename="bottom_up_candidates.txt") c =
    Footpatch_log.log_subdir ~dir ~filename (to_string c)
end

type td = Top_down.t

type bu = Bottom_up.t

type t =
    Top_down of td
  | Bottom_up of bu

let to_string = function
  | Top_down c -> Top_down.to_string c
  | Bottom_up c -> Bottom_up.to_string c

let pp ppf c = Format.fprintf ppf "%s" @@ to_string c

let pps () c = to_string c
