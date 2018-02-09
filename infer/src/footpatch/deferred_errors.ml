(** When exception/null derefs are handled by infer, it typically happens on
    demand and then returns unit. We'll save the exn and curr_node for
    processing patching afterward *)

type error = exn * Cfg.node

let errors : error list ref = ref []

let err_desc_of_t error =
  let _, err_desc, _, _, _, _, _ = Exceptions.recognize_exception (fst error) in
  Utils.pp_to_string Localise.pp_error_desc err_desc

(** abs.ml will report the same memory leak multiple times (probably
   because multiple paths) . Dedup. *)
let save (error : error) : unit =
  let ed = err_desc_of_t error in
  if IList.mem (fun ed e -> err_desc_of_t e = ed) ed !errors then ()
  else errors := error::!errors

let get () = !errors
