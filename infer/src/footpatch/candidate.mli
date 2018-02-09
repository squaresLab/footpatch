type prop = Prop.normal Prop.t

val in_reexec : bool ref

(** Are we in reexecution mode? *)
val set_reexec : bool -> unit

module Top_down : sig
  type t =
    { insn : Sil.instr
    ; pre : prop
    ; post : prop list
    ; f : string option
    ; loc : Location.t
    (* no way to extract procname from insn means i need to save it here *)
    ; pname : string;
    }

  val log : ?dir:string -> ?filename:string -> t -> unit

  val get_store : unit -> (Sil.instr, t list) Hashtbl.t

  val save : t -> unit

  val to_string : t -> string

  val dump_candidates_to_db : unit -> unit

  (** Load everything in td_candidates dir into store *)
  val load_global_store : unit -> unit

end

module Bottom_up : sig
  type t =
    { procname : Procname.t
    ; pre : prop
    ; post : prop list
    }

  val log : ?dir:string -> ?filename:string -> t -> unit

  val to_string : t -> string
end

type t

val to_string : t -> string

val pp : Format.formatter -> t -> unit

val pps : unit -> t -> string
