type range =
  { start_line: int
  ; end_line: int
  }

type t =
  | Insert of int * string list
  | Remove of range

val insert: start_line:int -> string list -> t

val remove: start_line:int -> end_line:int -> t

val to_string : t -> string

val pps : unit -> t -> string

val pp : Format.formatter -> t -> unit
