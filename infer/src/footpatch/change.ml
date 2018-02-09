type range =
  { start_line: int
  ; end_line: int
  }

type t =
  | Insert of int * string list
  | Remove of range

let insert ~start_line lines = Insert (start_line, lines)

let remove ~start_line ~end_line = Remove { start_line; end_line }

let to_string (change : t) =
  match change with
  | Insert (pos, l) ->
      let insertions = String.concat "\n" l in
      Format.sprintf "Insert @ %d:\t@.%s" pos insertions
  | Remove r ->
      Format.sprintf "Remove:@.\t%d - %d" r.start_line r.end_line

let pp ppf change = Format.fprintf ppf "%s" (to_string change)

let pps () change = to_string change
