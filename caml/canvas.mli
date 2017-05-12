type canvas = {
  max_warp_at_a_run: int;
  width: int;
  height: int;
  rows: Row.row list;
  warp: int option list;
}

val load_canvas : string -> int -> int -> canvas
val load_solved_canvas : string -> int -> int -> canvas

(** Returns |true| if any of checks failed *)
val self_check : canvas -> bool

val make_even : canvas -> canvas
val make_odd : canvas -> canvas
val to_string : canvas -> string
