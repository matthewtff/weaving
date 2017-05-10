type canvas = {
  width: int;
  height: int;
  rows: Row.row list;
  warp: int option list;
}

val load_canvas : string -> int -> canvas
val make_even : canvas -> canvas
val make_odd : canvas -> canvas
val to_string : canvas -> string
