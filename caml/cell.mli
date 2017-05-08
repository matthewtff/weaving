type z_index = Lower | Upper
type cell = { color: int option; index: z_index option }

val is_lower : cell -> bool
val is_upper : cell -> bool
val is_filled : cell -> bool
val get_index : cell -> z_index
val get_color : cell -> int

val color_to_string : int option -> string
val to_string : cell -> string
