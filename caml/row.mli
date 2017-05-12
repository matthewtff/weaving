type row = { max_at_a_run: int; weft_color: int option; cells: Cell.cell list }

val get_cell : row -> int -> Cell.cell
val get_cells : row -> Cell.cell list
val get_length: row -> int

val use_weft : row -> int option -> int -> row
val use_warp : row -> int -> row

val to_string : row -> string
val of_string : int -> string -> row
