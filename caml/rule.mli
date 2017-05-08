
(* Returns |true| if row failed check, |false| otherwise *)
val check_row : Row.row -> bool

(* Returns |true| if columns failed check, |false| otherwise *)
val check_columns : Row.row list -> int -> bool
