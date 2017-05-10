let rec transpose (rows: Cell.cell list list) =
  match rows with
    | [] -> []
    | [] :: tail -> transpose tail
    | _ -> List.map List.hd rows :: transpose (List.map List.tl rows)

let find_max_run (state: int * Cell.z_index option * int) (cell: Cell.cell) =
  let max_run, last_index, current_run = state in
  if cell.index = last_index then
    (max max_run (current_run + 1)), cell.index, current_run + 1
  else max_run, cell.index, 1

let check_at_a_run (max_at_a_run: int) (cells: Cell.cell list) =
  if max_at_a_run = -1 then false
  else
    let max_run, _, _ = List.fold_left find_max_run (1, None, 0) cells in
    max_run > max_at_a_run

let check_row (row: Row.row) =
  check_at_a_run row.max_at_a_run row.cells

let has_two_in_a_row (columns: Cell.cell list list) =
  let number_of_columns = List.length columns in
  let first_column: Cell.cell list = List.nth columns 0 in
  let last_column: Cell.cell list = List.nth columns (number_of_columns - 1) in
  List.exists (check_at_a_run 1) [first_column; last_column]

let check_columns (rows: Row.row list) (max_at_a_run: int) =
  if List.length rows = 0 then false
  else
    let columns = transpose (List.map Row.get_cells rows) in
    if has_two_in_a_row columns then true
    else List.exists (check_at_a_run max_at_a_run) columns
