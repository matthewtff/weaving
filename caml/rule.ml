type check_type = CheckWarp | CheckWeft

let rec transpose (rows: Cell.cell list list) =
  match rows with
    | [] -> []
    | [] :: tail -> transpose tail
    | _ -> List.map List.hd rows :: transpose (List.map List.tl rows)

let find_max_run (state: int * Cell.z_index option * int * (Cell.cell -> bool))
                 (cell: Cell.cell) =
  let max_run, last_index, current_run, check_fun = state in
  if (cell.index = last_index) && check_fun(cell) then
    (max max_run (current_run + 1)), cell.index, current_run + 1, check_fun
  else max_run, cell.index, 1, check_fun

let check_at_a_run (cells: Cell.cell list)
                   (max_at_a_run: int)
                   (check: check_type) =
  if max_at_a_run = -1 then false
  else
    let check_fun = match check with
      | CheckWeft -> (fun (cell: Cell.cell) -> Cell.is_upper(cell))
      | CheckWarp -> (fun (cell: Cell.cell) -> Cell.is_lower(cell)) in
    let max_run, _, _, _ =
      List.fold_left find_max_run (1, None, 0, check_fun) cells in
    max_run > max_at_a_run

let check_row (row: Row.row) =
  let row_failed = check_at_a_run row.cells row.max_at_a_run CheckWeft in
  if row_failed then
    let () = Printf.printf "Row failed : %s\n" (Row.to_string row) in true
  else false

let check_columns_runs (max_at_a_run: int) (column: Cell.cell list) =
  check_at_a_run column max_at_a_run CheckWarp

let has_two_in_a_row (columns: Cell.cell list list) =
  let number_of_columns = List.length columns in
  let first_column: Cell.cell list = List.nth columns 0 in
  let last_column: Cell.cell list = List.nth columns (number_of_columns - 1) in
  let columns_to_check = [first_column; last_column] in
  List.exists Core.Std.Fn.id (List.map (check_columns_runs 2) columns_to_check)

let check_columns (rows: Row.row list) (max_at_a_run: int) =
  if List.length rows = 0 then false
  else
    let columns = transpose (List.map Row.get_cells rows) in
    if has_two_in_a_row columns then false
    else
      let check_results = List.map (check_columns_runs max_at_a_run) columns in
      let columns_failed = List.exists Core.Std.Fn.id check_results in
      if columns_failed then
        let () = Printf.printf "Columns failed :(\n" in
        true
      else false
