type check_type = CheckAny | Warp

let rec transpose (rows: Cell.cell list list) =
  match rows with
    | [] -> []
    | [] :: tail -> transpose tail
    | _ -> List.map List.hd rows :: transpose (List.map List.tl rows)

let get_warp_color (warp: int option list) (index: int) =
  match Core.Std.List.nth warp index with
    | Some color -> color
    | None -> None

let color_warp (warp: int option list) (new_color: int option) (shift: int) =
  let map_helper = fun (index: int) (current_color: int option) ->
    if index != shift then current_color
    else new_color
  in Core.Std.List.mapi warp ~f:map_helper

let find_max_run (state: int * Cell.z_index option * int * (Cell.cell -> bool))
                 (cell: Cell.cell) =
  let max_run, last_index, current_run, check_fun = state in
  if (cell.index = last_index) && check_fun(cell) then
    (max max_run (current_run + 1)), cell.index, current_run + 1, check_fun
  else max_run, cell.index, 1, check_fun

let check_at_a_run (cells: Cell.cell list)
                   (max_at_a_run: int)
                   (check: check_type) =
  if max_at_a_run = -1 then true
  else
    let check_fun = match check with
      | CheckAny -> (fun _ -> true)
      | Warp -> (fun (cell: Cell.cell) -> Cell.is_lower(cell)) in
    let max_run, _, _, _ =
      List.fold_left find_max_run (1, None, 0, check_fun) cells in
    max_run <= max_at_a_run

let check_row (row: Row.row) =
  check_at_a_run row.cells row.max_at_a_run CheckAny

let check_columns_runs (max_at_a_run: int) (column: Cell.cell list) =
  check_at_a_run column max_at_a_run CheckAny

let has_two_in_a_row (columns: Cell.cell list list) =
  let number_of_columns = List.length columns in
  let first_column: Cell.cell list = List.nth columns 0 in
  let last_column: Cell.cell list = List.nth columns (number_of_columns - 1) in
  let columns_to_check = [first_column; last_column] in
  List.exists Core.Std.Fn.id (List.map (check_columns_runs 2) columns_to_check)

let check_columns (rows: Row.row list) (max_at_a_run: int) =
  if List.length rows = 0 then true
  else
    let columns = transpose (List.map Row.get_cells rows) in
      let check_results = List.map (check_columns_runs max_at_a_run) columns in
      List.exists Core.Std.Fn.id check_results

let rec weft_branch (row: Row.row) (warp: int option list) (shift: int) =
  let current_cell = Row.get_cell row shift in
  let current_color = current_cell.color in
  if row.weft_color = current_color || row.weft_color = None then
    fill_row (Row.use_weft row current_color shift) warp (shift + 1)
  else []

and warp_branch (row: Row.row) (warp: int option list) (shift: int) =
  let current_cell = Row.get_cell row shift in
  let current_color = current_cell.color in
  let warp_color = get_warp_color warp shift in
  if current_color = warp_color || warp_color = None then
    let colored_warp = color_warp warp current_color shift in
    fill_row (Row.use_warp row shift) colored_warp (shift + 1)
  else []

and fill_row (row: Row.row) (warp: int option list) (shift: int) =
  if shift = Row.get_length row then
    match check_row row with
    | true -> [row, warp]
    | false -> []
  else
    (weft_branch row warp shift) @ (warp_branch row warp shift)

and fill_warp (canvas: Canvas.canvas)
              (max_at_a_run: int)
              (warp: int option list)
              (rows: Row.row list) =
  let row_index = List.length rows in
  if not (check_columns rows max_at_a_run) then []
  else if row_index = canvas.height then [{
      Canvas.width = canvas.width; height = canvas.height;
      rows = rows; warp = warp
    }]
  else
    let last_filled_row = List.nth canvas.rows row_index in
    let continuations = fill_row last_filled_row warp 0 in
    let map_helper = fun row_info ->
      let row, warp = row_info in
      fill_warp canvas max_at_a_run warp (row :: rows)
    in
    List.concat (Core.Std.List.map ~f:map_helper continuations)

let print_solution (canvas: Canvas.canvas) =
  Printf.printf "Solution:\n%s\n" (Canvas.to_string canvas)

let () =
  let max_warp_at_a_run = 5 in
  let max_weft_at_a_run = 5 in
  let canvas = Canvas.load_canvas "data/real.data" max_weft_at_a_run in
  let () = Printf.printf "Canvas:\n%s\n" (Canvas.to_string canvas) in
  let results = fill_warp canvas max_warp_at_a_run canvas.warp [] in
  let () = Core.Std.printf "Number of solutions: %d\n" (List.length results) in
  Core.Std.List.iter results ~f:print_solution
