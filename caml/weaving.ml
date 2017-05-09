let get_warp_color (warp: int option list) (index: int) =
  match Core.Std.List.nth warp index with
    | Some color -> color
    | None -> None

let color_warp (warp: int option list) (new_color: int option) (shift: int) =
  let map_helper = fun (index: int) (current_color: int option) ->
    if index != shift then current_color
    else new_color
  in Core.Std.List.mapi warp ~f:map_helper

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
    match Rule.check_row row with
    | false -> [row, warp]
    | true -> []
  else
    (weft_branch row warp shift) @ (warp_branch row warp shift)

and fill_warp (canvas: Canvas.canvas)
              (max_at_a_run: int)
              (warp: int option list)
              (rows: Row.row list) =
  let row_index = List.length rows in
  if Rule.check_columns rows max_at_a_run then []
  else if row_index = canvas.height then [{
      Canvas.width = canvas.width; height = canvas.height;
      rows = rows; warp = warp
    }]
  else
    let last_filled_row = List.nth canvas.rows row_index in
    let continuations = fill_row last_filled_row warp 0 in
    let map_helper = fun row_info ->
      let row, warp = row_info in
      fill_warp canvas max_at_a_run warp (row :: rows) in
    List.concat (Core.Std.List.map ~f:map_helper continuations)

let print_solution (canvas: Canvas.canvas) =
  Printf.printf "Solution:\n%s\n" (Canvas.to_string canvas)

let () =
  let max_warp_at_a_run = 15 in
  let max_weft_at_a_run = 5 in
  let canvas = Canvas.load_canvas "data/real.data" max_weft_at_a_run in
  let () = Printf.printf "Canvas:\n%s\n" (Canvas.to_string canvas) in
  let results = fill_warp canvas max_warp_at_a_run canvas.warp [] in
  let () = Core.Std.List.iter results ~f:print_solution in
  Core.Std.printf "Number of solutions: %d\n" (List.length results)
