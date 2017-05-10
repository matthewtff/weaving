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
  let can_use_weft_color = row.weft_color = current_color ||
                           row.weft_color = None in
  let weft_is_predefined = current_cell.index = Some Cell.Upper in
  if can_use_weft_color || weft_is_predefined then
    fill_row (Row.use_weft row current_color shift) warp (shift + 1)
  else []

and warp_branch (row: Row.row) (warp: int option list) (shift: int) =
  let current_cell = Row.get_cell row shift in
  let current_color = current_cell.color in
  let warp_color = get_warp_color warp shift in
  let can_use_warp_color = current_color = warp_color ||
                           warp_color = None in
  let warp_is_predefined = current_cell.index = Some Cell.Lower in
  if can_use_warp_color || warp_is_predefined then
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
      rows = List.rev rows; warp = warp
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

let solve_canvas (message: string)
                 (canvas: Canvas.canvas)
                 (max_warp_at_a_run: int) =
  let () = Printf.printf "Solving canvas:\n%s\n" (Canvas.to_string canvas) in
  let results = fill_warp canvas max_warp_at_a_run canvas.warp [] in
  let number_of_results = List.length results in
  if number_of_results = 0 then 0
  else
    let () = Printf.printf "%s\n" message in
    let () = Core.Std.List.iter results ~f:print_solution in
    let () = Printf.printf "Number of solutions: %d\n" number_of_results in
    number_of_results

let solve_with_borders (canvas: Canvas.canvas) (max_warp_at_a_run: int) =
  let number_of_even_solutions = solve_canvas
    "Even solver" (Canvas.make_even canvas) max_warp_at_a_run in
  let number_of_odd_solutions = solve_canvas
    "Odd solver" (Canvas.make_odd canvas) max_warp_at_a_run in
  number_of_even_solutions + number_of_odd_solutions

let solve (filename: string) (max_warp_at_a_run: int) (max_weft_at_a_run: int) =
  let canvas = Canvas.load_canvas filename max_weft_at_a_run in
  if solve_canvas "Initial solver" canvas max_warp_at_a_run != 0 then ()
  else
    let () = Printf.printf "%s\n%s\n" "No solution" "Solving with borders..." in
    let number_of_solutions = solve_with_borders canvas max_warp_at_a_run in
    if number_of_solutions = 0 then
      let () = Printf.printf "%s" "No solutions found\n" in exit 1
    else Printf.printf "Total number of solutions: %d\n" number_of_solutions


let spec =
  let open Core.Std.Command.Spec in empty
    +> flag "max-warp-at-a-run" (optional_with_default 5 int)
      ~doc:"Maximum number of threads in a column"
    +> flag "max-weft-at-a-run" (optional_with_default 5 int)
      ~doc:"Maximum number of threads in a row"
    +> anon ("filename" %: file)

let command =
  Core.Std.Command.basic
    ~summary:"Solve weaving problem"
    ~readme:(fun () -> "Pass an input and wait for wonder!")
    spec
    (fun max_warp_at_a_run max_weft_at_a_run filename () ->
        solve filename max_warp_at_a_run max_weft_at_a_run)

let () = Core.Std.Command.run ~version:"0.0.1" ~build_info:"MCL" command
