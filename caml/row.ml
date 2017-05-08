type row = { max_at_a_run: int; weft_color: int option; cells: Cell.cell list }

let get_cell (r: row) (index: int) =
  match Core.Std.List.nth r.cells index with
    | Some cell -> cell
    | None -> assert false

let get_cells (r: row) = r.cells

let get_length (r: row) = List.length r.cells

let use_weft (r: row) (color: int option) (shift: int) =
  let map_helper = fun index (cell: Cell.cell) ->
    if index != shift then cell
    else { color = cell.color; index = Some Cell.Upper }
  in { max_at_a_run = r.max_at_a_run; weft_color = color;
       cells = Core.Std.List.mapi r.cells ~f:map_helper }

let use_warp (r: row) (shift: int) =
  let map_helper = fun index (cell: Cell.cell) ->
    if index != shift then cell
    else { color = cell.color; index = Some Cell.Lower }
  in { max_at_a_run = r.max_at_a_run; weft_color = r.weft_color;
       cells = Core.Std.List.mapi r.cells ~f:map_helper }


let to_string (r: row) =
  let weft_color_string: string = Cell.color_to_string r.weft_color in
  let cells_strings = Core.Std.List.map ~f:Cell.to_string r.cells in
  let cells_string = String.concat ", " cells_strings in
  Printf.sprintf "[ %s | %s ]" weft_color_string cells_string
