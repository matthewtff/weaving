type canvas = {
  max_warp_at_a_run: int;
  width: int;
  height: int;
  rows: Row.row list;
  warp: int option list;
}

(* Internal functions *)

let upper_cell = { Cell.color = None; index = Some Cell.Upper }
let lower_cell = { Cell.color = None; index = Some Cell.Lower }

let with_border (modulo: int) (index: int) (row: Row.row) =
  let cell = if index mod 2 = modulo then upper_cell else lower_cell in
  let new_cells = cell :: row.cells @ [cell] in
  { Row.max_at_a_run = row.max_at_a_run; weft_color = row.weft_color;
    cells = new_cells }

let append_borders (modulo: int) (canvas: canvas) =
  { max_warp_at_a_run = canvas.max_warp_at_a_run;
    width = canvas.width + 2; height = canvas.height;
    rows = Core.Std.List.mapi ~f:(with_border modulo) canvas.rows;
    warp = None :: canvas.warp @ [ None ]
  }

let rec generate_warp (width: int) (warp: int option list) =
  match width with
    | 0 -> warp
    | _ -> generate_warp (width - 1) (None :: warp)

let color_to_cell (color_value: int) =
  { Cell.color = Some color_value; index = None }

let get_width (rows: Row.row list) =
  match Core.Std.List.nth rows 0 with
    | None -> assert false
    | Some row -> List.length row.cells

let parse_warp (warp_str: string) =
  let colors = Core.Std.String.split ~on:',' warp_str in
  Core.Std.List.map ~f:Cell.string_to_color colors

let line_to_row (max_at_a_run: int) (line: string) =
  let splitted_line = Core.Std.String.split line ~on:',' in
  let colors_list = Core.Std.List.map ~f:Core.Std.Int.of_string splitted_line in
  { Row.max_at_a_run = max_at_a_run; weft_color = None;
    cells = Core.Std.List.map ~f:color_to_cell colors_list }

let warp_to_string (warp: int option list) =
  let color_strings = Core.Std.List.map ~f:Cell.color_to_string warp in
  Printf.sprintf "       %s" (String.concat ", " color_strings)

(* Public functions *)

let load_canvas (filename: string)
                (max_warp_at_a_run: int)
                (max_weft_at_a_run: int) =
  let lines = Core.Std.In_channel.read_lines filename in
  let rows = Core.Std.List.map ~f:(line_to_row max_weft_at_a_run) lines in
  let canvas_width = get_width rows in
  let warp = generate_warp canvas_width [] in
  {
    max_warp_at_a_run = max_warp_at_a_run;
    width = canvas_width;
    height = List.length rows;
    rows = rows;
    warp = warp
  }

let load_solved_canvas (filename: string)
                       (max_warp_at_a_run: int)
                       (max_weft_at_a_run: int) =
  let warp_line :: body_lines = Core.Std.In_channel.read_lines filename in
  let warp = parse_warp warp_line in
  { max_warp_at_a_run; width = List.length warp;
    height = List.length body_lines;
    rows = Core.Std.List.map ~f:(Row.of_string max_weft_at_a_run) body_lines;
    warp
  }

let self_check (canvas: canvas) =
  let row_check = List.exists Rule.check_row canvas.rows in
  let columns_check = Rule.check_columns canvas.rows canvas.max_warp_at_a_run in
  let () = Printf.printf "Row check: %B\nColumns check: %B\n"
    row_check columns_check in
  row_check || columns_check

let make_even (canvas : canvas) = append_borders 0 canvas
let make_odd (canvas : canvas) = append_borders 1 canvas

let to_string (c: canvas) =
  let rows_strings = Core.Std.List.map ~f:Row.to_string c.rows in
  let canvas_string = String.concat "\n" rows_strings in
  Printf.sprintf "%s\n%s" (warp_to_string c.warp) canvas_string
