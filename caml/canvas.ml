type canvas = {
  width: int;
  height: int;
  rows: Row.row list;
  warp: int option list;
}

(* Internal functions *)

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

let line_to_row (max_at_a_run: int) (line: string) =
  let splitted_line = Core.Std.String.split line ~on:',' in
  let colors_list = Core.Std.List.map ~f:Core.Std.Int.of_string splitted_line in
  { Row.max_at_a_run = max_at_a_run; weft_color = None;
    cells = Core.Std.List.map ~f:color_to_cell colors_list }

let warp_to_string (warp: int option list) =
  let color_strings = Core.Std.List.map ~f:Cell.color_to_string warp in
  let warp_string = String.concat ", " color_strings in
  Printf.sprintf "Warp: (%s)" warp_string

(* Public functions *)

let load_canvas (filename: string) (max_at_a_run: int) =
  let lines = Core.Std.In_channel.read_lines filename in
  let rows = Core.Std.List.map ~f:(line_to_row max_at_a_run) lines in
  let canvas_width = get_width rows in
  let warp = generate_warp canvas_width [] in
  {
    width = canvas_width;
    height = List.length rows;
    rows = rows;
    warp = warp
  }

let to_string (c: canvas) =
  let rows_strings = Core.Std.List.map ~f:Row.to_string c.rows in
  let canvas_string = String.concat "\n" rows_strings in
  Printf.sprintf "%s\n%s" (warp_to_string c.warp) canvas_string
