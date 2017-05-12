type z_index = Lower | Upper
type cell = { color: int option; index: z_index option }

(* Internal functions *)

let index_to_string (index: z_index option) =
  match index with
    | Some Lower -> "v"
    | Some Upper -> "^"
    | None -> "*"

let string_to_index (text: string) =
  let stripped_text = Core.Std.String.strip text in
  match text with
    | "v" -> Some Lower
    | "^" -> Some Upper
    | _ -> None

(* Public functions *)

let is_lower (c: cell) =
  match c.index with
    | Some index -> index = Lower
    | None -> false

let is_upper (c: cell) =
  match c.index with
    | Some index -> index = Upper
    | None -> false

let is_filled (c: cell) =
  match c.color with
  | Some _ -> c.index <> None
  | None -> false

let get_index (c: cell) =
  match c.index with
    | Some index -> index
    | None -> assert false

let get_color (c: cell) =
  match c.color with
    | Some color -> color
    | None -> assert false

let color_to_string (color : int option) =
  match color with
    | Some color -> Printf.sprintf "%d" color
    | None -> "None"

let to_string (c: cell) =
  let color_string: string = color_to_string c.color in
  let index_string_list: string list = [index_to_string c.index] in
  String.concat ":" (color_string :: index_string_list)

let string_to_color (text: string) =
  let stripped_text = Core.Std.String.strip text in
  if stripped_text = "None" then None
  else Some (Core.Std.Int.of_string stripped_text)

let of_string (text: string) =
  let [color_str; index_str] = Core.Std.String.split ~on:':' text in
  { color = string_to_color color_str; index = string_to_index index_str }
