type t =
  | Video of string * string option
  | Raster of string * string option * string option * (int * int) option
  | Vector of string * string option * string option
  | Audio of string
  | Photo of string
  | Youtube of string
  | Unknown of string list

val find_shortcodes : string -> ((int * int) * t) list
val find_labels : string -> t list
val img_expansion : string list -> t
