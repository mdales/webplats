type t =
  | Video of string * string option
  | Image of string * string option * string option
  | Audio of string
  | Photo of string
  | Youtube of string
  | Unknown of string list

val find_shortcodes : string -> ((int * int) * t) list
