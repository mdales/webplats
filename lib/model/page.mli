type image = { filename : string; description : string option }

type shortcode =
  | Video of string * string option
  | Image of string * string option * string option
  | Audio of string
  | Photo of string
  | Youtube of string
  | Unknown of string list

type t

val of_file : base:Fpath.t -> Fpath.t -> t
val title : t -> string
val url : t -> string
val date : t -> Ptime.t
val synopsis : t -> string option
val titleimage : t -> image option
val draft : t -> bool
val path : t -> Fpath.t
val body : t -> string
val tags : t -> string list
val shortcodes : t -> ((int * int) * shortcode) list
val images : t -> image list