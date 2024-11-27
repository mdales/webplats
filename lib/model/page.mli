type image = {
  filename : string;
  description : string option;
  dimensions : (int * int) option;
}

type t

val of_file : ?titleimage_details:bool -> base:Fpath.t -> Fpath.t -> t
val title : t -> string
val url : t -> string
val date : t -> Ptime.t
val synopsis : t -> string option
val titleimage : t -> image option
val draft : t -> bool
val path : t -> Fpath.t
val body : t -> string
val tags : t -> string list
val shortcodes : t -> ((int * int) * Shortcode.t) list
val images : t -> image list
