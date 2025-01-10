type image = {
  filename : string;
  description : string option;
  dimensions : (int * int) option;
}

type t

val of_string : string -> t
val update_titleimage : t -> image option -> t
val update_images : t -> image list -> t
val title : t -> string option
val date : t -> Ptime.t
val tags : t -> string list
val titleimage : t -> image option
val synopsis : t -> string option
val draft : t -> bool
val images : t -> image list
val aliases : t -> string list
val get_key_as_string : t -> string -> string option
val get_key_as_date : t -> string -> Ptime.t option
val get_key_as_string_list : t -> string -> string list
