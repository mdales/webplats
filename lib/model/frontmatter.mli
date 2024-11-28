type image = {
  filename : string;
  description : string option;
  dimensions : (int * int) option;
}

type t

val of_string : string -> t
val update_titleimage : t -> image option -> t
val title : t -> string option
val date : t -> Ptime.t
val tags : t -> string list
val titleimage : t -> image option
val synopsis : t -> string option
val draft : t -> bool
val images : t -> image list
