type image = { filename : string; description : string option }
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
