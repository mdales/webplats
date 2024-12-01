type t

val of_file : string -> t
val title : t -> string
val base_url : t -> string
val taxonomies : t -> string list
