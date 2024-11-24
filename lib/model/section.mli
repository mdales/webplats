type t

val of_directory : base:Fpath.t -> Fpath.t -> t

val pages : t -> Page.t list

val title : t -> string

val url : t -> string

