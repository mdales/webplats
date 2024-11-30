type t

val of_directory : Fpath.t -> t
val sections : t -> Section.t list
val path : t -> Fpath.t
val title : t -> string
val root_pages : t -> Page.t list
