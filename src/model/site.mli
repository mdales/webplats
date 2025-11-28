type t

val of_directory : Fpath.t -> t
val sections : t -> Section.t list
val title : t -> string
val toplevel : t -> Section.t
val path : t -> Fpath.t
val hugo_theme : t -> string
val taxonomies : t -> (string * Taxonomy.t) list
val uri : t -> Uri.t
val port : t -> int
val author : t -> string option
