type t

val of_file : Fpath.t -> t
val title : t -> string
val base_url : t -> Uri.t
val port : t -> int
val taxonomies : t -> (string * string) list
(* Taxonomies in Hugo are a tuple of frontmatter key and human readable key *)

val hugo_theme : t -> string
