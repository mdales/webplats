type t

val of_file : Fpath.t -> t
(** Loads the website config from a YAML file *)

val title : t -> string
val base_url : t -> Uri.t
val port : t -> int
val taxonomies : t -> (string * string) list
(* Taxonomies in Hugo are a tuple of frontmatter key and human readable key *)
val author : t -> string option

val hugo_theme : t -> string
