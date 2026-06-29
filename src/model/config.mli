type 'a t constraint 'a = [> Eio.Fs.dir_ty ]

val of_file : 'a Eio.Path.t -> 'a t
(** Loads the website config from a YAML file *)

val title : 'a t -> string
val base_url : 'a t -> Uri.t
val port : 'a t -> int
val taxonomies : 'a t -> (string * string) list
(* Taxonomies in Hugo are a tuple of frontmatter key and human readable key *)
val author : 'a t -> string option
val css_path : 'a t -> ('a Eio.Path.t) option

val hugo_theme : 'a t -> string
