type t

val of_directory : Eio.Fs.dir_ty Eio.Path.t -> t
val sections : t -> Section.t list
val title : t -> string
val toplevel : t -> Section.t
val path : t -> Eio.Fs.dir_ty Eio.Path.t
val hugo_theme : t -> string
val taxonomies : t -> (string * Taxonomy.t) list
val uri : t -> Uri.t
val port : t -> int
val author : t -> string option
val css_path : t -> Eio.Fs.dir_ty Eio.Path.t option
val css_digest_path : t -> Eio.Fs.dir_ty Eio.Path.t option
