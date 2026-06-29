type 'a t constraint 'a = [> Eio.Fs.dir_ty ]

val of_directory : 'a Eio.Path.t -> 'a t
val sections : 'a t -> ('a Section.t) list
val title : 'a t -> string
val toplevel : 'a t -> 'a Section.t
val path : 'a t -> 'a Eio.Path.t
val hugo_theme : 'a t -> string
val taxonomies : 'a t -> (string * ('a Taxonomy.t)) list
val uri : 'a t -> Uri.t
val port : 'a t -> int
val author : 'a t -> string option
val css_path : 'a t -> ('a Eio.Path.t) option
val css_digest_path : 'a t -> ('a Eio.Path.t) option
