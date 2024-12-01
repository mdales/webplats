type t

val v : ?synthetic:bool -> string -> string -> Page.t list -> t
(* [v ?synthetic title url pages] *)

val of_directory : base:Fpath.t -> Fpath.t -> t
(* Create a section based on a directory of files, recursively collecting index.md files within *)

val pages : t -> Page.t list
val title : t -> string
val url : ?page:Page.t -> t -> string
val synthetic : t -> bool
