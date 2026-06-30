type 'a t constraint 'a = [> Eio.Fs.dir_ty ]

val v : ?synthetic:bool -> string -> Uri.t -> 'a Page.t list -> 'a t
(* [v ?synthetic title url pages] *)

val of_directory : base:'a Eio.Path.t -> 'a Eio.Path.t -> 'a t
(* Create a section based on a directory of files, recursively collecting index.md files within *)

val updated_with_page : 'a t -> 'a Page.t -> 'a t
val pages : 'a t -> 'a Page.t list
val title : 'a t -> string
val uri : ?page:'a Page.t -> ?resource:string -> 'a t -> Uri.t
val synthetic : 'a t -> bool
