type 'a t constraint 'a = [> Eio.Fs.dir_ty ]

val v : string -> string -> ('a Section.t) list -> 'a t
val title : 'a t -> string
val tag : 'a t -> string
val sections : 'a t -> ('a Section.t) list
val uri : 'a t -> Uri.t
