type t

val v : string -> string -> Section.t list -> t
val title : t -> string
val tag : t -> string
val sections : t -> Section.t list
val url : t -> string
