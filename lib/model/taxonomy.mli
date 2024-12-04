type t

val v : string -> Section.t list -> t
val title : t -> string
val sections : t -> Section.t list
