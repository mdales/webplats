
val render_body : Page.t -> string

val render_head : site:Site.t -> ?sec:Section.t -> ?page:Page.t -> unit -> string