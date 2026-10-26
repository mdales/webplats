val render_body : site:Site.t -> Page.t -> string

val render_head :
  site:Site.t -> ?sec:Section.t -> ?page:Page.t -> unit -> Htmlit.El.html
