val render_body : _ Page.t -> string

val render_head :
  site:'a Site.t -> ?sec:'a Section.t -> ?page:'a Page.t -> unit -> Htmlit.El.html
