type image_loader_t =
  Page.t -> string -> int * int -> string -> string -> Dream.handler

val routes_for_frontmatter_image_list :
  Section.t -> Page.t -> image_loader_t -> Dream.route list

val routes_for_frontmatter_video_list : Section.t -> Page.t -> Dream.route list

val routes_for_image_shortcodes :
  Section.t -> Page.t -> image_loader_t -> Dream.route list

val routes_for_direct_shortcodes : Section.t -> Page.t -> Dream.route list
val collect_static_routes : Site.t -> Dream.route list
val routes_for_aliases : Site.t -> Dream.route list
val routes_for_redirect_for_sans_slash : Section.t -> Page.t -> Dream.route list
