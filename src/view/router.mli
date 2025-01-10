type thumbnail_loader_t =
  retina:bool -> Page.t -> string -> string -> Dream.handler

type image_loader_t =
  Page.t -> string -> int * int -> string -> string -> Dream.handler

type page_renderer_t =
  Site.t -> Section.t -> Page.t option -> Page.t -> Page.t option -> string

type meta_page_renderer_t = Page.t -> page_renderer_t
type section_renderer_t = Site.t -> Section.t -> string
type meta_section_renderer_t = Section.t -> section_renderer_t
type meta_taxonomy_section_renderer_t = Taxonomy.t -> Section.t -> section_renderer_t

type taxonomy_renderer_t = Site.t -> Taxonomy.t -> string
type meta_taxonomy_renderer_t = Taxonomy.t -> taxonomy_renderer_t

val routes_for_frontmatter_image_list :
  Section.t -> Page.t -> image_loader_t -> Dream.route list

val routes_for_frontmatter_video_list : Section.t -> Page.t -> Dream.route list

val routes_for_image_shortcodes :
  Section.t -> Page.t -> image_loader_t -> Dream.route list

val routes_for_titleimage :
  Section.t ->
  Page.t ->
  thumbnail_loader_t ->
  image_loader_t ->
  Dream.route list

val routes_for_direct_shortcodes : Section.t -> Page.t -> Dream.route list
val collect_static_routes : Site.t -> Dream.route list
val routes_for_aliases : Site.t -> Dream.route list
val routes_for_redirect_for_sans_slash : Section.t -> Page.t -> Dream.route list

val routes_for_page :
  Site.t ->
  Section.t ->
  Page.t option ->
  Page.t ->
  Page.t option ->
  meta_page_renderer_t ->
  thumbnail_loader_t ->
  image_loader_t ->
  Dream.route list

val routes_for_pages_in_section :
  Site.t ->
  Section.t ->
  meta_page_renderer_t ->
  thumbnail_loader_t ->
  image_loader_t ->
  Dream.route list

val routes_for_section :
  section_renderer:meta_section_renderer_t ->
  page_renderer:meta_page_renderer_t ->
  thumbnail_loader:thumbnail_loader_t ->
  image_loader:image_loader_t ->
  Site.t ->
  Section.t ->
  Dream.route list

val routes_for_taxonomies :
  taxonomy_renderer:meta_taxonomy_renderer_t ->
  taxonomy_section_renderer:meta_taxonomy_section_renderer_t ->
  page_renderer:meta_page_renderer_t ->
  thumbnail_loader:thumbnail_loader_t ->
  image_loader:image_loader_t ->
  Site.t ->
  Dream.route list
