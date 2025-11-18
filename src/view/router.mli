type thumbnail_loader_t = retina:bool -> Page.t -> Dream.handler
(** This function will be called when Webplats requires a thumbnail image for the page. The standard is to
    load the image specified in the frontmatter "titleimage" entry, but the renderer can decide to select
    whatever they like from the provided page. *)

type image_loader_t = Page.t -> string -> int * int -> Dream.handler
(** This function will be called to render a given image within the page at a specified file size. *)

type page_renderer_t =
  Site.t -> Section.t -> Page.t option -> Page.t -> Page.t option -> string
(** A page renderer should take a page and return the full expanded HTML for that page *)

type meta_page_renderer_t = Page.t -> page_renderer_t
(** The meta page renderer takes a page and returns the function used to render it on demand. *)

type section_renderer_t = Site.t -> Section.t -> string
type meta_section_renderer_t = Section.t -> section_renderer_t

type body_renderer_t = Page.t -> string
type meta_body_renderer_t = Page.t -> body_renderer_t

type meta_taxonomy_section_renderer_t =
  Taxonomy.t -> Section.t -> section_renderer_t

type taxonomy_renderer_t = Site.t -> Taxonomy.t -> string
type meta_taxonomy_renderer_t = Taxonomy.t -> taxonomy_renderer_t

val static_loader : string -> string -> Dream.handler

val of_site :
  section_renderer:meta_section_renderer_t ->
  taxonomy_renderer:meta_taxonomy_renderer_t ->
  taxonomy_section_renderer:meta_taxonomy_section_renderer_t ->
  page_renderer:meta_page_renderer_t ->
  page_body_renderer:meta_body_renderer_t ->
  thumbnail_loader:thumbnail_loader_t ->
  image_loader:image_loader_t ->
  Site.t ->
  Dream.route list
