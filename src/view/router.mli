type route_handler = Http.Request.t -> Cohttp_eio.Server.response

type route = {
  uri: Uri.t;
  handler: route_handler;
}

type 'a thumbnail_loader_t = retina:bool -> ('a Page.t) -> 'a Eio.Path.t
(** This function will be called when Webplats requires a thumbnail image for the page. The standard is to
    load the image specified in the frontmatter "titleimage" entry, but the renderer can decide to select
    whatever they like from the provided page. *)

type 'a image_loader_t = ('a Page.t) -> string -> int * int -> 'a Eio.Path.t
(** This function will be called to render a given image within the page at a specified file size. *)

type 'a diagram_loader_t = ('a Page.t) -> string -> 'a Eio.Path.t

type 'a page_renderer_t =
  ('a Site.t) -> ('a Section.t) -> ('a Page.t) option -> ('a Page.t) -> ('a Page.t) option -> Htmlit.El.html
(** A page renderer should take a page and return the full expanded HTML for that page *)

type 'a meta_page_renderer_t = ('a Page.t) -> 'a page_renderer_t
(** The meta page renderer takes a page and returns the function used to render it on demand. *)

type 'a section_renderer_t = 'a Site.t -> ('a Section.t) -> Htmlit.El.html
type 'a meta_section_renderer_t = ('a Section.t) -> 'a section_renderer_t

type 'a body_renderer_t = ('a Page.t) -> string
type 'a meta_body_renderer_t = ('a Page.t) -> 'a body_renderer_t

type 'a meta_taxonomy_section_renderer_t =
  ('a Taxonomy.t) -> ('a Section.t) -> 'a section_renderer_t

type 'a taxonomy_renderer_t = 'a Site.t -> ('a Taxonomy.t) -> Htmlit.El.html
type 'a meta_taxonomy_renderer_t = ('a Taxonomy.t) -> 'a taxonomy_renderer_t

val of_site :
  section_renderer:'a meta_section_renderer_t ->
  taxonomy_renderer:'a meta_taxonomy_renderer_t ->
  taxonomy_section_renderer:'a meta_taxonomy_section_renderer_t ->
  page_renderer:'a meta_page_renderer_t ->
  page_body_renderer:'a meta_body_renderer_t ->
  thumbnail_loader:'a thumbnail_loader_t ->
  image_loader:'a image_loader_t ->
  diagram_loader:'a diagram_loader_t ->
  'a Site.t ->
  route list
