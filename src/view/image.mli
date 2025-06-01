type scale = Fit | Fill

val render_image : Page.t -> string -> scale -> int * int -> Fpath.t
val render_thumbnail : Page.t -> int -> Fpath.t

val render_image_lwt : Page.t -> string -> scale -> int * int -> Fpath.t Lwt.t
val render_thumbnail_lwt : Page.t -> int -> Fpath.t Lwt.t
