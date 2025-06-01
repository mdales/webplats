type scale = Fit | Fill

val render_image_lwt : Page.t -> string -> scale -> int * int -> Fpath.t Lwt.t
val render_thumbnail_lwt : Page.t -> int -> Fpath.t Lwt.t
