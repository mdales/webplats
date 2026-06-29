type scale = Fit | Fill

val render_image : _ Eio.Process.mgr ->  'a Eio.Path.t -> 'a Page.t -> string -> scale -> int * int -> 'a Eio.Path.t
val render_thumbnail : _ Eio.Process.mgr ->  'a Eio.Path.t -> 'a Page.t -> int -> 'a Eio.Path.t
val render_diagram : _ Eio.Process.mgr ->  'a Eio.Path.t -> 'a Page.t -> string -> 'a Eio.Path.t
