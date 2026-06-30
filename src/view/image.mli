type scale = Fit | Fill

val render_image :
  _ Eio.Process.mgr ->
  Eio.Fs.dir_ty Eio.Path.t ->
  Page.t ->
  string ->
  scale ->
  int * int ->
  Eio.Fs.dir_ty Eio.Path.t

val render_thumbnail :
  _ Eio.Process.mgr -> Eio.Fs.dir_ty Eio.Path.t -> Page.t -> int -> Eio.Fs.dir_ty Eio.Path.t

val render_diagram :
  _ Eio.Process.mgr -> Eio.Fs.dir_ty Eio.Path.t -> Page.t -> string -> Eio.Fs.dir_ty Eio.Path.t
