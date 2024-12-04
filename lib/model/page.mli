type t

val v : string -> Fpath.t -> Frontmatter.t -> string -> t
val of_file : ?titleimage_details:bool -> string -> Fpath.t -> t
val url_name : t -> string
(* Pages don't have absolute URLs, as they may occur in virtual sections. The
   page's URL will be [site domain]/[section url name]/[page url name].

   To preserve compatibility with Hugo, we put pages that have the name
   index.md into a name based on their parent folder, and other markdown files
   into a folder based on their name without the extension. *)

val original_section : t -> string
val title : t -> string
val date : t -> Ptime.t
val synopsis : t -> string option
val titleimage : t -> Frontmatter.image option
val draft : t -> bool
val path : t -> Fpath.t
val body : t -> string
val tags : t -> string list
val shortcodes : t -> ((int * int) * Shortcode.t) list
val images : t -> Frontmatter.image list
val get_key_as_string : t -> string -> string option
val get_key_as_date : t -> string -> Ptime.t option
val get_key_as_string_list : t -> string -> string list
