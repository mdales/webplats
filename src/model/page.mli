type t

val v :
  ?base:Fpath.t option ->
  string ->
  string ->
  Fpath.t ->
  Frontmatter.t ->
  string ->
  t

val of_file : ?base:Fpath.t option -> string -> string -> Fpath.t -> t
val url_name : t -> string
(* Pages don't have absolute URLs, as they may occur in virtual sections. The
   page's URL will be [site domain]/[section url name]/[page url name].

   To preserve compatibility with Hugo, we put pages that have the name
   index.md into a name based on their parent folder, and other markdown files
   into a folder based on their name without the extension. *)

val original_section_title : t -> string
val original_section_url : t -> string
(* These two are a bodge, as I can't depend on Section.t, as it depends on Page.t *)

val title : t -> string
val date : t -> Ptime.t
val synopsis : t -> string option
val titleimage : t -> Frontmatter.image option
val draft : t -> bool
val path : t -> Fpath.t
val body : t -> string
val tags : t -> string list
val shortcodes : t -> ((int * int) * Shortcode.t) list
val content : t -> bool
val in_feed : t -> bool
val images : t -> Frontmatter.image list
val videos : t -> string list
val aliases : t -> string list
val get_key_as_string : t -> string -> string option
val get_key_as_bool : t -> string -> bool option
val get_key_as_date : t -> string -> Ptime.t option
val get_key_as_string_list : t -> string -> string list
val get_key_as_string_dict : t -> string -> (string * string) list
val get_key_as_yaml : t -> string -> Yaml.value option