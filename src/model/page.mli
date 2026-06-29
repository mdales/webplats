type 'a t constraint 'a = [> Eio.Fs.dir_ty ]

val v :
  ?base:('a Eio.Path.t) option ->
  string ->
  string ->
  'a Eio.Path.t ->
  Frontmatter.t ->
  string ->
  'a t

val of_file : ?base:'a Eio.Path.t option -> string -> string -> 'a Eio.Path.t -> 'a t
(** of_file base_path original_section_title original_section_url path *)

val url_name : 'a t -> string
(* Pages don't have absolute URLs, as they may occur in virtual sections. The
   page's URL will be [site domain]/[section url name]/[page url name].

   To preserve compatibility with Hugo, we put pages that have the name
   index.md into a name based on their parent folder, and other markdown files
   into a folder based on their name without the extension. *)

val original_section_title : 'a t -> string
val original_section_url : 'a t -> string
(* These two are a bodge, as I can't depend on Section.t, as it depends on Page.t *)

val title : 'a t -> string
val date : 'a t -> Ptime.t
val synopsis : 'a t -> string option
val titleimage : 'a t -> Frontmatter.image option
val draft : 'a t -> bool
val path : 'a t -> 'a Eio.Path.t
val body : 'a t -> string
val tags : 'a t -> string list
val resources : 'a t -> string list
val scripts : 'a t -> string list
val shortcodes : 'a t -> ((int * int) option * Shortcode.t) list
val content : 'a t -> bool
val in_feed : 'a t -> bool
val images : 'a t -> Frontmatter.image list
val videos : 'a t -> string list
val aliases : 'a t -> string list
val get_key_as_string : 'a t -> string -> string option
val get_key_as_bool : 'a t -> string -> bool option
val get_key_as_date : 'a t -> string -> Ptime.t option
val get_key_as_string_list : 'a t -> string -> string list
val get_key_as_string_dict : 'a t -> string -> (string * string) list
val get_key_as_yaml : 'a t -> string -> Yaml.value option
val has_chart : 'a t -> bool
val has_map : 'a t -> bool
