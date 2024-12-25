type t = {
  original_section_title : string;
  original_section_url : string;
  frontmatter : Frontmatter.t;
  body : string;
  path : Fpath.t;
  base : Fpath.t option;
  shortcodes : ((int * int) * Shortcode.t) list;
}

let read_frontmatter path =
  let raw_frontmatter, body_markdown =
    In_channel.with_open_text (Fpath.to_string path) (fun ic ->
        let parts = In_channel.input_all ic |> Astring.String.cuts ~sep:"---" in
        match parts with
        | _ :: fm :: body -> (fm, String.concat "\n---\n" body)
        | _ ->
            failwith
              (Printf.sprintf "failed to parse %s" (Fpath.to_string path)))
  in
  let frontmatter = Frontmatter.of_string raw_frontmatter in
  (frontmatter, body_markdown)

let image_with_dimensions path (img : Frontmatter.image option) =
  match img with
  | None -> img
  | Some img -> (
      try
        (* We use metadata rather than camlimage here as it's way faster *)
        let metadata =
          Metadata.Image.parse_file
            (Fpath.to_string (Fpath.add_seg path img.filename))
        in
        let width = int_of_string (List.assoc "width" metadata)
        and height = int_of_string (List.assoc "height" metadata) in
        Some { img with dimensions = Some (width, height) }
      with
      | Failure _ ->
          Dream.log "Failed to parse %s"
            (Fpath.to_string (Fpath.add_seg path img.filename));
          Some img
      | Metadata.Invalid ->
          Dream.log "Error reading metadata %s"
            (Fpath.to_string (Fpath.add_seg path img.filename));
          Some img)

let image_shortcode_with_dimensions path filename alt code =
  try
    let metadata =
      Metadata.Image.parse_file (Fpath.to_string (Fpath.add_seg path filename))
    in
    let width = int_of_string (List.assoc "width" metadata)
    and height = int_of_string (List.assoc "height" metadata) in
    Shortcode.Image (filename, alt, code, Some (width, height))
  with
  | Failure _ ->
      Dream.log "Failed to parse %s"
        (Fpath.to_string (Fpath.add_seg path filename));
      Shortcode.Image (filename, alt, code, None)
  | Metadata.Invalid ->
      Dream.log "Error reading metadata %s"
        (Fpath.to_string (Fpath.add_seg path filename));
      Shortcode.Image (filename, alt, code, None)

let update_shortcodes dir sl =
  List.map
    (fun (pos, sc) ->
      match sc with
      | Shortcode.Image (fn, alt, code, _) ->
          (pos, image_shortcode_with_dimensions dir fn alt code)
      | x -> (pos, x))
    sl

(* --- public interface --- *)

let v ?(base = None) original_section_title original_section_url path
    frontmatter body =
  let shortcodes =
    Shortcode.find_shortcodes body |> update_shortcodes (Fpath.parent path)
  in
  {
    original_section_title;
    original_section_url;
    frontmatter;
    body;
    path;
    base;
    shortcodes;
  }

let of_file ?(base = None) original_section_title original_section_url path =
  let frontmatter, body =
    try read_frontmatter path
    with Not_found | Invalid_argument _ ->
      failwith
        (Printf.sprintf "Failed to find key in %s" (Fpath.to_string path))
  in
  let frontmatter =
    Frontmatter.update_titleimage frontmatter
      (image_with_dimensions (Fpath.parent path)
         (Frontmatter.titleimage frontmatter))
  in
  v ~base original_section_title original_section_url path frontmatter body

let title t =
  match Frontmatter.title t.frontmatter with Some t -> t | None -> "Untitled"

let url_name t =
  let raw =
    match t.base with
    | None -> (
        let basename = Fpath.basename (Fpath.rem_ext t.path) in
        match basename with
        | "index" -> Fpath.basename (Fpath.parent t.path)
        | x -> x)
    | Some base ->
        let p = Option.get (Fpath.rem_prefix base t.path) in
        let dirpath =
          match Fpath.basename t.path with
          | "index.md" -> Fpath.parent p
          | _ -> Fpath.rem_ext p
        in
        Fpath.to_string (Fpath.rem_empty_seg dirpath)
  in
  let lower_raw = String.lowercase_ascii raw in
  String.fold_left
    (fun acc c ->
      match c with '?' | '&' -> acc | x -> Printf.sprintf "%s%c" acc x)
    "" lower_raw

let original_section_title t = t.original_section_title
let original_section_url t = t.original_section_url
let date t = Frontmatter.date t.frontmatter
let synopsis t = Frontmatter.synopsis t.frontmatter
let titleimage t = Frontmatter.titleimage t.frontmatter
let draft t = Frontmatter.draft t.frontmatter
let path t = Fpath.parent t.path
let body t = t.body
let tags t = Frontmatter.tags t.frontmatter
let shortcodes t = t.shortcodes
let images t = Frontmatter.images t.frontmatter
let videos t = Frontmatter.get_key_as_string_list t.frontmatter "videos"
let aliases t = Frontmatter.aliases t.frontmatter
let get_key_as_string t key = Frontmatter.get_key_as_string t.frontmatter key
let get_key_as_date t key = Frontmatter.get_key_as_date t.frontmatter key

let get_key_as_string_list t key =
  Frontmatter.get_key_as_string_list t.frontmatter key
