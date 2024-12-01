type t = {
  frontmatter : Frontmatter.t;
  body : string;
  path : Fpath.t;
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
      with Failure _ ->
        Dream.log "Failed to parse %s"
          (Fpath.to_string (Fpath.add_seg path img.filename));
        Some img)

(* --- public interface --- *)

let v path frontmatter body =
  let shortcodes = Shortcode.find_shortcodes body in
  { frontmatter; body; path; shortcodes }

let of_file ?(titleimage_details = false) path =
  let frontmatter, body =
    try read_frontmatter path
    with Not_found | Invalid_argument _ ->
      failwith
        (Printf.sprintf "Failed to find key in %s" (Fpath.to_string path))
  in
  let frontmatter =
    match titleimage_details with
    | false -> frontmatter
    | true ->
        Frontmatter.update_titleimage frontmatter
          (image_with_dimensions (Fpath.parent path)
             (Frontmatter.titleimage frontmatter))
  in
  v path frontmatter body

let title t =
  match Frontmatter.title t.frontmatter with Some t -> t | None -> "Untitled"

let url_name t =
  let basename = Fpath.basename (Fpath.rem_ext t.path) in
  match basename with "index" -> Fpath.basename (Fpath.parent t.path) | x -> x

let date t = Frontmatter.date t.frontmatter
let synopsis t = Frontmatter.synopsis t.frontmatter
let titleimage t = Frontmatter.titleimage t.frontmatter
let draft t = Frontmatter.draft t.frontmatter
let path t = Fpath.parent t.path
let body t = t.body
let tags t = Frontmatter.tags t.frontmatter
let shortcodes t = t.shortcodes
let images t = Frontmatter.images t.frontmatter
let get_key_as_string t key = Frontmatter.get_key_as_string t.frontmatter key
let get_key_as_date t key = Frontmatter.get_key_as_date t.frontmatter key
