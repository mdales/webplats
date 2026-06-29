type 'a t = {
  original_section_title : string;
  original_section_url : string;
  frontmatter : Frontmatter.t;
  body : string;
  path : 'a Eio.Path.t;
  base : ('a Eio.Path.t) option;
  shortcodes : ((int * int) option * Shortcode.t) list;
} constraint 'a = [> Eio.Fs.dir_ty ]

let read_frontmatter path =
  let raw_frontmatter, body_markdown =
    Eio.Path.with_open_in path (fun file ->
      let content = Eio.Flow.read_all file in
      let parts = Astring.String.cuts ~sep:"---" content in
      match parts with
      | _ :: fm :: body -> (fm, String.concat "\n---\n" body)
      | _ ->
          failwith
            (Fmt.str "failed to parse %a" Eio.Path.pp path))
  in
  let frontmatter = Frontmatter.of_string raw_frontmatter in
  (frontmatter, body_markdown)

let image_with_dimensions path (img : Frontmatter.image option) =
  match img with
  | None -> img
  | Some img -> (
      try
        let metadata =
          Metadata.Image.parse_file
            (Option.get (Eio.Path.native (Eio.Path.(path / img.filename))))
        in
        let width = int_of_string (List.assoc "width" metadata)
        and height = int_of_string (List.assoc "height" metadata) in
        Some { img with dimensions = Some (width, height) }
      with
      | Failure _ ->
          Fmt.pr "Failed to parse %a@\n"
            Eio.Path.pp Eio.Path.(path / img.filename);
          Some img
      | Metadata.Invalid ->
          Fmt.pr "Error reading metadata %a@\n"
            Eio.Path.pp Eio.Path.(path / img.filename);
          Some img)

let image_shortcode_with_dimensions path filename alt code =
  try
    let metadata =
      Metadata.Image.parse_file (Option.get (Eio.Path.native (Eio.Path.(path / filename))))
    in
    let width = int_of_string (List.assoc "width" metadata)
    and height = int_of_string (List.assoc "height" metadata) in
    Shortcode.Raster (filename, alt, code, Some (width, height))
  with
  | Invalid_argument _ ->
      Fmt.pr "Failed to process path %a + %s@\n" Eio.Path.pp path filename;
      Shortcode.Raster (filename, alt, code, None)
  | Failure _ ->
      Fmt.pr "Failed to parse %a@\n"
        Eio.Path.pp Eio.Path.(path / filename);
      Shortcode.Raster (filename, alt, code, None)
  | Metadata.Invalid ->
      Fmt.pr "Error reading metadata %a@\n"
        Eio.Path.pp Eio.Path.(path / filename);
      Shortcode.Raster (filename, alt, code, None)

let compare_shortcode_with_dimensions path fn1 fn2 l1 l2  =
  (* Lazy: just take dims off first image for now *)
  try
    let metadata =
      Metadata.Image.parse_file (Option.get (Eio.Path.native (Eio.Path.(path / fn1))))
    in
    let width = int_of_string (List.assoc "width" metadata)
    and height = int_of_string (List.assoc "height" metadata) in
    Shortcode.CompareRaster (fn1, fn2, l1, l2, Some (width, height))
  with
  | Invalid_argument _ ->
      Fmt.pr "Failed to process path %a + %s@" Eio.Path.pp path fn1;
      Shortcode.CompareRaster (fn1, fn2, l1, l2, None)
  | Failure _ ->
      Fmt.pr "Failed to parse %a@" Eio.Path.pp Eio.Path.(path / fn1);
        Shortcode.CompareRaster (fn1, fn2, l1, l2, None)
  | Metadata.Invalid ->
      Fmt.pr "Error reading metadata %a@" Eio.Path.pp Eio.Path.(path / fn1);
        Shortcode.CompareRaster (fn1, fn2, l1, l2, None)

let update_shortcodes dir sl =
  List.map
    (fun (pos, sc) ->
      match sc with
      | Shortcode.Raster (fn, alt, code, _) ->
          (pos, image_shortcode_with_dimensions dir fn alt code)
      | Shortcode.CompareRaster (fn1, fn2, l1, l2, _) ->
        (pos, compare_shortcode_with_dimensions dir fn1 fn2 l1 l2)
      | x -> (pos, x))
    sl

(* --- public interface --- *)

let v ?(base = None) original_section_title original_section_url path
    frontmatter body =
  let parent, _ = Option.get (Eio.Path.split path) in
  let shortcodes =
    Shortcode.find_shortcodes body
    |> List.map (fun (p, sc) -> (Some p, sc))
    |> update_shortcodes parent
  in
  let markdown_codes =
    Shortcode.find_labels body
    |> List.map (fun t -> (None, t))
    |> update_shortcodes parent
  in
  let code_blocks =
    Shortcode.find_codes body
    |> List.map (fun t -> (None, t))
  in
  {
    original_section_title;
    original_section_url;
    frontmatter;
    body;
    path;
    base;
    shortcodes = shortcodes @ markdown_codes @ code_blocks;
  }

let of_file ?(base = None) original_section_title original_section_url path =
  let frontmatter, body =
    try read_frontmatter path
    with Not_found | Invalid_argument _ ->
      failwith
        (Fmt.str "Failed to find key in %a" Eio.Path.pp path)
  in
  let parent, _ = Option.get (Eio.Path.split path) in
  let updated_images =
    List.map
      (fun i -> Option.get (image_with_dimensions parent (Some i)))
      (Frontmatter.images frontmatter)
  in
  let frontmatter = Frontmatter.update_images frontmatter updated_images in
  let frontmatter =
    Frontmatter.update_titleimage frontmatter
      (image_with_dimensions parent
         (Frontmatter.titleimage frontmatter))
  in
  v ~base original_section_title original_section_url path frontmatter body

let title t =
  match Frontmatter.title t.frontmatter with Some t -> t | None -> "Untitled"

let url_name t =
  let raw =
    match t.base with
    | None -> (
        let parent, basename = Option.get (Eio.Path.split t.path) in
        match basename with
        | "index.md" -> (
          let _, parentname = Option.get(Eio.Path.split parent) in parentname
        )
        | x -> (
          let idx = String.rindex basename '.' in
          String.sub basename 0 ((String.length basename) - idx)
        ))
    | Some base ->
        let p = Option.get (Path.rem_prefix base t.path) in
        let parent, basename = Option.get (Eio.Path.split t.path) in
        let dirpath =
          match basename with
          | "index.md" -> (
             Option.get (Path.rem_prefix base parent)
          )
          | _ -> (
            let pstr = Option.get (Eio.Path.native parent) in
            let idx = String.rindex pstr '.' in
            String.sub pstr 0 ((String.length basename) - idx)
          )
        in
        (*Fpath.to_string (Fpath.rem_empty_seg dirpath)*)dirpath
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
let path t =
  let parent, _ = Option.get (Eio.Path.split t.path) in parent
let body t = t.body
let tags t = Frontmatter.tags t.frontmatter
let resources t = Frontmatter.get_key_as_string_list t.frontmatter "resources"
let scripts t = Frontmatter.get_key_as_string_list t.frontmatter "scripts"
let shortcodes t = t.shortcodes
let content t = Frontmatter.content t.frontmatter
let in_feed t = Frontmatter.in_feed t.frontmatter
let images t = Frontmatter.images t.frontmatter
let videos t = Frontmatter.get_key_as_string_list t.frontmatter "videos"
let aliases t = Frontmatter.aliases t.frontmatter
let get_key_as_bool t key = Frontmatter.get_key_as_bool t.frontmatter key
let get_key_as_string t key = Frontmatter.get_key_as_string t.frontmatter key
let get_key_as_date t key = Frontmatter.get_key_as_date t.frontmatter key

let get_key_as_string_list t key =
  Frontmatter.get_key_as_string_list t.frontmatter key

let get_key_as_string_dict t key =
  Frontmatter.get_key_as_string_dict t.frontmatter key

let get_key_as_yaml t key = Frontmatter.get_key_as_yaml t.frontmatter key

let has_chart t =
  List.fold_left
    (fun acc (_, sc) ->
      acc || match sc with Shortcode.Chart _ -> true | _ -> false)
    false (shortcodes t)

let has_map t =
  (* In future we may have rasters etc., hence the name not matching the single shortcode *)
  List.fold_left
    (fun acc (_, sc) ->
      acc || match sc with Shortcode.GeoJSON _ -> true | _ -> false)
    false (shortcodes t)
