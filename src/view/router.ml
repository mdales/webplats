open Astring
open Fpath

type thumbnail_loader_t =
  retina:bool -> Page.t -> string -> string -> Dream.handler

type image_loader_t =
  Page.t -> string -> int * int -> string -> string -> Dream.handler

type page_renderer_t =
  Site.t -> Section.t -> Page.t option -> Page.t -> Page.t option -> string

type meta_page_renderer_t = Page.t -> page_renderer_t
type section_renderer_t = Site.t -> Section.t -> string
type meta_section_renderer_t = Section.t -> section_renderer_t
type meta_taxonomy_section_renderer_t = Taxonomy.t -> Section.t -> section_renderer_t

type taxonomy_renderer_t = Site.t -> Taxonomy.t -> string
type meta_taxonomy_renderer_t = Taxonomy.t -> taxonomy_renderer_t

let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"|]
let months = [| "Jan" ; "Feb" ; "Mar" ; "Apr" ; "May" ; "Jun" ; "Jul" ; "Aug" ; "Sep" ; "Oct" ; "Nov"; "Dec" |]

let ptime_to_last_modiofied (t : Ptime.t) : string =
  (* TODO: convert to GMT *)
  let dow = days.(Ptime.weekday_num t) in
  let ((year, month, day), ((hours, mins, seconds), _tz)) = Ptime.to_date_time t in
  Printf.sprintf "%s, %d %s %d %02d:%02d:%02d GMT" dow day months.(month - 1) year hours mins seconds

let static_loader root path _request =
  (* In dream the loader is called after the path has been validated,
  which is why there is no path validation here *)
  let root = match (Fpath.of_string root) with Ok x -> x | _ -> failwith "bad root" in
  let path = match (Fpath.of_string path) with Ok x -> x | _ -> failwith "bad path" in
  let full_path = Fpath.normalize (Fpath.append root path) in
  let full_path = Fpath.to_string full_path in

  let stats = Unix.stat full_path in
  let mtime = Option.get (Ptime.of_float_s (stats.st_mtime)) in
  let last_modified = ptime_to_last_modiofied mtime in

  let content_type = Magic_mime.lookup full_path in
  let headers = [("Content-type", content_type); ("Last-modified", last_modified)] in
  Dream.respond ~headers
   (In_channel.with_open_bin full_path (fun ic ->
        In_channel.input_all ic))

let direct_loader page filename _root _path request =
  (* This wrapper just avoids the need to do reverse lookups based on how I mangle file names *)
  static_loader (Fpath.to_string (Page.path page)) filename request

let routes_for_frontmatter_image_list sec page (image_loader : image_loader_t) =
  List.concat_map
    (fun (i : Frontmatter.image) ->
      [
        (* non retina *)
        Dream.get
          (Section.url ~page sec ^ i.filename)
          (Dream.static ~loader:(image_loader page i.filename (720, 1200)) "");
        (* retina *)
        (let name, ext = Fpath.split_ext (Fpath.v i.filename) in
         let retina_name = Fpath.to_string name ^ "@2x" ^ ext in
         Dream.get
           (Section.url ~page sec ^ retina_name)
           (Dream.static
              ~loader:(image_loader page i.filename (720 * 2, 1200 * 2))
              ""));
      ])
    (Page.images page)

let routes_for_frontmatter_video_list sec page =
  List.map
    (fun filename ->
      Dream.get
        (Section.url ~page sec ^ filename)
        (fun _ ->
          Dream.respond
            (In_channel.with_open_bin
               (Fpath.to_string (Fpath.add_seg (Page.path page) filename))
               (fun ic -> In_channel.input_all ic))))
    (Page.videos page)

let routes_for_image_shortcodes sec page (image_loader : image_loader_t) =
  List.concat_map
    (fun (_, sc) ->
      match sc with
      | Shortcode.Raster (filename, _, _, _) ->
          [
            Dream.get
              (Section.url ~page sec ^ filename)
              (Dream.static ~loader:(image_loader page filename (800, 600)) "");
            (let name, ext = Fpath.split_ext (Fpath.v filename) in
             let retina_name = Fpath.to_string name ^ "@2x" ^ ext in
             Dream.get
               (Section.url ~page sec ^ retina_name)
               (Dream.static
                  ~loader:(image_loader page filename (800 * 2, 600 * 2))
                  ""));
          ]
      | _ -> [])
    (Page.shortcodes page)

let routes_for_titleimage sec page thumbnail_loader image_loader =
  let page_url = Section.url ~page sec in
  match Page.titleimage page with
  | None -> []
  | Some img -> (
      (* Basic thumbnails *)
      let _, ext = Fpath.split_ext (Fpath.v img.filename) in
      match ext with
      | ".svg" -> [
        Dream.get
          (page_url ^ "thumbnail.svg")
          (Dream.static ~loader:(direct_loader page img.filename) "")]
      | _ ->
      [
        Dream.get
          (page_url ^ "thumbnail.jpg")
          (Dream.static ~loader:(thumbnail_loader ~retina:false page) "");
        Dream.get
          (page_url ^ "thumbnail@2x.jpg")
          (Dream.static ~loader:(thumbnail_loader ~retina:true page) "");
        Dream.get (page_url ^ "preview.jpg")
          (Dream.static
             ~loader:(image_loader page img.filename (2048, 2048))
             "");
      ]
      @
      (* The photos images are also in the title image *)
      match Page.original_section_title page with
      | "photos" ->
          let name, ext = Fpath.split_ext (Fpath.v img.filename) in
          let retina_name = Fpath.to_string name ^ "@2x" ^ ext in
          [
            Dream.get
              (page_url ^ "scrn_" ^ img.filename)
              (Dream.static
                 ~loader:(image_loader page img.filename (1008, 800))
                 "");
            Dream.get
              (page_url ^ "scrn_" ^ retina_name)
              (Dream.static
                 ~loader:(image_loader page img.filename (2016, 1600))
                 "");
            (* This is the direct full resolution download *)
            Dream.get (page_url ^ img.filename)
              (Dream.static ~loader:(direct_loader page img.filename) "");
          ]
          @ [
              (* This is the album thumbnails *)
              Dream.get
                (page_url ^ "album_" ^ img.filename)
                (Dream.static
                   ~loader:(image_loader page img.filename (300, 300))
                   "");
              Dream.get
                (page_url ^ "album_" ^ retina_name)
                (Dream.static
                   ~loader:(image_loader page img.filename (600, 600))
                   "");
            ]
      | _ -> [])

let routes_for_direct_shortcodes sec page =
  List.concat_map
    (fun (_, sc) ->
      match sc with
      | Shortcode.Vector (r, _, _) -> [ r ]
      | Shortcode.Video (r, None) -> [ r ]
      | Shortcode.Video (r, Some t) -> [ r; t ]
      | Shortcode.Audio r -> [ r ]
      | _ -> [])
    (Page.shortcodes page)
  |> List.map (fun filename ->
         Dream.get
           (Section.url ~page sec ^ filename)
           (Dream.static ~loader:(direct_loader page filename) ""))

let collect_static_routes site =
  let website_dir = Site.path site in
  let website_static_dir = website_dir / "static" in
  let theme_static_dir =
    website_dir / "themes" / Site.hugo_theme site / "static"
  in

  let things_to_be_published =
    List.concat_map
      (fun static_dir ->
        Sys.readdir (Fpath.to_string static_dir)
        |> Array.to_list
        |> List.map (fun n -> static_dir / n))
      [ website_static_dir; theme_static_dir ]
  in

  List.map
    (fun path ->
      let basename = Fpath.basename path in
      match Sys.is_directory (Fpath.to_string path) with
      | true ->
          Dream.get
            (Printf.sprintf "/%s/**" basename)
            (Dream.static ~loader:static_loader (Fpath.to_string (path / ".")))
      | false ->
          Dream.get ("/" ^ basename)
            (Dream.static
               ~loader:static_loader
               ""))
    things_to_be_published

let routes_for_aliases site =
  List.concat_map
    (fun sec ->
      List.concat_map
        (fun page ->
          List.map
            (fun alias ->
              Dream.get alias (fun r ->
                  Dream.redirect ~status:`Moved_Permanently r
                    (Section.url ~page sec)))
            (Page.aliases page))
        (Section.pages sec))
    (Site.sections site)

let routes_for_redirect_for_sans_slash sec page =
  let page_url = Section.url ~page sec in
  match String.is_suffix ~affix:"/" page_url with
  | false -> []
  | true ->
      let sans_slash =
        String.with_range ~len:(String.length page_url - 1) page_url
      in
      [
        Dream.get sans_slash (fun r ->
            Dream.redirect ~status:`Moved_Permanently r page_url);
      ]

let routes_for_page site sec previous_page page next_page page_renderer
    thumbnail_loader image_loader =
  match (Page.content page) with false -> [] | true ->
  Dream.get (Section.url ~page sec) (fun _ ->
    let stats = Unix.stat (Fpath.to_string (Page.path page)) in
    let mtime = Option.get (Ptime.of_float_s (stats.st_mtime)) in
    let last_modified = ptime_to_last_modiofied mtime in
    let headers = [("Last-modified", last_modified)] in
      (page_renderer page) site sec previous_page page next_page |> Dream.html ~headers)
  :: (routes_for_redirect_for_sans_slash sec page
     @ routes_for_titleimage sec page thumbnail_loader image_loader
     @ routes_for_frontmatter_image_list sec page image_loader
     @ routes_for_frontmatter_video_list sec page
     @ routes_for_image_shortcodes sec page image_loader
     @ routes_for_direct_shortcodes sec page)

let routes_for_pages_in_section site sec page_renderer thumbnail_loader
    image_loader =
  let pages = Section.pages sec in
  match pages with
  | [] -> []
  | hd :: tl ->
      let rec loop prev current rest =
        let nextpage = match rest with [] -> None | hd :: _ -> Some hd in
        let routes =
          routes_for_page site sec prev current nextpage page_renderer
            thumbnail_loader image_loader
        in
        routes
        @ match rest with [] -> [] | hd :: tl -> loop (Some current) hd tl
      in
      loop None hd tl

let routes_for_section ~section_renderer ~page_renderer ~thumbnail_loader
    ~image_loader site sec =
  Dream.get (Section.url sec) (fun _ ->
      (section_renderer sec) site sec |> Dream.html)
  :: Dream.get
       (Section.url sec ^ "index.xml")
       (fun _ ->
         Rss.render_rss site (Section.pages sec |> List.map (fun p -> (sec, p)))
         |> Dream.respond ~headers:[("Content-Type", "application/rss+xml")])
  :: routes_for_pages_in_section site sec page_renderer thumbnail_loader
       image_loader

let routes_for_taxonomies ~taxonomy_renderer ~taxonomy_section_renderer ~page_renderer
    ~thumbnail_loader ~image_loader site =
  let taxonomies = Site.taxonomies site in
  List.concat_map
    (fun (name, taxonomy) ->
      Dream.log "Taxonomy %s: %d terms" name
        (List.length (Taxonomy.sections taxonomy));

      Dream.get (Taxonomy.url taxonomy) (fun _ ->
          let render_taxonomy = taxonomy_renderer taxonomy
          in
          render_taxonomy site taxonomy |> Dream.html)
      :: List.concat_map
           (fun sec ->
             routes_for_section
               ~section_renderer:(taxonomy_section_renderer taxonomy)
               ~page_renderer ~thumbnail_loader ~image_loader site sec)
           (Taxonomy.sections taxonomy))
    taxonomies
