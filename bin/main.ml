open Fpath
open Webplats

let thumbnail_loader page thumbnail_size _root _path _request =
  let path = Snapshots.render_thumbnail page thumbnail_size in
  Dream.respond
    (In_channel.with_open_bin path (fun ic -> In_channel.input_all ic))

let snapshot_image_loader page image bounds _root _path _request =
  let path = Snapshots.render_image_fit page image bounds in
  Dream.respond
    (In_channel.with_open_bin path (fun ic -> In_channel.input_all ic))

let direct_loader page filename _root _path _request =
  let path = Fpath.to_string (Fpath.add_seg (Page.path page) filename) in
  Dream.respond
    (In_channel.with_open_bin path (fun ic -> In_channel.input_all ic))

let routes_for_titleimage sec page thumbnail_loader retina_thumbnail_loader =
  let page_url = Section.url ~page sec in
  match Page.titleimage page with
  | None -> []
  | Some img -> (
      (* Basic thumbnails *)
      [
        Dream.get
          (page_url ^ "thumbnail.jpg")
          (Dream.static ~loader:(thumbnail_loader page) "");
        Dream.get
          (page_url ^ "thumbnail@2x.jpg")
          (Dream.static ~loader:(retina_thumbnail_loader page) "");
      ]
      @
      (* The photos images are also in the title image *)
      match Section.title sec with
      | "photos" ->
          let name, ext = Fpath.split_ext (Fpath.v img.filename) in
          let retina_name = Fpath.to_string name ^ "@2x" ^ ext in
          [
            Dream.get
              (page_url ^ "scrn_" ^ img.filename)
              (Dream.static
                 ~loader:(snapshot_image_loader page img.filename (1008, 800))
                 "");
            Dream.get
              (page_url ^ "scrn_" ^ retina_name)
              (Dream.static
                 ~loader:(snapshot_image_loader page img.filename (2016, 1600))
                 "");
            Dream.get (page_url ^ img.filename)
              (Dream.static ~loader:(direct_loader page img.filename) "");
          ]
      | _ -> [])

let routes_for_snapshot_images sec page =
  List.concat_map
    (fun (i : Frontmatter.image) ->
      [
        (* non retina *)
        Dream.get
          (Section.url ~page sec ^ i.filename)
          (Dream.static
             ~loader:(snapshot_image_loader page i.filename (720, 1200))
             "");
        (* retina *)
        (let name, ext = Fpath.split_ext (Fpath.v i.filename) in
         let retina_name = Fpath.to_string name ^ "@2x" ^ ext in
         Dream.get
           (Section.url ~page sec ^ retina_name)
           (Dream.static
              ~loader:
                (snapshot_image_loader page i.filename (720 * 2, 1200 * 2))
              ""));
      ])
    (Page.images page)

let routes_for_image_shortcodes sec page =
  List.concat_map
    (fun (_, sc) ->
      match sc with
      | Shortcode.Image (filename, _, _) ->
          [
            Dream.get
              (Section.url ~page sec ^ filename)
              (Dream.static
                 ~loader:(snapshot_image_loader page filename (800, 600))
                 "");
            (let name, ext = Fpath.split_ext (Fpath.v filename) in
             let retina_name = Fpath.to_string name ^ "@2x" ^ ext in
             Dream.get
               (Section.url ~page sec ^ retina_name)
               (Dream.static
                  ~loader:
                    (snapshot_image_loader page filename (800 * 2, 600 * 2))
                  ""));
          ]
      | _ -> [])
    (Page.shortcodes page)

let routes_for_direct_shortcodes sec page =
  List.concat_map
    (fun (_, sc) ->
      match sc with
      | Shortcode.Video (r, None) -> [ r ]
      | Shortcode.Video (r, Some t) -> [ r; t ]
      | Shortcode.Audio r -> [ r ]
      | _ -> [])
    (Page.shortcodes page)
  |> List.map (fun filename ->
         Dream.get
           (Section.url ~page sec ^ filename)
           (fun _ ->
             Dream.respond
               (In_channel.with_open_bin
                  (Fpath.to_string (Fpath.add_seg (Page.path page) filename))
                  (fun ic -> In_channel.input_all ic))))

let routes_for_page sec previous_page page next_page page_renderer
    thumbnail_loader retina_thumbnail_loader =
  Dream.get (Section.url ~page sec) (fun _ ->
      page_renderer sec previous_page page next_page |> Dream.html)
  :: (routes_for_titleimage sec page thumbnail_loader retina_thumbnail_loader
     @ routes_for_snapshot_images sec page
     @ routes_for_image_shortcodes sec page
     @ routes_for_direct_shortcodes sec page)

let routes_for_pages_in_section sec page_renderer thumbnail_loader
    retina_thumbnail_loader : Dream.route list =
  let pages = Section.pages sec in
  match pages with
  | [] -> []
  | hd :: tl ->
      let rec loop prev current rest =
        let nextpage = match rest with [] -> None | hd :: _ -> Some hd in
        let routes =
          routes_for_page sec prev current nextpage page_renderer
            thumbnail_loader retina_thumbnail_loader
        in
        routes
        @ match rest with [] -> [] | hd :: tl -> loop (Some current) hd tl
      in
      loop None hd tl

let routes_for_section site sec =
  let ( section_renderer,
      page_renderer,
      thumbnail_loader,
      retina_thumbnail_loader ) =
  match Section.title sec with
  | "posts" ->
      ( Posts.render_section,
        Renderer.render_page,
        (fun p -> thumbnail_loader p 300),
        fun p -> thumbnail_loader p 600 )
  | "photos" ->
      ( Photos.render_section,
        Photos.render_page,
        (fun p ->
          let i = Option.get (Page.titleimage p) in
          snapshot_image_loader p i.filename (640, 350)),
        fun p ->
          let i = Option.get (Page.titleimage p) in
          snapshot_image_loader p i.filename (1280, 700) )
  | "sounds" | "snapshots" ->
      ( Snapshots.render_section,
        Snapshots.render_page,
        (fun p -> thumbnail_loader p 300),
        fun p -> thumbnail_loader p 600 )
  | _ ->
      ( Renderer.render_section,
        Renderer.render_page,
        (fun p -> thumbnail_loader p 300),
        fun p -> thumbnail_loader p 600 )
  in
  
  Dream.get (Section.url sec) (fun _ ->
      section_renderer sec |> Dream.html)
  :: Dream.get
       (Section.url sec ^ "index.xml")
       (fun _ ->
         Rss.render_rss site
           (Section.pages sec |> List.map (fun p -> (sec, p)))
         |> Dream.html)
  :: routes_for_pages_in_section sec page_renderer thumbnail_loader
       retina_thumbnail_loader

let routes_for_taxonomies site =
  let taxonomies = Site.taxonomies site in
  List.concat_map (fun (name, taxonomy) ->
    Dream.log "Taxonomy %s: %d terms" name (List.length (Taxonomy.sections taxonomy));
    List.concat_map (fun sec ->
      routes_for_section site sec
    ) (Taxonomy.sections taxonomy)
  ) taxonomies

let collect_static_routes site =
  let website_dir = Site.path site in
  let theme_static_dir =
    website_dir / "themes" / Site.hugo_theme site / "static"
  in

  let theme_things_to_be_published =
    Sys.readdir (Fpath.to_string theme_static_dir)
    |> Array.to_list
    |> List.map (fun n -> theme_static_dir / n)
  in

  List.map
    (fun path ->
      let basename = Fpath.basename path in
      match Sys.is_directory (Fpath.to_string path) with
      | true ->
          Dream.get
            (Printf.sprintf "/%s/**" basename)
            (Dream.static (Fpath.to_string (path / ".")))
      | false ->
          Dream.get ("/" ^ basename) (Dream.static (Fpath.to_string path)))
    theme_things_to_be_published

let () =
  let website_dir =
    match Array.to_list Sys.argv with
    | [ _; path ] -> Fpath.v path
    | _ -> failwith "Expected one arg, your website dir"
  in

  let site = Site.of_directory website_dir in

  let toplevel =
    [
      Dream.get "/" (fun _ -> Index.render_index site |> Dream.html);
      Dream.get "/index.xml" (fun _ ->
          Rss.render_rss site
            (Site.sections site
            |> List.concat_map (fun sec ->
                   Section.pages sec |> List.map (fun p -> (sec, p)))
            |> List.sort (fun (_, a) (_, b) ->
                   Ptime.compare (Page.date b) (Page.date a)))
          |> Dream.html);
      Dream.get "/static/**"
        (Dream.static (Fpath.to_string (website_dir / "static" / ".")));
    ]
    @ collect_static_routes site
  in

  let sections = List.concat_map (routes_for_section site) (Site.sections site) in
  let taxonomies = routes_for_taxonomies site in
  Dream.log "Adding %d routes" (List.length (toplevel @ sections @ taxonomies));
  Dream.run ~error_handler:(Dream.error_template (Renderer.render_error site))
  @@ Dream.logger
  @@ Dream.router (toplevel @ sections @ taxonomies)
