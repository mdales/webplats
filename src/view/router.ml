open Cohttp_eio
open Htmlit
open Astring

type route_handler = Http.Request.t -> Cohttp_eio.Server.response

type route = {
  uri: Uri.t;
  handler: route_handler;
}

type 'a thumbnail_loader_t = retina:bool -> ('a Page.t) -> 'a Eio.Path.t
(** This function will be called when Webplats requires a thumbnail image for the page. The standard is to
    load the image specified in the frontmatter "titleimage" entry, but the renderer can decide to select
    whatever they like from the provided page. *)

type 'a image_loader_t = ('a Page.t) -> string -> int * int -> 'a Eio.Path.t
(** This function will be called to render a given image within the page at a specified file size. *)

type 'a diagram_loader_t = ('a Page.t) -> string -> 'a Eio.Path.t

type 'a page_renderer_t =
  ('a Site.t) -> ('a Section.t) -> ('a Page.t) option -> ('a Page.t) -> ('a Page.t) option -> Htmlit.El.html
(** A page renderer should take a page and return the full expanded HTML for that page *)

type 'a meta_page_renderer_t = ('a Page.t) -> 'a page_renderer_t
(** The meta page renderer takes a page and returns the function used to render it on demand. *)

type 'a section_renderer_t = 'a Site.t -> ('a Section.t) -> Htmlit.El.html
type 'a meta_section_renderer_t = ('a Section.t) -> 'a section_renderer_t

type 'a body_renderer_t = ('a Page.t) -> string
type 'a meta_body_renderer_t = ('a Page.t) -> 'a body_renderer_t

type 'a meta_taxonomy_section_renderer_t =
  ('a Taxonomy.t) -> ('a Section.t) -> 'a section_renderer_t

type 'a taxonomy_renderer_t = 'a Site.t -> ('a Taxonomy.t) -> Htmlit.El.html
type 'a meta_taxonomy_renderer_t = ('a Taxonomy.t) -> 'a taxonomy_renderer_t

let render_static_file path req =
  Eio.Switch.run @@ fun sw ->
  let stat = Eio.Path.stat ~follow:true path in
  let mtime = Option.get (Ptime.of_float_s stat.mtime) in
  let last_modified = Httphelper.ptime_to_last_modified mtime in
  let content_type = Magic_mime.lookup (Eio.Path.native_exn path) in

  let file = Eio.Path.open_in ~sw path in

  let request_headers = Http.Request.headers req in
  let range_header = Http.Header.get request_headers "range" in
  let file_size = Optint.Int63.to_int64 stat.size in
  match range_header with
  | Some range_str when String.is_prefix ~affix:"bytes=" range_str -> (
      match Httphelper.parse_range range_str file_size with
      | Some (start_pos, end_pos) ->
          let len = Int64.add (Int64.sub end_pos start_pos) 1L in
          let headers =
            Http.Header.of_list
              [
                ("Accept-Ranges", "bytes");
                ("Content-Length", Int64.to_string len);
                ( "Content-Range",
                  Printf.sprintf "bytes %Ld-%Ld/%Ld" start_pos end_pos file_size
                );
                ("Content-Type", content_type);
                ("Last-Modified", last_modified);
              ]
          in

          let _ = Eio.File.seek file (Optint.Int63.of_int64 start_pos) `Set in
          let buf = Eio.Buf_read.of_flow file ~max_size:(Int64.to_int len) in
          let bytes = Eio.Buf_read.take (Int64.to_int len) buf in
          let body = Eio.Flow.string_source bytes in
          Cohttp_eio.Server.respond ~status:`Partial_content ~headers ~body ()
      | None ->
          let headers =
            Http.Header.of_list
              [
                ("Accept-Ranges", "bytes");
                ("Content-Length", Optint.Int63.to_string stat.size);
                ("Content-Type", content_type);
                ("Last-Modified", last_modified);
              ]
          in
          let bytes = Eio.Flow.read_all file in
          let body = Eio.Flow.string_source bytes in
          Cohttp_eio.Server.respond ~status:`OK ~headers ~body ())
  | _ ->
      let headers =
        Http.Header.of_list
          [
            ("Accept-Ranges", "bytes");
            ("Content-Length", Optint.Int63.to_string stat.size);
            ("Content-Type", content_type);
            ("Last-Modified", last_modified);
          ]
      in
      let bytes = Eio.Flow.read_all file in
      let body = Eio.Flow.string_source bytes in
      Cohttp_eio.Server.respond ~status:`OK ~headers ~body ()

let deferred_render_static_file f req =
  let path = f () in
  render_static_file path req

let routes_for_titleimage sec page thumbnail_loader image_loader =
  match Page.titleimage page with
  | None -> []
  | Some img -> (
      (* Basic thumbnails *)
      let _, ext = Fpath.split_ext (Fpath.v img.filename) in
      match ext with
      | ".svg" ->
          [
            {
              uri=Section.uri ~page ~resource:"thumbnail.svg" sec;
              handler=render_static_file Eio.Path.((Page.path page) / img.filename)
            }
          ]
      | _ -> (
          [
            {
              uri=Section.uri ~page ~resource:"thumbnail.jpg" sec;
              handler=deferred_render_static_file (fun _ -> thumbnail_loader ~retina:false page)
            };
            {
              uri=Section.uri ~page ~resource:"thumbnail@2x.jpg" sec;
              handler=deferred_render_static_file (fun _ -> thumbnail_loader ~retina:true page)
            };
            {
              uri=Section.uri ~page ~resource:"preview.jpg" sec;
              handler=deferred_render_static_file (fun _ -> image_loader page img.filename (2048, 2048))
            };
          ]
          @
          (* The photos images are also in the title image *)
          match Page.original_section_title page with
          | "photos" ->
              let name, ext = Fpath.split_ext (Fpath.v img.filename) in
              let retina_name = Fpath.to_string name ^ "@2x" ^ ext in
              [
                {
                  uri=Section.uri ~page ~resource:("scrn_" ^ img.filename) sec;
                  handler=deferred_render_static_file (fun _ -> image_loader page img.filename (1008, 800))
                };
                {
                  uri=Section.uri ~page ~resource:("scrn_" ^ retina_name) sec;
                  handler=deferred_render_static_file (fun _ -> image_loader page img.filename (2016, 1600))
                };
                {
                  uri=Section.uri ~page ~resource:img.filename sec;
                  handler=render_static_file Eio.Path.((Page.path page) / img.filename)
                };
              ]
              @ [
                  (* This is the album thumbnails *)
                  {
                    uri=Section.uri ~page ~resource:("album_" ^ img.filename) sec;
                    handler=deferred_render_static_file (fun _ -> image_loader page img.filename (300, 300))
                  };
                  {
                    uri=Section.uri ~page ~resource:("album_" ^ retina_name) sec;
                    handler=deferred_render_static_file (fun _ -> image_loader page img.filename (600, 600))
                  };
                ]
          | _ -> []))

let routes_for_frontmatter_image_list sec page image_loader =
  List.concat_map
    (fun (i : Frontmatter.image) ->
      [
        (* non retina *)
        {
          uri=Section.uri ~page ~resource:i.filename sec;
          handler=deferred_render_static_file (fun _ -> image_loader page i.filename (720, 1200))
        };
        (* retina *)
        (let name, ext = Fpath.split_ext (Fpath.v i.filename) in
         let retina_name = Fpath.to_string name ^ "@2x" ^ ext in
         {
           uri=Section.uri ~page ~resource:retina_name sec;
           handler=deferred_render_static_file (fun _ -> image_loader page i.filename (720 * 2, 1200 * 2))
         };);
      ])
    (Page.images page)

let routes_for_frontmatter_video_list sec page =
  List.map
    (fun filename ->
      {
        uri=Section.uri ~page ~resource:filename sec;
        handler=render_static_file Eio.Path.((Page.path page) / filename)
      }
    )
    (Page.videos page)

let inner_image_for_shortcodes sec page image_loader filename =
  [
    {
      uri=Section.uri ~page ~resource:filename sec;
      handler=deferred_render_static_file (fun _ -> image_loader page filename (900, 700))
    };
    (
      let name, ext = Fpath.split_ext (Fpath.v filename) in
      let retina_name = Fpath.to_string name ^ "@2x" ^ ext in
      {
        uri=Section.uri ~page ~resource:retina_name sec;
        handler=deferred_render_static_file (fun _ -> image_loader page filename (900 * 2, 700 * 2))
      };
    )
  ]

let routes_for_image_shortcodes sec page image_loader =
  let curried_f = inner_image_for_shortcodes sec page image_loader in
  List.concat_map
    (fun (_, sc) ->
      match sc with
      | Shortcode.Raster (filename, _, _, _) -> curried_f filename
      | Shortcode.CompareRaster (filename1, filename2, _, _, _) ->
        [filename1; filename2] |> List.concat_map curried_f
      | _ -> [])
    (Page.shortcodes page)

let routes_for_diagram_shortcodes sec page diagram_loader =
  List.concat_map
    (fun (_, sc) ->
      match sc with
      | Shortcode.Diagram code -> (
        let hash = Digest.string code |> Digest.to_hex in
        let filename = Printf.sprintf "%s.svg" hash in
        [
          {
            uri=Section.uri ~page ~resource:filename sec;
            handler=deferred_render_static_file (fun _ -> diagram_loader page code)
          }
        ]
      )
      | _ -> []
    ) (Page.shortcodes page)

let routes_for_direct_shortcodes sec page =
  List.concat_map
    (fun (_, sc) ->
      match sc with
      | Shortcode.Vector (r, _, _) -> [ r ]
      | Shortcode.Video (r, None, _) -> [ r ]
      | Shortcode.Video (r, Some t, _) -> [ r; t ]
      | Shortcode.Audio r -> [ r ]
      | Shortcode.Chart (_, r, _, _) -> [ r ]
      | Shortcode.GeoJSON r -> [ r ]
      | _ -> [])
    (Page.shortcodes page)
  |> List.map (fun filename ->
    let fullpath = Eio.Path.((Page.path page) / filename) in
    {
      uri=Section.uri ~page ~resource:filename sec;
      handler=render_static_file fullpath
    })

let routes_for_scripts_and_resources sec page =
  Page.resources page @ Page.scripts page
  |> List.map (fun filename ->
    let fullpath = Eio.Path.((Page.path page) / filename) in
    {
      uri=Section.uri ~page ~resource:filename sec;
      handler=render_static_file fullpath
    })

let routes_for_aliases site =
  List.concat_map
    (fun sec ->
      List.concat_map
        (fun page ->
          List.map
            (fun alias ->
              {
                uri=Uri.of_string alias;
                handler=(fun _ ->
                  let headers = Http.Header.of_list [("Location", (Uri.to_string (Section.uri ~page sec)))] in
                  Cohttp_eio.Server.respond ~status:`Moved_permanently ~headers ~body:(Eio.Flow.string_source "") ()
                )
              }
            )
            (Page.aliases page))
        (Section.pages sec))
    (Site.sections site)

let routes_for_redirect_for_sans_slash sec page =
  let page_url = Uri.to_string (Section.uri ~page sec) in
  match String.is_suffix ~affix:"/" page_url with
  | false -> []
  | true ->
      let sans_slash =
        String.with_range ~len:(String.length page_url - 1) page_url
      in
      [
        {
          uri=Uri.of_string sans_slash;
          handler=(fun _ ->
            let headers = Http.Header.of_list [("Location", page_url)] in
            Cohttp_eio.Server.respond ~status:`Moved_permanently ~headers ~body:(Eio.Flow.string_source "") ()
          )
        }
      ]

let routes_for_page site sec previous_page page next_page page_renderer
    thumbnail_loader image_loader diagram_loader =
  match Page.content page with
  | false -> []
  | true ->
      (* Dream.log "Adding %s" (Uri.to_string (Section.uri ~page sec)); *)
      {
        uri=Section.uri ~page sec;
        handler=
        (fun _ ->
          let stat = Eio.Path.stat ~follow:true (Page.path page) in
          let mtime = Option.get (Ptime.of_float_s stat.mtime) in
          let last_modified = Httphelper.ptime_to_last_modified mtime in
          let headers = Http.Header.of_list [ ("Last-modified", last_modified) ] in
          let body =
          (page_renderer page) site sec previous_page page next_page
          |> El.to_string ~doctype:true
          in
          Cohttp_eio.Server.respond_string ~headers ~status:`OK ~body ())
      }
      :: (routes_for_redirect_for_sans_slash sec page
        @ routes_for_titleimage sec page thumbnail_loader image_loader
        @ routes_for_frontmatter_image_list sec page image_loader
        @ routes_for_frontmatter_video_list sec page
        @ routes_for_image_shortcodes sec page image_loader
        @ routes_for_diagram_shortcodes sec page diagram_loader
        @ routes_for_direct_shortcodes sec page
        @ routes_for_scripts_and_resources sec page)

let routes_for_pages_in_section site sec page_renderer thumbnail_loader
    image_loader diagram_loader =
  let pages = Section.pages sec in
  match pages with
  | [] -> []
  | hd :: tl ->
      let rec loop prev current rest =
        let nextpage = match rest with [] -> None | hd :: _ -> Some hd in
        let routes =
          routes_for_page site sec prev current nextpage page_renderer
            thumbnail_loader image_loader diagram_loader
        in
        routes
        @ match rest with [] -> [] | hd :: tl -> loop (Some current) hd tl
      in
      loop None hd tl

let routes_for_feed base_section site page_list =
  [
    {
        uri=Section.uri ~resource:"index.xml" base_section;
        handler=(fun _ ->
            let body = Feed.render_atom base_section site page_list in
            let headers = Http.Header.of_list
              [
                ("Content-Type", "application/rss+xml");
              ] in
            Cohttp_eio.Server.respond_string ~headers ~status:`OK ~body ()
        )
    };
    {
        uri=Section.uri ~resource:"feed.json" base_section;
        handler=(fun _ ->
            let feed = Feed.render_jsonfeed base_section site page_list in
            match feed with
            | Result.Ok body -> (
                let headers = Http.Header.of_list
                [
                    ("Content-Type", "application/feed+json");
                ] in
                Cohttp_eio.Server.respond_string ~headers ~status:`OK ~body ()
            )
            | _ -> (
                let body = "<h1>Something went wrong</h1>" in
                Cohttp_eio.Server.respond_string ~status:`Internal_server_error ~body ()
            )
        )
    }
  ]

let routes_for_section ~section_renderer ~page_renderer ~page_body_renderer
    ~thumbnail_loader ~image_loader ~diagram_loader site sec =
  let raw_pages_in_feed =
    match Section.title sec with
    | "website" ->
        List.concat_map
          (fun sec ->
            Section.pages sec
            |> List.map (fun p -> (sec, p, page_body_renderer p)))
          (Site.sections site)
    | _ ->
        Section.pages sec |> List.map (fun p -> (sec, p, page_body_renderer p))
  in
  let pages_in_feed =
    raw_pages_in_feed
    |> List.sort (fun (_, a, _) (_, b, _) ->
           Ptime.compare (Page.date b) (Page.date a))
  in
  Fmt.pr "%s@\n" (Uri.to_string (Section.uri sec));
  {
    uri=Section.uri sec;
    handler=(fun _ ->
        let body = (section_renderer sec) site sec
        |> El.to_string ~doctype:true in
        Cohttp_eio.Server.respond_string ~status:`OK ~body ()
    )
  } :: (routes_for_feed sec site pages_in_feed
   @ routes_for_pages_in_section site sec page_renderer thumbnail_loader
       image_loader diagram_loader)

let rec inner_collect_static_routes src_path prefix =
  Eio.Path.read_dir src_path
  |> List.concat_map (fun name ->
    let full_path = Eio.Path.(src_path / name) in
    let url_path = prefix ^ "/" ^ name in
    match Eio.Path.is_directory full_path with
    | true -> (
      inner_collect_static_routes full_path url_path
    )
    | false -> (
      [{
        uri = Uri.of_string url_path;
        handler=render_static_file full_path
      }]
    )
  )

let collect_static_routes site =
  let website_dir = Site.path site in
  let website_static_dir = Eio.Path.(website_dir / "static") in
  let theme_static_dir = Eio.Path.(website_dir / "themes" / Site.hugo_theme site / "static") in

  let things_to_be_published =
    List.concat_map
      (fun static_dir ->
        inner_collect_static_routes static_dir ""
      )
      [ website_static_dir; theme_static_dir ]
  in
  things_to_be_published

let routes_for_taxonomies ~taxonomy_renderer ~taxonomy_section_renderer
    ~page_renderer ~page_body_renderer ~thumbnail_loader ~image_loader ~diagram_loader site =
  let taxonomies = Site.taxonomies site in
  List.concat_map
    (fun (name, taxonomy) ->
      {
        uri=Taxonomy.uri taxonomy;
        handler=(
          let render_taxonomy = taxonomy_renderer taxonomy in
          let body = render_taxonomy site taxonomy |> El.to_string ~doctype:true in
          fun _ ->
            Cohttp_eio.Server.respond_string ~status:`OK ~body ()
        )
      }
      :: List.concat_map
           (fun sec ->
             routes_for_section
               ~section_renderer:(taxonomy_section_renderer taxonomy)
               ~page_renderer ~page_body_renderer ~thumbnail_loader
               ~image_loader ~diagram_loader site sec)
           (Taxonomy.sections taxonomy))
    taxonomies

let of_site ~section_renderer ~taxonomy_renderer ~taxonomy_section_renderer
~page_renderer ~page_body_renderer ~thumbnail_loader ~image_loader ~diagram_loader site =

  let static = collect_static_routes site in

  let sections =
    List.concat_map
      (routes_for_section ~thumbnail_loader
        ~image_loader ~diagram_loader ~section_renderer
        ~page_renderer ~page_body_renderer site)
      (Site.sections site)
  in

  let taxonomies =
    routes_for_taxonomies ~thumbnail_loader
      ~image_loader ~diagram_loader ~taxonomy_renderer
      ~taxonomy_section_renderer ~page_renderer ~page_body_renderer
      site
  in

  let aliases = routes_for_aliases site in

  let css_routes = match Site.css_digest_path site with
  | None -> []
  | Some digest_path -> [
    (
      let site_path = Site.path site in
      let stem = Option.get (Path.rem_prefix site_path digest_path) in
      let te = Option.get (Site.css_path site) in
      {
        uri=Uri.of_string ("/" ^ stem);
        handler=render_static_file te
      }
    )
  ]
  in

  sections @ taxonomies @ aliases @ static @ css_routes
