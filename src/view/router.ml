open Cohttp_eio
open Htmlit
open Astring
open Fpath


type route_handler = Http.Request.t -> Cohttp_eio.Server.response

type route = {
  uri: Uri.t;
  handler: route_handler;
}


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
    thumbnail_loader image_loader =
  match Page.content page with
  | false -> []
  | true ->
      (* Dream.log "Adding %s" (Uri.to_string (Section.uri ~page sec)); *)
      {
        uri=Section.uri ~page sec;
        handler=
        (fun _ ->
          let stats = Unix.stat (Fpath.to_string (Page.path page)) in
          let mtime = Option.get (Ptime.of_float_s stats.st_mtime) in
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
        @ routes_for_diagram_shortcodes sec page
        @ routes_for_direct_shortcodes sec page
        @ routes_for_scripts_and_resources sec page)

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
    ~thumbnail_loader ~image_loader site sec =
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
  {
    uri=Section.uri sec;
    handler=(fun _ ->
        let body = (section_renderer sec) site sec
        |> El.to_string ~doctype:true in
        Cohttp_eio.Server.respond_string ~status:`OK ~body ()
    )
  } :: (routes_for_feed sec site pages_in_feed
   @ routes_for_pages_in_section site sec page_renderer thumbnail_loader
       image_loader) *)

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
        esc_dream_get
          (Printf.sprintf "/%s/**" basename)
          (Dream.static ~loader:static_loader (Fpath.to_string (path / ".")))
    | false ->
        esc_dream_get ("/" ^ basename) (Dream.static ~loader:static_loader ""))
  things_to_be_published

let of_site ~section_renderer ~taxonomy_renderer ~taxonomy_section_renderer
~page_renderer ~page_body_renderer ~thumbnail_loader ~image_loader site =

  let static = collect_static_routes site in

  let sections =
    List.concat_map
      (routes_for_section ~thumbnail_loader
        ~image_loader ~section_renderer
        ~page_renderer ~page_body_renderer site)
      (Site.sections site)
  in

  let taxonomies =
    routes_for_taxonomies ~thumbnail_loader
      ~image_loader ~taxonomy_renderer
      ~taxonomy_section_renderer ~page_renderer ~page_body_renderer
      site
  in

  let aliases = routes_for_aliases site in

  let css_routes = match Site.css_digest_path site with
  | None -> []
  | Some digest_path -> [
    (
      let te = Option.get (Site.css_path site) in
      esc_dream_get digest_path (
        Dream.static ~loader:(replacement_loader te) ""
      )
    )
  ]
  in

  sections @ taxonomies @ aliases @ static @ css_routes
