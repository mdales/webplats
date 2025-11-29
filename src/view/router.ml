module Stdlib_string = String
open Astring
open Fpath

type thumbnail_loader_t = retina:bool -> Page.t -> Dream.request -> Dream.response Lwt.t

type image_loader_t = Page.t -> string -> int * int -> Dream.handler

type page_renderer_t =
  Site.t -> Section.t -> Page.t option -> Page.t -> Page.t option -> string

type meta_page_renderer_t = Page.t -> page_renderer_t
type section_renderer_t = Site.t -> Section.t -> string
type meta_section_renderer_t = Section.t -> section_renderer_t
type body_renderer_t = Page.t -> string
type meta_body_renderer_t = Page.t -> body_renderer_t

type meta_taxonomy_section_renderer_t =
  Taxonomy.t -> Section.t -> section_renderer_t

type taxonomy_renderer_t = Site.t -> Taxonomy.t -> string
type meta_taxonomy_renderer_t = Taxonomy.t -> taxonomy_renderer_t

module Message = Dream_pure.Message
module Stream = Dream_pure.Stream

let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]

let esc_dream_get a b =
  Dream.get (Uri.pct_decode a) b

let months =
  [|
    "Jan";
    "Feb";
    "Mar";
    "Apr";
    "May";
    "Jun";
    "Jul";
    "Aug";
    "Sep";
    "Oct";
    "Nov";
    "Dec";
  |]

let ptime_to_last_modified (t : Ptime.t) : string =
  (* TODO: convert to GMT *)
  let dow = days.(Ptime.weekday_num t) in
  let (year, month, day), ((hours, mins, seconds), _tz) =
    Ptime.to_date_time t
  in
  Printf.sprintf "%s, %d %s %d %02d:%02d:%02d GMT" dow day
    months.(month - 1)
    year hours mins seconds

let static_return_entire_file full_path =
  let stats = Unix.stat full_path in
  let mtime = Option.get (Ptime.of_float_s stats.st_mtime) in
  let last_modified = ptime_to_last_modified mtime in
  let file_size = Int64.of_int stats.st_size in
  let content_type = Magic_mime.lookup full_path in

  let headers =
    [
      ("Content-Type", content_type);
      ("Last-Modified", last_modified);
      ("Accept-Ranges", "bytes");
      ("Content-Length", Int64.to_string file_size);
    ]
  in
  Lwt_io.(with_file ~mode:Input full_path) (fun channel ->
      let%lwt content = Lwt_io.read channel in
      Message.response ~headers (Stream.string content) Stream.null
      |> Lwt.return)

let static_return_partial_file start_pos end_pos full_path =
  let stats = Unix.stat full_path in
  let mtime = Option.get (Ptime.of_float_s stats.st_mtime) in
  let last_modified = ptime_to_last_modified mtime in
  let file_size = Int64.of_int stats.st_size in
  let content_type = Magic_mime.lookup full_path in

  let content_length = Int64.add (Int64.sub end_pos start_pos) 1L in
  let headers =
    [
      ("Content-Type", content_type);
      ("Last-Modified", last_modified);
      ("Accept-Ranges", "bytes");
      ( "Content-Range",
        Printf.sprintf "bytes %Ld-%Ld/%Ld" start_pos end_pos file_size );
      ("Content-Length", Int64.to_string content_length);
    ]
  in

  Lwt_io.(with_file ~mode:Input full_path) (fun channel ->
      let%lwt () = Lwt_io.set_position channel start_pos in
      let buffer_size = min 8192L content_length |> Int64.to_int in
      let buffer = Bytes.create buffer_size in
      let rec read_chunk remaining acc =
        if Int64.equal remaining 0L then
          Lwt.return (Stdlib_string.concat "" (List.rev acc))
        else
          let to_read =
            min (Int64.of_int buffer_size) remaining |> Int64.to_int
          in
          let%lwt bytes_read = Lwt_io.read_into channel buffer 0 to_read in
          if bytes_read = 0 then
            Lwt.return (Stdlib_string.concat "" (List.rev acc))
          else
            let chunk = Bytes.sub_string buffer 0 bytes_read in
            read_chunk
              (Int64.sub remaining (Int64.of_int bytes_read))
              (chunk :: acc)
      in
      let%lwt content = read_chunk content_length [] in
      Message.response ~status:`Partial_Content ~headers (Stream.string content)
        Stream.null
      |> Lwt.return)

let static_loader root path request =
  (* In dream the loader is called after the path has been validated,
  which is why there is no path validation here *)
  let root =
    match Fpath.of_string root with Ok x -> x | _ -> failwith "bad root"
  in
  let path =
    match Fpath.of_string path with Ok x -> x | _ -> failwith "bad path"
  in
  let full_path = Fpath.normalize (Fpath.append root path) in
  let full_path = Fpath.to_string full_path in

  let stats = Unix.stat full_path in
  let file_size = Int64.of_int stats.st_size in

  let range_header = Dream.header request "Range" in

  Lwt.catch
    (fun () ->
      match range_header with
      | Some range_str when Stdlib_string.starts_with ~prefix:"bytes=" range_str
        -> (
          match Http.parse_range range_str file_size with
          | Some (start_pos, end_pos) ->
              static_return_partial_file start_pos end_pos full_path
          | None -> static_return_entire_file full_path)
      | _ -> static_return_entire_file full_path)
    (fun _exn ->
      Message.response ~status:`Not_Found Stream.empty Stream.null |> Lwt.return)

let direct_loader page filename _root _path request =
  (* This wrapper just avoids the need to do reverse lookups based on how I mangle file names *)
  static_loader (Fpath.to_string (Page.path page)) filename request

let routes_for_frontmatter_image_list sec page (image_loader : image_loader_t) =
  List.concat_map
    (fun (i : Frontmatter.image) ->
      [
        (* non retina *)
        esc_dream_get
          (Uri.to_string (Section.uri ~page ~resource:i.filename sec))
          (Dream.static ~loader:(fun _root _path -> image_loader page i.filename (720, 1200)) "");
        (* retina *)
        (let name, ext = Fpath.split_ext (Fpath.v i.filename) in
         let retina_name = Fpath.to_string name ^ "@2x" ^ ext in
         esc_dream_get
           (Uri.to_string (Section.uri ~page ~resource:retina_name sec))
           (Dream.static
              ~loader:(fun _root _path -> image_loader page i.filename (720 * 2, 1200 * 2))
              ""));
      ])
    (Page.images page)

let routes_for_frontmatter_video_list sec page =
  List.map
    (fun filename ->
      esc_dream_get
        (Uri.to_string (Section.uri ~page ~resource:filename sec))
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
            esc_dream_get
              (Uri.to_string (Section.uri ~page ~resource:filename sec))
              (Dream.static ~loader:(fun _root _path -> image_loader page filename (800, 600)) "");
            (let name, ext = Fpath.split_ext (Fpath.v filename) in
             let retina_name = Fpath.to_string name ^ "@2x" ^ ext in
             esc_dream_get
               (Uri.to_string (Section.uri ~page ~resource:retina_name sec))
               (Dream.static
                  ~loader:(fun _root _path -> image_loader page filename (800 * 2, 600 * 2))
                  ""));
          ]
      | _ -> [])
    (Page.shortcodes page)

let routes_for_titleimage sec page thumbnail_loader image_loader =
  match Page.titleimage page with
  | None -> []
  | Some img -> (
      (* Basic thumbnails *)
      let _, ext = Fpath.split_ext (Fpath.v img.filename) in
      match ext with
      | ".svg" ->
          [
            esc_dream_get
              (Uri.to_string (Section.uri ~page ~resource:"thumbnail.svg" sec))
              (Dream.static ~loader:(direct_loader page img.filename) "");
          ]
      | _ -> (
          [
            esc_dream_get
              (Uri.to_string (Section.uri ~page ~resource:"thumbnail.jpg" sec))
              (Dream.static ~loader:(fun _root _path -> thumbnail_loader ~retina:false page) "");
            esc_dream_get
              (Uri.to_string
                 (Section.uri ~page ~resource:"thumbnail@2x.jpg" sec))
              (Dream.static ~loader:(fun _root _path -> thumbnail_loader ~retina:true page) "");
            esc_dream_get
              (Uri.to_string (Section.uri ~page ~resource:"preview.jpg" sec))
              (Dream.static
                 ~loader:(fun _root _path -> image_loader page img.filename (2048, 2048))
                 "");
          ]
          @
          (* The photos images are also in the title image *)
          match Page.original_section_title page with
          | "photos" ->
              let name, ext = Fpath.split_ext (Fpath.v img.filename) in
              let retina_name = Fpath.to_string name ^ "@2x" ^ ext in
              [
                esc_dream_get
                  (Uri.to_string
                     (Section.uri ~page ~resource:("scrn_" ^ img.filename) sec))
                  (Dream.static
                     ~loader:(fun _root _path -> image_loader page img.filename (1008, 800))
                     "");
                esc_dream_get
                  (Uri.to_string
                     (Section.uri ~page ~resource:("scrn_" ^ retina_name) sec))
                  (Dream.static
                     ~loader:(fun _root _path -> image_loader page img.filename (2016, 1600))
                     "");
                (* This is the direct full resolution download *)
                esc_dream_get
                  (Uri.to_string (Section.uri ~page ~resource:img.filename sec))
                  (Dream.static ~loader:(direct_loader page img.filename) "");
              ]
              @ [
                  (* This is the album thumbnails *)
                  esc_dream_get
                    (Uri.to_string
                       (Section.uri ~page ~resource:("album_" ^ img.filename)
                          sec))
                    (Dream.static
                       ~loader:(fun _root _path -> image_loader page img.filename (300, 300))
                       "");
                  esc_dream_get
                    (Uri.to_string
                       (Section.uri ~page ~resource:("album_" ^ retina_name) sec))
                    (Dream.static
                       ~loader:(fun _root _path -> image_loader page img.filename (600, 600))
                       "");
                ]
          | _ -> []))

let routes_for_direct_shortcodes sec page =
  List.concat_map
    (fun (_, sc) ->
      match sc with
      | Shortcode.Vector (r, _, _) -> [ r ]
      | Shortcode.Video (r, None, _) -> [ r ]
      | Shortcode.Video (r, Some t, _) -> [ r; t ]
      | Shortcode.Audio r -> [ r ]
      | Shortcode.Chart (_, r, _, _) -> [ r ]
      | _ -> [])
    (Page.shortcodes page)
  |> List.map (fun filename ->
         esc_dream_get
           (Uri.to_string (Section.uri ~page ~resource:filename sec))
           (Dream.static ~loader:(direct_loader page filename) ""))

let routes_for_scripts_and_resources sec page =
  Page.resources page @ Page.scripts page
  |> List.map (fun filename ->
         esc_dream_get
           (Uri.to_string (Section.uri ~page ~resource:filename sec))
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
          esc_dream_get
            (Printf.sprintf "/%s/**" basename)
            (Dream.static ~loader:static_loader (Fpath.to_string (path / ".")))
      | false ->
          esc_dream_get ("/" ^ basename) (Dream.static ~loader:static_loader ""))
    things_to_be_published

let routes_for_aliases site =
  List.concat_map
    (fun sec ->
      List.concat_map
        (fun page ->
          List.map
            (fun alias ->
              esc_dream_get alias (fun r ->
                  Dream.redirect ~status:`Moved_Permanently r
                    (Uri.to_string (Section.uri ~page sec))))
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
        esc_dream_get sans_slash (fun r ->
            Dream.redirect ~status:`Moved_Permanently r page_url);
      ]

let routes_for_page site sec previous_page page next_page page_renderer
    thumbnail_loader image_loader =
  match Page.content page with
  | false -> []
  | true ->
      (* Dream.log "Adding %s" (Uri.to_string (Section.uri ~page sec)); *)
      esc_dream_get
        (Uri.to_string (Section.uri ~page sec))
        (fun _ ->
          let stats = Unix.stat (Fpath.to_string (Page.path page)) in
          let mtime = Option.get (Ptime.of_float_s stats.st_mtime) in
          let last_modified = ptime_to_last_modified mtime in
          let headers = [ ("Last-modified", last_modified) ] in
          (page_renderer page) site sec previous_page page next_page
          |> Dream.html ~headers)
      :: (routes_for_redirect_for_sans_slash sec page
         @ routes_for_titleimage sec page thumbnail_loader image_loader
         @ routes_for_frontmatter_image_list sec page image_loader
         @ routes_for_frontmatter_video_list sec page
         @ routes_for_image_shortcodes sec page image_loader
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
    esc_dream_get
      (Uri.to_string (Section.uri ~resource:"index.xml" base_section))
      (fun _ ->
        Rss.render_rss base_section site page_list
        |> Dream.respond ~headers:[ ("Content-Type", "application/rss+xml") ]);
    esc_dream_get
      (Uri.to_string (Section.uri ~resource:"feed.json" base_section))
      (fun _ ->
        let feed = Rss.render_jsonfeed base_section site page_list in
        match feed with
        | Result.Ok body ->
            Dream.respond
              ~headers:[ ("Content-Type", "application/feed+json") ]
              body
        | _ ->
            Dream.html ~status:`Internal_Server_Error
              "<h1>Something went wrong</h1>");
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
  esc_dream_get
    (Uri.to_string (Section.uri sec))
    (fun _ -> (section_renderer sec) site sec |> Dream.html)
  :: (routes_for_feed sec site pages_in_feed
     @ routes_for_pages_in_section site sec page_renderer thumbnail_loader
         image_loader)

let routes_for_taxonomies ~taxonomy_renderer ~taxonomy_section_renderer
    ~page_renderer ~page_body_renderer ~thumbnail_loader ~image_loader site =
  let taxonomies = Site.taxonomies site in
  List.concat_map
    (fun (name, taxonomy) ->
      Dream.log "Taxonomy %s: %d terms" name
        (List.length (Taxonomy.sections taxonomy));

      esc_dream_get (Uri.to_string (Taxonomy.uri taxonomy)) (fun _ ->
          let render_taxonomy = taxonomy_renderer taxonomy in
          render_taxonomy site taxonomy |> Dream.html)
      :: List.concat_map
           (fun sec ->
             routes_for_section
               ~section_renderer:(taxonomy_section_renderer taxonomy)
               ~page_renderer ~page_body_renderer ~thumbnail_loader
               ~image_loader site sec)
           (Taxonomy.sections taxonomy))
    taxonomies

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

  sections @ taxonomies @ aliases @ static