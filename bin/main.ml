open Webplats

let thumbnail_loader page thumbnail_size _root _path _request =
  let path = Snapshots.render_thumbnail page thumbnail_size in
  Dream.respond
    (In_channel.with_open_bin path (fun ic -> In_channel.input_all ic))

let snapshot_image_loader page image bounds _root _path _request =
  let path = Snapshots.render_image_fit page image bounds in
  Dream.respond
    (In_channel.with_open_bin path (fun ic -> In_channel.input_all ic))

let () =
  let site =
    Site.of_directory (Fpath.v "/Users/michael/Sites/mynameismwd.org/content")
  in

  let toplevel =
    [
      Dream.get "/" (fun _ -> Index.render_index site |> Dream.html);
      Dream.get "/index.xml" (fun _ ->
          Rss.render_rss site
            (List.concat_map Section.pages (Site.sections site)
            |> List.sort (fun a b -> Ptime.compare (Page.date b) (Page.date a))
            )
          |> Dream.html);
      Dream.get "/css/**"
        (Dream.static "/Users/michael/Sites/mynameismwd.org/public/css/.");
      Dream.get "/face/**"
        (Dream.static "/Users/michael/Sites/mynameismwd.org/public/face/.");
    ]
  in

  let sections =
    List.concat_map
      (fun sec ->
        let 
          section_renderer, 
          page_renderer,
          thumbnail_loader,
          retina_thumbnail_loader = match (Section.title sec) with
        | "posts" -> (
          Posts.render_section,
          Renderer.render_page,
          (fun p -> thumbnail_loader p 300),
          (fun p -> thumbnail_loader p 600)
        )
        | "photos" -> (          
          Photos.render_section,
          Photos.render_page,
          (fun p -> 
            let i = Option.get (Page.titleimage p) in
            snapshot_image_loader p i.filename (640, 350)
          ),          
          (fun p -> 
            let i = Option.get (Page.titleimage p) in
            snapshot_image_loader p i.filename (1280, 700)
          )
        )
        | "sounds" | "snapshots" -> (
          Snapshots.render_section,
          Snapshots.render_page,
          (fun p -> thumbnail_loader p 300),
          (fun p -> thumbnail_loader p 600)
        )
        | _-> (
          Renderer.render_section, 
          Renderer.render_page,
          (fun p -> thumbnail_loader p 300),
          (fun p -> thumbnail_loader p 600)
        )
        in
        
        (Dream.get (Section.url sec) (fun _ -> section_renderer sec |> Dream.html))
        
        :: Dream.get
             (Section.url sec ^ "index.xml")
             (fun _ -> Rss.render_rss site (Section.pages sec) |> Dream.html)
        :: List.concat_map
             (fun p ->
               (
                Dream.get (Page.url p) (fun _ -> page_renderer sec p |> Dream.html))
               :: ((* title images *)
                   (match Page.titleimage p with
                   | None -> []
                   | Some img ->(
                       [
                         Dream.get
                           (Page.url p ^ "thumbnail.jpg")
                           (Dream.static ~loader:(thumbnail_loader p) "");
                         Dream.get
                           (Page.url p ^ "thumbnail@2x.jpg")
                           (Dream.static ~loader:(retina_thumbnail_loader p) "");
                       ] @ (match (Section.title sec) with 
                         | "photos" -> let name, ext =
                                 Fpath.split_ext (Fpath.v img.filename)
                               in
                               let retina_name =
                                 Fpath.to_string name ^ "@2x" ^ ext
                               in[
                           Dream.get
                             (Page.url p ^ img.filename)
                             (Dream.static ~loader:(snapshot_image_loader p img.filename (1008, 800)) "");
                           Dream.get
                             (Page.url p ^ retina_name)
                             (Dream.static ~loader:(snapshot_image_loader p img.filename (2016, 1600)) "");
                         ]
                         | _ -> []
                       )))
                  (* snapshot style images from frontmatter *)
                  @ List.concat_map
                      (fun (i : Page.image) ->
                        [
                          Dream.get
                            (Page.url p ^ i.filename)
                            (Dream.static
                               ~loader:
                                 (snapshot_image_loader p i.filename (720, 1200))
                               "");
                          (let name, ext =
                             Fpath.split_ext (Fpath.v i.filename)
                           in
                           let retina_name =
                             Fpath.to_string name ^ "@2x" ^ ext
                           in
                           Dream.get
                             (Page.url p ^ retina_name)
                             (Dream.static
                                ~loader:
                                  (snapshot_image_loader p i.filename
                                     (720 * 2, 1200 * 2))
                                ""));
                        ])
                      (Page.images p)
                  (* images from shortcodes *)
                  @ List.concat_map
                      (fun (_, sc) ->
                        match sc with
                        | Shortcode.Image (filename, _, _) ->
                            [
                              Dream.get
                                (Page.url p ^ filename)
                                (Dream.static
                                   ~loader:
                                     (snapshot_image_loader p filename (800, 600))
                                   "");
                              (let name, ext =
                                 Fpath.split_ext (Fpath.v filename)
                               in
                               let retina_name =
                                 Fpath.to_string name ^ "@2x" ^ ext
                               in
                               Dream.get
                                 (Page.url p ^ retina_name)
                                 (Dream.static
                                    ~loader:
                                      (snapshot_image_loader p filename
                                         (800 * 2, 600 * 2))
                                    ""));
                            ]
                        | _ -> [])
                      (Page.shortcodes p)
                  (* shortcodes that are directly mapped *)
                  @ (List.concat_map
                       (fun (_, sc) ->
                         match sc with
                         | Shortcode.Video (r, None) -> [ r ]
                         | Shortcode.Video (r, Some t) -> [ r; t ]
                         | Shortcode.Audio r -> [ r ]
                         | _ -> [])
                       (Page.shortcodes p)
                    |> List.map (fun filename ->
                           Dream.get
                             (Page.url p ^ filename)
                             (fun _ ->
                               Dream.respond
                                 (In_channel.with_open_bin
                                    (Fpath.to_string
                                       (Fpath.add_seg (Page.path p) filename))
                                    (fun ic -> In_channel.input_all ic)))))))
             (Section.pages sec))
      (Site.sections site)
  in
  Dream.log "Adding %d routes" (List.length (toplevel @ sections));
  Dream.run @@ Dream.logger @@ Dream.router (toplevel @ sections)
