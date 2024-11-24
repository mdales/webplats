open Webplats

let loader page thumbnail_size _root _path _request =
  let path = Snapshots.render_thumbnail page thumbnail_size in
  Dream.respond
    (In_channel.with_open_bin path (fun ic -> In_channel.input_all ic))

let () =
  let site =
    Site.of_directory (Fpath.v "/Users/michael/Sites/mynameismwd.org/content")
  in

  let toplevel =
    [
      Dream.get "/" (fun _ -> Renderer.render_index site |> Dream.html);
      Dream.get "/css/**"
        (Dream.static "/Users/michael/Sites/mynameismwd.org/public/css/.");
      Dream.get "/face/**"
        (Dream.static "/Users/michael/Sites/mynameismwd.org/public/face/.");
    ]
  in

  let sections =
    List.concat_map
      (fun sec ->
        (match Section.title sec with
        | "snapshots" ->
            Dream.get (Section.url sec) (fun _ ->
                Snapshots.render_section sec |> Dream.html)
        | "posts" ->
            Dream.get (Section.url sec) (fun _ ->
                Posts.render_section sec |> Dream.html)
        | "photos" ->
            Dream.get (Section.url sec) (fun _ ->
                Photos.render_section sec |> Dream.html)
        | _ ->
            Dream.get (Section.url sec) (fun _ ->
                Renderer.render_section sec |> Dream.html))
        :: List.concat_map
             (fun p ->
               Dream.get (Page.url p) (fun _ ->
                   Renderer.render_page sec p |> Dream.html)
               ::
               (match Page.titleimage p with
               | None -> []
               | Some _img ->
                   [
                     Dream.get
                       (Page.url p ^ "thumbnail.jpg")
                       (Dream.static ~loader:(loader p 300) "");
                     Dream.get
                       (Page.url p ^ "thumbnail@2x.jpg")
                       (Dream.static ~loader:(loader p 600) "");
                   ]))
             (Section.pages sec))
      (Site.sections site)
  in

  Dream.run @@ Dream.logger @@ Dream.router (toplevel @ sections)
