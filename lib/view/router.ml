open Fpath

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
            (Dream.static (Fpath.to_string (path / ".")))
      | false ->
          Dream.get ("/" ^ basename)
            (Dream.static
               ~loader:(fun _root _path _request ->
                 Dream.respond
                   (In_channel.with_open_bin (Fpath.to_string path) (fun ic ->
                        In_channel.input_all ic)))
               ""))
    things_to_be_published
