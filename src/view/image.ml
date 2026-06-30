type scale = Fit | Fill

let makedirs path =
  let segs =
    Fpath.segs path |> List.map (fun p -> match p with "" -> "/" | _ -> p)
  in
  match segs with
  | [] -> ()
  | hd :: tl ->
      ignore
        (List.fold_left
           (fun acc seg ->
             let p = Fpath.add_seg acc seg in
             (match Sys.file_exists (Fpath.to_string p) with
             | true -> ()
             | false -> Sys.mkdir (Fpath.to_string p) 0o755);
             p)
           (Fpath.v hd) tl)

let render_diagram process_mgr cache_dir page code =
  let hash = Digest.string code |> Digest.to_hex in
  let filename = Printf.sprintf "%s.svg" hash in
  let target_folder = Eio.Path.(cache_dir / Page.url_name page) in
  let target_path = Eio.Path.(target_folder / filename) in
  match Eio.Path.is_file target_path with
  | true -> target_path
  | false ->
      (try Eio.Path.mkdir ~perm:0o755 target_folder
       with Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> ());
      Eio.Switch.run @@ fun sw ->
      let stdin_r, stdin_w = Eio.Process.pipe ~sw process_mgr in
      Eio.Fiber.both
        (fun () ->
          Eio.Flow.copy_string code stdin_w;
          Eio.Flow.close stdin_w)
        (fun () ->
          Eio.Process.run process_mgr ~stdin:stdin_r
            [ "d2"; "--sketch"; "-"; Option.get (Eio.Path.native target_path) ]);
      target_path

let render_image_fill process_mgr cache_dir page filename (max_width, max_height)
    =
  let imgpath = Eio.Path.(Page.path page / filename) in
  let targetname =
    Printf.sprintf "image_fill_%dx%d_%s" max_width max_height filename
  in
  let target_folder = Eio.Path.(cache_dir / Page.url_name page) in
  let target_path = Eio.Path.(target_folder / targetname) in
  match Eio.Path.is_file target_path with
  | true -> target_path
  | false ->
      (try Eio.Path.mkdir ~perm:0o755 target_folder
       with Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> ());
      let metadata =
        Metadata.Image.parse_file (Option.get (Eio.Path.native imgpath))
      in
      let width = int_of_string (List.assoc "width" metadata)
      and height = int_of_string (List.assoc "height" metadata) in

      let fwidth = float_of_int width and fheight = float_of_int height in
      let f_max_width = float_of_int max_width
      and f_max_height = float_of_int max_height in
      let ftarget =
        match fwidth /. fheight > f_max_width /. f_max_height with
        | true -> f_max_width
        | false -> f_max_height
      in
      let wratio = ftarget /. fwidth and hratio = ftarget /. fheight in
      let ratio = max wratio hratio in
      let newwidth = int_of_float (ratio *. fwidth)
      and newheight = int_of_float (ratio *. fheight) in

      Eio.Process.run process_mgr
        [
          "gm";
          "convert";
          Option.get (Eio.Path.native imgpath);
          "-resize";
          Printf.sprintf "%dx%d" newwidth newheight;
          Option.get (Eio.Path.native target_path);
        ];
      target_path

let render_image_fit process_mgr cache_dir page filename (max_width, max_height)
    =
  let imgpath = Eio.Path.(Page.path page / filename) in
  let targetname =
    Printf.sprintf "image_fit_%dx%d_%s" max_width max_height filename
  in
  let target_folder = Eio.Path.(cache_dir / Page.url_name page) in
  let target_path = Eio.Path.(target_folder / targetname) in
  match Eio.Path.is_file target_path with
  | true -> target_path
  | false -> (
      (try Eio.Path.mkdir ~perm:0o755 target_folder
       with Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> ());
      let metadata =
        Metadata.Image.parse_file (Option.get (Eio.Path.native imgpath))
      in
      let width = int_of_string (List.assoc "width" metadata)
      and height = int_of_string (List.assoc "height" metadata) in
      let fwidth = float_of_int width and fheight = float_of_int height in
      let wratio = float_of_int max_width /. fwidth
      and hratio = float_of_int max_height /. fheight in
      match wratio >= 1.0 && hratio >= 1.0 with
      | true -> imgpath
      | false ->
          let ratio = min wratio hratio in
          let newwidth = int_of_float (ratio *. fwidth)
          and newheight = int_of_float (ratio *. fheight) in
          Eio.Process.run process_mgr
            [
              "gm";
              "convert";
              Option.get (Eio.Path.native imgpath);
              "-resize";
              Printf.sprintf "%dx%d" newwidth newheight;
              Option.get (Eio.Path.native target_path);
            ];
          target_path)

let render_image process_mgr cache_dir page filename scale
    (max_width, max_height) =
  match scale with
  | Fit ->
      render_image_fit process_mgr cache_dir page filename
        (max_width, max_height)
  | Fill ->
      render_image_fill process_mgr cache_dir page filename
        (max_width, max_height)

let render_thumbnail process_mgr cache_dir page thumbnail_size =
  match Page.titleimage page with
  | None -> failwith "blah"
  | Some titleimg ->
      render_image_fill process_mgr cache_dir page titleimg.filename
        (thumbnail_size, thumbnail_size)
