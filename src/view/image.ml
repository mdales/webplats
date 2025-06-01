type scale = Fit | Fill

let cache_dir () =
  let p =
    match Sys.getenv_opt "WEBPLATS_CACHE_DIR" with
    | Some x -> x
    | None -> (
        match Sys.getenv_opt "TMPDIR" with Some x -> x | None -> "/tmp")
  in
  Fpath.v p

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
             | false -> Sys.mkdir (Fpath.to_string p) 0755);
             p)
           (Fpath.v hd) tl)

let _render_image_fill page filename (max_width, max_height) =
  let imgpath = Fpath.add_seg (Page.path page) filename in
  let targetname =
    Printf.sprintf "image_fill_%dx%d_%s" max_width max_height filename
  in
  let target_folder =
    Fpath.append (cache_dir ()) (Fpath.v (Page.url_name page))
  in
  let target_path = Fpath.to_string (Fpath.add_seg target_folder targetname) in
  match Sys.file_exists target_path with
  | true -> Fpath.v target_path
  | false ->
      makedirs target_folder;
      let img = Images.load (Fpath.to_string imgpath) [] in
      let width, height = Images.size img in
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
      let resultimg =
        match img with
        | Rgb24 rgb ->
            let resized = Rgb24.resize None rgb newwidth newheight in
            let cropped =
              Rgb24.sub resized
                ((newwidth - max_width) / 2)
                ((newheight - max_height) / 2)
                max_width max_height
            in
            Images.Rgb24 cropped
        | Rgba32 rgba ->
            let resized = Rgba32.resize None rgba newwidth newheight in
            let cropped =
              Rgba32.sub resized
                ((newwidth - max_width) / 2)
                ((newheight - max_height) / 2)
                max_width max_height
            in
            Images.Rgba32 cropped
        | _ -> failwith "unexpcted image format"
      in
      Images.save target_path None [] resultimg;
      Fpath.v target_path

let _render_image_fit page filename (max_width, max_height) =
  let imgpath = Fpath.add_seg (Page.path page) filename in
  let targetname =
    Printf.sprintf "image_fit_%dx%d_%s" max_width max_height filename
  in
  let target_folder =
    Fpath.append (cache_dir ()) (Fpath.v (Page.url_name page))
  in
  let target_path = Fpath.to_string (Fpath.add_seg target_folder targetname) in
  match Sys.file_exists target_path with
  | true -> Fpath.v target_path
  | false -> (
      makedirs target_folder;
      let img = Images.load (Fpath.to_string imgpath) [] in
      let width, height = Images.size img in
      let fwidth = float_of_int width and fheight = float_of_int height in
      let wratio = float_of_int max_width /. fwidth
      and hratio = float_of_int max_height /. fheight in
      match wratio >= 1.0 && hratio >= 1.0 with
      | true -> imgpath
      | false ->
          let ratio = min wratio hratio in
          let newwidth = int_of_float (ratio *. fwidth)
          and newheight = int_of_float (ratio *. fheight) in
          let resultimg =
            match img with
            | Rgb24 rgb ->
                let resized = Rgb24.resize None rgb newwidth newheight in
                Images.Rgb24 resized
            | Rgba32 rgba ->
                let resized = Rgba32.resize None rgba newwidth newheight in
                Images.Rgba32 resized
            | _ -> failwith "unexpcted image format"
          in
          Images.save target_path None [] resultimg;
          Fpath.v target_path)

let render_image_fill_lwt page filename (max_width, max_height) =
  let imgpath = Fpath.add_seg (Page.path page) filename in
  let targetname =
    Printf.sprintf "image_fill_%dx%d_%s" max_width max_height filename
  in
  let target_folder =
    Fpath.append (cache_dir ()) (Fpath.v (Page.url_name page))
  in
  let target_path = Fpath.to_string (Fpath.add_seg target_folder targetname) in
  match Sys.file_exists target_path with
  | true -> Lwt.return (Fpath.v target_path)
  | false ->
      makedirs target_folder;
      let img = Images.load (Fpath.to_string imgpath) [] in
      let width, height = Images.size img in
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
      let rp = Lwt_process.exec ("gm", [| "gm" ; "convert" ; (Fpath.to_string imgpath) ; "-resize" ; (Printf.sprintf "%dx%d" newwidth newheight) ; target_path |]) in
      Lwt.bind rp (fun _res -> Lwt.return (Fpath.v target_path))

let render_image_fit_lwt page filename (max_width, max_height) =
  let imgpath = Fpath.add_seg (Page.path page) filename in
  let targetname =
    Printf.sprintf "image_fit_%dx%d_%s" max_width max_height filename
  in
  let target_folder =
    Fpath.append (cache_dir ()) (Fpath.v (Page.url_name page))
  in
  let target_path = Fpath.to_string (Fpath.add_seg target_folder targetname) in
  match Sys.file_exists target_path with
  | true -> Lwt.return (Fpath.v target_path)
  | false -> (
    makedirs target_folder;
    let img = Images.load (Fpath.to_string imgpath) [] in
    let width, height = Images.size img in
    let fwidth = float_of_int width and fheight = float_of_int height in
    let wratio = float_of_int max_width /. fwidth
    and hratio = float_of_int max_height /. fheight in
    match wratio >= 1.0 && hratio >= 1.0 with
    | true -> Lwt.return imgpath
    | false -> (
      let ratio = min wratio hratio in
      let newwidth = int_of_float (ratio *. fwidth)
      and newheight = int_of_float (ratio *. fheight) in
      let rp = Lwt_process.exec ("gm", [| "gm" ; "convert" ; (Fpath.to_string imgpath) ; "-resize" ; (Printf.sprintf "%dx%d" newwidth newheight) ; target_path |]) in
      Lwt.bind rp (fun _res -> Lwt.return (Fpath.v target_path))
    )
  )

let render_image_lwt page filename scale (max_width, max_height) =
  match scale with
  | Fit -> render_image_fit_lwt page filename (max_width, max_height)
  | Fill -> render_image_fill_lwt page filename (max_width, max_height)

let render_thumbnail_lwt page thumbnail_size =
  match Page.titleimage page with
  | None -> failwith "blah"
  | Some titleimg ->
      render_image_fill_lwt page titleimg.filename (thumbnail_size, thumbnail_size)
