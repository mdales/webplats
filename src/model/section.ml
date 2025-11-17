type t = { title : string; pages : Page.t list; uri : Uri.t; synthetic : bool }

let rec find_markdown_files path =
  Sys.readdir (Fpath.to_string path)
  |> Array.to_list
  |> List.map (fun p -> Fpath.append path (Fpath.v p))
  |> List.concat_map (fun p ->
         match Sys.is_directory (Fpath.to_string p) with
         | true -> find_markdown_files p
         | false -> ( match Fpath.get_ext p with ".md" -> [ p ] | _ -> []))

let v ?(synthetic = true) title uri pages = { title; pages; uri; synthetic }
let updated_with_page t p = { t with pages = p :: t.pages }

let of_directory ~base path =
  let url_path =
    match Fpath.rem_prefix base path with
    | Some path -> "/" ^ Fpath.to_string path
    | None -> failwith "base is not parent directory"
  in

  let title = Fpath.basename path in

  let paths =
    find_markdown_files path
    |> List.filter (fun p -> Fpath.basename p = "index.md")
  in
  let pages =
    List.map (Page.of_file ~base:(Some path) title url_path) paths
    |> List.filter (fun p -> not (Page.draft p))
    |> List.sort (fun a b -> Ptime.compare (Page.date b) (Page.date a))
  in
  v ~synthetic:false title (Uri.of_string url_path) pages

let title t = t.title
let pages t = t.pages
let synthetic t = t.synthetic

let uri ?page ?resource t =
  match page with
  | None -> (
      match resource with
      | None -> t.uri
      | Some filename ->
          let current_path = Fpath.v (Uri.path t.uri) in
          let extended_path = Fpath.add_seg current_path filename in
          Uri.with_uri ~path:(Some (Fpath.to_string extended_path)) t.uri)
  | Some p ->
      let page_name = Page.url_name p in
      (* This should be built into Uri IMNSHO, but there is no append path function :/ *)
      let page_path =
        match resource with
        | None -> Fpath.v (page_name ^ "/")
        | Some filename -> Fpath.add_seg (Fpath.v page_name) filename
      in
      let current_path = Fpath.v (Uri.path t.uri) in
      let extended_path = Fpath.append current_path page_path in
      Uri.with_uri ~path:(Some (Fpath.to_string extended_path)) t.uri
