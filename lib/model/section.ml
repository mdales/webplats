type t = { title : string; pages : Page.t list; url : string; synthetic : bool }

let rec find_markdown_files path =
  Sys.readdir (Fpath.to_string path)
  |> Array.to_list
  |> List.map (fun p -> Fpath.append path (Fpath.v p))
  |> List.concat_map (fun p ->
         match Sys.is_directory (Fpath.to_string p) with
         | true -> find_markdown_files p
         | false -> ( match Fpath.get_ext p with ".md" -> [ p ] | _ -> []))

let v ?(synthetic = true) title url pages = { title; pages; url; synthetic }

let updated_with_page t p = { t with pages = p :: t.pages}

let of_directory ~base path =
  let url = Fpath.rem_prefix base path in
  let url =
    match url with
    | Some url -> "/" ^ Fpath.to_string url
    | None -> failwith "base is not parent directory"
  in

  let title = (Fpath.basename path) in

  let paths =
    find_markdown_files path
    |> List.filter (fun p -> Fpath.basename p = "index.md")
  in
  let pages =
    List.map
      (Page.of_file ~titleimage_details:(Fpath.basename path = "photos") title)
      paths
    |> List.filter (fun p -> not (Page.draft p))
    |> List.sort (fun a b -> Ptime.compare (Page.date b) (Page.date a))
  in
  v ~synthetic:false title url pages

let title t = t.title
let pages t = t.pages
let synthetic t = t.synthetic

let url ?page t =
  match page with
  | None -> t.url
  | Some p -> Printf.sprintf "%s%s/" t.url (Page.url_name p)
