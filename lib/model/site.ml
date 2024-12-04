type t = {
  sections : Section.t list;
  toplevel : Section.t;
  config : Config.t;
  path : Fpath.t;
}

let of_directory path =
  let config =
    List.fold_left
      (fun acc fname ->
        match acc with
        | Some _ -> acc
        | None -> (
            let config_path = Fpath.add_seg path fname in
            try
              Printf.printf "%s\n" (Fpath.to_string config_path);
              Some (Config.of_file config_path)
            with Sys_error _ -> None))
      None
      [ "hugo.yml"; "hugo.yaml"; "config.yml"; "config.yaml" ]
  in
  let config =
    match config with None -> failwith "Failed to find config" | Some c -> c
  in

  let content_path = Fpath.add_seg path "content" in

  let root_listing =
    Sys.readdir (Fpath.to_string content_path)
    |> Array.to_list
    |> List.map (fun p -> Fpath.append content_path (Fpath.v p))
  in

  let sections =
    List.filter_map
      (fun p ->
        match Sys.is_directory (Fpath.to_string p) with
        | false -> None
        | true -> Some (Fpath.to_dir_path p))
      root_listing
    |> List.map (fun p -> Section.of_directory ~base:content_path p)
    |> List.sort (fun a b ->
           Ptime.compare
             (Page.date (List.hd (Section.pages b)))
             (Page.date (List.hd (Section.pages a))))
  in

  let root_pages =
    List.filter_map
      (fun p ->
        match Fpath.get_ext p with ".md" -> Some (Page.of_file p) | _ -> None)
      root_listing
  in

  let toplevel = Section.v "website" "/" root_pages in

  { sections; toplevel; config; path }

let sections t = t.toplevel :: t.sections
let title t = Config.title t.config
let toplevel t = t.toplevel
let path t = t.path
let hugo_theme t = Config.hugo_theme t.config
