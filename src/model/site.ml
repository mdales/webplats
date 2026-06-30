type t = {
  sections : Section.t list;
  toplevel : Section.t;
  config : Config.t;
  path : Eio.Fs.dir_ty Eio.Path.t;
  taxonomies : (string * Taxonomy.t) list;
  css_digest_path : Eio.Fs.dir_ty Eio.Path.t option;
}

let build_taxonomy taxonomy_name pages =
  Printf.printf "Building taxonomy %s" taxonomy_name;
  List.fold_left
    (fun acc page ->
      let sl =
        Page.get_key_as_string_list page taxonomy_name
        |> List.map String.lowercase_ascii
      in
      List.fold_left
        (fun acc term ->
          match List.assoc_opt term acc with
          | None ->
              let term_for_url =
                String.map (fun c -> match c with ' ' -> '-' | x -> x) term
              in
              ( term,
                Section.v ~synthetic:true term
                  (Uri.of_string
                     (Printf.sprintf "/%s/%s/" taxonomy_name term_for_url))
                  [ page ] )
              :: acc
          | Some section ->
              (term, Section.updated_with_page section page)
              :: List.remove_assoc term acc)
        acc sl)
    []
    (List.sort (fun a b -> Ptime.compare (Page.date a) (Page.date b)) pages)
  (* The above sort is backwards as when we build the list per tag it'll be reversed again. *)
  |> List.map (fun (_, v) -> v)

let of_directory path =
  let config =
    List.fold_left
      (fun acc fname ->
        match acc with
        | Some _ -> acc
        | None -> (
            let config_path = Eio.Path.(path / fname) in
            try
              Fmt.pr "%a@\n" Eio.Path.pp config_path;
              Some (Config.of_file config_path)
            with Sys_error _ -> None))
      None
      [ "hugo.yml"; "hugo.yaml"; "config.yml"; "config.yaml" ]
  in
  let config =
    match config with None -> failwith "Failed to find config" | Some c -> c
  in

  let content_path = Eio.Path.(path / "content") in

  let root_listing =
    Eio.Path.read_dir content_path
    |> List.map (fun item -> Eio.Path.(content_path / item))
  in

  let sections =
    List.filter_map
      (fun p ->
        match Eio.Path.is_directory p with
        | false -> None
        | true -> (
            match Eio.Path.is_file Eio.Path.(p / "index.md") with
            | false -> Some p
            | true -> None))
      root_listing
    |> List.map (fun p -> Section.of_directory ~base:content_path p)
    |> List.filter_map (fun s ->
        match Section.pages s with [] -> None | _ -> Some s)
    |> List.sort (fun a b ->
        Ptime.compare
          (Page.date (List.hd (Section.pages b)))
          (Page.date (List.hd (Section.pages a))))
  in

  let root_pages =
    List.filter_map
      (fun p ->
        let _, basename = Option.get (Eio.Path.split p) in
        let fbasename = Fpath.v basename in
        match Fpath.get_ext fbasename with
        | ".md" -> Some (Page.of_file "root" "/" p)
        | _ -> (
            match Eio.Path.is_directory p with
            | false -> None
            | true -> (
                let root_dir_path = Eio.Path.(p / "index.md") in
                match Eio.Path.is_file root_dir_path with
                | true -> Some (Page.of_file "root" "/" root_dir_path)
                | false -> None)))
      root_listing
  in

  let toplevel = Section.v "website" (Uri.of_string "/") root_pages in

  let css_digest_path =
    match Config.css_path config with
    | None -> None
    | Some p ->
        let digest =
          Eio.Path.with_open_in p (fun f ->
              Digest.to_hex (Digest.string (Eio.Flow.read_all f)))
        in
        let parent, basename = Option.get (Eio.Path.split p) in
        let digest_path =
          Eio.Path.(parent / Printf.sprintf "%s_%s" digest basename)
        in
        Some digest_path
  in

  let taxonomies =
    List.map
      (fun (tag, name) ->
        (* Super inefficient, but hopefully works and we can optimise later... *)
        let pages = List.concat_map Section.pages sections in
        let sections = build_taxonomy tag pages in
        (tag, Taxonomy.v tag name sections))
      (Config.taxonomies config)
  in

  { sections; toplevel; config; path; taxonomies; css_digest_path }

let sections t = t.toplevel :: t.sections
let title t = Config.title t.config
let toplevel t = t.toplevel
let path t = t.path
let hugo_theme t = Config.hugo_theme t.config
let taxonomies t = t.taxonomies
let uri t = Config.base_url t.config
let port t = Config.port t.config
let author t = Config.author t.config
let css_digest_path t = t.css_digest_path
let css_path t = Config.css_path t.config
