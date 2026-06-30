let recent_pages pages =
  let rec loop count pages =
    match count with
    | 0 -> []
    | _ -> (
        match pages with
        | [] -> []
        | (sec, page, renderer) :: tl -> (
            match Page.in_feed page with
            | true -> (sec, page, renderer) :: loop (count - 1) tl
            | false -> loop count tl))
  in
  loop 10 pages

let render_jsonfeed base_section site pages =
  (* let feed_url = Uri.with_uri ~path:(Some "feed.json") base_url in *)
  let pages = recent_pages pages in
  let items =
    List.map
      (fun (sec, page, renderer) ->
        (* let canonical_url = Uri.to_string (Uri.with_uri ~path:(Some (Uri.path (Section.uri ~page sec))) (Site.uri site)) in *)
        (* let page_url = match (Page.get_key_as_string page "source") with
    | Some url -> url
    | None -> canonical_url
    in *)
        let tags = Page.tags page in
        Jsonfeed.Item.create
          ~id:
            (Uri.to_string
               (Uri.with_uri
                  ~path:(Some (Uri.path (Section.uri ~page sec)))
                  (Site.uri site)))
          ~url:
            (Uri.to_string
               (Uri.with_uri
                  ~path:(Some (Uri.path (Section.uri ~page sec)))
                  (Site.uri site)))
          ~title:(Page.title page)
          ~content:(`Html (renderer page))
          ~tags ())
      pages
  in
  let feed =
    Jsonfeed.create ~title:(Site.title site)
      ~home_page_url:
        (Uri.to_string
           (Uri.with_uri
              ~path:(Some (Uri.path (Section.uri base_section)))
              (Site.uri site)))
      ~feed_url:
        (Uri.to_string
           (Uri.with_uri
              ~path:
                (Some
                   (Uri.path (Section.uri ~resource:{|index.xml|} base_section)))
              (Site.uri site)))
      ~items ()
  in
  Jsonfeed.to_string feed

let render_atom base_section site pages =
  let author =
    match Site.author site with
    | Some name -> Syndic.Atom.author name
    | None -> Syndic.Atom.author "Unknown author"
  in

  let items =
    List.map
      (fun (sec, page, renderer) ->
        let title : Syndic.Atom.text_construct = Text (Page.title page) in

        let url =
          match Page.get_key_as_string page "source" with
          | Some url -> Uri.of_string url
          | None ->
              Uri.with_uri
                ~path:(Some (Uri.path (Section.uri ~page sec)))
                (Site.uri site)
        in
        let link =
          Syndic.Atom.link ~title:(Page.title page) ~rel:Alternate url
        in

        let content : Syndic.Atom.content = Html (None, renderer page) in
        let updated = Page.date page in
        let authors = (author, []) in

        Syndic.Atom.entry ~id:url ~authors ~title ~updated ~published:updated
          ~content ~links:[ link ] ())
      (recent_pages pages)
  in

  let feed_link =
    Uri.with_uri
      ~path:(Some (Uri.path (Section.uri base_section)))
      (Site.uri site)
  in
  let feed_title : Syndic.Atom.text_construct = Text (Site.title site) in
  let generator =
    Syndic.Atom.generator
      ~uri:(Uri.of_string "https://github.com/mdales/webplats/")
      "webplats"
  in
  let updated =
    match pages with (_, hd, _) :: _ -> Page.date hd | [] -> Ptime.epoch
  in

  let channel_uri =
    Uri.with_uri
      ~path:(Some (Uri.path (Section.uri base_section)))
      (Site.uri site)
  in
  let self_uri =
    Uri.with_uri
      ~path:(Some (Uri.path (Section.uri ~resource:{|index.xml|} base_section)))
      (Site.uri site)
  in
  let links =
    [
      Syndic.Atom.link ~rel:Alternate ~type_media:"text/html" channel_uri;
      Syndic.Atom.link ~rel:Self ~type_media:"application/atom+xml" self_uri;
    ]
  in

  let feed =
    Syndic.Atom.feed ~id:feed_link ~links ~authors:[ author ] ~title:feed_title
      ~updated ~generator items
  in

  let buf = Buffer.create 4096 in
  Syndic.Atom.output feed (`Buffer buf);
  Buffer.contents buf
