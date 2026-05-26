open Htmlit

let render_head_generic site =
    [
        El.link ~at:[At.rel "stylesheet"; At.href "/css/base.css"; At.media "screen"] ();
    ]
    @
    List.map (fun dim ->
        El.link ~at:[
            At.rel "icon";
            At.v "sizes" (Printf.sprintf "%dx%d" dim dim);
            At.href (Printf.sprintf "/img/favicon-%d.png" dim);
        ] ()
    ) [32;57;180;192]
    @
    List.map (fun dim ->
        El.link ~at:[
            At.rel "apple-touch-icon";
            At.v "sizes" (Printf.sprintf "%dx%d" dim dim);
            At.href (Printf.sprintf "/img/favicon-%d.png" dim);
        ] ()
    ) [57;180]
    @
    List.map (fun url ->
        El.link ~at:[
            At.rel "me";
            At.href url;
        ] ()
    ) [
        "https://mastodon.me.uk/@mdales";
        "https://toot.mynameismwd.org/@michael";
    ]
    @
    List.concat_map (fun sec ->
        [
            El.link ~at:[
                At.rel "feed";
                At.title (Printf.sprintf "%s: %s" (Site.title site) (Section.title sec));
                At.type' "application/rss+xml";
                At.href (Uri.to_string (Section.uri ~resource:"index.xml" sec))
            ] ();
            El.link ~at:[
                At.rel "alternate";
                At.title (Printf.sprintf "%s: %s" (Site.title site) (Section.title sec));
                At.type' "application/rss+xml";
                At.href (Uri.to_string (Section.uri ~resource:"index.xml" sec))
            ] ();
            El.link ~at:[
            At.rel "alternate";
                At.title (Printf.sprintf "JSONfeed: %s" (Section.title sec));
                At.type' "application/json+feed";
                At.href (Uri.to_string (Section.uri ~resource:"feed.json" sec))
            ] ();
        ]
    ) (Site.sections site)
    @
    (
    match (Site.author site) with Some name -> [El.meta ~at:[
        At.name "author"; At.content name
    ] ();]
    | None -> []
    )
    @
    [
        El.meta ~at:[
            At.v "property" "og:site_name";
            At.content (Site.title site)
        ] ();
    ]

let render_head_section sec =
    [
        El.title [El.txt (Section.title sec)];
        El.meta ~at:[
            At.v "property" "og:type";
            At.content "website";
        ] ();
        El.meta ~at:[
            At.v "property" "og:title";
            At.content (Section.title sec);
        ] ();
        El.meta ~at:[
            At.v "itemprop" "name";
            At.content (Section.title sec);
        ] ();
        El.meta ~at:[
            At.v "property" "og:url";
            At.content (Uri.to_string (Section.uri sec));
        ] ();
        El.meta ~at:[
            At.v "itemprop" "url";
            At.content (Uri.to_string (Section.uri sec));
        ] ();
    ]

let render_head_page sec page =
    [
        El.title [El.txt (Page.title page)];
        El.meta ~at:[
            At.v "property" "og:type";
            At.content "article";
        ] ();
        El.meta ~at:[
            At.v "property" "og:title";
            At.content (Page.title page);
        ] ();
        El.meta ~at:[
            At.v "itemprop" "name";
            At.content (Page.title page);
        ] ();
        El.meta ~at:[
            At.v "property" "og:url";
            At.content (Uri.to_string (Section.uri ~page sec));
        ] ();
        El.meta ~at:[
            At.v "itemprop" "url";
            At.content (Uri.to_string (Section.uri ~page sec));
        ] ();
    ]
    @
    (match (Page.synopsis page) with Some syn -> (
        [
            El.meta ~at:[
                At.v "property" "og:description";
                At.content syn;
            ] ();
            El.meta ~at:[
                At.v "itemprop" "description";
                At.content syn;
            ] ();
        ]
    )
    | None -> [])
    @
    (match (Page.titleimage page) with Some i -> (
        [
            El.meta ~at:[
                At.v "property" "og:image";
                At.content "preview.jpg";
            ] ();
            El.meta ~at:[
                At.v "itemprop" "image";
                At.content "preview.jpg";
            ] ();
        ]
        @
        (
        match i.description with Some desc -> (
            [
                El.meta ~at:[
                    At.v "property" "og:image:alt";
                    At.content desc;
                ] ();
                El.meta ~at:[
                    At.v "itemprop" "image:alt";
                    At.content desc;
                ] ();
            ]
        )
        | None -> []
        )
    )
    | None -> [])
    @
    List.map (fun filename ->
        El.script ~at:[
            At.src filename;
            At.type' "module";
        ] [];
    ) (Page.scripts page)

let render_head_unknown site =
    [
        El.title [El.txt (Site.title site)];
        El.meta ~at:[
            At.v "property" "og:type";
            At.content "website";
        ] ();
    ]

let render_head ~site ~sec ~page () =
    let headers = match page with
    | Some p -> (
        let chart_headers = match Page.has_chart p with
        | true -> [
            El.script ~at:[
                At.src "https://cdn.jsdelivr.net/npm/vega@6;"
            ] [];
            El.script ~at:[
                At.src "https://cdn.jsdelivr.net/npm/vega-lite@6;"
            ] [];
            El.script ~at:[
                At.src "https://cdn.jsdelivr.net/npm/vega-embed@7;"
            ] [];
        ]
        | false -> []
        in
        let map_headers = match Page.has_map p with
        | true -> [
            El.link ~at:[At.rel "stylesheet"; At.href "/leaflet@1.9.4/dist/leaflet.css"] ();
            El.script ~at:[At.src "/leaflet@1.9.4/dist/leaflet.js"] [];
        ]
        | false -> []
        in
        (render_head_page (Option.get sec) p) @ map_headers @ chart_headers
    )
    | None -> (
        match sec with
        | Some s -> render_head_section s
        | None -> render_head_unknown site
    )
    in

    let all_headers = headers @ (render_head_generic site) in

    El.head all_headers
