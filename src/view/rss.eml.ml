let recent_pages pages =
	let rec loop count pages =
		match count with
		| 0 -> []
		| _ -> (
			match pages with
			| [] -> []
			| (sec, page, renderer) :: tl -> (
        match Page.in_feed page with
        | true -> (sec, page, renderer) :: (loop (count - 1) tl)
        | false -> loop count tl
      )
		)
	in
	loop 10 pages

let render_rss base_section site pages =
  <rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">
  <channel>
    <title><%s Site.title site %></title>
    <link><%s Uri.to_string (Uri.with_uri ~path:(Some (Uri.path (Section.uri base_section))) (Site.uri site)) %></link>
    <description>Recent content on <%s Site.title site %></description>
    <generator>https://github.com/mdales/webplats/</generator>
    <language>en-us</language>
% (match pages with (_, hd, _) :: _ ->
    <lastBuildDate><%s Ptime.to_rfc3339 (Page.date hd) %></lastBuildDate>
% | [] -> ());
    <atom:link href="<%s Uri.to_string (Uri.with_uri ~path:(Some (Uri.path (Section.uri ~resource:{|index.xml|} base_section))) (Site.uri site)) %>" rel="self" type="application/rss+xml" />
% (List.iter (fun (sec, page, renderer) ->
        <item>
          <title><%s Page.title page %></title>
% (match (Page.get_key_as_string page "source") with Some url ->
          <link><%s url %></link>
% | None -> (
          <link><%s Uri.to_string (Uri.with_uri ~path:(Some (Uri.path (Section.uri ~page sec))) (Site.uri site)) %></link>
% ));
          <pubDate><%s Ptime.to_rfc3339 (Page.date page) %></pubDate>
          <guid><%s Uri.to_string (Uri.with_uri ~path:(Some (Uri.path (Section.uri ~page sec))) (Site.uri site)) %></guid>

          <description>
% (match (Page.get_key_as_bool page "rss-inline") with Some false ->
    <p><a href="<%s Uri.to_string (Section.uri ~page sec) %>">Please visit page for details.</a></p>
% | _ -> (
      <%s renderer page %>
% ));
          </description>
        </item>
% ) (recent_pages pages));

    </channel>
  </rss>

let render_jsonfeed base_section site pages =
  (* let feed_url = Uri.with_uri ~path:(Some "feed.json") base_url in *)
  let pages = recent_pages pages in
  let items = List.map (fun (sec, page, renderer) ->
    (* let canonical_url = Uri.to_string (Uri.with_uri ~path:(Some (Uri.path (Section.uri ~page sec))) (Site.uri site)) in *)
    (* let page_url = match (Page.get_key_as_string page "source") with
    | Some url -> url
    | None -> canonical_url
    in *)
    let tags = Page.tags page in
    Jsonfeed.Item.create
    ~id:( Uri.to_string (Uri.with_uri ~path:(Some (Uri.path (Section.uri ~page sec))) (Site.uri site)))
    ~url:( Uri.to_string (Uri.with_uri ~path:(Some (Uri.path (Section.uri ~page sec))) (Site.uri site)))
    ~title:(Page.title page)
    ~content:(`Html (renderer page))
    ~tags:tags
    ()
  ) pages in
  let feed = Jsonfeed.create
    ~title:(Site.title site)
    ~home_page_url:(Uri.to_string (Uri.with_uri ~path:(Some (Uri.path (Section.uri base_section))) (Site.uri site)))
    ~feed_url:(Uri.to_string (Uri.with_uri ~path:(Some (Uri.path (Section.uri ~resource:{|index.xml|} base_section))) (Site.uri site)))
    ~items:items
    () in
  Jsonfeed.to_string feed
