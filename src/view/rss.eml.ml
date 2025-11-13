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

let render_rss site pages =
  <rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">
  <channel>
    <title><%s Site.title site %></title>
    <link><%s Uri.to_string (Site.url site) %></link>
    <description>Recent content on my <%s Site.title site %></description>
    <generator>https://github.com/mdales/webplats/</generator>
    <language>en-us</language>
% (match pages with (_, hd, _) :: _ ->
    <lastBuildDate><%s Ptime.to_rfc3339 (Page.date hd) %></lastBuildDate>
% | [] -> ());
    <atom:link href="<%s Uri.to_string (Uri.with_uri ~path:(Some {|index.xml|}) (Site.url site)) %>" rel="self" type="application/rss+xml" />
% (List.iter (fun (sec, page, renderer) ->
        <item>
          <title><%s Page.title page %></title>
% (match (Page.get_key_as_string page "source") with Some url ->
          <link><%s url %></link>
% | None -> (
          <link><%s Uri.to_string (Uri.with_uri ~path:(Some (Section.url ~page sec)) (Site.url site)) %></link>
% ));
          <pubDate><%s Ptime.to_rfc3339 (Page.date page) %></pubDate>
          <guid><%s Uri.to_string (Uri.with_uri ~path:(Some (Section.url ~page sec)) (Site.url site)) %></guid>

          <description>
% (match Page.original_section_title page with "photos" ->
% let img = Option.get (Page.titleimage page) in
             <%s Shortcodes.render_raster ((Section.url ~page sec) ^ img.filename) img.description None None %>
% | _ -> ());
% (match (Page.get_key_as_bool page "rss-inline") with Some false ->
    <p><a href="<%s Section.url ~page sec %>">Please visit page for details.</a></p>
% | _ -> (
      <%s renderer page %>
% ));
          </description>
        </item>
% ) (recent_pages pages));

    </channel>
  </rss>

let render_jsonfeed site pages =
  let pages = recent_pages pages in
  let items = List.map (fun (sec, page, renderer) ->
    let canonical_url = Uri.to_string (Uri.with_uri ~path:(Some (Section.url ~page sec)) (Site.url site)) in
    let page_url = match (Page.get_key_as_string page "source") with
    | Some url -> url
    | None -> canonical_url
    in
    let tags = Page.tags page in
    Jsonfeed.Item.create
    ~id:canonical_url
    ~url:page_url
    ~title:(Page.title page)
    ~content:(`Html (renderer page))
    ~tags:tags
    ()
  ) pages in
  let feed = Jsonfeed.create
    ~title:(Site.title site)
    ~home_page_url:(Uri.to_string (Site.url site))
    ~feed_url:(Printf.sprintf "%s/feed.json" (Uri.to_string (Site.url site)))
    ~items:items
    () in
  Jsonfeed.to_string feed
