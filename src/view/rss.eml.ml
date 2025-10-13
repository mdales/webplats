let recent_pages pages =
	let rec loop count pages =
		match count with
		| 0 -> []
		| _ -> (
			match pages with
			| [] -> []
			| (sec, page) :: tl -> (
        match Page.in_feed page with
        | true -> (sec, page) :: (loop (count - 1) tl)
        | false -> loop count tl
      )
		)
	in
	loop 10 pages

let render_page_images sec page =
  <div>
% (List.iter (fun (i : Frontmatter.image) ->
%   let name, ext = Fpath.split_ext (Fpath.v i.filename) in
%   let retina_filename = Printf.sprintf "%s@2x%s" (Fpath.to_string name) ext in
     <div class="snapshotitem single">
       <figure class="single">
         <img
            src="<%s Section.url ~page sec %><%s i.filename %>"
            srcset="<%s Section.url ~page sec %><%s retina_filename %> 2x, <%s Section.url ~page sec %><%s i.filename %> 1x"
% (match (i.description) with Some desc ->
            alt="<%s desc %>"
% | None -> ());
         />
       </figure>
      </div>
% ) (Page.images page));
  </div>

let render_rss site pages =
  <rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">
  <channel>
    <title><%s Site.title site %></title>
    <link><%s Uri.to_string (Site.url site) %></link>
    <description>Recent content on my <%s Site.title site %></description>
    <generator>https://github.com/mdales/webplats/</generator>
    <language>en-us</language>
% (match pages with (_, hd) :: _ ->
    <lastBuildDate><%s Ptime.to_rfc3339 (Page.date hd) %></lastBuildDate>
% | [] -> ());
    <atom:link href="<%s Uri.to_string (Uri.with_uri ~path:(Some {|index.xml|}) (Site.url site)) %>" rel="self" type="application/rss+xml" />
% (List.iter (fun (sec, page) ->
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
      <%s Render.render_body page %>
      <%s render_page_images sec page %>
% ));


          </description>
        </item>
% ) (recent_pages pages));

    </channel>
  </rss>
