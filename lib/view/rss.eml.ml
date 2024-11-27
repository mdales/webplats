let recent_pages pages = 
	let rec loop count pages = 
		match count with
		| 0 -> []
		| _ -> (
			match pages with
			| [] -> []
			| hd :: tl -> hd :: (loop (count - 1) tl)
		)
	in 
	loop 10 pages

let render_rss site pages =
  <rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">
  <channel>
    <title><%s Site.title site %></title>
    <link>https://mynameismwd.org/</link>
    <description>Recent content on my <%s Site.title site %></description>
    <generator>https://github.com/mdales/webplats/</generator>
    <language>en-us</language>
% (match pages with hd :: _ -> 
    <lastBuildDate><%s Ptime.to_rfc3339 (Page.date hd) %></lastBuildDate>
% | [] -> ());
    <atom:link href="https://mynameismwd.org/index.xml" rel="self" type="application/rss+xml" />
% (List.iter (fun page ->
        <item>
          <title><%s Page.title page %></title>
          <link>https://mynameismwd.org<%s Page.url page %></link>
          <pubDate><%s Ptime.to_rfc3339 (Page.date page) %></pubDate>
          
          <guid>https://mynameismwd.org<%s Page.url page %></guid>
          <description>
          	Working on content in the RSS feed!
          </description>
        </item>
% ) (recent_pages pages));
    
    </channel>
  </rss>
