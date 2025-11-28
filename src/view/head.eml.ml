
let render_head_generic site =
  <link rel="stylesheet" href="/css/base.css" type="text/css" media="screen">
  <link rel="icon" href="/img/favicon-32.png" sizes="32x32">
  <link rel="icon" href="/img/favicon-57.png" sizes="57x57">
  <link rel="icon" href="/img/favicon-180.png" sizes="180x180">
  <link rel="icon" href="/img/favicon-192.png" sizes="192x192">
  <link rel="apple-touch-icon" href="/img/favicon-57.png" sizes="57x57">
  <link rel="apple-touch-icon" href="/img/favicon-180.png" sizes="180x180">
  <link rel="me" href="https://mastodon.me.uk/@mdales">
  <link rel="me" href="https://toot.mynameismwd.org/@michael">
% (List.iter (fun sec ->
  <link href="<%s Uri.to_string (Section.uri ~resource:{|index.xml|} sec) %>" rel="alternate" type="application/rss+xml" title="<%s Site.title site %>: <%s Section.title sec %>" />
  <link href="<%s Uri.to_string (Section.uri ~resource:{|index.xml|} sec) %>" rel="feed" type="application/rss+xml" title="<%s Site.title site %>: <%s Section.title sec %>" />
  <link rel="alternate" type="application/feed+json" title="JSON Feed" href="<%s Uri.to_string (Section.uri ~resource:{|feed.json|} sec) %>">
% ) (Site.sections site));
% (match (Site.author site) with Some name ->
  <meta name="author" content="<%s name %>">
% | None -> ());
  <meta property="og:site_name" content="<%s Site.title site %>"/>

let render_head_section sec =
  <meta property="og:type" content="website"/>
  <title><%s Section.title sec %></title>
  <meta property="og:title" content="<%s Section.title sec %>"/>
  <meta itemprop="name" content="<%s Section.title sec %>"/>
  <meta property="twitter:title" content="<%s Section.title sec %>"/>
  <meta property="og:url" content="<%s Uri.to_string (Section.uri sec) %>"/>
  <meta itemprop="url" content="<%s Uri.to_string (Section.uri sec) %>"/>
  <meta property="twitter:url" content="<%s Uri.to_string (Section.uri sec) %>"/>

let render_head_page sec page =
  <meta property="og:type" content="article"/>
  <title><%s Page.title page %></title>
  <meta property="og:title" content="<%s Page.title page %>"/>
  <meta itemprop="name" content="<%s Page.title page %>"/>
  <meta property="twitter:title" content="<%s Page.title page %>"/>
  <meta property="og:url" content="<%s Uri.to_string (Section.uri ~page sec) %>"/>
  <meta itemprop="url" content="<%s Uri.to_string (Section.uri ~page sec) %>"/>
  <meta property="twitter:url" content="<%s Uri.to_string (Section.uri ~page sec) %>"/>
% (match (Page.synopsis page) with Some syn ->
  <meta property="og:description" content="<%s syn %>"/>
  <meta itemprop="description" content="<%s syn %>"/>
  <meta property="twitter:description" content="<%s syn %>"/>
% | None -> ());
% (match (Page.titleimage page) with Some i ->
  <meta property="og:image" content="preview.jpg"/>
  <meta itemprop="image" content="preview.jpg"/>
  <meta property="twitter:image" content="preview.jpg"/>
% (match i.description with Some desc ->
  <meta property="og:image:alt" content="<%s desc %>"/>
  <meta property="image:alt" content="<%s desc %>"/>
  <meta property="twitter:image:alt" content="<%s desc %>"/>
% | None -> ());
% | None -> ());
% List.iter (fun filename ->
  <script type="module" src="<%s filename %>"></script>
% ) (Page.scripts page);

let render_head_unknown site =
  <meta property="og:type" content="website"/>
  <title><%s Site.title site %></title>

let render_head ~site ~sec ~page () =
  <head>
    <%s! render_head_generic site %>
% (match page with Some p ->
    <%s! render_head_page (Option.get sec) p %>
% (match (Page.has_chart p) with true ->
    <script src="https://cdn.jsdelivr.net/npm/vega@6"></script>
    <script src="https://cdn.jsdelivr.net/npm/vega-lite@6"></script>
    <script src ="https://cdn.jsdelivr.net/npm/vega-embed@7"></script>
% | false -> ());
% | None -> (
% match sec with Some s ->
    <%s! render_head_section s %>
% | None -> (
    <%s! render_head_unknown site %>
% )));
  </head>
