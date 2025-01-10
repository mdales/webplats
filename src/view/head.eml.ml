
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
  <link href="<%s Section.url sec %>index.xml" rel="alternate" type="application/rss+xml" title="<%s Site.title site %>: <%s Section.title sec %>" />
  <link href="<%s Section.url sec %>index.xml" rel="feed" type="application/rss+xml" title="<%s Site.title site %>: <%s Section.title sec %>" />
% ) (Site.sections site));
  <meta property="og:site_name" content="<%s Site.title site %>"/>

let render_head_section sec = 
  <meta property="og:type" content="website"/>
  <title><%s Section.title sec %></title>
  <meta property="og:title" content="<%s Section.title sec %>"/>
  <meta itemprop="name" content="<%s Section.title sec %>"/>
  <meta property="twitter:title" content="<%s Section.title sec %>"/>
  <meta property="og:url" content="<%s Section.url sec %>"/>
  <meta itemprop="url" content="<%s Section.url sec %>"/>
  <meta property="twitter:url" content="<%s Section.url sec %>"/>

let render_head_page sec page = 
  <meta property="og:type" content="article"/>
  <title><%s Page.title page %></title>
  <meta property="og:title" content="<%s Page.title page %>"/>
  <meta itemprop="name" content="<%s Page.title page %>"/>
  <meta property="twitter:title" content="<%s Page.title page %>"/>
  <meta property="og:url" content="<%s Section.url ~page sec %>"/>
  <meta itemprop="url" content="<%s Section.url ~page sec %>"/>
  <meta property="twitter:url" content="<%s Section.url ~page sec %>"/>
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

let render_head_unknown site =
  <meta property="og:type" content="website"/>
  <title><%s Site.title site %></title>

let render_head ~site ~sec ~page () =
  <head>
    <%s! render_head_generic site %>
% (match page with Some p ->
    <%s! render_head_page (Option.get sec) p %>
% | None -> (
% match sec with Some s ->
    <%s! render_head_section s %>
% | None -> (
    <%s! render_head_unknown site %>
% )));
  </head>