open Astring

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
% (match (Page.titleimage page) with Some _ ->
  <meta property="og:image" content="preview.jpg"/>
  <meta itemprop="image" content="preview.jpg"/>
  <meta property="twitter:image" content="preview.jpg"/>
% | None -> ());

let render_head_unknown site =
  <meta property="og:type" content="website"/>
  <title><%s Site.title site %></title>

let render_head ~site ?sec ?page () = 
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
  
let render_header url title = 
  <div class="header stripes">
    <header role="banner">
      <a ref="home">
        <h1><a href="/">my name is mwd</a></h1>
        <h2>the <a href="<%s url %>"><%s title %></a> of Michael Winston Dales</h2>
      </a>
    </header>
  </div>
  
let months = [| "Jan" ; "Feb" ; "Mar" ; "Apr" ; "May" ; "Jun" ; "Jul" ; "Aug" ; "Sept" ; "Oct" ; "Nov"; "Dec" |]
  
let ptime_to_str (t : Ptime.t) : string = 
  let ((year, month, day), _) = Ptime.to_date_time t in
  Printf.sprintf "%s %d, %d" months.(month - 1) day year
  
let render_footer ()   = 
  <div id="foot" class="stripes">
    <nav>
      <div id="endlinks">
        <div>
          <ul class="rsslinks">
            <li>
              <a href="/">All</a> 
              (<a href="/index.xml" type="application/rss+xml" target="_blank">RSS</a>)
            </li>
            <li>
              <a href="/posts/">Posts</a> 
              (<a href="/posts/index.xml" type="application/rss+xml" target="_blank">RSS</a>)
            </li>
            <li>
              <a href="/sounds/">Sounds</a> 
              (<a href="/sounds/index.xml" type="application/rss+xml" target="_blank">RSS</a>)
            </li>
          </ul>
        </div>
        <div>
          <ul class="rsslinks">
            <li>
              <a href="/photos/">Photos</a> 
              (<a href="/photos/index.xml" type="application/rss+xml" target="_blank">RSS</a>)
            </li>
            <li>
              <a href="/snapshots/">Snapshos</a> 
              (<a href="/snapshots/index.xml" type="application/rss+xml" target="_blank">RSS</a>)
            </li>
            <li>
              <a href="/zines/">Zines</a> 
              (<a href="/zines/index.xml" type="application/rss+xml" target="_blank">RSS</a>)
            </li>
          </ul>
        </div>
        <div>
          <ul class="rsslinks">
          	<li><a href="https://mwdales-guitars.uk">Guitar making</a></li>
          	<li><a href="https://digitalflapjack.com">Computering</a></li>
          </ul>
        </div>
        <div>
          <ul class="rsslinks">
          	<li><a href="https://toot.mynameismwd.org/@michael">Social</a></li>
          	<li><a href="/about/">About</a></li>
          	<li><a href="/search/">Search</a></li>
          </ul>
        </div>
      </div>
    </nav>
  </div>

let render_section site sec =
  <html>
  <%s! (render_head ~site ~sec ()) %>
  <body>
    <div class="almostall">
      <%s! render_header (Section.url sec) (Section.title sec) %>
    
    <ul>
% (Section.pages sec) |> List.iter begin fun (page) ->
      <li>
      <a href="<%s Section.url ~page sec %>/">
          <%s Page.title page %>
        </a>
      </li>
% end;
    
    </ul>
    
    <%s! render_footer () %>
  </body>
  </html>

let cmark_to_html : strict:bool -> safe:bool -> string -> string =
fun ~strict ~safe md ->
  let doc = Cmarkit.Doc.of_string ~strict md in
  Cmarkit_html.of_doc ~safe doc
  
let render_body page = 
  let unrendered_markdown = Page.body page in
  let ordered_shortcodes = List.sort (fun ((a, _), _) ((b, _), _) -> b - a) (Page.shortcodes page) in
  let body = List.fold_left (fun body ((loc, len), shortcode) ->
    let rendered_shortcode = Shortcodes.render_shortcode shortcode in
    let before = String.with_index_range ~last:(loc - 1) body
    and after = String.with_index_range ~first:(loc + len) body in
    before ^ rendered_shortcode ^ after
  ) unrendered_markdown ordered_shortcodes in
  cmark_to_html ~strict:false ~safe:false body 

let render_error site _error _debug_info suggested_response =
  let status = Dream.status suggested_response in
  let code = Dream.status_to_int status
  and reason = Dream.status_to_string status in

  Dream.set_header suggested_response "Content-Type" Dream.text_html;
  Dream.set_body suggested_response begin
    <html>
    <%s! (render_head ~site ()) %>
    <body>
      <div class="almostall">
        <%s! render_header (Section.url (Site.toplevel site)) (Section.title (Site.toplevel site)) %>
        <div id="container">
          <div class="content">
            <section role="main">
              <div class="article">
                <article>
                  <h2><%i code %> <%s reason %></h2>
                </article>
              </div>
            </section>
          </div>
        </div>
        <%s! render_footer () %>   
      </div>
    </body>
    </html>
  end;
  Lwt.return suggested_response

let render_page site sec previous_page page next_page =
  <html>
  <%s! (render_head ~site ~sec ~page ()) %>
  <body>
    <div class="almostall">
      <%s! render_header (Section.url sec) (Section.title sec) %>
      <div id="container">
        <div class="content">
          <section role="main">
            <div class="article">
              <article>
                <div class="headerflex">
                  <div class="headerflextitle">
                    <h3><%s Page.title page %></h3>
                  </div>
                  <div class="headerflexmeta">
                    <p><%s ptime_to_str (Page.date page) %></p>
                  </div>
                </div>
                <%s! render_body page %>
                
                <div class="postscript">
                  <ul>
% (match previous_page with Some page -> 
                    <li><strong>Next</strong>: <a href="<%s Section.url ~page sec %>"><%s Page.title page %></a></li>
% | None -> ());
% (match next_page with Some page -> 
                    <li><strong>Previous</strong>: <a href="<%s Section.url ~page sec %>"><%s Page.title page %></a></li>
% | None -> ());
                  </ul>
                </div>
                
              </article>
            </div>
          </section>
        </div>
      </div>
      <%s! render_footer () %>   
    </div>
  </body>
  </html>
  
let render_taxonomy site taxonomy =
  <html>
  <%s! (render_head ~site ()) %>
  <body>
    <div class="almostall">
      <%s! render_header (Taxonomy.url taxonomy) (Taxonomy.title taxonomy) %>
    
    <ul>
% (Taxonomy.sections taxonomy) |> List.iter begin fun (sec) ->
      <li>
      <a href="<%s Section.url sec %>">
          <%s Section.title sec %>
        </a> - <%d List.length (Section.pages sec) %> items
      </li>
% end;
    
    </ul>
    
    <%s! render_footer () %>
  </body>
  </html>