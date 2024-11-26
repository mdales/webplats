open Astring

let render_head title = 
  <head>
  <link rel="stylesheet" href="/css/base.min.1baad36ee4ee027cab80c240d7d6f3bd25a6b673801984ba90cb19fd0c96c0c35914a8be655c092babd943d87ce43c99874d9415f746837ae01492338478ab7b.css" integrity="sha512-G6rTbuTuAnyrgMJA19bzvSWmtnOAGYS6kMsZ/QyWwMNZFKi+ZVwJK6vZQ9h85DyZh02UFfdGg3rgFJIzhHirew==" type="text/css" media="screen">
  <title><%s title %></title>
  </head>
  
let render_header title = 
  <div class="header stripes">
    <header role="banner">
      <a ref="home">
        <h1>my name is mwd</h1>
        <h2>the <%s title %> of Michael Winston Dales</h2>
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

let render_section sec =
  <html>
  <%s! (render_head (Section.title sec)) %>
  <body>
    <div class="almostall">
      <%s! render_header (Section.title sec) %>
    
    <ul>
% (Section.pages sec) |> List.iter begin fun (page) ->
      <li>
      <a href="<%s Page.url page %>">
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

let render_page sec page =
  <%s! (render_head (Page.title page)) %>
  <body>
    <div class="almostall">
      <%s! render_header (Section.title sec) %>
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
              </article>
            </div>
          </section>
        </div>
      </div>
      <%s! render_footer () %>   
    </div>
  </body>
  </html>