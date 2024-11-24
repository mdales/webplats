
let render_head title = 
  <head>
  <link rel="stylesheet" href="/css/base.min.1baad36ee4ee027cab80c240d7d6f3bd25a6b673801984ba90cb19fd0c96c0c35914a8be655c092babd943d87ce43c99874d9415f746837ae01492338478ab7b.css" integrity="sha512-G6rTbuTuAnyrgMJA19bzvSWmtnOAGYS6kMsZ/QyWwMNZFKi+ZVwJK6vZQ9h85DyZh02UFfdGg3rgFJIzhHirew==" type="text/css" media="screen">
  <title><%s title %></title>
  </head>
  
let render_header title = 
  <div class="header stripes">
    <header role="banner">
      <a ref="home">
        <h1><%s title %></h1>
        <h2>the website of Michael Winston Dales</h2>
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
      </div>
    </nav>
  </div>

let render_index site =
  <html>
  <%s! (render_head (Site.title site)) %>
  <body>
    <div class="almostall">
      <%s! render_header (Site.title site) %>
      
      <div id="container">
        <div class="content">
          <section role="main">
            <div class="article">
              <h2>My things here</h2>
                <div class="index">
% (Site.sections site) |> List.iter begin fun (sec) ->
                  <div>
                    <a href="<%s Section.url sec %>">
                      <div class="homebutton colour-<%s Section.title sec %>">
                        <h3><%s Section.title sec %></h3>
                        <p>
                          <%d List.length (Section.pages sec) %> <%s Section.title sec %>,
                          last updated <%s ptime_to_str (Page.date (List.hd (Section.pages sec))) %></p>
                      </div>
                    </a>
                  </div>
% end;
                </div>
              <h2>My other sites</h2>
                <div class="index">
                </div>
            </div>
          </section>
        </div>
      </div>      
    </div>
    <%s! render_footer () %>
  </body>
  </html>

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


let render_page page =
  <html>
  <head>
  <link rel="stylesheet" href="/css/base.min.1baad36ee4ee027cab80c240d7d6f3bd25a6b673801984ba90cb19fd0c96c0c35914a8be655c092babd943d87ce43c99874d9415f746837ae01492338478ab7b.css" integrity="sha512-G6rTbuTuAnyrgMJA19bzvSWmtnOAGYS6kMsZ/QyWwMNZFKi+ZVwJK6vZQ9h85DyZh02UFfdGg3rgFJIzhHirew==" type="text/css" media="screen">
  <title><%s Page.title page %></title>
  </head>
  <body>
    <h1><%s Page.title page %></h1>
  </body>
  </html>