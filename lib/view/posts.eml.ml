

let months = [| "Jan" ; "Feb" ; "Mar" ; "Apr" ; "May" ; "Jun" ; "Jul" ; "Aug" ; "Sep" ; "Oct" ; "Nov"; "Dec" |]
  
let ptime_to_str (t : Ptime.t) : string = 
  let ((year, month, day), _) = Ptime.to_date_time t in
  Printf.sprintf "%d %s %d" day months.(month - 1) year
  
let render_section sec = 
  <html>
  <%s! (Renderer.render_head (Section.title sec)) %>
  <body>
    <div class="almostall">
      <%s! Renderer.render_header (Section.title sec) %>
      
      <div id="container">
        <div class="content">
          <section role="main">
            <div class="blogcontents">
% (Section.pages sec) |> List.iter begin fun (page) ->
            <div class="blogcontents__item">
              <ul class="leaders">
                  <li>
                    <span><a href="<%s Page.url page %>"><%s Page.title page %></a></span>
                    <span><%s ptime_to_str (Page.date page) %></span>
                  </li>
              </ul>
              <div class="blogcontents__item__inner">
                <div>
                    <p><%s (match (Page.synopsis page) with None -> "" | Some p -> p) %></p>
                </div>
              </div>
            </div>
% end;  
              
            </div>
          </section>
        </div>
      </div>      
    </div>
    <%s! Renderer.render_footer () %>
  </body>
  </html>