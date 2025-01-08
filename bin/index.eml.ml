open Webplats

let months = [| "Jan" ; "Feb" ; "Mar" ; "Apr" ; "May" ; "Jun" ; "Jul" ; "Aug" ; "Sept" ; "Oct" ; "Nov"; "Dec" |]

let ptime_to_str (t : Ptime.t) : string = 
  let ((year, month, day), _) = Ptime.to_date_time t in
  Printf.sprintf "%d %s %d" day months.(month - 1) year
  
let render_index site =
  <html>
  <%s! Renderer.render_head ~site () %>
  <body>
    <div class="almostall">
      <%s! Renderer.render_header (Section.url (Site.toplevel site)) (Section.title (Site.toplevel site)) %>
      
      <div id="container">
        <div class="content">
          <section role="main">
            <div class="article">
              <h2>My things here</h2>
                <div class="index">
% (Site.sections site) |> List.filter (fun s -> not (Section.synthetic s)) |> List.iter begin fun (sec) ->
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
                      <div>
                          <a href="https://mwdales-guitars.uk/">
                              <div class="homebutton colour-EF">
                                  <h3>M. W. Dales Guitars</h3>
                                  <p>Guitar building, designing, 3D-printing, laser-cutting</p>
                              </div>
                          </a>
                      </div>
                      <div>
                          <a href="https://digitalflapjack.com/">
                              <div class="homebutton colour-DF">
                                  <h3>Digital Flapjack</h3>
                                  <p>Programming mostly, with a bit of design and planning too</p>
                              </div>
                          </a>
                      </div>
                  </div>
            </div>
          </section>
        </div>
      </div>  
    <%s! Renderer.render_footer () %>    
    </div>
  </body>
  </html>