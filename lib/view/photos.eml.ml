


let render_header title = 
  <div class="miniheader">
    <div class="miniheadertitle">
      <header role="banner">
        <a ref="home">
          <h1>my name is mwd: <%s title %></h1>
        </a>
      </header>
    </div>
  </div>

let render_section sec = 
  <html>
  <%s! (Renderer.render_head (Section.title sec)) %>
  <body>
    <div class="almostall">
      <%s! render_header (Section.title sec) %>
      
      <div id="container">
        <div class="gallery">
         
% (Section.pages sec) |> List.iter begin fun (page) ->
                <div class="tagcell colour-<%s Section.title sec %>">
                  <div class="tagcelllabel">
                    <span><%s Page.title page %></span>
                  </div>
                  <div class="tagcellinner">
                    <a href="<%s Page.url page %>">
                      <div class="tagcellimg">
                        <figure>
                          <img
                            loading="lazy"
% (match (Page.titleimage page) with Some i ->
                            src="<%s Page.url page %>thumbnail.jpg"
                            srcset="<%s Page.url page %>thumbnail@2x.jpg 2x, <%s Page.url page %>thumbnail.jpg 1x"
% (match (i.description) with Some desc ->
                            alt="<%s desc %>"
% | None -> ());
                            
% | None -> ());
                          />
                        </figure>
                      </div>
                    </a>                  
                  </div>
                </div>
% end;  
        </div>
      </div>      
    </div>
    <%s! Renderer.render_footer () %>
  </body>
  </html>