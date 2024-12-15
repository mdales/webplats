  
let render_section sec = 
  <html>
  <%s! (Renderer.render_head (Section.title sec)) %>
  <body>
    <div class="almostall">
      <%s! Renderer.render_header (Section.url sec) (Section.title sec) %>
      
      <div id="container">
        <div class="content">
          <section role="main">
            <div class="tagcontents">
              <h2>Snapshots</h2>
              <div class="tagcellholder">
    
% (Section.pages sec) |> List.iter begin fun (page) ->
                <div class="tagcell colour-<%s Page.original_section_title page %>">
                  <div class="tagcelllabel">
                    <span><%s Page.title page %></span>
                  </div>
                  <div class="tagcellinner">
                    <a href="<%s Section.url ~page sec %>">
                      <div class="tagcellimg">
                        <figure>
                          <img
                            loading="lazy"
% (match (Page.titleimage page) with Some i ->
                            src="<%s Section.url ~page sec %>thumbnail.jpg"
                            srcset="<%s Section.url ~page sec %>thumbnail@2x.jpg 2x, <%s Section.url ~page sec %>thumbnail.jpg 1x"
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
          </section>
        </div>
      </div>
      <%s! Renderer.render_footer () %>      
    </div>
  </body>
  </html>


let render_page sec previous_page page next_page =
  <html>
  <%s! (Renderer.render_head (Page.title page)) %>
  <body>
    <div class="almostall">
      <%s! Renderer.render_header (Section.url sec) (Section.title sec) %>
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
                    <p><%s Renderer.ptime_to_str (Page.date page) %></p>
                  </div>
                </div>
                <%s! Renderer.render_body page %>
                <div class="snapshotlist">
% List.iter (fun (i : Frontmatter.image) ->
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
                     <div class="holder holder-top-left"></div>
                     <div class="holder holder-top-right"></div>
                     <div class="holder holder-bottom-left"></div>
                     <div class="holder holder-bottom-right"></div>
                   </div>
% ) (Page.images page);
                </div>
                
                <div class="postscript">
                  <ul>
% (match previous_page with Some page -> 
                    <li><strong>Next</strong>: <a href="<%s Section.url sec ~page %>/"><%s Page.title page %></a></li>
% | None -> ());
% (match next_page with Some page -> 
                    <li><strong>Previous</strong>: <a href="<%s Section.url ~page sec %>/"><%s Page.title page %></a></li>
% | None -> ());
                  </ul>
                </div>
                
              </article>
            </div>
          </section>
        </div>
      </div>
      <%s! Renderer.render_footer () %>   
    </div>
  </body>
  </html>
  