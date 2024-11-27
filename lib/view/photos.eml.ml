
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
  
let fit_dimensions width height =
  let max_width = 640
  and max_height = 350 in
  let fwidth = float_of_int width
  and fheight = float_of_int height in
  let wratio = (float_of_int max_width) /. fwidth
  and hratio = (float_of_int max_height) /. fheight in
  match (wratio >= 1.0) && (hratio >= 1.0) with
  | true -> (width, height)
  | false -> (
    let ratio = min wratio hratio in
    let newwidth = int_of_float (ratio *. fwidth)
    and newheight = int_of_float (ratio *. fheight) in
    (newwidth, newheight)
  )

let render_section sec = 
  <html>
  <%s! (Renderer.render_head (Section.title sec)) %>
  <body>
    <div class="almostall">
      <%s! render_header (Section.title sec) %>
      
      <div id="container">
        <div class="gallery">
         
% (Section.pages sec) |> List.iter begin fun (page) ->
% let i = Option.get (Page.titleimage page) in
          <article>
            <div class="galleryitem gallerylandscape">
              <div class="galleryimage">
                <a href="<%s Page.url page %>">
                  <img
                    loading="lazy"
                    src="<%s Page.url page %>thumbnail.jpg"
                    srcset="<%s Page.url page %>thumbnail@2x.jpg 2x, <%s Page.url page %>thumbnail.jpg 1x"
                    title="<%s Page.title page %>"
% (match (i.dimensions) with Some (width, height) ->
% let width, height = fit_dimensions width height in
                    width="<%d width %>"
                    height="<%d height %>"
% | None -> ());
% (match (i.description) with Some desc ->
                    alt="<%s desc %>"
% | None -> ());
                  />
                </a>  
              </div>
              <div class="gallerycard gallerycard-landscape"
% (match (i.dimensions) with Some (width, height) ->
% let width, _ = fit_dimensions width height in
% let adjusted_width = width + 40 in
                style="width: <%d adjusted_width %>px;"
% | None -> ());
              >
                <a href="<%s Page.url page %>" class="title"><%s Page.title page %></a><br/><br/>
                <div class="gallerycardinner">
                  <div>
                    <%s Renderer.ptime_to_str (Page.date page) %><br/>
                  </div>
                </div>
              </div>
            </div>
          </article>
% end;  
        </div>
      </div>      
    </div>
  </body>
  </html>