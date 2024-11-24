
let render_thumbnail page thumbnail_size = 
  match (Page.titleimage page) with
  | None -> failwith "blah"
  | Some titleimg -> (
    let imgpath = Fpath.add_seg (Page.path page) titleimg.filename in
    let targetname = Printf.sprintf "/tmp/thumbnail_%d_%s" thumbnail_size titleimg.filename  in
    match Sys.file_exists targetname with
    | true -> targetname
    | false -> (
      let img = Images.load (Fpath.to_string imgpath) [] in
      let width, height = Images.size img in
      let newwidth, newheight = match (width > height) with
      | true -> (((thumbnail_size * width) / height), thumbnail_size)
      | false -> (thumbnail_size, ((thumbnail_size * height) / width))
      in   
      let rgb = match img with
      | Rgb24 a -> a
      | _ -> failwith "unexpcted imagae format"
      in
      let resized = Rgb24.resize None rgb newwidth newheight in
      let cropped = Rgb24.sub resized ((newwidth - thumbnail_size) / 2) ((newheight - thumbnail_size) / 2) thumbnail_size thumbnail_size in
      let resultimg = Images.Rgb24 cropped in
      Images.save targetname None [] resultimg;
      targetname
    )
   )
  

let render_section sec = 
  <html>
  <%s! (Renderer.render_head (Section.title sec)) %>
  <body>
    <div class="almostall">
      <%s! Renderer.render_header (Section.title sec) %>
      
      <div id="container">
        <div class="content">
          <section role="main">
            <div class="tagcontents">
              <h2>Snapshots</h2>
              <div class="tagcellholder">
    
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
          </section>
        </div>
      </div>      
    </div>
    <%s! Renderer.render_footer () %>
  </body>
  </html>
