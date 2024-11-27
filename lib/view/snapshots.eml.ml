
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
      | _ -> failwith "unexpcted image format"
      in
      let resized = Rgb24.resize None rgb newwidth newheight in
      let cropped = Rgb24.sub resized ((newwidth - thumbnail_size) / 2) ((newheight - thumbnail_size) / 2) thumbnail_size thumbnail_size in
      let resultimg = Images.Rgb24 cropped in
      Images.save targetname None [] resultimg;
      targetname
    )
   )

let render_image_fit page (image : Page.image) (max_width, max_height) =
  let imgpath = Fpath.add_seg (Page.path page) image.filename in
    let targetname = Printf.sprintf "/tmp/image_%dx%d_%s" max_width max_height image.filename  in
    match Sys.file_exists targetname with
    | true -> targetname
    | false -> (
      let img = Images.load (Fpath.to_string imgpath) [] in
      let width, height = Images.size img in
      let fwidth = float_of_int width
      and fheight = float_of_int height in
      let wratio = (float_of_int max_width) /. fwidth
      and hratio = (float_of_int max_height) /. fheight in
      match (wratio >= 1.0) && (hratio >= 1.0) with
      | true -> (Fpath.to_string imgpath)
      | false -> (
        let ratio = min wratio hratio in
        let newwidth = int_of_float (ratio *. fwidth)
        and newheight = int_of_float (ratio *. fheight) in        
        let rgb = match img with
        | Rgb24 a -> a
        | _ -> failwith "unexpcted image format"
        in
        let resized = Rgb24.resize None rgb newwidth newheight in
        let resultimg = Images.Rgb24 resized in
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
      <%s! Renderer.render_footer () %>      
    </div>
  </body>
  </html>


let render_page sec page = 
  <%s! (Renderer.render_head (Page.title page)) %>
  <body>
    <div class="almostall">
      <%s! Renderer.render_header (Section.title sec) %>
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
% List.iter (fun (i : Page.image) ->
%   let name, ext = Fpath.split_ext (Fpath.v i.filename) in
%   let retina_filename = Printf.sprintf "%s@2x%s" (Fpath.to_string name) ext in
                   <div class="snapshotitem single">
                     <figure class="single">
                       <img
                          src="<%s Page.url page %><%s i.filename %>"
                          srcset="<%s Page.url page %><%s retina_filename %> 2x, <%s Page.url page %><%s i.filename %> 1x"
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
              </article>
            </div>
          </section>
        </div>
      </div>
      <%s! Renderer.render_footer () %>   
    </div>
  </body>
  </html>
  