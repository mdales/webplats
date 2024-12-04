
let months = [| "January" ; "Febuary" ; "March" ; "April" ; "May" ; "June" ; "July" ; "August" ; "September" ; "October" ; "November"; "December" |]


let ptime_to_str (t : Ptime.t) : string = 
  let ((year, month, day), _) = Ptime.to_date_time t in
  Printf.sprintf "%d %s %d"day  months.(month - 1) year
  

let fit_dimensions max_width max_height width height =
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

let location_info page =
  let city = match (Page.get_key_as_string page "City") with
  | None -> ""
  | Some s -> Printf.sprintf "%s, " s
  in
  let country = match (Page.get_key_as_string page "Country") with Some s -> s | None -> "" in
  match country with 
  | "" | "United States" | "United States of America" -> Printf.sprintf "%s%s<br/>" city (match (Page.get_key_as_string page "State") with Some x -> x | None -> "")
  | _ -> Printf.sprintf "%s%s<br/>" city country


let camera_info page = 
  match (Page.get_key_as_string page "Make") with
  | None -> ""
  | Some make -> (
    let model = match (Page.get_key_as_string page "Model") with
      | None -> ""
      | Some "ILCE-7RM2" -> "A7RII"
      | Some model -> model
    in
    let camera = Printf.sprintf "%s %s" make model in
    let lens = match (Page.get_key_as_string page "LensInfo") with
      | None -> ""
      | Some info -> (
        let lensmake = match (Page.get_key_as_string page "LensMake") with None -> "" | Some m -> m in
        Printf.sprintf "with a %s %s lens" lensmake info
      )
    in
    Printf.sprintf "%s%s" camera lens
  )

let render_header sec = 
  <div class="miniheader">
    <div class="miniheadertitle">
      <header role="banner">
        <a ref="home">
          <h1>my name is mwd: <%s Section.title sec %></h1>
        </a>
      </header>
    </div>
  </div>

let render_section sec = 
  <html>
  <%s! (Renderer.render_head (Section.title sec)) %>
  <body>
    <div class="almostall">
      <%s! render_header sec %>
      
      <div id="container">
        <div class="gallery">
         
% (Section.pages sec) |> List.iter begin fun (page) ->
% let i = Option.get (Page.titleimage page) in
          <article>
            <div class="galleryitem gallerylandscape">
              <div class="galleryimage">
                <a href="<%s Section.url ~page sec %>">
                  <img
                    loading="lazy"
                    src="<%s Section.url ~page sec %>thumbnail.jpg"
                    srcset="<%s Section.url ~page sec %>thumbnail@2x.jpg 2x, <%s Section.url ~page sec %>thumbnail.jpg 1x"
                    title="<%s Page.title page %>"
% (match (i.dimensions) with Some (width, height) ->
% let width, height = fit_dimensions 640 350 width height in
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
% let width, _ = fit_dimensions 640 350 width height in
% let adjusted_width = width + 40 in
                style="width: <%d adjusted_width %>px;"
% | None -> ());
              >
                <a href="<%s Section.url ~page sec %>" class="title"><%s Page.title page %></a><br/><br/>
                <div class="gallerycardinner">
                  <div>
                    <%s! location_info page %>                    
% let date = match (Page.get_key_as_date page "Taken") with Some d -> d | None -> (Page.date page) in
                <%s ptime_to_str date %><br/>
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
  
 
let render_page sec previous_page page next_page =
  <%s! (Renderer.render_head (Page.title page)) %>
  <body>
    <div class="almostall">
      <%s! render_header sec %>
      <div id="container">
       <div class="article galoveride">
       <article>
        <div class="gallery singlegallery">
% let i = Option.get (Page.titleimage page) in
% (match (i.dimensions) with Some (width, height) ->
% let name, ext = Fpath.split_ext (Fpath.v i.filename) in
% let retina_filename = Printf.sprintf "%s@2x%s" (Fpath.to_string name) ext in
% let width, height = fit_dimensions 1008 800 width height in
% let layout = match width > height with true -> "landscape" | false -> "portrait" in
% let adjusted_width = width + 40 in
          <div class="galleryitem gallery<%s layout %>">
            <div class="galleryimage">
               <img
                  loading="lazy"
                  src="<%s Section.url ~page sec %>scrn_<%s i.filename %>"
                  srcset="<%s Section.url sec ~page %>scrn_<%s retina_filename %> 2x, <%s Section.url ~page sec %>scrn_<%s i.filename %> 1x"
                  title="<%s Page.title page %>"
                  width="<%d width %>"
                  height="<%d height %>"
% (match (i.description) with Some desc ->
                  alt="<%s desc %>"
% | None -> ());
                />
            </div>
            <div class="gallerycard gallerycard-<%s layout %>"
% (match layout with "landscape" ->
                style="width: <%d adjusted_width %>px;"
% | _ -> ());
            >
              <a href="<%s Section.url ~page sec %>" class="title"><%s Page.title page %></a><br/><br/>
              <div class="gallerycardinner">
                <div class="gallerycardcontent">
                  <%s! Renderer.render_body page %>
                </div>
              </div>
              <div>
                <%s! location_info page %>
% let date = match (Page.get_key_as_date page "Taken") with Some d -> d | None -> (Page.date page) in
                <%s ptime_to_str date %><br/>
                <%s camera_info page %><br/>
% (match (Page.get_key_as_string page "Caption") with Some caption ->
                <%s caption %> film <br/>
% | None -> ());
                <a href="https://creativecommons.org/licenses/by-nc/4.0/">License CC BY-NC</a>
                - <a href="<%s i.filename %>" download>Download</a><br/>
              </div>              
              <div class="photo">
                <div class="headerflex">
% (match previous_page with Some page -> 
                    <a class="prev" href="<%s Section.url ~page sec %>">&#10094;</a>
% | None -> ());
% (match next_page with Some page -> 
                    <a class="next" href="<%s Section.url ~page sec %>">&#10095;</a>
% | None -> ());
              </div>
             </div>
            </div>
          </div>
% | None -> ());
        </div>
       </article>
       </div>
      </div>  
    </div>
  </body>
  </html>
  