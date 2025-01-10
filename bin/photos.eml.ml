open Webplats

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
        Printf.sprintf " with a %s %s lens" lensmake info
      )
    in
    Printf.sprintf "%s%s" camera lens
  )

let render_header title = 
  <div class="miniheader">
    <div class="miniheadertitle">
      <header role="banner">
        <a ref="home">
          <h1>my name is mwd: <%s title %></h1>
        </a>
      </header>
    </div>
    <div class="miniheadernav">
      <ul>
        <li><a href="/photos/">Latest</a></li>
        <li><a href="/albums/">Albums</a></li>
      </ul>
    </div>
  </div>

let render_section site sec = 
  <html>
  <%s! (Render.render_head ~site ~sec ()) %>
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
  
 
let render_page site sec previous_page page next_page =
  <html>
  <%s! (Render.render_head ~site ~sec ~page ()) %>
  <body>
    <div class="almostall">
      <%s! render_header (Section.title sec) %>
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
                  <%s! Render.render_body page %>
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
% let albums = Page.get_key_as_string_list page "albums" in
% (match albums with _hd :: _tl ->
                <br/>Appears in:<br/>
% | [] -> ());
% albums |> List.iter begin fun (album) ->
                <a href="/albums/<%s String.lowercase_ascii album |> String.map (fun c -> match c with ' ' -> '-' | x -> x) %>/">• <%s album %></a><br/>
% end; 
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

let rec take n l = 
  match n with
  | 0 -> []
  | _ -> (
    match l with 
    | [] -> []
    | hd :: tl -> hd :: (take (n - 1) tl)
  )

let render_taxonomy site taxonomy =
  <html>
    <%s! (Render.render_head ~site ()) %>
    <body>
      <div class="almostall">
        <%s! render_header (Taxonomy.title taxonomy) %>
        <div id="container">
          <div class="gallery">  
% (Taxonomy.sections taxonomy) |> List.iter begin fun (sec) ->
            <div class="galleryitem gallerylandscape album">
              <a href="<%s Section.url sec %>">
                <div class="albumstack">
% (Section.pages sec) |> take 3 |> List.iter begin fun (page) ->
% let i = Option.get (Page.titleimage page) in
% (match (i.dimensions) with Some (width, height) ->
% let layout = match width > height with true -> "Y" | false -> "X" in
% let rot = Random.int_in_range ~min:(-5) ~max:5 in
% let shift = Random.int_in_range ~min:(-7) ~max:7 in
% let name, ext = Fpath.split_ext (Fpath.v i.filename) in
% let retina_filename = Printf.sprintf "%s@2x%s" (Fpath.to_string name) ext in
                  <div 
                    class="galleryimage"
                    style="transform: rotate(<%d rot %>deg) translate<%s layout %>(<%d shift %>px);"
                  >
                    <img
                      loading="lazy"
                      src="<%s Page.original_section_url page %><%s Page.url_name page %>/album_<%s i.filename %>"
                      srcset="<%s Page.original_section_url page %><%s Page.url_name page %>/album_<%s retina_filename %> 2x, <%s Page.original_section_url page %><%s Page.url_name page %>/album_<%s i.filename %> 1x"
% (match (i.description) with Some desc ->
                      alt="<%s desc %>"
% | None -> ());
                    />
                  </div>
% | None -> ());
% end;
                </div>
              </a>
              <div class="gallerycard albumcard">
                <p><a href="<%s Section.url sec %>"><%s Section.title sec %></a><br><%d List.length (Section.pages sec) %> photos</p>
              </div>
            </div>
% end;
          </div>
        </div>
      </div>
    </body>
  </html>
