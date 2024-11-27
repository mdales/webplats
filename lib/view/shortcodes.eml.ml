
let render_video filename thumb_opt =
  <div class="video">
    <video controls
% (match thumb_opt with Some t ->
    poster="<%s t %>"
% | None -> ());
    >
    <source src="<%s! filename %>" type="video/mp4"/>
    Your browser does not support the video element.
    </video>
  </div>
  
let render_audio filename =
  <div class="listimage">
    <div>
      <audio controls>
        <source src="<%s! filename %>" type="audio/mp4"/>
        Your browser does not support the audio element.
      </audio>
    </div>
  </div>
  
let render_youtube code = 
  <div class="listimage">
    <div>
      <iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/<%s! code %>" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
    </div>
  </div>

let render_image filename alt klass =
  <div class="listimage">
    <div>
      <figure>
        <img
% (match klass with Some name ->
          class="<%s name %>"
% | None -> ());
          class="rounded"
          src="<%s filename %>"
% let name, ext = Fpath.split_ext (Fpath.v filename) in
% let retina_filename = Printf.sprintf "%s@2x%s" (Fpath.to_string name) ext in
          srcset="<%s retina_filename %> 2x, <%s filename %> 1x"
% (match (alt) with Some desc ->
          alt="<%s desc %>"
% | None -> ());       
        />
      </figure>
    </div>
  </div>

let render_photo reference =
    <p>TODO: photo link to <a href="/photos/<%s reference %>/"><%s reference %></a></p>

let render_shortcode shortcode =
  match shortcode with
  | Shortcode.Video (filename, thumb_opt) -> render_video filename thumb_opt
  | Shortcode.Audio (filename) -> render_audio filename
  | Shortcode.Youtube (code) -> render_youtube code
  | Shortcode.Image (filename, alt, klass) -> render_image filename alt klass
  | Shortcode.Photo (reference) -> render_photo reference
  | _ -> ""
