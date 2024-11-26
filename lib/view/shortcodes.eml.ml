
let render_video filename thumb_opt =
  <div class="video">
    <video controls
% (match thumb_opt with Some t ->
    poster="<%s t %>"
% | None -> ());
    >
    <source src="<%s filename %>" type="video/mp4"/>
    Your browser does not support the video element.
    </video>
  </div>
  
let render_audio filename =
  <div class="listimage">
    <div>
      <audio controls>
        <source src="<%s filename %>" type="audio/mp4"/>
        Your browser does not support the audio element.
      </audio>
    </div>
  </div>
  
let render_youtube code = 
  <div class="listimage">
    <div>
      <iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/<%s code %>" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
    </div>
  </div>⏎ 

let render_shortcode shortcode =
  match shortcode with
  | Page.Video (filename, thumb_opt) -> render_video filename thumb_opt
  | Page.Audio (filename) -> render_audio filename
  | Page.Youtube (code) -> render_youtube code
  | _ -> ""
