
let render_video filename thumb_opt looped =
  <div class="video">
    <video controls
% (match looped with true ->
    loop
% | false -> ());
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

let is_image_retina dims =
  match dims with
  | None -> true
  | Some (width, height) -> (
    (width > (800 * 2)) && (height > (600 * 2))
  )

let render_raster filename alt klass dims =
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
% (match (is_image_retina dims) with true ->
% let retina_filename = Printf.sprintf "%s@2x%s" (Fpath.to_string name) ext in
          srcset="<%s retina_filename %> 2x, <%s filename %> 1x"
% | false -> ());
% (match (alt) with Some desc ->
          alt="<%s desc %>"
% | None -> ());
        />
      </figure>
    </div>
  </div>

let render_vector filename alt klass =
    <div class="listimage">
      <div>
        <figure>
          <img
% (match klass with Some name ->
            class="<%s name %>"
% | None -> ());
            class="rounded"
            src="<%s filename %>"
% (match (alt) with Some desc ->
            alt="<%s desc %>"
% | None -> ());
          />
        </figure>
      </div>
    </div>

let render_photo reference =
    <p>TODO: photo link to <a href="/photos/<%s reference %>/"><%s reference %></a></p>

let render_chart _style filename xaxis yaxis =
  <div
% let identifier = Printf.sprintf "chart%d" (Random.int 1024) in
    id="<%s identifier %>">
  </div>
  <script>
    var vlSpec = {
      $schema: 'https://vega.github.io/schema/vega-lite/v6.json',
      mark: 'line',
      data: {"url": "<%s filename %>"},
      width: 1000,
      "encoding": {
        "x": {"field": "<%s xaxis %>", "type": "nominal",  "axis": {"labels": false}},
        "y": {"field": "<%s yaxis %>", "type": "quantitative"}
      }
    };
    vegaEmbed('#<%s identifier %>', vlSpec);
  </script>


let render_shortcode shortcode =
  match shortcode with
  | Shortcode.Video (filename, thumb_opt, looped) -> render_video filename thumb_opt looped
  | Shortcode.Audio (filename) -> render_audio filename
  | Shortcode.Youtube (code) -> render_youtube code
  | Shortcode.Raster (filename, alt, klass, dims) -> render_raster filename alt klass dims
  | Shortcode.Vector (filename, alt, klass) -> render_vector filename alt klass
  | Shortcode.Photo (reference) -> render_photo reference
  | Shortcode.Chart (style, filename, xaxis, yaxis) -> render_chart style filename xaxis yaxis
  | _ -> ""
