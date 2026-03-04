
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
    (width > (900 * 2)) && (height > (700 * 2))
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

let render_compare_rasters filename1 filename2 dims =
  <div class="compare"
% let identifier = Printf.sprintf "compare%d" (Random.int 1024) in
    id="<%s identifier %>"
  >
    <img class="before"
      src="<%s filename1 %>"
% let name, ext = Fpath.split_ext (Fpath.v filename1) in
% (match (is_image_retina dims) with true ->
% let retina_filename = Printf.sprintf "%s@2x%s" (Fpath.to_string name) ext in
          srcset="<%s retina_filename %> 2x, <%s filename1 %> 1x"
% | false -> ());
    alt="Before">
    <img class="after"
      src="<%s filename2 %>"
% let name, ext = Fpath.split_ext (Fpath.v filename2) in
% (match (is_image_retina dims) with true ->
% let retina_filename = Printf.sprintf "%s@2x%s" (Fpath.to_string name) ext in
          srcset="<%s retina_filename %> 2x, <%s filename2 %> 1x"
% | false -> ());
      alt="After">
    <div class="handle"></div>
    <span class="label label-before">Before</span>
    <span class="label label-after">After</span>
    <input type="range" min="0" max="100" value="50"
           oninput="updateCompare(this)">
  </div>
  <script>
  function updateCompare(slider) {
    const pct = slider.value;
    const wrap = slider.closest('.compare');
    wrap.querySelector('.after').style.clipPath = `inset(0 ${100 - pct}% 0 0)`;
    wrap.querySelector('.handle').style.left = `${pct}%`;
  }
  </script>

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

let render_geojson filename =
  <div
% let identifier = Printf.sprintf "geojson%d" (Random.int 1024) in
  id="<%s identifier %>" class="map">
  </div>
  <script>
    const map_<%s identifier %> = L.map('<%s identifier %>').setView([20.0, 0.0], 2);
    L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
      attribution: '© <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
    }).addTo(map_<%s identifier %>);
    fetch('<%s filename %>')
      .then(r => r.json())
      .then(data => {
        const layer = L.geoJSON(data, { style: { color: '#e63946', fillOpacity: 0.2 } })
          .addTo(map_<%s identifier %>);
        map_<%s identifier %>.fitBounds(layer.getBounds(), { padding: [20, 20], maxZoom: 14 });
      });
  </script>

let render_diagram code =
  let hash = Digest.string code |> Digest.to_hex in
  let filename = Printf.sprintf "%s.svg" hash in
  render_vector filename None (Some "d2")

let render_shortcode shortcode =
  match shortcode with
  | Shortcode.Video (filename, thumb_opt, looped) -> render_video filename thumb_opt looped
  | Shortcode.Audio (filename) -> render_audio filename
  | Shortcode.Youtube (code) -> render_youtube code
  | Shortcode.Raster (filename, alt, klass, dims) -> render_raster filename alt klass dims
  | Shortcode.Vector (filename, alt, klass) -> render_vector filename alt klass
  | Shortcode.Photo (reference) -> render_photo reference
  | Shortcode.Chart (style, filename, xaxis, yaxis) -> render_chart style filename xaxis yaxis
  | Shortcode.GeoJSON (filename) -> render_geojson filename
  | Shortcode.Diagram (code) -> render_diagram code
  | Shortcode.CompareRaster (filename1, filename2, dims) -> render_compare_rasters filename1 filename2 dims
  | _ -> ""
