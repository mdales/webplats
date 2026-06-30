open Htmlit

let render_video filename thumb_opt looped =
  let looped_at =
    match looped with true -> [ At.true' "looped" ] | false -> []
  and thumb_at =
    match thumb_opt with Some t -> [ At.v "poster" t ] | None -> []
  in
  let at = looped_at @ thumb_at in
  let html =
    El.div
      [
        El.video ~at
          [
            El.source ~at:[ At.src filename; At.type' "video/mp4" ] ();
            El.txt "Your browser does not support the video element.";
          ];
      ]
  in
  [ html ]

let render_audio filename =
  let html =
    El.div
      ~at:[ At.class' "listimage" ]
      [
        El.div
          [
            El.audio
              ~at:[ At.true' "controls" ]
              [
                El.source ~at:[ At.src filename; At.type' "audio/mp4" ] ();
                El.txt "Your browser does not support the audio element.";
              ];
          ];
      ]
  in
  [ html ]

let render_youtube code =
  let html =
    El.div
      ~at:[ At.class' "listimage" ]
      [
        El.div
          [
            El.iframe
              ~at:
                [
                  At.width 560;
                  At.height 315;
                  At.src
                    (Printf.sprintf "https://www.youtube-nocookie.com/embed/%s"
                       code);
                  At.title "YouTube video player";
                  At.v "frameborder" "0";
                  At.v "allow"
                    "accelerometer; autoplay; clipboard-write; \
                     encrypted-media; gyroscope; picture-in-picture";
                  At.true' "allowfullscreen";
                ]
              [];
          ];
      ]
  in
  [ html ]

let is_image_retina dims =
  match dims with
  | None -> true
  | Some (width, height) -> width > 900 * 2 && height > 700 * 2

let render_raster filename alt klass dims =
  let name, ext = Fpath.split_ext (Fpath.v filename) in
  let srcset =
    match is_image_retina dims with
    | true ->
        let retina_filename =
          Printf.sprintf "%s@2x%s" (Fpath.to_string name) ext
        in
        [
          At.v "srcset" (Printf.sprintf "%s 2x, %s 1x" retina_filename filename);
        ]
    | false -> []
  in

  let alttext = match alt with Some desc -> [ At.alt desc ] | None -> [] in

  let extraclass =
    match klass with Some name -> [ At.class' name ] | None -> []
  in

  let at =
    [ At.class' "rounded"; At.src filename ] @ srcset @ alttext @ extraclass
  in

  let html =
    El.div
      ~at:[ At.class' "listimage" ]
      [ El.div [ El.figure [ El.img ~at () ] ] ]
  in
  [ html ]

let render_compare_rasters filename1 filename2 label1 label2 dims =
  let identifier = Printf.sprintf "compare%d" (Random.int 1024) in

  let images =
    List.map
      (fun (filename, klass, label) ->
        let name, ext = Fpath.split_ext (Fpath.v filename) in
        let srcset =
          match is_image_retina dims with
          | true ->
              let retina_filename =
                Printf.sprintf "%s@2x%s" (Fpath.to_string name) ext
              in
              [
                At.v "srcset"
                  (Printf.sprintf "%s 2x, %s 1x" retina_filename filename);
              ]
          | false -> []
        in
        let at = [ At.class' klass; At.src filename; At.alt label ] @ srcset in
        El.img ~at ())
      [ (filename1, "before", label1); (filename2, "after", label2) ]
  in

  let html =
    El.div
      ~at:[ At.class' "compare"; At.id identifier ]
      (images
      @ [
          El.div ~at:[ At.class' "handle" ] [];
          El.span
            ~at:[ At.class' "label"; At.class' "label-before" ]
            [ El.txt label1 ];
          El.span
            ~at:[ At.class' "label"; At.class' "label-after" ]
            [ El.txt label2 ];
          El.input
            ~at:
              [
                At.type' "range";
                At.v "min" "0";
                At.v "max" "100";
                At.value "50";
                At.v "oninput" "updateCompare(this)";
              ]
            ();
        ])
  and script =
    El.script
      [
        El.unsafe_raw
          {|
function updateCompare(slider) {
    const pct = slider.value;
    const wrap = slider.closest('.compare');
    wrap.querySelector('.after').style.clipPath = `inset(0 ${100 - pct}% 0 0)`;
    wrap.querySelector('.handle').style.left = `${pct}%`;
    wrap.querySelector('.label-before').style.opacity = (100 - pct) / 100;
    wrap.querySelector('.label-after').style.opacity = pct / 100;
}
document.querySelectorAll('.compare input[type=range]')
  .forEach(s => updateCompare(s));
    |};
      ]
  in
  [ html; script ]

let render_vector filename alt klass =
  let customclass =
    match klass with Some name -> [ At.class' name ] | None -> []
  in
  let alttext = match alt with Some desc -> [ At.alt desc ] | None -> [] in
  let at = [ At.class' "rounded"; At.src filename ] @ customclass @ alttext in
  let html =
    El.div
      ~at:[ At.class' "listimage" ]
      [ El.div [ El.figure [ El.img ~at () ] ] ]
  in
  [ html ]

let render_photo reference =
  let url = Printf.sprintf "/photos/%s/" reference in
  let html =
    El.p
      [
        El.txt "TODO: photo link to ";
        El.a ~at:[ At.href url ] [ El.txt reference ];
      ]
  in
  [ html ]

let render_chart _style filename xaxis yaxis =
  let identifier = Printf.sprintf "chart%d" (Random.int 1024) in
  let html = El.div ~at:[ At.id identifier ] [] in
  let script =
    El.script
      [
        El.unsafe_raw
          (Printf.sprintf
             {|
    var vlSpec = {
      $schema: 'https://vega.github.io/schema/vega-lite/v6.json',
      mark: 'line',
      data: {"url": "%s"},
      width: 1000,
      "encoding": {
        "x": {"field": "%s", "type": "nominal",  "axis": {"labels": false}},
        "y": {"field": "%s", "type": "quantitative"}
      }
    };
    vegaEmbed('#%s', vlSpec);
    |}
             filename xaxis yaxis identifier);
      ]
  in
  [ html; script ]

let render_geojson filename =
  let identifier = Printf.sprintf "geojson%d" (Random.int 1024) in
  let html = El.div ~at:[ At.class' "map"; At.id identifier ] [] in
  let script =
    El.script
      [
        El.unsafe_raw
          (Printf.sprintf
             {|
  const map_%s = L.map('%s').setView([20.0, 0.0], 2);
  L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
    attribution: '© <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
  }).addTo(map_%s);
  fetch('%s')
    .then(r => r.json())
    .then(data => {
      const layer = L.geoJSON(data, { style: { color: '#e63946', fillOpacity: 0.2 } })
        .addTo(map_%s);
      map_%s.fitBounds(layer.getBounds(), { padding: [20, 20], maxZoom: 14 });
    });
    |}
             identifier identifier identifier filename identifier identifier);
      ]
  in
  [ html; script ]

let render_diagram code =
  let hash = Digest.string code |> Digest.to_hex in
  let filename = Printf.sprintf "%s.svg" hash in
  render_vector filename None (Some "d2")

let render_shortcode shortcode =
  match shortcode with
  | Shortcode.Video (filename, thumb_opt, looped) ->
      render_video filename thumb_opt looped
  | Shortcode.Audio filename -> render_audio filename
  | Shortcode.Youtube code -> render_youtube code
  | Shortcode.Raster (filename, alt, klass, dims) ->
      render_raster filename alt klass dims
  | Shortcode.Vector (filename, alt, klass) -> render_vector filename alt klass
  | Shortcode.Photo reference -> render_photo reference
  | Shortcode.Chart (style, filename, xaxis, yaxis) ->
      render_chart style filename xaxis yaxis
  | Shortcode.GeoJSON filename -> render_geojson filename
  | Shortcode.Diagram code -> render_diagram code
  | Shortcode.CompareRaster (filename1, filename2, label1, label2, dims) ->
      render_compare_rasters filename1 filename2 label1 label2 dims
  | _ -> []
