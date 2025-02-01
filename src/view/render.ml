open Astring

let render_markdown_images doc =
  let open Cmarkit in
  let inline _ n =
    match n with
    | Inline.Image (node, meta) -> (
      let t = match (Inline.Link.text node) with
      | Inline.Text (s, _) -> Some s
      | _ -> None
      in
      let r = match (Inline.Link.reference node) with
      | `Inline (link, _) -> (
        let s, _ = Option.get (Link_definition.dest link) in s
      )
      | `Ref (_, _, _) -> "ref"
      in
      let shortcode = Shortcode.Raster (r, t, None, None) in
      let html = Shortcodes.render_shortcode shortcode in
      Mapper.ret (Inline.Raw_html (Block_line.tight_list_of_string html, meta))
    )
    | _ -> Mapper.default
  in
  let mapper = Mapper.make ~inline () in
  Mapper.map_doc mapper doc

let render_body page =
  let unrendered_markdown = Page.body page in
  let ordered_shortcodes = Page.shortcodes page
  |> List.filter_map (fun (pos, sc) ->
    match pos with
    | Some p -> Some (p, sc)
    | None -> None
  )
  |> List.sort (fun ((a, _), _) ((b, _), _) -> b - a)
  in
  let body = List.fold_left (fun body ((loc, len), shortcode) ->
    let rendered_shortcode = Shortcodes.render_shortcode shortcode in
    let before = String.with_index_range ~last:(loc - 1) body
    and after = String.with_index_range ~first:(loc + len) body in
    before ^ rendered_shortcode ^ after
  ) unrendered_markdown ordered_shortcodes in
  Cmarkit.Doc.of_string ~strict:false body
  |> render_markdown_images
  |> Hilite_markdown.transform
  |> Cmarkit_html.of_doc ~safe:false

let render_head ~site ?sec ?page () =
  Head.render_head ~site ~sec ~page ()
