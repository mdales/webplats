open Astring

let cmark_to_html : strict:bool -> safe:bool -> string -> string =
  fun ~strict ~safe md ->
    let doc = Cmarkit.Doc.of_string ~strict md in
    Cmarkit_html.of_doc ~safe doc
    
let render_body page = 
  let unrendered_markdown = Page.body page in
  let ordered_shortcodes = List.sort (fun ((a, _), _) ((b, _), _) -> b - a) (Page.shortcodes page) in
  let body = List.fold_left (fun body ((loc, len), shortcode) ->
    let rendered_shortcode = Shortcodes.render_shortcode shortcode in
    let before = String.with_index_range ~last:(loc - 1) body
    and after = String.with_index_range ~first:(loc + len) body in
    before ^ rendered_shortcode ^ after
  ) unrendered_markdown ordered_shortcodes in
  cmark_to_html ~strict:false ~safe:false body 

let render_head ~site ?sec ?page () =
  Head.render_head ~site ~sec ~page ()